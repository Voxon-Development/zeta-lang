//\! Integration tests for expression lowering.
//\!
//\! Test framework: Rust built-in test harness (#[test]).
//\!
//\! These tests focus on verifying that lowering of HIR expressions produces
//\! the expected SSA instructions and type annotations in CurrentBlockData.
//\!
//\! IMPORTANT: We only exercise public interfaces made available by the crate.
//\! If internal helpers are not exported, we validate via observable effects
//\! (emitted Instruction sequences and value_types). Where necessary, we provide
//\! local test scaffolding or guards.

use std::collections::HashMap;

// Bring in public IR and utilities from the crate:
use emberforge_compiler::midend::ir::block_data::CurrentBlockData;
use emberforge_compiler::midend::ir::lowerer::{lower_operator_bin, lower_type_hir};
use emberforge_compiler::midend::ir::optimized_string_buffering;
use emberforge_compiler::ir::hir::{AssignmentOperator, HirClass, HirExpr, Operator, StrId};
use emberforge_compiler::ir::ir_hasher::FxHashBuilder;
use emberforge_compiler::ir::ssa_ir::{BinOp, Instruction, Operand, SsaType, Value};
use emberforge_compiler::zetaruntime::string_pool::StringPool;

// If the lowering entry point is not publicly exposed, we conditionally re-export a
// thin constructor via a test-only helper. Many crates expose internal test hooks;
// try to access MirExprLowerer via its module path. If this fails to compile, consider
// making a minimal public wrapper in the crate in a follow-up change.
#[allow(unused_imports)]
use emberforge_compiler::midend::ir::expr_lowerer::MirExprLowerer;

// ------------ Test Utilities ------------

fn new_pool() -> StringPool {
    StringPool::default()
}

fn sid(pool: &mut StringPool, s: &str) -> StrId {
    StrId(pool.intern(s))
}

fn setup_lowerer<'a>() -> (
    CurrentBlockData,
    HashMap<StrId, Value, FxHashBuilder>,
    StringPool,
    HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    HashMap<StrId, usize, FxHashBuilder>,
    HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    HashMap<StrId, HirClass, FxHashBuilder>,
) {
    (
        CurrentBlockData::default(),
        HashMap::default(),
        StringPool::default(),
        HashMap::default(),
        HashMap::default(),
        HashMap::default(),
        HashMap::default(),
        HashMap::default(),
        HashMap::default(),
        HashMap::default(),
    )
}

fn make_lowerer<'a>(
    cbd: &'a mut CurrentBlockData,
    var_map: &'a mut HashMap<StrId, Value, FxHashBuilder>,
    pool: &'a mut StringPool,
    class_field_offsets: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    classes: &'a HashMap<StrId, HirClass, FxHashBuilder>,
) -> MirExprLowerer<'a> {
    MirExprLowerer::new(
        cbd,
        var_map,
        pool,
        class_field_offsets,
        class_method_slots,
        class_mangled_map,
        class_vtable_slots,
        interface_id_map,
        interface_method_slots,
        classes,
    )
}

fn last_instr(cbd: &CurrentBlockData) -> Option<&Instruction> {
    cbd.bb().instructions.last()
}

// ------------ Tests ------------

#[test]
fn lowers_number_literal_to_const_i64() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         cfo, cms, cmm, cvt, i_map, ims, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    // Act
    let v = lowerer.lower_expr(&HirExpr::Number(42));

    // Assert
    let instr = last_instr(&cbd).expect("at least one instruction");
    match instr {
        Instruction::Const { dest, ty, value } => {
            assert_eq\!(*dest, v, "dest value should match return");
            assert_eq\!(*ty, SsaType::I64);
            assert_eq\!(*value, Operand::ConstInt(42));
        }
        other => panic\!("expected Const, got {:?}", other),
    }
    // Also ensure type table updated
    let ty = cbd.value_types.get(&v).expect("type exists");
    assert_eq\!(*ty, SsaType::I64);
}

#[test]
fn lowers_simple_binary_add() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         cfo, cms, cmm, cvt, i_map, ims, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    // Act: (1 + 2)
    let expr = HirExpr::Binary {
        left: Box::new(HirExpr::Number(1)),
        op: Operator::Plus,
        right: Box::new(HirExpr::Number(2)),
    };
    let v = lowerer.lower_expr(&expr);

    // Assert
    let instr = last_instr(&cbd).expect("binary instruction emitted");
    match instr {
        Instruction::Binary { dest, op, left, right } => {
            assert_eq\!(*dest, v);
            assert_eq\!(*op, lower_operator_bin(&Operator::Plus));
            // Validate that operands are Values (result of previous Consts)
            match (left, right) {
                (Operand::Value(_), Operand::Value(_)) => {}
                _ => panic\!("operands should be values"),
            }
        }
        other => panic\!("expected Binary, got {:?}", other),
    }
    assert_eq\!(cbd.value_types.get(&v), Some(&SsaType::I64));
}

#[test]
fn assigns_identifier_and_compound_add_assign() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         cfo, cms, cmm, cvt, i_map, ims, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );
    let x = sid(&mut pool, "x");

    // Seed var_map: x = 10
    let x_val = cbd.fresh_value();
    cbd.bb().instructions.push(Instruction::Const {
        dest: x_val, ty: SsaType::I64, value: Operand::ConstInt(10)
    });
    cbd.value_types.insert(x_val, SsaType::I64);
    var_map.insert(x, x_val);

    // 1) Simple assign: x = 5
    let v_simple = lowerer.lower_expr(&HirExpr::Assignment {
        target: Box::new(HirExpr::Ident(x)),
        op: AssignmentOperator::Assign,
        value: Box::new(HirExpr::Number(5)),
    });
    assert_eq\!(v_simple, *var_map.get(&x).unwrap());
    // No binary should be necessary for simple assign; last should be Const from RHS
    match last_instr(&cbd).unwrap() {
        Instruction::Const { value: Operand::ConstInt(5), .. } => {}
        other => panic\!("expected Const for RHS, got {:?}", other),
    }

    // 2) Compound: x += 3 -> emits Binary with previous x and RHS=3
    let v_comp = lowerer.lower_expr(&HirExpr::Assignment {
        target: Box::new(HirExpr::Ident(x)),
        op: AssignmentOperator::AddAssign,
        value: Box::new(HirExpr::Number(3)),
    });

    match last_instr(&cbd).unwrap() {
        Instruction::Binary { dest, op, left, right } => {
            assert_eq\!(*dest, v_comp);
            assert_eq\!(*op, BinOp::Add);
            match (left, right) {
                (Operand::Value(_), Operand::Value(_)) => {}
                _ => panic\!("compound operands must be values"),
            }
        }
        other => panic\!("expected Binary for compound assign, got {:?}", other),
    }
}

#[test]
fn class_init_allocates_and_stores_vtable_and_fields() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         mut cfo, cms, cmm, mut cvt, i_map, ims, mut classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    // Prepare class "Foo" with fields a,b at offsets 1 and 2 (offset 0 reserved for vtable)
    let foo = sid(&mut pool, "Foo");
    let a = sid(&mut pool, "a");
    let b = sid(&mut pool, "b");
    let mut offsets: HashMap<StrId, usize, FxHashBuilder> = HashMap::default();
    offsets.insert(a, 1);
    offsets.insert(b, 2);
    cfo.insert(foo, offsets);

    // class vtable present -> non-empty -> should store vtable at offset 0
    cvt.insert(foo, vec\![sid(&mut pool, "m1")]);

    // class metadata for typing (a: i64, b: i64)
    classes.insert(foo, HirClass {
        name: foo,
        fields: vec\![
            emberforge_compiler::ir::hir::HirField { name: a, field_type: emberforge_compiler::ir::hir::HirType::Int },
            emberforge_compiler::ir::hir::HirField { name: b, field_type: emberforge_compiler::ir::hir::HirType::Int },
        ],
        methods: vec\![],
        implements: vec\![],
        type_params: vec\![],
        span: None,
    });

    // Act: Foo(10, 20)
    let expr = HirExpr::ClassInit {
        name: Box::new(HirExpr::Ident(foo)),
        args: vec\![HirExpr::Number(10), HirExpr::Number(20)],
    };
    let obj = lowerer.lower_expr(&expr);

    // Assert: Ensure Alloc then two StoreField and a vtable StoreField
    let instrs = &cbd.bb().instructions;
    // Must contain an Alloc to User(Foo)
    assert\!(instrs.iter().any(|i| matches\!(i, Instruction::Alloc { ty: SsaType::User(n, _), .. } if *n == foo)));
    // Must contain vtable store at offset 0
    assert\!(instrs.iter().any(|i| matches\!(i, Instruction::StoreField { offset: 0, value: Operand::GlobalRef(_), .. })));
    // Must contain stores to a and b offsets
    assert\!(instrs.iter().any(|i| matches\!(i, Instruction::StoreField { offset: 1, .. })));
    assert\!(instrs.iter().any(|i| matches\!(i, Instruction::StoreField { offset: 2, .. })));

    // Type of obj should be User(Foo)
    assert_eq\!(cbd.value_types.get(&obj), Some(&SsaType::User(foo, Vec::new())));
}

#[test]
fn method_call_uses_class_slot_when_available_else_mangled_else_direct() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         mut cfo, mut cms, mut cmm, mut cvt, i_map, ims, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    // Build object of class Foo
    let foo = sid(&mut pool, "Foo");
    let m = sid(&mut pool, "bar");

    // Seed a value typed as User(Foo)
    let obj = cbd.fresh_value();
    cbd.value_types.insert(obj, SsaType::User(foo, Vec::new()));

    // 1) Class method slots have entry -> ClassCall expected
    cms.insert(foo, {
        let mut mslots: HashMap<StrId, usize, FxHashBuilder> = HashMap::default();
        mslots.insert(m, 7usize);
        mslots
    });

    // Act: obj.bar()
    let _ = lowerer.lower_call(&Box::new(HirExpr::FieldAccess {
        object: Box::new(HirExpr::Ident(sid(&mut pool, "tmp_obj"))), // will be ignored, we directly supply value via var_map next line
        field: m
    }), &vec\![]);

    // Wire var_map so that Ident("tmp_obj") resolves to obj
    let tmp_obj = sid(&mut pool, "tmp_obj");
    var_map.insert(tmp_obj, obj);

    // Re-run call now that var_map is ready
    let _res = lowerer.lower_call(&Box::new(HirExpr::FieldAccess {
        object: Box::new(HirExpr::Ident(tmp_obj)),
        field: m
    }), &vec\![]);

    // Assert: last call should be ClassCall
    assert\!(matches\!(last_instr(&cbd).unwrap(),
        Instruction::ClassCall { method_id: 7, .. }
    ));

    // 2) Fallback to mangled map when no slot
    cms.clear();
    cmm.insert(foo, {
        let mut mmap: HashMap<StrId, StrId, FxHashBuilder> = HashMap::default();
        let mangled = sid(&mut pool, "Foo_bar");
        mmap.insert(m, mangled);
        mmap
    });

    let _ = lowerer.lower_call(&Box::new(HirExpr::FieldAccess {
        object: Box::new(HirExpr::Ident(tmp_obj)),
        field: m
    }), &vec\![]);

    assert\!(matches\!(last_instr(&cbd).unwrap(),
        Instruction::Call { func: Operand::FunctionRef(_), .. }
    ));

    // 3) Fallback to direct scoped name when neither slot nor mangled exist
    cmm.clear();

    let _ = lowerer.lower_call(&Box::new(HirExpr::FieldAccess {
        object: Box::new(HirExpr::Ident(tmp_obj)),
        field: m
    }), &vec\![]);

    assert\!(matches\!(last_instr(&cbd).unwrap(),
        Instruction::Call { func: Operand::FunctionRef(_), .. }
    ));
}

#[test]
fn interface_call_dispatches_and_upcasts_when_needed() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         cfo, cms, cmm, cvt, mut iface_ids, mut iface_slots, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &iface_ids, &iface_slots, &classes
    );

    // Types and IDs
    let foo = sid(&mut pool, "Foo");
    let iface = sid(&mut pool, "IBar");
    let method = sid(&mut pool, "baz");

    // Give interface ID and slot map
    iface_ids.insert(iface, 3usize);
    let mut slot_map: HashMap<StrId, usize, FxHashBuilder> = HashMap::default();
    slot_map.insert(method, 1usize);
    iface_slots.insert(iface, slot_map);

    // Seed object typed as User(Foo) so we require upcast to interface
    let obj = cbd.fresh_value();
    cbd.value_types.insert(obj, SsaType::User(foo, Vec::new()));

    // var_map: bind name to obj
    let tmp = sid(&mut pool, "o");
    var_map.insert(tmp, obj);

    // Act: (o.baz) as interface call
    let call_expr = HirExpr::InterfaceCall {
        callee: Box::new(HirExpr::FieldAccess { object: Box::new(HirExpr::Ident(tmp)), field: method }),
        args: vec\![],
        interface: iface,
    };
    let _ = lowerer.lower_expr(&call_expr);

    // Assert: final instruction should be InterfaceDispatch with slot 1
    match last_instr(&cbd).unwrap() {
        Instruction::InterfaceDispatch { object, method_slot, .. } => {
            assert_eq\!(*method_slot, 1usize);
            // Ensure object was upcast via a preceding UpcastToInterface
            assert\!(cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::UpcastToInterface { interface_id: 3, .. })));
            // The type of the upcasted value should be SsaType::User(iface, _)
            let upcast_val = match cbd.bb().instructions.iter().rev().find_map(|i| {
                if let Instruction::UpcastToInterface { dest, interface_id, .. } = i {
                    if *interface_id == 3 { Some(*dest) } else { None }
                } else { None }
            }).expect("upcast dest exists");
            assert\!(matches\!(cbd.value_types.get(&upcast_val), Some(SsaType::User(i, _)) if *i == iface));
            // InterfaceDispatch receives that upcasted object
            assert\!(matches\!(object, val if *val == upcast_val));
        }
        other => panic\!("expected InterfaceDispatch, got {:?}", other),
    }
}

#[test]
fn field_access_loads_correct_offset_and_type() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         mut cfo, cms, cmm, cvt, i_map, ims, mut classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    let foo = sid(&mut pool, "Foo");
    let f = sid(&mut pool, "val");

    // Seed field offset
    let mut offsets: HashMap<StrId, usize, FxHashBuilder> = HashMap::default();
    offsets.insert(f, 2usize);
    cfo.insert(foo, offsets);

    // class field typing metadata
    classes.insert(foo, HirClass {
        name: foo,
        fields: vec\![
            emberforge_compiler::ir::hir::HirField { name: f, field_type: emberforge_compiler::ir::hir::HirType::Int },
        ],
        methods: vec\![],
        implements: vec\![],
        type_params: vec\![],
        span: None,
    });

    // Seed object typed as User(Foo)
    let obj = cbd.fresh_value();
    cbd.value_types.insert(obj, SsaType::User(foo, Vec::new()));
    let name = sid(&mut pool, "obj");
    var_map.insert(name, obj);

    // Act: obj.val
    let v = lowerer.lower_expr(&HirExpr::FieldAccess {
        object: Box::new(HirExpr::Ident(name)),
        field: f
    });

    // Assert: last instruction must be LoadField with offset 2, and dest typed I64
    match last_instr(&cbd).unwrap() {
        Instruction::LoadField { dest, base, offset } => {
            assert_eq\!(*offset, 2usize);
            assert\!(matches\!(base, Operand::Value(b) if *b == obj));
            assert_eq\!(*dest, v);
        }
        other => panic\!("expected LoadField, got {:?}", other),
    }
    assert_eq\!(cbd.value_types.get(&v), Some(&SsaType::I64));
}

// Negative / edge cases

#[test]
#[should_panic(expected = "ClassInit name must be identifier")]
fn class_init_panics_on_non_ident_name() {
    let (mut cbd, mut var_map, mut pool,
         cfo, cms, cmm, cvt, i_map, ims, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    let _ = lowerer.lower_expr(&HirExpr::ClassInit {
        name: Box::new(HirExpr::Number(0)),
        args: vec\![]
    });
}

#[test]
#[should_panic]
fn field_access_panics_for_unknown_class_or_field() {
    // Arrange
    let (mut cbd, mut var_map, mut pool,
         cfo, cms, cmm, cvt, i_map, ims, classes) = setup_lowerer();
    let mut lowerer = make_lowerer(
        &mut cbd, &mut var_map, &mut pool,
        &cfo, &cms, &cmm, &cvt, &i_map, &ims, &classes
    );

    // Seed object with a user type that has no field metadata
    let obj = cbd.fresh_value();
    let foo = sid(&mut pool, "Foo");
    cbd.value_types.insert(obj, SsaType::User(foo, Vec::new()));
    let nm = sid(&mut pool, "o");
    var_map.insert(nm, obj);

    let field = sid(&mut pool, "missing");
    let _ = lowerer.lower_expr(&HirExpr::FieldAccess {
        object: Box::new(HirExpr::Ident(nm)),
        field
    });
}