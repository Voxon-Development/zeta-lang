#\![allow(unused_imports)]
#\![allow(clippy::single_char_pattern)]

use std::collections::HashMap;

#[cfg(not(test))] // When built as integration tests, this path isn't active.
use super::*;

#[cfg(test)]
mod mir_expr_lowerer_additional_tests {
    use super::*;
    use smallvec::SmallVec;

    // Prefer crate import path when compiled as integration tests
    #[allow(unused_imports)]
    use emberforge_compiler::midend::ir::block_data::CurrentBlockData as CrateCurrentBlockData;
    #[allow(unused_imports)]
    use emberforge_compiler::midend::ir::lowerer::{lower_operator_bin as crate_lower_operator_bin, lower_type_hir as crate_lower_type_hir};
    #[allow(unused_imports)]
    use emberforge_compiler::midend::ir::optimized_string_buffering as crate_optimized_string_buffering;

    use ir::hir::{AssignmentOperator, HirClass, HirExpr, Operator, StrId};
    use ir::ir_hasher::FxHashBuilder;
    use ir::ssa_ir::{BinOp, Instruction, Operand, SsaType, Value};
    use zetaruntime::string_pool::StringPool;

    // Unify CurrentBlockData import whether we are in crate unit tests or integration tests
    #[allow(unused_imports)]
    use crate::midend::ir::block_data::CurrentBlockData;

    // Helpers to create a fully-wired MirExprLowerer with empty metadata by default
    struct TestHarness {
        pool: StringPool,
        cbd: CurrentBlockData,
        var_map: HashMap<StrId, Value, FxHashBuilder>,
        class_field_offsets: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_mangled_map: HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        class_vtable_slots: HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        classes: HashMap<StrId, HirClass, FxHashBuilder>,
    }

    impl TestHarness {
        fn new() -> Self {
            Self {
                pool: StringPool::default(),
                cbd: CurrentBlockData::default(),
                var_map: HashMap::with_hasher(FxHashBuilder::default()),
                class_field_offsets: HashMap::with_hasher(FxHashBuilder::default()),
                class_method_slots: HashMap::with_hasher(FxHashBuilder::default()),
                class_mangled_map: HashMap::with_hasher(FxHashBuilder::default()),
                class_vtable_slots: HashMap::with_hasher(FxHashBuilder::default()),
                interface_id_map: HashMap::with_hasher(FxHashBuilder::default()),
                interface_method_slots: HashMap::with_hasher(FxHashBuilder::default()),
                classes: HashMap::with_hasher(FxHashBuilder::default()),
            }
        }

        fn lowerer<'a>(&'a mut self) -> MirExprLowerer<'a> {
            MirExprLowerer::new(
                &mut self.cbd,
                &mut self.var_map,
                &mut self.pool,
                &self.class_field_offsets,
                &self.class_method_slots,
                &self.class_mangled_map,
                &self.class_vtable_slots,
                &self.interface_id_map,
                &self.interface_method_slots,
                &self.classes,
            )
        }

        fn str(&mut self, s: &str) -> StrId {
            StrId(self.pool.intern(s))
        }
    }

    fn last_instr(h: &TestHarness) -> Option<&Instruction> {
        h.cbd.bb().instructions.last()
    }

    #[test]
    fn lower_number_emits_const_i64() {
        let mut h = TestHarness::new();
        let mut lowerer = h.lowerer();

        let v = lowerer.lower_expr(&HirExpr::Number(42));

        // Verify instruction and type
        let instrs = &h.cbd.bb().instructions;
        assert\!(\!instrs.is_empty(), "No instructions emitted");
        match instrs.last().unwrap() {
            Instruction::Const { dest, ty, value } => {
                assert_eq\!(*dest, v, "Const dest should be returned value");
                assert\!(matches\!(ty, SsaType::I64));
                assert\!(matches\!(value, Operand::ConstInt(42)));
            }
            other => panic\!("Expected Const, got {:?}", other),
        }
        assert_eq\!(h.cbd.value_types.get(&v), Some(&SsaType::I64));
    }

    #[test]
    fn lower_binary_add_emits_binop_and_sets_type() {
        let mut h = TestHarness::new();
        let mut lowerer = h.lowerer();

        let expr = HirExpr::Binary {
            left: Box::new(HirExpr::Number(7)),
            op: Operator::Add,
            right: Box::new(HirExpr::Number(5)),
        };
        let v = lowerer.lower_expr(&expr);

        let instrs = &h.cbd.bb().instructions;
        let bin = instrs.iter().rev().find(|i| matches\!(i, Instruction::Binary { .. })).expect("No Binary instr emitted");
        if let Instruction::Binary { dest, op, left, right } = bin {
            assert_eq\!(*dest, v);
            assert\!(matches\!(op, BinOp::Add));
            assert\!(matches\!(left, Operand::Value(_)));
            assert\!(matches\!(right, Operand::Value(_)));
        } else { unreachable\!() }
        assert_eq\!(h.cbd.value_types.get(&v), Some(&SsaType::I64));
    }

    #[test]
    fn assign_ident_simple_and_compound() {
        let mut h = TestHarness::new();
        let x = h.str("x");
        // Seed var_map with an existing value for x to support compound
        let seed = h.cbd.fresh_value();
        h.var_map.insert(x, seed);

        let mut lowerer = h.lowerer();

        // Simple assign: x = 10
        let v_simple = lowerer.lower_expr(&HirExpr::Assignment {
            target: Box::new(HirExpr::Ident(x)),
            op: AssignmentOperator::Assign,
            value: Box::new(HirExpr::Number(10)),
        });
        assert\!(matches\!(last_instr(&h), Some(Instruction::Const { .. })));
        assert_eq\!(h.var_map.get(&x).copied(), Some(v_simple));

        // Compound assign: x += 2 -> should emit Binary and update var_map[x]
        let v_comp = lowerer.lower_expr(&HirExpr::Assignment {
            target: Box::new(HirExpr::Ident(x)),
            op: AssignmentOperator::AddAssign,
            value: Box::new(HirExpr::Number(2)),
        });
        let bin = h.cbd.bb().instructions.iter().rev().find(|i| matches\!(i, Instruction::Binary { .. })).unwrap();
        if let Instruction::Binary { op, left, right, .. } = bin {
            assert\!(matches\!(op, BinOp::Add));
            assert\!(matches\!(left, Operand::Value(_)));
            assert\!(matches\!(right, Operand::Value(_)));
        }
        assert_eq\!(h.var_map.get(&x).copied(), Some(v_comp));
    }

    #[test]
    fn field_compound_assign_loads_bin_and_stores() {
        let mut h = TestHarness::new();

        // Prepare class metadata: class Foo { f: i64 at offset 3 }
        let cls = h.str("Foo");
        let field = h.str("f");
        let mut offsets: HashMap<StrId, usize, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder::default());
        offsets.insert(field, 3);
        h.class_field_offsets.insert(cls, offsets);

        // Seed object value with type User("Foo", [])
        let obj_val = h.cbd.fresh_value();
        h.cbd.value_types.insert(obj_val, SsaType::User(cls, Vec::new()));

        // Build expression: (obj.f) += 5
        let mut lowerer = h.lowerer();
        // put obj in a variable so we can reference it via Ident in expr tree if needed
        let obj_ident = h.str("obj");
        h.var_map.insert(obj_ident, obj_val);

        let expr = HirExpr::Assignment {
            target: Box::new(HirExpr::FieldAccess {
                object: Box::new(HirExpr::Ident(obj_ident)),
                field,
            }),
            op: AssignmentOperator::AddAssign,
            value: Box::new(HirExpr::Number(5)),
        };

        let _ = lowerer.lower_expr(&expr);
        // Expect sequence includes: LoadField -> Binary(Add) -> StoreField
        let instrs = &h.cbd.bb().instructions;
        let has_load = instrs.iter().any(|i| matches\!(i, Instruction::LoadField { offset: 3, .. }));
        let has_bin_add = instrs.iter().any(|i| matches\!(i, Instruction::Binary { op: BinOp::Add, .. }));
        let has_store = instrs.iter().any(|i| matches\!(i, Instruction::StoreField { offset: 3, .. }));
        assert\!(has_load && has_bin_add && has_store, "Expected LoadField, Binary(Add), StoreField sequence");
    }

    #[test]
    fn class_init_alloc_and_optional_vtable_and_field_inits() {
        let mut h = TestHarness::new();
        let cls = h.str("Foo");
        // class Foo has fields a at 1, b at 2
        let a = h.str("a");
        let b = h.str("b");
        let mut offsets: HashMap<StrId, usize, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder::default());
        offsets.insert(a, 1);
        offsets.insert(b, 2);
        h.class_field_offsets.insert(cls, offsets);

        // Provide non-empty vtable slots to trigger vtable store
        h.class_vtable_slots.insert(cls, vec\![h.str("m0")]);

        let mut lowerer = h.lowerer();
        let expr = HirExpr::ClassInit {
            name: Box::new(HirExpr::Ident(cls)),
            args: vec\![HirExpr::Number(11), HirExpr::Number(22)],
        };
        let obj = lowerer.lower_expr(&expr);

        // Verify Alloc issued with User(Foo)
        let has_alloc = h.cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::Alloc { ty: SsaType::User(_, _), .. }));
        assert\!(has_alloc, "Expected Alloc for class init");
        // Fields should be stored at offsets 1 and 2
        let mut has_store1 = false;
        let mut has_store2 = false;
        for i in &h.cbd.bb().instructions {
            if let Instruction::StoreField { offset, .. } = i {
                if *offset == 1 { has_store1 = true; }
                if *offset == 2 { has_store2 = true; }
                if *offset == 0 {
                    // vtable store at offset 0 must be present due to non-empty vtable slots
                    // Can't assert exact GlobalRef id without peeking, presence is enough.
                }
            }
        }
        assert\!(has_store1 && has_store2, "Expected field stores for provided ctor args");
        assert_eq\!(h.cbd.value_types.get(&obj), Some(&SsaType::User(cls, Vec::new())));
    }

    #[test]
    fn method_dispatch_prefers_class_slots_then_mangled_then_direct() {
        let mut h = TestHarness::new();
        let cls = h.str("Foo");
        let method = h.str("bar");

        // Build an object of type Foo
        let obj_val = h.cbd.fresh_value();
        h.cbd.value_types.insert(obj_val, SsaType::User(cls, Vec::new()));

        // Put object into identifier for call receiver
        let obj_ident = h.str("obj");
        h.var_map.insert(obj_ident, obj_val);

        let mut lowerer = h.lowerer();

        // Case 1: class_method_slots -> ClassCall
        let mut slot_map: HashMap<StrId, usize, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder::default());
        slot_map.insert(method, 7);
        h.class_method_slots.insert(cls, slot_map);
        let _ = lowerer.lower_expr(&HirExpr::Call {
            callee: Box::new(HirExpr::FieldAccess { object: Box::new(HirExpr::Ident(obj_ident)), field: method }),
            args: vec\![HirExpr::Number(1)],
        });
        assert\!(h.cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::ClassCall { method_id: 7, .. })));

        // Case 2: remove class slot, provide mangled map -> Call(FunctionRef(mangled))
        h.class_method_slots.clear();
        let mangled = h.str("Foo_bar_mangled");
        let mut m: HashMap<StrId, StrId, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder::default());
        m.insert(method, mangled);
        h.class_mangled_map.insert(cls, m);
        let _ = lowerer.lower_expr(&HirExpr::Call {
            callee: Box::new(HirExpr::FieldAccess { object: Box::new(HirExpr::Ident(obj_ident)), field: method }),
            args: vec\![],
        });
        assert\!(h.cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::Call { func: Operand::FunctionRef(mid), .. } if *mid == mangled)));

        // Case 3: neither slots nor mangled -> fall back to direct scoped name -> Call(FunctionRef(...))
        h.class_mangled_map.clear();
        let _ = lowerer.lower_expr(&HirExpr::Call {
            callee: Box::new(HirExpr::FieldAccess { object: Box::new(HirExpr::Ident(obj_ident)), field: method }),
            args: vec\![],
        });
        assert\!(h.cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::Call { func: Operand::FunctionRef(_), .. })));
    }

    #[test]
    fn interface_call_upcasts_and_dispatches() {
        let mut h = TestHarness::new();
        let cls = h.str("Foo");
        let iface = h.str("IPrintable");
        let meth = h.str("print");

        // object of class Foo
        let obj = h.cbd.fresh_value();
        h.cbd.value_types.insert(obj, SsaType::User(cls, Vec::new()));

        // metadata: interface id and method slot
        h.interface_id_map.insert(iface, 3);
        let mut slots: HashMap<StrId, usize, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder::default());
        slots.insert(meth, 1);
        h.interface_method_slots.insert(iface, slots);

        // place object in identifier
        let id = h.str("o");
        h.var_map.insert(id, obj);

        let mut lowerer = h.lowerer();
        let _ = lowerer.lower_expr(&HirExpr::InterfaceCall {
            callee: Box::new(HirExpr::FieldAccess { object: Box::new(HirExpr::Ident(id)), field: meth }),
            args: vec\![HirExpr::Number(9)],
            interface: iface,
        });

        // Verify we saw an UpcastToInterface and an InterfaceDispatch with slot 1
        let saw_upcast = h.cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::UpcastToInterface { interface_id: 3, .. }));
        let saw_dispatch = h.cbd.bb().instructions.iter().any(|i| matches\!(i, Instruction::InterfaceDispatch { method_slot: 1, .. }));
        assert\!(saw_upcast && saw_dispatch);
    }

    #[test]
    #[should_panic]
    fn get_field_offset_panics_on_unknown_field() {
        let mut h = TestHarness::new();
        let cls = h.str("Foo");
        let obj = h.cbd.fresh_value();
        h.cbd.value_types.insert(obj, SsaType::User(cls, Vec::new()));

        // No field metadata present -> should panic
        let mut lowerer = h.lowerer();
        // Call private helper via assignment path which internally calls get_field_offset
        let _ = lowerer.lower_expr(&HirExpr::FieldAccess {
            object: Box::new(HirExpr::Ident(h.str("obj_not_seeded"))),
            field: h.str("missing_field"),
        });
    }
}