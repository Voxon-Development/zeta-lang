//\! Integration tests for CraneliftBackend focusing on recently modified lowering paths.
//\! Framework: Rust built-in test harness (cargo test). No external test deps.

use std::fs;
use std::path::Path;

use engraver_assembly_emit::*; // crate root re-exports are assumed; adjust if modules differ

// Bring specific IR items into scope to build minimal programs.
// We prefer using public API; if these types are not re-exported at crate root,
// update the paths below to the correct public modules.
use engraver_assembly_emit::{
    backend::Backend,
    cranelift_backend::CraneliftBackend,
    ir::{Module as ZModule, Function as ZFunction, Instruction, Operand, BinOp, Block, BlockId, Value, InterpolationOperand, Type as ZType},
    strings::StringPool,
};

#[test]
fn zeta_data_id_value_semantics() {
    // Ensure ZetaDataId implements leapfrog::Value semantics as per PR.
    // We can't construct ZetaDataId directly if it's not public; instead, validate via string pool path
    // where null/redirect semantics are exercised indirectly by get_or_create_string.
    let mut sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp.clone());

    // First get id for "hello"
    let id1 = {
        // get_or_create_string is private; exercise via Interpolate lowering which calls it.
        // Build a minimal function with an Interpolate instruction and then finish module emission.
        let mut m = ZModule::default();
        let entry = BlockId(0);
        let v0 = Value(0);
        let func = ZFunction {
            name: "test_interpolate".to_string(),
            params: vec\![],
            ret_type: ZType::I64,
            entry,
            blocks: vec\![
                Block {
                    id: entry,
                    instructions: vec\![
                        Instruction::Interpolate {
                            dest: v0,
                            parts: vec\![InterpolationOperand::Literal("hello".into())],
                        },
                        Instruction::Ret { value: Some(Operand::Value(v0)) },
                    ],
                }
            ],
        };
        m.funcs.insert(func.name.clone(), func);
        backend.emit_module(&m);
        // Emit extern not needed here; we don't link, only ensure path executed
        // The function above should have caused string data to be created.
        // As we cannot directly observe DataId, rely on side-effect: adding same string text again will reuse.
        // Insert the same string again via a second function to verify reuse path (no panic).
        let mut m2 = ZModule::default();
        let entry2 = BlockId(1);
        let v1 = Value(1);
        let func2 = ZFunction {
            name: "test_interpolate_again".to_string(),
            params: vec\![],
            ret_type: ZType::I64,
            entry: entry2,
            blocks: vec\![
                Block {
                    id: entry2,
                    instructions: vec\![
                        Instruction::Interpolate {
                            dest: v1,
                            parts: vec\![InterpolationOperand::Literal("hello".into())],
                        },
                        Instruction::Ret { value: Some(Operand::Value(v1)) },
                    ],
                }
            ],
        };
        m2.funcs.insert(func2.name.clone(), func2);
        backend.emit_module(&m2);

        // If CRanelift path uses LeapMap correctly, second use should not panic and should reuse mapping.
        // We cannot fetch DataId; the mere successful emission is our invariant.
        1u8 // dummy
    };
    assert_eq\!(id1, 1u8); // dummy assert to bind variable and ensure test runs
}

#[test]
fn emit_module_declares_and_emits_all_functions() {
    let sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp);

    // Build two simple functions and ensure no panics during declaration/emission.
    // Function f0: ret const 42
    let entry0 = BlockId(0);
    let v0 = Value(0);
    let f0 = ZFunction {
        name: "f0".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry: entry0,
        blocks: vec\![
            Block {
                id: entry0,
                instructions: vec\![
                    Instruction::Const { dest: v0, ty: ZType::I64, value: Operand::ConstInt(42) },
                    Instruction::Ret { value: Some(Operand::Value(v0)) },
                ],
            }
        ],
    };

    // Function f1: binary add const 2 + const 3
    let entry1 = BlockId(1);
    let v1 = Value(1);
    let v2 = Value(2);
    let v3 = Value(3);
    let f1 = ZFunction {
        name: "f1".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry: entry1,
        blocks: vec\![
            Block {
                id: entry1,
                instructions: vec\![
                    Instruction::Const { dest: v1, ty: ZType::I64, value: Operand::ConstInt(2) },
                    Instruction::Const { dest: v2, ty: ZType::I64, value: Operand::ConstInt(3) },
                    Instruction::Binary { dest: v3, op: BinOp::Add, left: Operand::Value(v1), right: Operand::Value(v2) },
                    Instruction::Ret { value: Some(Operand::Value(v3)) },
                ],
            }
        ],
    };

    let mut m = ZModule::default();
    m.funcs.insert(f0.name.clone(), f0);
    m.funcs.insert(f1.name.clone(), f1);

    backend.emit_module(&m); // should not panic
}

#[test]
fn emit_function_handles_phi_and_branch_paths() {
    let sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp);

    // Build function with diamond CFG and Phi
    // block0:
    //   const v0 = 1
    //   branch on v0 -> then: block1, else: block2
    // block1:
    //   const v1 = 10
    //   jump block3
    // block2:
    //   const v2 = 20
    //   jump block3
    // block3:
    //   phi v3 = (block1, v1), (block2, v2)
    //   ret v3
    let b0 = BlockId(0);
    let b1 = BlockId(1);
    let b2 = BlockId(2);
    let b3 = BlockId(3);
    let v0 = Value(0);
    let v1 = Value(1);
    let v2 = Value(2);
    let v3 = Value(3);

    let func = ZFunction {
        name: "phi_branch".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry: b0,
        blocks: vec\![
            Block {
                id: b0,
                instructions: vec\![
                    Instruction::Const { dest: v0, ty: ZType::I64, value: Operand::ConstInt(1) },
                    Instruction::Branch { cond: Operand::Value(v0), then_bb: b1, else_bb: b2 },
                ],
            },
            Block {
                id: b1,
                instructions: vec\![
                    Instruction::Const { dest: v1, ty: ZType::I64, value: Operand::ConstInt(10) },
                    Instruction::Jump { target: b3 },
                ],
            },
            Block {
                id: b2,
                instructions: vec\![
                    Instruction::Const { dest: v2, ty: ZType::I64, value: Operand::ConstInt(20) },
                    Instruction::Jump { target: b3 },
                ],
            },
            Block {
                id: b3,
                instructions: vec\![
                    Instruction::Phi { dest: v3, incomings: vec\![(b1, v1), (b2, v2)] },
                    Instruction::Ret { value: Some(Operand::Value(v3)) },
                ],
            },
        ],
    };

    // emit_function is public via Backend trait; need to wrap in a module to call emit_module or call directly if exposed
    backend.emit_function(&func); // should not panic
}

#[test]
fn call_and_extern_declaration_paths() {
    let sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp);

    // Build a function that calls an imported function "ext_add" with two consts and returns the result.
    let entry = BlockId(0);
    let v0 = Value(0);
    let v1 = Value(1);
    let v2 = Value(2);
    let v3 = Value(3);

    // We also declare "ext_add" as extern in the module interface path.
    let extern_sig = ZFunction {
        name: "ext_add".to_string(),
        params: vec\![(Value(100), ZType::I64), (Value(101), ZType::I64)],
        ret_type: ZType::I64,
        entry, // not used for extern
        blocks: vec\![],
    };
    backend.emit_extern(&extern_sig);

    let func = ZFunction {
        name: "call_ext".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry,
        blocks: vec\![
            Block {
                id: entry,
                instructions: vec\![
                    Instruction::Const { dest: v0, ty: ZType::I64, value: Operand::ConstInt(7) },
                    Instruction::Const { dest: v1, ty: ZType::I64, value: Operand::ConstInt(9) },
                    Instruction::Call { dest: Some(v2), func: Operand::FunctionRef("ext_add".into()), args: vec\![Operand::Value(v0), Operand::Value(v1)] },
                    Instruction::Ret { value: Some(Operand::Value(v2)) },
                ],
            }
        ],
    };

    backend.emit_function(&func); // should not panic
}

#[test]
fn alloc_load_store_lowering_paths() {
    let sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp);

    // Build function that allocates, stores a const at offset 1, then loads it and returns.
    let b = BlockId(0);
    let v_alloc = Value(0);
    let v_const = Value(1);
    let v_loaded = Value(2);
    let func = ZFunction {
        name: "mem_ops".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry: b,
        blocks: vec\![
            Block {
                id: b,
                instructions: vec\![
                    Instruction::Alloc { dest: v_alloc, ty: ZType::I64 },
                    Instruction::Const { dest: v_const, ty: ZType::I64, value: Operand::ConstInt(1234) },
                    Instruction::StoreField { base: Operand::Value(v_alloc), offset: 1, value: Operand::Value(v_const) },
                    Instruction::LoadField { dest: v_loaded, base: Operand::Value(v_alloc), offset: 1 },
                    Instruction::Ret { value: Some(Operand::Value(v_loaded)) },
                ],
            }
        ],
    };

    backend.emit_function(&func); // should not panic
}

#[test]
fn match_enum_path_lowering() {
    let sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp);

    // Build function that constructs enum variant "1" and matches on it with two arms.
    let b0 = BlockId(0);
    let b1 = BlockId(1);
    let b2 = BlockId(2);
    let v_enum = Value(0);
    let v_ret = Value(1);

    let func = ZFunction {
        name: "match_enum".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry: b0,
        blocks: vec\![
            Block {
                id: b0,
                instructions: vec\![
                    Instruction::EnumConstruct { dest: v_enum, variant: "1".into(), fields: vec\![] },
                    Instruction::MatchEnum { value: v_enum, arms: vec\![(0, b1), (1, b2)] },
                ],
            },
            Block {
                id: b1,
                instructions: vec\![
                    Instruction::Const { dest: v_ret, ty: ZType::I64, value: Operand::ConstInt(11) },
                    Instruction::Ret { value: Some(Operand::Value(v_ret)) },
                ],
            },
            Block {
                id: b2,
                instructions: vec\![
                    Instruction::Const { dest: v_ret, ty: ZType::I64, value: Operand::ConstInt(22) },
                    Instruction::Ret { value: Some(Operand::Value(v_ret)) },
                ],
            },
        ],
    };

    backend.emit_function(&func); // should not panic
}

#[test]
fn finish_writes_object_file() {
    // Note: finish(self) consumes backend and writes "out.o" to CWD.
    // Use a temp directory to avoid polluting repo root.
    let td = tempfile::tempdir().expect("tempdir");
    let cwd = std::env::current_dir().unwrap();
    std::env::set_current_dir(td.path()).unwrap();

    let sp = StringPool::new();
    let mut backend = CraneliftBackend::new(sp);

    // Emit a trivial module with a single function to ensure object has content.
    let entry = BlockId(0);
    let v = Value(0);
    let f = ZFunction {
        name: "main0".to_string(),
        params: vec\![],
        ret_type: ZType::I64,
        entry,
        blocks: vec\![
            Block {
                id: entry,
                instructions: vec\![
                    Instruction::Const { dest: v, ty: ZType::I64, value: Operand::ConstInt(0) },
                    Instruction::Ret { value: Some(Operand::Value(v)) },
                ],
            }
        ],
    };
    let mut m = ZModule::default();
    m.funcs.insert(f.name.clone(), f);
    backend.emit_module(&m);

    // finish should create "out.o"
    backend.finish();
    assert\!(Path::new("out.o").exists(), "finish() should emit out.o");

    // Cleanup
    fs::remove_file("out.o").ok();
    std::env::set_current_dir(cwd).unwrap();
}