// Integration tests for CraneliftBackend focusing on recent diff changes.
// Framework: Rust built-in test harness (cargo test). No new dev-dependencies.
//
// These tests emphasize:
// - String interning behavior via CraneliftBackend::get_or_create_string (id stability, empty/unicode).
// - Smoke coverage of emit_function paths: Const/Binary/Ret, Phi fallback, Call, Interpolate.
// - Smoke coverage of emit_module with a minimal module.
//
// Note: ZetaDataId is an internal type; we validate string/data paths via public APIs instead.

#\![cfg(test)]
#\![allow(unused_imports)]
#\![allow(clippy::needless_return)]

use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;

use zetaruntime::string_pool::StringPool;

use ir::ssa_ir::{
    BinOp, Block, BlockId, Function, Instruction, InterpolationOperand, Module, Operand, Type, Value,
};

fn new_backend() -> CraneliftBackend {
    let sp = StringPool::new().expect("StringPool::new should succeed for tests");
    CraneliftBackend::new(sp)
}

mod string_intern_tests {
    use super::*;

    #[test]
    fn same_literal_returns_same_data_id() {
        let mut be = new_backend();
        let id1 = be.get_or_create_string("hello");
        let id2 = be.get_or_create_string("hello");
        assert_eq\!(id1, id2, "Same string must intern to the same DataId");

        let id3 = be.get_or_create_string("world");
        assert_ne\!(id1, id3, "Different strings should produce different DataIds");
    }

    #[test]
    fn handles_empty_and_unicode() {
        let mut be = new_backend();

        let e1 = be.get_or_create_string("");
        let e2 = be.get_or_create_string("");
        assert_eq\!(e1, e2, "Empty string interning should be stable");

        let u1 = be.get_or_create_string("héllo-世界");
        let u2 = be.get_or_create_string("héllo-世界");
        assert_eq\!(u1, u2, "Unicode interning should be stable");
    }
}

mod emit_function_smoke_paths {
    use super::*;

    #[test]
    fn const_binary_ret() {
        let mut be = new_backend();
        let a = Value(10);
        let b = Value(11);
        let c = Value(12);

        let f = Function {
            name: "add2".into(),
            params: vec\![(a, Type::I64), (b, Type::I64)],
            ret_type: Type::I64,
            entry: BlockId(0),
            blocks: vec\![Block {
                id: BlockId(0),
                instructions: vec\![
                    Instruction::Binary {
                        dest: c,
                        op: BinOp::Add,
                        left: Operand::Value(a),
                        right: Operand::Value(b),
                    },
                    Instruction::Ret {
                        value: Some(Operand::Value(c)),
                    },
                ],
            }],
        };

        // Should lower without panicking.
        be.emit_function(&f);
    }

    #[test]
    fn phi_join_with_missing_incoming_falls_back_to_zero() {
        let mut be = new_backend();

        let x = Value(1);       // parameter used as branch condition
        let else_val = Value(2);
        let phi_out = Value(3);

        let entry = Block {
            id: BlockId(0),
            instructions: vec\![Instruction::Branch {
                cond: Operand::Value(x), // use param (lowered by backend) rather than ConstInt to avoid unimplemented\!()
                then_bb: BlockId(1),
                else_bb: BlockId(2),
            }],
        };
        let then_bb = Block {
            id: BlockId(1),
            instructions: vec\![Instruction::Jump { target: BlockId(3) }],
        };
        let else_bb = Block {
            id: BlockId(2),
            instructions: vec\![
                Instruction::Const {
                    dest: else_val,
                    ty: Type::I64,
                    value: Operand::ConstInt(42),
                },
                Instruction::Jump { target: BlockId(3) },
            ],
        };
        let join = Block {
            id: BlockId(3),
            instructions: vec\![
                // Missing incoming from then_bb intentionally triggers zero-fallback path in lowering
                Instruction::Phi {
                    dest: phi_out,
                    incomings: vec\![(BlockId(2), else_val)],
                },
                Instruction::Ret {
                    value: Some(Operand::Value(phi_out)),
                },
            ],
        };

        let f = Function {
            name: "phi_join".into(),
            params: vec\![(x, Type::I64)],
            ret_type: Type::I64,
            entry: BlockId(0),
            blocks: vec\![entry, then_bb, else_bb, join],
        };

        // Should lower without panicking.
        be.emit_function(&f);
    }

    #[test]
    fn call_and_interpolate_paths() {
        let mut be = new_backend();

        let out = Value(20);
        let f = Function {
            name: "interp".into(),
            params: vec\![],
            ret_type: Type::I64,
            entry: BlockId(0),
            blocks: vec\![Block {
                id: BlockId(0),
                instructions: vec\![
                    Instruction::Interpolate {
                        dest: out,
                        parts: vec\![
                            InterpolationOperand::Literal("Hello".into()),
                            InterpolationOperand::Literal(", world".into()),
                        ],
                    },
                    Instruction::Call {
                        dest: None,
                        func: Operand::FunctionRef("callee".into()),
                        args: vec\![Operand::ConstInt(1), Operand::ConstInt(2)],
                    },
                    Instruction::Ret {
                        value: Some(Operand::Value(out)),
                    },
                ],
            }],
        };

        be.emit_function(&f);

        // Validate interning via public API (indirectly covers string data path).
        let id1 = be.get_or_create_string("Hello");
        let id2 = be.get_or_create_string(", world");
        assert_ne\!(id1, id2, "Different literals should map to distinct data ids");
    }
}

mod emit_module_smoke {
    use super::*;

    #[test]
    fn single_function_module_emits_without_panic() {
        let mut be = new_backend();

        let a = Value(1);
        let b = Value(2);
        let c = Value(3);

        let func = Function {
            name: "foo".into(),
            params: vec\![(a, Type::I64), (b, Type::I64)],
            ret_type: Type::I64,
            entry: BlockId(0),
            blocks: vec\![Block {
                id: BlockId(0),
                instructions: vec\![
                    Instruction::Binary {
                        dest: c,
                        op: BinOp::Add,
                        left: Operand::Value(a),
                        right: Operand::Value(b),
                    },
                    Instruction::Ret {
                        value: Some(Operand::Value(c)),
                    },
                ],
            }],
        };

        let mut m = Module { funcs: std::collections::BTreeMap::new() };
        m.funcs.insert(func.name.clone(), func);

        // Should declare and emit functions without panicking.
        be.emit_module(&m);
    }
}