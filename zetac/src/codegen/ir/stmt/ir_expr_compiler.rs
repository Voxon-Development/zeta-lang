use std::ops::Deref;
use ir::bump::{AtomicBump, Bump};
use ir::Bytecode;
use crate::ast::*;
use crate::codegen::ir::ir_buffer::ByteWriter;
use crate::codegen::ir::ir_compiler::ClassTable;
use crate::codegen::ir::stmt::ir_stmt_compiler::parse_ast_op_to_ir;

pub struct IRExprCompiler;

impl IRExprCompiler {
    pub fn compile_expr(&self, expr: &Expr, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut ir = ByteWriter::new();

        match expr {
            Expr::Number(n) => {
                ir.write_u8(Bytecode::PushInt as u8);
                ir.write_i64(*n);
            }

            Expr::Boolean(b) => {
                ir.write_u8(Bytecode::PushBool as u8);
                ir.write_u8(*b as u8);
            }

            Expr::String(s) => {
                ir.write_u8(Bytecode::PushStr as u8);
                ir.write_string(s);
            }

            Expr::Ident(ident) => {
                if classes.name_to_id.contains_key(ident) {
                    // It's a class name, used in a ClassInit
                    // Don't emit anything — ClassInit will handle it
                } else {
                    ir.write_u8(Bytecode::LoadVar as u8);
                    ir.write_string(ident);
                }
            }

            Expr::ClassInit { callee, arguments } => {
                for arg in arguments {
                    ir.extend(self.compile_expr(arg, classes)); // Push args first
                }

                ir.write_u8(Bytecode::ClassInit as u8);

                match callee.deref() {
                    Expr::Ident(ident) => {
                        ir.write_u64(*classes.name_to_id.get(ident).unwrap() as u64); // Write class ID
                    }
                    _ => panic!("Unsupported class init expression: {:#?}", callee),
                }

                ir.write_u8(arguments.len() as u8); // Write arity (if required)
            }

            Expr::Binary { left, op, right } => {
                ir.extend(self.compile_expr(left, classes));
                ir.extend(self.compile_expr(right, classes));
                ir.write_u8(parse_ast_op_to_ir(op));
            }

            Expr::FieldAccess { object, field } => {
                ir.extend(self.compile_expr(object, classes));
                ir.write_u8(Bytecode::LoadField as u8);
                ir.write_string(field);
            }

            Expr::Assignment { lhs, op, rhs } => {
                match (&**lhs, op) {
                    (Expr::Ident(ident), Op::Assign) => {
                        ir.extend(self.compile_expr(rhs, classes)); // compute RHS
                        ir.write_u8(Bytecode::StoreVar as u8);
                        ir.write_string(ident);
                    }
                    (Expr::FieldAccess { object, field }, Op::Assign) => {
                        ir.extend(self.compile_expr(object, classes));
                        ir.extend(self.compile_expr(rhs, classes));
                        ir.write_u8(Bytecode::StoreField as u8);
                        ir.write_string(field);
                    }
                    (Expr::FieldAccess { object, field }, compound_op) => {
                        ir.extend(self.compile_expr(object, classes));
                        ir.write_u8(Bytecode::Dup as u8);

                        ir.write_u8(Bytecode::LoadField as u8);
                        ir.write_string(field);

                        ir.extend(self.compile_expr(rhs, classes));

                        ir.write_u8(parse_ast_op_to_ir(compound_op));

                        ir.write_u8(Bytecode::StoreField as u8);
                        ir.write_string(field);
                    }

                    (Expr::Ident(ident), compound_op) => {
                        // For compound assignments like +=, -=, etc.
                        ir.write_u8(Bytecode::LoadVar as u8);
                        ir.write_string(ident); // load existing value
                        ir.extend(self.compile_expr(rhs, classes)); // compute RHS
                        ir.write_u8(parse_ast_op_to_ir(compound_op)); // apply binary op
                        ir.write_u8(Bytecode::StoreVar as u8);
                        ir.write_string(ident); // re-store
                    }
                    (Expr::ArrayIndex { array, index }, Op::Assign) => {
                        ir.extend(self.compile_expr(array, classes));
                        ir.extend(self.compile_expr(index, classes));
                        ir.extend(self.compile_expr(rhs, classes));
                        ir.write_u8(Bytecode::ArraySet as u8);
                    }
                    _ => unimplemented!("Unsupported assignment LHS: {:?}", lhs),
                }
            }

            Expr::Call { callee, arguments } => {
                for arg in arguments {
                    ir.extend(self.compile_expr(arg, classes));
                }

                match &**callee {
                    Expr::Ident(ident) => {
                        ir.write_u8(Bytecode::Call as u8);
                        ir.write_string(ident);
                        ir.write_u8(arguments.len() as u8);
                    }
                    Expr::FieldAccess { object, field } => {
                        ir.extend(self.compile_expr(object, classes));
                        ir.write_u8(Bytecode::CallMethod as u8);
                        ir.write_string(field);
                        ir.write_u8(arguments.len() as u8);
                    }
                    _ => panic!("Unsupported callee"),
                }
            }

            _ => panic!("Unsupported expression: {:?}", expr),
        }

        ir.into_bytes()
    }
}
