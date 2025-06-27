use std::ops::Deref;
use ir::Bytecode;
use crate::ast::*;
use crate::codegen::ir::ir_buffer::ByteWriter;
use crate::codegen::ir::ir_compiler::ClassTable;

pub struct IRStmtCompiler;

impl IRStmtCompiler {
    pub fn compile_stmts(&self, stmts: &Vec<Stmt>, classes: &ClassTable) -> Vec<u8> {
        let mut bytecode = Vec::new();
        for stmt in stmts {
            let stmt_ir: Vec<u8> = self.compile_stmt(stmt, classes);
            bytecode.extend(stmt_ir);
        }
        bytecode
    }

    pub fn compile_stmt(&self, stmt: &Stmt, classes: &ClassTable) -> Vec<u8> {
        match stmt {
            Stmt::ExprStmt(InternalExprStmt { expr }) => self.compile_expr(expr, classes),
            Stmt::Return(ReturnStmt { value }) => {
                let mut w = ByteWriter::new();
                w.write_u8(Bytecode::Return as u8);
                if let Some(v) = value {
                    w.extend(self.compile_expr(v, classes));
                }
                w.into_bytes()
            }
            Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt, classes),
            Stmt::While(while_stmt) => self.compile_while_stmt(while_stmt, classes),
            Stmt::For(for_stmt) => self.compile_for_stmt(for_stmt, classes),
            Stmt::Match(match_stmt) => self.compile_match_stmt(match_stmt, classes),
            Stmt::Let(let_stmt) => self.compile_let_stmt(let_stmt, classes),
            Stmt::UnsafeBlock(UnsafeBlock { block }) => self.compile_stmts(&block.block, classes),
            _ => {
                eprintln!("Unsupported statement: {:#?}", stmt);
                vec![]
            },
        }
    }

    fn compile_expr(&self, expr: &Expr, classes: &ClassTable) -> Vec<u8> {
        let mut ir = ByteWriter::new();
        match expr {
            Expr::Binary { left, op, right } => {
                ir.extend(self.compile_expr(left, classes));
                ir.extend(self.compile_expr(right, classes));
                ir.write_u8(parse_ast_op_to_ir(op));
            }
            Expr::ClassInit { callee, arguments } => {
                for arg in arguments {
                    ir.extend(self.compile_expr(arg, classes)); // Push args first
                }

                ir.write_u8(Bytecode::ClassInit as u8);

                match callee.deref() {
                    Expr::Ident(ident) => {
                        ir.write_u64(classes.name_to_id[ident] as u64); // Write class ID
                    }
                    _ => panic!("Unsupported class init expression: {:#?}", callee),
                }

                ir.write_u8(arguments.len() as u8); // Write arity (if required)
            }
            Expr::Array { elements } => {
                for el in elements {
                    ir.extend(self.compile_expr(el, classes));
                }
                ir.write_u8(Bytecode::ArrayAlloc as u8);
                ir.write_u16(elements.len() as u16);
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
                        // Compound assignment: load field, apply op, store back
                        ir.extend(self.compile_expr(&*object.clone(), classes)); // clone object for load
                        ir.write_u8(Bytecode::LoadField as u8);
                        ir.write_string(field);

                        ir.extend(self.compile_expr(rhs, classes));
                        ir.write_u8(parse_ast_op_to_ir(compound_op)); // e.g. Mul

                        ir.extend(self.compile_expr(object, classes)); // re-evaluate object
                        ir.write_u8(Bytecode::Swap as u8); // put object before result
                        ir.write_u8(Bytecode::StoreField as u8);
                        ir.write_string(field);
                    }
                    (Expr::Ident(ident), compound_op) => {
                        // For compound assignments like `+=`, `-=`, etc.
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
            Expr::ArrayIndex { array, index } => {
                ir.extend(self.compile_expr(array, classes));
                ir.extend(self.compile_expr(index, classes));
                ir.write_u8(Bytecode::ArrayGet as u8);
            }
            Expr::ArrayInit { .. } => {
                ir.write_u8(Bytecode::ArrayAlloc as u8);
            }
            Expr::ExprList { exprs } => {
                for e in exprs {
                    ir.extend(self.compile_expr(e, classes));
                }
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
            Expr::FieldAccess { object, field } => {
                ir.extend(self.compile_expr(object, classes)); // push base object
                ir.write_u8(Bytecode::LoadField as u8);
                ir.write_string(field);
            }

            Expr::Number(num) => {
                ir.write_u8(Bytecode::PushInt as u8);
                ir.write_i64(*num);
            }
            Expr::String(str) => {
                ir.write_u8(Bytecode::PushStr as u8);
                ir.write_string(str);
            }
            Expr::Boolean(b) => {
                ir.write_u8(Bytecode::PushBool as u8);
                ir.write_u8(*b as u8);
            }
            Expr::Call { callee, arguments } => {
                for arg in arguments {
                    ir.extend(self.compile_expr(arg, classes));
                }

                ir.write_u8(Bytecode::Call as u8);

                match &**callee {
                    Expr::Ident(ident) => {
                        ir.write_string(ident); // function name
                    }
                    _ => {
                        panic!("Dynamic call targets not supported yet");
                    }
                }

                ir.write_u8(arguments.len() as u8); // arity
            }

            Expr::Comparison { lhs, op, rhs } => {
                ir.extend(self.compile_expr(lhs, classes));
                ir.extend(self.compile_expr(rhs, classes));
                ir.write_u8(match op {
                    ComparisonOp::Equal => Bytecode::Eq as u8,
                    ComparisonOp::NotEqual => Bytecode::Ne as u8,
                    ComparisonOp::GreaterThan => Bytecode::Gt as u8,
                    ComparisonOp::GreaterThanOrEqual => Bytecode::Ge as u8,
                    ComparisonOp::LessThan => Bytecode::Lt as u8,
                    ComparisonOp::LessThanOrEqual => Bytecode::Le as u8,
                });
            }
            _ => {}
        }
        let ir = ir.into_bytes();
        ir
    }

    fn compile_if_stmt(&self, if_stmt: &IfStmt, classes: &ClassTable) -> Vec<u8> {
        let mut ir = self.compile_expr(&if_stmt.condition, classes);

        let jump_if_false_pos = ir.len();
        ir.push(Bytecode::JumpIfFalse as u8);
        ir.extend(&[0, 0]); // placeholder offset

        let then_ir = self.compile_stmts(&if_stmt.then_branch.block, classes);
        let then_len = then_ir.len() as u16;
        ir.extend(then_ir);

        // Patch offset
        let offset_bytes = then_len.to_le_bytes();
        ir[jump_if_false_pos + 1] = offset_bytes[0];
        ir[jump_if_false_pos + 2] = offset_bytes[1];

        if let Some(else_branch) = &if_stmt.else_branch {
            match &**else_branch {
                ElseBranch::Else(block) => ir.extend(self.compile_stmts(&block.block, classes)),
                ElseBranch::If(nested_if) => ir.extend(self.compile_if_stmt(nested_if, classes)),
            }
        }

        ir
    }

    fn compile_while_stmt(&self, while_stmt: &WhileStmt, classes: &ClassTable) -> Vec<u8> {
        let mut ir = Vec::new();

        let start_pos = ir.len();
        ir.extend(self.compile_expr(&while_stmt.condition, classes));

        let jump_out_pos = ir.len();
        ir.push(Bytecode::JumpIfFalse as u8);
        ir.extend(&[0, 0]);

        let body = self.compile_stmts(&while_stmt.block.block, classes);
        ir.extend(body.clone());

        ir.push(Bytecode::Jump as u8);
        let loop_back = ir.len() + 3 - start_pos;
        let offset = (-(loop_back as i16)) as u16;
        ir.extend(&offset.to_le_bytes());

        let body_len = body.len() as u16;
        let jump_out = body_len.to_le_bytes();
        ir[jump_out_pos + 1] = jump_out[0];
        ir[jump_out_pos + 2] = jump_out[1];

        ir
    }

    fn compile_for_stmt(&self, for_stmt: &ForStmt, classes: &ClassTable) -> Vec<u8> {
        let mut ir = Vec::new();
        if let Some(init) = &for_stmt.let_stmt {
            ir.extend(self.compile_let_stmt(init, classes));
        }
        if let Some(cond) = &for_stmt.condition {
            ir.extend(self.compile_expr(cond, classes));
        }
        if let Some(inc) = &for_stmt.increment {
            ir.extend(self.compile_expr(inc, classes));
        }
        ir.extend(self.compile_stmts(&for_stmt.block.block, classes));
        ir
    }

    fn compile_match_stmt(&self, _match_stmt: &MatchStmt, classes: &ClassTable) -> Vec<u8> {
        todo!()
    }

    fn compile_let_stmt(&self, let_stmt: &LetStmt, classes: &ClassTable) -> Vec<u8> {
        let mut ir = ByteWriter::new();
        match &let_stmt.value.deref() {
            Expr::RegionInit => {
                ir.write_u8(Bytecode::NewRegion as u8);
                ir.write_string(&let_stmt.ident); // variable name

                ir.into_bytes()
            },
            _ => {
                ir.extend(self.compile_expr(&let_stmt.value, classes));
                ir.write_u8(Bytecode::StoreVar as u8);
                ir.write_string(&let_stmt.ident);

                println!("Let stmt: {:?}", let_stmt);
                let var_type = parse_ast_to_ir(let_stmt.type_annotation.clone().unwrap());

                ir.into_bytes()
            }
        }
    }
}

#[inline]
fn parse_ast_to_ir(ast_type: Type) -> ir::BytecodeType {
    match ast_type {
        Type::U8 => ir::BytecodeType::U8,
        Type::I8 => ir::BytecodeType::I8,
        Type::U16 => ir::BytecodeType::U16,
        Type::I16 => ir::BytecodeType::I16,
        Type::I32 => ir::BytecodeType::I32,
        Type::F32 => ir::BytecodeType::F32,
        Type::F64 => ir::BytecodeType::F64,
        Type::I64 => ir::BytecodeType::I64,
        Type::String => ir::BytecodeType::String,
        Type::Boolean => ir::BytecodeType::Boolean,
        Type::UF32 => ir::BytecodeType::UF32,
        Type::U32 => ir::BytecodeType::U32,
        Type::U64 => ir::BytecodeType::U64,
        Type::I128 => ir::BytecodeType::I128,
        Type::U128 => ir::BytecodeType::U128,
        Type::UF64 => ir::BytecodeType::UF64,
        Type::Void => ir::BytecodeType::Void,
        Type::Array(_, _) => ir::BytecodeType::Array,
        Type::Class(_) => ir::BytecodeType::Class,
    }
}

#[inline]
const fn parse_ast_op_to_ir(op: &Op) -> u8 {
    match op {
        Op::Add => Bytecode::Add as u8,
        Op::Sub => Bytecode::Sub as u8,
        Op::Mul => Bytecode::Mul as u8,
        Op::Div => Bytecode::Div as u8,
        Op::Mod => Bytecode::Mod as u8,
        Op::Shl => Bytecode::Shl as u8,
        Op::Shr => Bytecode::Shr as u8,
        Op::BitOr => Bytecode::BitOr as u8,
        Op::BitXor => Bytecode::BitXor as u8,
        Op::BitAnd => Bytecode::BitAnd as u8,
        Op::Assign => Bytecode::Assign as u8,
        Op::AddAssign => Bytecode::AddAssign as u8,
        Op::SubAssign => Bytecode::SubAssign as u8,
        Op::MulAssign => Bytecode::MulAssign as u8,
        Op::DivAssign => Bytecode::DivAssign as u8,
        Op::ModAssign => Bytecode::ModAssign as u8,
        Op::ShlAssign => Bytecode::ShlAssign as u8,
        Op::ShrAssign => Bytecode::ShrAssign as u8,
        Op::BitOrAssign => Bytecode::BitOrAssign as u8,
        Op::BitXorAssign => Bytecode::BitXorAssign as u8,
        Op::BitAndAssign => Bytecode::BitAndAssign as u8,
        Op::PowAssign => Bytecode::PowAssign as u8,
        Op::Eq => Bytecode::Eq as u8,
        Op::Neq => Bytecode::Ne as u8,
        Op::Lt => Bytecode::Lt as u8,
        Op::Lte => Bytecode::Le as u8,
        Op::Gt => Bytecode::Gt as u8,
        Op::Gte => Bytecode::Ge as u8,
    }
}