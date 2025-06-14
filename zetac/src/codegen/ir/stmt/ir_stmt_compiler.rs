use ir::Bytecode;
use crate::ast::*;
use crate::codegen::ir::ir_buffer::ByteWriter;

pub struct IRStmtCompiler;

impl IRStmtCompiler {
    pub fn compile_stmts(&self, stmts: &Vec<Stmt>) -> Vec<u8> {
        let mut bytecode = Vec::new();
        for stmt in stmts {
            bytecode.extend(self.compile_stmt(&stmt));
        }
        bytecode
    }

    pub fn compile_stmt(&self, stmt: &Stmt) -> Vec<u8> {
        match stmt {
            Stmt::ExprStmt(InternalExprStmt { expr }) => self.compile_expr(expr),
            Stmt::Return(ReturnStmt { value }) => {
                let mut w = ByteWriter::new();
                w.write_u8(Bytecode::Return as u8);
                if let Some(v) = value {
                    w.extend(self.compile_expr(v));
                }
                w.into_bytes()
            }
            Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt),
            Stmt::While(while_stmt) => self.compile_while_stmt(while_stmt),
            Stmt::For(for_stmt) => self.compile_for_stmt(for_stmt),
            Stmt::Match(match_stmt) => self.compile_match_stmt(match_stmt),
            Stmt::Let(let_stmt) => self.compile_let_stmt(let_stmt),
            Stmt::UnsafeBlock(UnsafeBlock { block }) => self.compile_stmts(&block.block),
            _ => todo!(),
        }
    }

    fn compile_expr(&self, expr: &Expr) -> Vec<u8> {
        let mut ir = ByteWriter::new();
        match expr {
            Expr::Binary { left, op, right } => {
                ir.extend(self.compile_expr(left));
                ir.write_u8(parse_ast_op_to_ir(op) as u8);
                ir.extend(self.compile_expr(right));
            }
            Expr::Array { elements } => {
                for el in elements {
                    ir.extend(self.compile_expr(el));
                }
                ir.write_u8(Bytecode::ArrayAlloc as u8);
                ir.write_u16(elements.len() as u16);
            }
            Expr::Assignment { lhs, op, rhs } => {
            }
            Expr::ArrayIndex { array, index } => {
                ir.extend(self.compile_expr(array));
                ir.extend(self.compile_expr(index));
                ir.write_u8(Bytecode::ArrayGet as u8);
            }
            Expr::ArrayInit { .. } => {
                ir.write_u8(Bytecode::ArrayAlloc as u8);
            }
            Expr::ExprList { exprs } => {
                for e in exprs {
                    ir.extend(self.compile_expr(e));
                }
            }
            Expr::Ident(ident) => {
                ir.write_u8(Bytecode::LoadVar as u8);
                ir.write_string(ident);
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
                ir.write_u8(Bytecode::Call as u8);
                match &**callee {
                    Expr::Ident(ident) => {
                        ir.write_u16(ident.len() as u16);
                        ir.write_string(ident);
                    }
                    _ => {
                        ir.extend(self.compile_expr(callee));
                    }
                }
                ir.write_u8(arguments.len() as u8);
                for arg in arguments {
                    ir.extend(self.compile_expr(arg));
                }
            }
            Expr::Comparison { lhs, op, rhs } => {
                ir.extend(self.compile_expr(lhs));
                ir.extend(self.compile_expr(rhs));
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
        ir.into_bytes()
    }

    fn compile_if_stmt(&self, if_stmt: &IfStmt) -> Vec<u8> {
        let mut ir = self.compile_expr(&if_stmt.condition);

        let jump_if_false_pos = ir.len();
        ir.push(Bytecode::JumpIfFalse as u8);
        ir.extend(&[0, 0]); // placeholder offset

        let then_ir = self.compile_stmts(&if_stmt.then_branch.block);
        let then_len = then_ir.len() as u16;
        ir.extend(then_ir);

        // Patch offset
        let offset_bytes = then_len.to_le_bytes();
        ir[jump_if_false_pos + 1] = offset_bytes[0];
        ir[jump_if_false_pos + 2] = offset_bytes[1];

        if let Some(else_branch) = &if_stmt.else_branch {
            match &**else_branch {
                ElseBranch::Else(block) => ir.extend(self.compile_stmts(&block.block)),
                ElseBranch::If(nested_if) => ir.extend(self.compile_if_stmt(nested_if)),
            }
        }

        ir
    }

    fn compile_while_stmt(&self, while_stmt: &WhileStmt) -> Vec<u8> {
        let mut ir = Vec::new();

        let start_pos = ir.len();
        ir.extend(self.compile_expr(&while_stmt.condition));

        let jump_out_pos = ir.len();
        ir.push(Bytecode::JumpIfFalse as u8);
        ir.extend(&[0, 0]);

        let body = self.compile_stmts(&while_stmt.block.block);
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

    fn compile_for_stmt(&self, for_stmt: &ForStmt) -> Vec<u8> {
        let mut ir = Vec::new();
        if let Some(init) = &for_stmt.let_stmt {
            ir.extend(self.compile_let_stmt(init));
        }
        if let Some(cond) = &for_stmt.condition {
            ir.extend(self.compile_expr(cond));
        }
        if let Some(inc) = &for_stmt.increment {
            ir.extend(self.compile_expr(inc));
        }
        ir.extend(self.compile_stmts(&for_stmt.block.block));
        ir
    }

    fn compile_match_stmt(&self, _match_stmt: &MatchStmt) -> Vec<u8> {
        todo!()
    }

    fn compile_let_stmt(&self, let_stmt: &LetStmt) -> Vec<u8> {
        let mut ir = ByteWriter::new();
        ir.extend(self.compile_expr(&let_stmt.value));
        ir.write_u8(Bytecode::StoreVar as u8);
        ir.write_string(&let_stmt.ident);

        let var_type = parse_ast_to_ir(let_stmt.type_annotation.clone().unwrap());
        ir.write_u8(var_type as u8);

        ir.into_bytes()
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
const fn parse_ast_op_to_ir(op: &Op) -> Bytecode {
    match op {
        Op::Add => Bytecode::Add,
        Op::Sub => Bytecode::Sub,
        Op::Mul => Bytecode::Mul,
        Op::Div => Bytecode::Div,
        Op::Mod => Bytecode::Mod,
        Op::Shl => Bytecode::Shl,
        Op::Shr => Bytecode::Shr,
        Op::BitOr => Bytecode::BitOr,
        Op::BitXor => Bytecode::BitXor,
        Op::BitAnd => Bytecode::BitAnd,
        Op::Assign => Bytecode::Assign,
        Op::AddAssign => Bytecode::AddAssign,
        Op::SubAssign => Bytecode::SubAssign,
        Op::MulAssign => Bytecode::MulAssign,
        Op::DivAssign => Bytecode::DivAssign,
        Op::ModAssign => Bytecode::ModAssign,
        Op::ShlAssign => Bytecode::ShlAssign,
        Op::ShrAssign => Bytecode::ShrAssign,
        Op::BitOrAssign => Bytecode::BitOrAssign,
        Op::BitXorAssign => Bytecode::BitXorAssign,
        Op::BitAndAssign => Bytecode::BitAndAssign,
        Op::Eq => Bytecode::Eq,
        Op::Neq => Bytecode::Ne,
        Op::Lt => Bytecode::Lt,
        Op::Lte => Bytecode::Le,
        Op::Gt => Bytecode::Gt,
        Op::Gte => Bytecode::Ge,
    }
}