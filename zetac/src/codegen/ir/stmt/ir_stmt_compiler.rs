use std::ops::Deref;
use ir::bump::AtomicBump;
use ir::Bytecode;
use crate::ast::*;
use crate::codegen::ir::ir_buffer::ByteWriter;
use crate::codegen::ir::ir_compiler::ClassTable;
use crate::codegen::ir::stmt::ir_expr_compiler::IRExprCompiler;

static EXPR_COMPILER: IRExprCompiler = IRExprCompiler;

pub struct IRStmtCompiler;

impl IRStmtCompiler {
    pub fn compile_stmts(&self, stmts: &Vec<Stmt, AtomicBump>, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut bytecode: Vec<u8, AtomicBump> = Vec::new_in(AtomicBump::new());
        for stmt in stmts {
            let stmt_ir: Vec<u8, AtomicBump> = self.compile_stmt(stmt, classes);
            bytecode.extend(stmt_ir);
        }
        bytecode
    }

    pub fn compile_stmt(&self, stmt: &Stmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        match stmt {
            Stmt::ExprStmt(InternalExprStmt { expr }) => EXPR_COMPILER.compile_expr(expr, classes),
            Stmt::Return(ReturnStmt { value }) => {
                let mut w = ByteWriter::new();
                w.write_u8(Bytecode::Return as u8);
                if let Some(v) = value {
                    w.extend(EXPR_COMPILER.compile_expr(v, classes));
                }
                w.into_bytes()
            }
            Stmt::If(if_stmt) => self.compile_if_stmt(if_stmt, classes),
            Stmt::While(while_stmt) => self.compile_while_stmt(while_stmt, classes),
            Stmt::For(for_stmt) => self.compile_for_stmt(for_stmt, classes),
            Stmt::Match(match_stmt) => self.compile_match_stmt(match_stmt, classes),
            Stmt::Let(let_stmt) => self.compile_let_stmt(let_stmt, classes),
            Stmt::Const(const_stmt) => self.compile_const_stmt(const_stmt, classes),
            Stmt::UnsafeBlock(UnsafeBlock { block }) => self.compile_stmts(&block.block, classes),
            _ => {
                eprintln!("Unsupported statement: {:#?}", stmt);
                Vec::new_in(AtomicBump::new())
            },
        }
    }

    fn compile_if_stmt(&self, if_stmt: &IfStmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut ir = EXPR_COMPILER.compile_expr(&if_stmt.condition, classes);

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

    fn compile_while_stmt(&self, while_stmt: &WhileStmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut ir: Vec<u8, AtomicBump> = Vec::new_in(AtomicBump::new());

        let start_pos = ir.len();
        ir.extend(EXPR_COMPILER.compile_expr(&while_stmt.condition, classes));

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

    fn compile_for_stmt(&self, for_stmt: &ForStmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut ir = Vec::new_in(AtomicBump::new());
        if let Some(init) = &for_stmt.let_stmt {
            ir.extend(self.compile_let_stmt(init, classes));
        }
        if let Some(cond) = &for_stmt.condition {
            ir.extend(EXPR_COMPILER.compile_expr(cond, classes));
        }
        if let Some(inc) = &for_stmt.increment {
            ir.extend(EXPR_COMPILER.compile_expr(inc, classes));
        }
        ir.extend(self.compile_stmts(&for_stmt.block.block, classes));
        ir
    }

    fn compile_match_stmt(&self, _match_stmt: &MatchStmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        todo!()
    }

    fn compile_let_stmt(&self, let_stmt: &LetStmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut ir = ByteWriter::new();
        match &let_stmt.value.deref() {
            Expr::RegionInit => {
                ir.write_u8(Bytecode::NewRegion as u8);
                ir.write_string(&let_stmt.ident); // variable name

                ir.into_bytes()
            },
            _ => {
                ir.extend(EXPR_COMPILER.compile_expr(&let_stmt.value, classes));
                ir.write_u8(Bytecode::StoreVar as u8);
                ir.write_string(&let_stmt.ident);

                println!("Let stmt: {:?}", let_stmt);
                let var_type = parse_ast_to_ir(let_stmt.type_annotation.clone().unwrap());

                ir.into_bytes()
            }
        }
    }

    fn compile_const_stmt(&self, const_stmt: &ConstStmt, classes: &ClassTable) -> Vec<u8, AtomicBump> {
        let mut ir = ByteWriter::new();
        match &const_stmt.value.deref() {
            Expr::RegionInit => {
                ir.write_u8(Bytecode::NewRegion as u8);
                ir.write_string(&const_stmt.ident); // variable name

                ir.into_bytes()
            },
            _ => {
                ir.extend(EXPR_COMPILER.compile_expr(&const_stmt.value, classes));
                ir.write_u8(Bytecode::StoreVar as u8);
                ir.write_string(&const_stmt.ident);

                ir.into_bytes()
            }
        }
    }
}

#[inline]
pub(crate) fn parse_ast_to_ir(ast_type: Type) -> ir::BytecodeType {
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
pub(crate) const fn parse_ast_op_to_ir(op: &Op) -> u8 {
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