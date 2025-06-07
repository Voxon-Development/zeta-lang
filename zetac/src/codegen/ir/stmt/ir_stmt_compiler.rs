use ir::Bytecode;
use crate::ast::*;

pub struct IRStmtCompiler;

impl IRStmtCompiler {
    pub fn new() -> Self {
        IRStmtCompiler
    }
    
    pub fn compile_stmts(&self, stmts: &Vec<Stmt>) -> Vec<Bytecode> {
        let mut bytecode = Vec::new();
        for stmt in stmts {
            bytecode.extend(self.compile_stmt(&stmt));
        }
        
        bytecode
    }
    
    pub fn compile_stmt(&self, stmt: &Stmt) -> Vec<Bytecode> {
        match stmt {
            Stmt::ExprStmt(InternalExprStmt { expr }) => {
                self.compile_expr(expr)
            }
            Stmt::Return(ReturnStmt { value }) => {
                let mut return_bytecode = vec![Bytecode::Return];
                if let Some(value) = value {
                    return_bytecode.extend(self.compile_expr(value));
                }
                return_bytecode

            }
            Stmt::If(if_stmt) => {
                self.compile_if_stmt(if_stmt)
            }
            Stmt::While(while_stmt) => {
                self.compile_while_stmt(while_stmt)
            }
            Stmt::For(for_stmt) => {
                self.compile_for_stmt(for_stmt)
            }
            Stmt::Match(match_stmt) => {
                self.compile_match_stmt(match_stmt)
            }
            Stmt::Let(let_stmt) => {
                self.compile_let_stmt(let_stmt)
            }
            Stmt::UnsafeBlock(UnsafeBlock { block }) => {
                self.compile_stmts(&block.block)
            }
            
            _ => todo!()
        }
    }
    
    fn compile_expr(&self, expr: &Expr) -> Vec<Bytecode> {
        let mut ir = Vec::new();
        match expr {
            Expr::Binary { left, op, right } => {
                ir.extend(self.compile_expr(&left));
                ir.push(parse_ast_op_to_ir(op));
                ir.extend(self.compile_expr(&right));
            }
            Expr::Array { elements } => {
                /*ir.push(Bytecode::AllocArray {
                    array_type: parse_ast_to_ir(elements[0].get_type()),
                    num_of_elements: elements.len(),
                });*/
                ir.extend(self.compile_expr(&elements));
            }
            Expr::ArrayIndex { array, index } => {
                ir.extend(self.compile_expr(&array));
                ir.extend(self.compile_expr(&index));
                ir.push(Bytecode::ArrayGet);
            }
            Expr::ArrayInit { array_type, num_of_elements } => {
                ir.push(Bytecode::AllocArray {
                    array_type: parse_ast_to_ir(array_type.clone()),
                    num_of_elements: *num_of_elements,
                });
            }
            Expr::ExprList { exprs } => {
                for expr in exprs {
                    ir.extend(self.compile_expr(&expr));
                }
            }
            Expr::Ident(ident) => {
                ir.push(Bytecode::LoadVar {
                    name: ident.clone(),
                });
            }
            Expr::Number(num) => {
                ir.push(Bytecode::PushInt(*num as usize));
            }
            Expr::String(str) => {
                ir.push(Bytecode::PushStr(str.clone().as_ptr()));
            }
            Expr::Boolean(bool) => {
                ir.push(Bytecode::PushBool(*bool));
            }
            Expr::Call { callee, arguments } => {
                ir.push(Bytecode::Call);
                ir.extend(self.compile_expr(&callee));
                for arg in arguments {
                    ir.extend(self.compile_expr(arg));
                }
            }
            Expr::Comparison { lhs, op, rhs } => {
                ir.extend(self.compile_expr(&lhs));
                ir.extend(self.compile_expr(&rhs));
                ir.push(match op {
                    ComparisonOp::Equal => Bytecode::Eq,
                    ComparisonOp::NotEqual => Bytecode::Ne,
                    ComparisonOp::GreaterThan => Bytecode::Gt,
                    ComparisonOp::GreaterThanOrEqual => Bytecode::Ge,
                    ComparisonOp::LessThan => Bytecode::Lt,
                    ComparisonOp::LessThanOrEqual => Bytecode::Le,
                });
            }
            _ => todo!()
        }
        
        ir
    }

    fn compile_if_stmt(&self, if_stmt: &IfStmt) -> Vec<Bytecode> {
        let mut ir = Vec::new();

        ir.extend(self.compile_expr(&if_stmt.condition));

        let jump_if_false_index = ir.len();
        ir.push(Bytecode::JumpIfFalse(0)); // placeholder

        let then_ir = self.compile_stmts(&if_stmt.then_branch.block);
        ir.extend(then_ir);

        let jump_to_merge_index = ir.len();
        ir.push(Bytecode::Jump(0)); // placeholder

        let else_label = ir.len();

        // Compile else branch if exists
        if let Some(else_branch) = &if_stmt.else_branch {
            let else_ir = match &**else_branch {
                ElseBranch::Else(expr) => self.compile_stmts(&expr.block),
                ElseBranch::If(nested_if) => self.compile_if_stmt(nested_if),
            };
            ir.extend(else_ir);
        }

        // Label for merge block
        let merge_label = ir.len();

        // Patch jump targets
        if let Bytecode::JumpIfFalse(ref mut target) = ir[jump_if_false_index] {
            *target = else_label;
        }

        if let Bytecode::Jump(ref mut target) = ir[jump_to_merge_index] {
            *target = merge_label;
        }

        ir
    }

    fn compile_while_stmt(&self, while_stmt: &WhileStmt) -> Vec<Bytecode> {
        let mut ir = Vec::new();

        // Compile the condition
        let cond_val = self.compile_expr(&while_stmt.condition);

        // Compile the body
        let body = self.compile_stmts(&while_stmt.block.block);

        // Compile the merge block
        let merge_block = self.compile_stmts(&while_stmt.block.block);

        ir.extend(cond_val);
        ir.extend(body);
        ir.extend(merge_block);
        ir
    }

    fn compile_for_stmt(&self, for_stmt: &ForStmt) -> Vec<Bytecode> {
        let mut ir = Vec::new();

        // Compile the init
        let init_val = self.compile_let_stmt(&for_stmt.let_stmt.clone().unwrap());
        ir.extend(init_val);

        // Compile the condition
        let cond_val = self.compile_expr(&for_stmt.condition.clone().unwrap());
        ir.extend(cond_val);

        // Compile the increment
        let increment_val = self.compile_expr(&for_stmt.increment.clone().unwrap());
        ir.extend(increment_val);

        // Compile the body
        let body = self.compile_stmts(&for_stmt.block.block);
        ir.extend(body);
        ir
    }

    fn compile_match_stmt(&self, match_stmt: &MatchStmt) -> Vec<Bytecode> {
        todo!()
    }

    fn compile_let_stmt(&self, let_stmt: &LetStmt) -> Vec<Bytecode> {
        let mut ir = Vec::new();

        let value = self.compile_expr(&let_stmt.value);

        ir.push(Bytecode::StoreVar {
            name: let_stmt.ident.clone(),
        });

        let var_type = parse_ast_to_ir(let_stmt.type_annotation.clone().unwrap());
        match var_type {
            ir::BytecodeType::U8 => ir.push(Bytecode::PushU8(0)),
            ir::BytecodeType::I8 => ir.push(Bytecode::PushI8(0)),
            ir::BytecodeType::U16 => ir.push(Bytecode::PushU16(0)),
            ir::BytecodeType::I16 => ir.push(Bytecode::PushI16(0)),
            ir::BytecodeType::I32 => ir.push(Bytecode::PushI32(0)),
            ir::BytecodeType::F32 => ir.push(Bytecode::PushF32(0.0)),
            ir::BytecodeType::F64 => ir.push(Bytecode::PushF64(0.0)),
            ir::BytecodeType::I64 => ir.push(Bytecode::PushI64(0)),
            ir::BytecodeType::String => ir.push(Bytecode::PushStr("".to_string().as_ptr())),
            ir::BytecodeType::Boolean => ir.push(Bytecode::PushBool(false)),
            ir::BytecodeType::UF32 => ir.push(Bytecode::PushUF32(0.0)),
            ir::BytecodeType::U32 => ir.push(Bytecode::PushU32(0)),
            ir::BytecodeType::U64 => ir.push(Bytecode::PushU64(0)),
            ir::BytecodeType::I128 => ir.push(Bytecode::PushI128(0)),
            ir::BytecodeType::UF64 => ir.push(Bytecode::PushUF64(0.0)),
            ir::BytecodeType::U128 => ir.push(Bytecode::PushU128(0)),
            ir::BytecodeType::Void => todo!(),
            ir::BytecodeType::Array(_, _) => ir.extend(value),
            ir::BytecodeType::Class(_) => todo!(),
        }

        ir
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
        Type::Array(ty, _) => ir::BytecodeType::Array(Box::from(parse_ast_to_ir(*ty)), None),
        Type::Class(name) => ir::BytecodeType::Class(name),
    }
}

#[inline]
fn parse_ast_op_to_ir(op: &Op) -> Bytecode {
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
        Op::Gte => Bytecode::Ge
    }
}