use crate::rules::rule::{TypeCheckerRule, TypeRule};
use crate::type_checker_ctx::TypeCheckerCtx;
use ir::ast::Type::I8;
use ir::ast::{
    Block, ElseBranch, Expr, ForStmt, FuncDecl, IfStmt, LetStmt, Op, ReturnStmt, Stmt, Type,
    WhileStmt,
};
use ir::context::Context;
use ir::hir::StrId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::bump::GrowableBump;

pub struct TypeChecker<'a> {
    rules: Vec<TypeCheckerRule, GrowableBump>,
    pub ctx: TypeCheckerCtx,
    symbol_table: HashMap<StrId, Type, FxHashBuilder>,
    pub context: Rc<RefCell<Context<'a>>>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>) -> Self {
        Self {
            rules: Vec::new_in(GrowableBump::new(size_of::<TypeCheckerRule>() * 1024, align_of::<TypeCheckerRule>())),
            ctx: TypeCheckerCtx::new(),
            symbol_table: HashMap::with_hasher(FxHashBuilder),
            context,
        }
    }

    pub fn get_expr_type(&mut self, expr: &Expr) -> Type {
        match expr {
            _ => I8,
        }
    }

    pub fn add_rule<R: TypeRule + 'static>(&mut self, rule: R) {
        self.rules.push(TypeCheckerRule::External(Box::new(rule)));
    }

    pub fn check_program(&mut self, stmts: &[Stmt]) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // First pass: collect declarations
        for stmt in stmts {
            if let Err(e) = self.collect_declarations(stmt) {
                errors.push(e);
            }
        }

        // Second pass: type check
        for stmt in stmts {
            if let Err(e) = self.check_stmt(stmt) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn collect_declarations(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let(LetStmt {
                ident,
                type_annotation,
                ..
            }) => {
                self.symbol_table
                    .insert(ident.clone(), type_annotation.clone());
                Ok(())
            }
            Stmt::FuncDecl(FuncDecl {
                name,
                params: _,
                return_type: _,
                ..
            }) => {
                // TODO: Handle function types properly
                self.symbol_table.insert(name.clone(), Type::Void);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let(LetStmt {
                ident: _,
                value,
                type_annotation,
                ..
            }) => {
                let value_ty = self.check_expr(value)?;
                if &value_ty != type_annotation {
                    return Err(format!(
                        "Type mismatch: expected {}, found {}",
                        type_annotation, value_ty
                    ));
                }
                Ok(())
            }
            Stmt::Return(ReturnStmt { value }) => {
                if let Some(expr) = value {
                    self.check_expr(expr)?;
                }
                Ok(())
            }
            Stmt::If(IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                self.check_condition(condition)?;
                self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    match else_branch.as_ref() {
                        ElseBranch::Else(block) => self.check_block(block)?,
                        ElseBranch::If(if_stmt) => {
                            self.check_stmt(&Stmt::If(if_stmt.as_ref().clone()))?
                        }
                    }
                }
                Ok(())
            }
            Stmt::While(WhileStmt { condition, block }) => {
                self.check_condition(condition)?;
                self.check_block(block)
            }
            Stmt::For(ForStmt {
                let_stmt,
                condition,
                increment,
                block,
            }) => {
                if let Some(let_stmt) = let_stmt {
                    self.check_stmt(&Stmt::Let(let_stmt.clone()))?;
                }
                if let Some(cond) = condition {
                    self.check_condition(cond)?;
                }
                if let Some(inc) = increment {
                    self.check_expr(inc)?;
                }
                self.check_block(block)
            }
            Stmt::ExprStmt(expr_stmt) => {
                self.check_expr(&expr_stmt.expr)?;
                Ok(())
            }
            _ => Ok(()), // Handle other statements
        }
    }

    fn check_block(&mut self, block: &Block) -> Result<(), String> {
        let mut errors = Vec::new();
        let old_scope = self.symbol_table.clone();

        for stmt in &block.block {
            if let Err(e) = self.check_stmt(stmt) {
                errors.push(e);
            }
        }

        self.symbol_table = old_scope;

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors.join("\n"))
        }
    }

    pub fn check_condition(&mut self, expr: &Expr) -> Result<(), String> {
        let ty = self.check_expr(expr)?;
        if ty != Type::Boolean {
            return Err(format!("Condition must be boolean, found {}", ty));
        }
        Ok(())
    }

    pub fn check_expr(&mut self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Number { .. } => Ok(Type::I32),
            Expr::Decimal { .. } => Ok(Type::F64),
            Expr::Boolean { .. } => Ok(Type::Boolean),
            Expr::String { .. } => Ok(Type::String),
            Expr::Char { .. } => Ok(Type::Char),
            Expr::Binary { left, op, right, .. } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;

                match op {
                    Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod => {
                        if left_ty.is_numeric() && right_ty.is_numeric() {
                            // Numeric promotion
                            if left_ty == Type::F64 || right_ty == Type::F64 {
                                Ok(Type::F64)
                            } else if left_ty == Type::I64 || right_ty == Type::I64 {
                                Ok(Type::I64)
                            } else if left_ty == Type::I32 || right_ty == Type::I32 {
                                Ok(Type::I32)
                            } else {
                                Ok(Type::I32) // Default to i32 for integer literals
                            }
                        } else {
                            Err(format!(
                                "Cannot apply {:?} to {} and {}",
                                op, left_ty, right_ty
                            ))
                        }
                    }
                    Op::Eq | Op::Neq | Op::Lt | Op::Lte | Op::Gt | Op::Gte => {
                        if left_ty == right_ty {
                            Ok(Type::Boolean)
                        } else {
                            Err(format!("Cannot compare {} with {}", left_ty, right_ty))
                        }
                    }
                    _ => Err(format!("Unsupported operation: {:?}", op)),
                }
            }
            Expr::Ident { name, .. } => self
                .symbol_table
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable: {}", self.context.borrow_mut().string_pool.resolve_string(&*name))),
            Expr::Call {
                callee: _,
                arguments, ..
            } => {
                // For now, just check the arguments
                for arg in arguments {
                    self.check_expr(arg)?;
                }

                Ok(Type::Void)
            }
            _ => Ok(Type::Void),
        }
    }
}
