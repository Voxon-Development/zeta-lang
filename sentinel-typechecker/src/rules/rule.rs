use crate::type_checker::TypeChecker;
use ir::ast::{ClassDecl, Expr, FuncDecl, Stmt, Type};

pub enum TypeCheckerRule {
    MathematicalRule(MathematicalTypeRule),
    External(Box<dyn TypeRule>),
}

pub struct MathematicalTypeRule {
    
}

impl TypeRule for MathematicalTypeRule {
    
}

pub trait TypeRule {
    fn check_func(&self, _ctx: &mut TypeChecker, _func: &FuncDecl) {}
    fn check_class(&self, _ctx: &mut TypeChecker, _class: &ClassDecl) {}
    fn check_stmt(&self, _ctx: &mut TypeChecker, _stmt: &Stmt) {}
    fn check_expr(&self, _ctx: &mut TypeChecker, _expr: &Expr) -> Option<Type> { None }
}

