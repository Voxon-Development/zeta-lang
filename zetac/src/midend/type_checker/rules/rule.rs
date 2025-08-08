use crate::midend::ir::hir::{HirClass, HirExpr, HirFunc, HirStmt, HirType};
use crate::midend::type_checker::type_checker::TypeCheckerCtx;

pub trait TypeRule {
    fn check_func(&self, ctx: &mut TypeCheckerCtx, func: &HirFunc) {}
    fn check_class(&self, ctx: &mut TypeCheckerCtx, class: &HirClass) {}
    fn check_stmt(&self, ctx: &mut TypeCheckerCtx, stmt: &HirStmt) {}
    fn check_expr(&self, ctx: &mut TypeCheckerCtx, expr: &HirExpr) -> Option<HirType> { None }
}

