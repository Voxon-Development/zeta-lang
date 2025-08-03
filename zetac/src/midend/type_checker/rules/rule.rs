use crate::midend::ir::hir::{HirExpr, HirFunc, HirModule};
use crate::midend::type_checker::context::TypeContext;
use crate::midend::type_checker::errors::reporter::ErrorReporter;

// A pluggable interface for custom type-checking rules.
pub trait Rule<R: ErrorReporter> {
    // Returns a unique identifier for the rule (useful for debugging or selective enabling).
    fn name(&self) -> &str;

    // Runs the rule on a module level (e.g. global type constraints, extern validation).
    fn check_module(&self, module: &HirModule, ctx: &mut TypeContext<R>);

    // Runs the rule on a function level (e.g. return type validation).
    fn check_function(&self, func: &HirFunc, ctx: &mut TypeContext<R>);

    // Runs the rule on an expression level (e.g. constant folding type check).
    fn check_expr(&self, expr: &HirExpr, ctx: &mut TypeContext<R>);
}
