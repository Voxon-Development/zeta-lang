use crate::midend::ir::hir::{HirExpr, HirFunc, HirModule, HirType};
use crate::midend::type_checker::context::TypeContext;
use crate::midend::type_checker::errors::error::{TypeError, TypeResult};
use crate::midend::type_checker::errors::reporter::ErrorReporter;

// The entry point for type-checking the program.
trait TypeChecker<R: ErrorReporter> {
    // Type-checks an entire module.
    // - Registers all types, traits, and functions into the context.
    // - Delegates to other traits (resolver, solver) for detailed checks.
    fn check_module(&self, module: &HirModule, ctx: &mut TypeContext<R>) -> Result<(), TypeError>;

    // Type-checks a single function.
    // - Validates parameter types and return type.
    // - Builds constraints for the body (for inference).
    fn check_function(&self, func: &HirFunc, ctx: &mut TypeContext<R>) -> Result<(), TypeError>;

    // Type-checks a single expression (used inside functions or REPL).
    // - Returns the inferred or validated type of the expression.
    fn check_expr(&self, expr: &HirExpr, ctx: &mut TypeContext<R>) -> TypeResult<HirType>;
}
