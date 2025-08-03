use crate::midend::ir::hir::HirType;
use crate::midend::type_checker::context::TypeContext;
use crate::midend::type_checker::errors::error::TypeError;
use crate::midend::type_checker::errors::reporter::ErrorReporter;

// Resolves type names and generic types into actual type representations.
pub trait TypeResolver<R: ErrorReporter> {
    // Resolves a type name like "int" or "MyStruct" into a concrete Type.
    fn resolve_type_name(&self, name: &str, ctx: &TypeContext<R>) -> Result<HirType, TypeError>;

    // Resolves a generic type application like "Vec<T>" or "Option<int>".
    // This substitutes type parameters and ensures correct number of generics.
    fn resolve_generic(&self, base: &HirType, params: &[HirType], ctx: &TypeContext<R>) -> Result<HirType, TypeError>;
}
