pub mod context;
pub mod decl_lowering;
pub mod expr_lowering;
pub mod lambda_hoisting;
pub mod module_lowering;
pub mod monomorphization;
pub mod stmt_lowering;
pub mod utils;

#[cfg(test)]
mod tests;

pub use context::{HirLowerer, LoweringCtx};
pub use utils::{lower_cmp_operator, lower_visibility, type_suffix_with_pool};
