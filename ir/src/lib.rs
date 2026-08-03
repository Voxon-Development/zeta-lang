pub mod alias_reasoner_pass;
pub mod analysis_context;
pub mod ast;
pub mod auto_imports;
pub mod borrow_checker;
pub mod borrow_checking_pass;
pub mod diagnostics_context;
pub mod errors;
pub mod hir;
pub mod hir_utils;
pub mod ir_conversion;
pub mod ir_hasher;
pub mod layout;
pub mod nll_cfg;
pub mod pretty;
pub mod registry;
pub mod span;
pub mod ssa_ir;
pub mod tests;
pub mod tokens;

#[macro_export]
macro_rules! zdebug {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            if std::env::var_os("ZETA_DEBUG").is_some() {
                eprintln!("[zeta-debug] {}", format!($($arg)*));
            }
        }
    };
}
