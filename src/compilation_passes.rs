use crate::main_structs::CompilerError;
use ir::ast::Stmt;
use ir::errors::reporter::ErrorReporter;
use ir::hir::HirModule;
use scribe_parser::hir_lowerer::HirLowerer;
use sentinel_typechecker::TypeChecker;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

pub fn pass_hir_lowering<'a, 'bump>(
    statements: Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> Result<HirModule<'a, 'bump>, CompilerError<'a>> {
    let mut lowerer = HirLowerer::new(context, bump);
    let module = lowerer.lower_module(statements);
    Ok(module)
}

#[warn(dead_code)]
pub fn pass_type_checking<'a>(
    module: &HirModule,
    _context: Arc<StringPool>,
    file_name: &str,
) -> Result<(), CompilerError<'a>> {
    let mut type_checker = TypeChecker::new();
    type_checker
        .check_module(module)
        .map_err(|e| CompilerError::TypeError(e.to_string()))?;

    let mut error_reporter: ErrorReporter = ErrorReporter::new();
    error_reporter.add_source_file(file_name.into(), "".into());

    if error_reporter.has_errors() {
        error_reporter.report_all();
        return Err(CompilerError::TypeCheckError);
    }

    Ok(())
}
