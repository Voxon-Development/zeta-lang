use ir::hir::HirModule;
use ir::errors::reporter::ErrorReporter;
use scribe_parser::hir_lowerer::HirLowerer;
use sentinel_typechecker::TypeChecker;
use zetaruntime::string_pool::StringPool;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use std::sync::Arc;
use ir::ast::Stmt;
use crate::main_structs::CompilerError;

/// Pass 1: HIR Lowering
/// Converts parsed AST to High-Level IR
pub fn pass_hir_lowering<'a, 'bump>(
    statements: Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> Result<HirModule<'a, 'bump>, CompilerError> {
    let mut lowerer = HirLowerer::new(context, bump);
    println!("Lowering module");
    let module = lowerer.lower_module(statements);
    Ok(module)
}

/// Pass 2: Type Checking and CTRC Analysis
/// Verifies type correctness and performs compile-time reference counting analysis
pub fn pass_type_checking_and_ctrc(
    module: &HirModule,
    context: Arc<StringPool>,
    file_name: &str,
) -> Result<(), CompilerError> {
    // Type checking
    let mut type_checker = TypeChecker::new();
    type_checker.check_module(module)
        .map_err(|e| CompilerError::TypeError(e.to_string()))?;

    // CTRC analysis
    let temp_bump = GrowableBump::new(4096, 8);
    let ctrc = ctrc_graph::analyze_hir_for_ctrc(module, &temp_bump);

    let mut error_reporter: ErrorReporter = ErrorReporter::new();
    error_reporter.add_source_file(file_name.into(), "".into());
    ctrc_graph::analyze_ctrc_and_report(&ctrc, &*context, &mut error_reporter, file_name);

    if error_reporter.has_errors() {
        error_reporter.report_all();
        return Err(CompilerError::TypeCheckError);
    }

    Ok(())
}

/// Pass 3: Monomorphization
pub fn pass_monomorphization(_module: &HirModule) -> Result<(), CompilerError> {
    Ok(())
}
