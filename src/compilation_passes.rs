use crate::main_structs::CompilerError;
use codex_dependency_graph::dep_graph::DepGraph;
use ir::ast::Stmt;
use ir::errors::reporter::ErrorReporter;
use ir::hir::HirModule;
use scribe_parser::hir_lowerer::HirLowerer;
use scribe_parser::hir_lowerer::lambda_hoisting::LambdaHoister;
use sentinel_typechecker::TypeChecker;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

pub fn pass_hir_lowering<'a, 'bump>(
    statements: Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>,
    dep_graph: &'a DepGraph,
    module_idx: usize,
) -> Result<HirModule<'a, 'bump>, CompilerError<'a>> {
    let mut lowerer = HirLowerer::new(context.clone(), bump.clone(), dep_graph);
    let module = lowerer.lower_module(statements, module_idx);
    let hoister = LambdaHoister::new(bump.clone(), context.clone(), module.name);
    Ok(hoister.run(module))
}

pub fn register_all_modules<'a, 'bump>(
    hir_modules: &[HirModule<'a, 'bump>],
    type_checker: Rc<RefCell<TypeChecker<'a, 'bump>>>,
) {
    let mut checker = type_checker.borrow_mut();
    for module_idx in 0..hir_modules.len() {
        checker.register_module(&hir_modules[module_idx], module_idx);
    }
}

pub fn check_all_module_bodies<'a, 'bump>(
    hir_modules: &[HirModule<'a, 'bump>],
    file_names: &[&str],
    type_checker: Rc<RefCell<TypeChecker<'a, 'bump>>>,
) -> Result<(), CompilerError<'a>> {
    let mut checker = type_checker.borrow_mut();
    for module_idx in 0..hir_modules.len() {
        let module = &hir_modules[module_idx];
        let file_name = file_names[module_idx];

        checker
            .check_module_body(module, module_idx)
            .map_err(|e| CompilerError::TypeError(e.to_string()))?;

        let mut error_reporter: ErrorReporter = ErrorReporter::new();
        error_reporter.add_source_file(file_name.into(), "".into());
        if error_reporter.has_errors() {
            error_reporter.report_all();
            return Err(CompilerError::TypeCheckError);
        }
    }
    Ok(())
}
