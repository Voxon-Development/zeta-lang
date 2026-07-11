use crate::main_structs::CompilerError;
use codex_dependency_graph::dep_graph::DepGraph;
use ir::ast::Stmt;
use ir::errors::reporter::ErrorReporter;
use ir::hir::{HirModule, StrId};
use ir::registry::global_registry::GlobalRegistry;
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
    statements: Vec<Stmt<'a, 'bump>, &GrowableBump<'bump>>,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>,
    dep_graph: &'a RefCell<DepGraph>,
    module_idx: usize,
    registry: GlobalRegistry<'a, 'bump>,
) -> Result<HirModule<'a, 'bump>, CompilerError<'a>> {
    let mut lowerer = HirLowerer::new(context.clone(), bump.clone(), dep_graph, registry);
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
    file_names_and_contents: &[(StrId, StrId)],
    type_checker: Rc<RefCell<TypeChecker<'a, 'bump>>>,
) -> Result<(), CompilerError<'a>> {
    let mut checker = type_checker.borrow_mut();
    let mut error_reporter: ErrorReporter = ErrorReporter::new();
    for module_idx in 0..hir_modules.len() {
        let module = &hir_modules[module_idx];
        let file_name_and_content = file_names_and_contents[module_idx];
        checker.check_module_body(module, module_idx);
        error_reporter.add_source_file(
            file_name_and_content.0.as_str().to_string(),
            file_name_and_content.1.as_str().to_string(),
        );
    }
    let errors = checker.errors().to_vec();
    if !errors.is_empty() {
        for error in errors {
            error_reporter.add_type_error(error);
        }
        error_reporter.report_all();
        return Err(CompilerError::TypeCheckError);
    }
    Ok(())
}
