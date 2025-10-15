use emberforge_compiler::midend::ir::module_lowerer::MirModuleLowerer;
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::context::Context;
use ir::errors::reporter::ErrorReporter;
use ir::hir::HirModule;
use scribe_parser::hir_lowerer::HirLowerer;
use scribe_parser::parser::Rule;
use sentinel_typechecker::rules::MathTypeRule;
use sentinel_typechecker::type_checker::TypeChecker;
use snmalloc_rs::SnMalloc;
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use std::{fs, io};
use thiserror::Error;
use zetaruntime::string_pool::StringPool;

#[global_allocator]
static ALLOCATOR: SnMalloc = SnMalloc;

fn main() -> Result<(), CompilerError> {
    // Compile MIR to machine code using cranelift
    let string_pool = Box::leak(Box::new(
        StringPool::new().map_err(|_| CompilerError::FailedToAllocateStringPool)?,
    ));

    let error_reporter = ErrorReporter::new();

    let context = Rc::new(RefCell::new(Context {
        string_pool, // now &'static mut StringPool
        error_reporter,
    }));

    let backend = Rc::new(RefCell::new(CraneliftBackend::new(context.clone())));

    // Limit the number of files processed to prevent resource exhaustion
    const MAX_FILES: usize = 100;

    let files: Vec<_> = fs::read_dir(".")
        .map_err(|e| CompilerError::FailedToReadDirectory(e.to_string()))?
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| {
            path.extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext == "zeta")
                .unwrap_or(false)
        })
        .take(MAX_FILES)
        .collect();

    if files.is_empty() {
        eprintln!("No .zeta files found in current directory");
        return Ok(());
    }

    println!("Processing {} .zeta files", files.len());

    let hir_modules: Result<Vec<HirModule>, CompilerError> = files
        .iter()
        .map(|path| -> Result<HirModule, CompilerError> {
            // Read the source
            let contents = fs::read_to_string(path)
                .map_err(|e| CompilerError::FailedToReadFile(path.clone(), e))?;

            // Leak it to get a &'static str lifetime
            let leaked_src: &'static str = Box::leak(contents.into_boxed_str());

            // Leak the filename too (optional but safer for parser spans)
            let leaked_filename: &'static str = Box::leak(
                path.file_name()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned()
                    .into_boxed_str(),
            );

            let stmts = scribe_parser::parser::parse_program(
                leaked_src,
                leaked_filename,
                context.clone(),
            ).map_err(|e| CompilerError::ParserError(path.clone(), e))?;

            let mut type_checker = TypeChecker::new(context.clone());
            type_checker.add_rule(MathTypeRule);
            type_checker
                .check_program(stmts.as_slice())
                .map_err(|e| CompilerError::TypeCheckerErrors(e))?;

            let mut lowerer = HirLowerer::new(context.clone());
            let module = lowerer.lower_module(stmts);
            Ok(module)
        })
        .collect();

    hir_modules?.iter().for_each(|hir_module| {
        let backend = backend.clone();

        let mir_module_lowerer = MirModuleLowerer::new(context.clone());
        let mir_module = mir_module_lowerer.lower_module(hir_module);

        backend.borrow_mut().emit_module(&mir_module);
    });

    let backend = Rc::try_unwrap(backend)
        .map_err(|_| CompilerError::BackendStillBorrowed)?
        .into_inner();

    backend.finish();
    Ok(())
}

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Failed to read directory: {0}")]
    FailedToReadDirectory(String),

    #[error("Failed to read file {0}: {1}")]
    FailedToReadFile(PathBuf, io::Error),

    #[error("Failed to allocate string pool.")]
    FailedToAllocateStringPool,

    #[error("Failed to parse module {0}: {1}")]
    ParserError(PathBuf, pest::error::Error<Rule>),

    #[error("Failed to compile module due to type errors: {0:?}")]
    TypeCheckerErrors(Vec<String>),

    #[error("Weird error i think")]
    BackendStillBorrowed,
}
