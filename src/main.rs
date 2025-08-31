use emberforge_compiler::midend::ir::module_lowerer::MirModuleLowerer;
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::hir::HirModule;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;
use scribe_parser::hir_lowerer::HirLowerer;
use scribe_parser::parser::Rule;
use sentinel_typechecker::type_checker::TypeChecker;
use snmalloc_rs::SnMalloc;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::{fs, io};
use thiserror::Error;
use sentinel_typechecker::rules::MathTypeRule;
use zetaruntime::string_pool::StringPool;

#[global_allocator]
static ALLOCATOR: SnMalloc = SnMalloc;

#[tokio::main]
async fn main() -> Result<(), CompilerError> {
    // Compile MIR to machine code using cranelift
    let string_pool = StringPool::new().map_err(|_| CompilerError::FailedToAllocateStringPool)?;
    let backend = Arc::new(Mutex::new(CraneliftBackend::new(string_pool)));

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

    // Parallel stage: produce HIR modules with proper error handling and type checking
    let hir_modules: Result<Vec<HirModule>, CompilerError> = files
        .par_iter()
        .map(|path| -> Result<HirModule, CompilerError> {
            let contents = fs::read_to_string(path)
                .map_err(|e| CompilerError::FailedToReadFile(path.clone(), e))?;

            let stmts = scribe_parser::parser::parse_program(&contents)
                .map_err(|e| CompilerError::ParserError(path.clone(), e))?;

            let mut type_checker = TypeChecker::new();
            type_checker.add_rule(MathTypeRule);
            type_checker.check_program(stmts.as_slice())
                .map_err(|e| CompilerError::TypeCheckerErrors(e))?;

            let mut lowerer = HirLowerer::new();
            let module = lowerer.lower_module(stmts);
            Ok(module)


        })
        .collect();

    // Serial stage: emit machine code
    hir_modules?
        .par_iter()
        .for_each(|hir_module| {
            let backend = backend.clone();

            let mir_module_lowerer = MirModuleLowerer::new();
            let mir_module = mir_module_lowerer.lower_module(hir_module);

            backend.lock().unwrap().emit_module(&mir_module);
        });

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
    TypeCheckerErrors(Vec<String>)
}