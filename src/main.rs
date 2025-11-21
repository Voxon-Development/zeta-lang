#![feature(str_as_str)]
#![feature(allocator_api)]

use codex_dependency_graph::module_collection_builder::ModuleBuilder;
use codex_dependency_graph::topo::topo_sort;
use emberforge_compiler::midend::ir::module_lowerer::MirModuleLowerer;
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;
use scribe_parser::hir_lowerer::HirLowerer;
use scribe_parser::parser::descent_parser::ParserError;
use ctrc_graph::diagnostics::analyze_ctrc_and_report;
use ir::ast::Stmt;
use ir::hir::HirModule;
use ir::ssa_ir::Module;
use ir::errors::reporter::ErrorReporter;

use std::ffi::OsStr;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use std::panic::AssertUnwindSafe;

use futures::future::join_all;
use thiserror::Error;
use tokio::task::JoinHandle;
use walkdir::{DirEntry, WalkDir};
use snmalloc_rs::SnMalloc;
use futures::FutureExt;
use tokio::sync::Semaphore;
use ctrc_graph::hir_integration::convenience::analyze_and_pretty_print;

#[global_allocator]
static ALLOCATOR: SnMalloc = SnMalloc;

#[tokio::main]
async fn main() -> Result<(), CompilerError> {
    let start = Instant::now();

    for _ in 1..1000 {
        let result = AssertUnwindSafe::catch_unwind(AssertUnwindSafe(run_compiler()))
            .await;

        let duration = start.elapsed();
        let millis = duration.as_millis();
        let nanos = duration.as_nanos();
        match result {
            Ok(Ok(_)) => {
                println!("Finished build in {}ms (or {}ns)", millis, nanos);
            }
            Ok(Err(e)) => {
                eprintln!("Compilation failed after {}ms (or {}ns) \nError: {:?}", millis, nanos, e);
            }
            Err(_) => {
                eprintln!("Panic occurred after {}ms (or {}ns)", millis, nanos);
            }
        }
    }

    Ok(())
}

async fn run_compiler() -> Result<(), CompilerError> {
    let string_pool: StringPool = StringPool::new()
        .map_err(|_| CompilerError::FailedToAllocateStringPool)?;

    let files: Vec<PathBuf> = WalkDir::new("src")
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().is_file() && entry.path()
                 .extension()
                 .and_then(|ext| ext.to_str())
                 .map(|ext| ext == "zeta")
                 .unwrap_or(false)
        })
        .map(DirEntry::into_path)
        .collect();

    if files.is_empty() {
        eprintln!("No .zeta files found in current directory");
        return Ok(());
    }

    println!("Processing {} .zeta files", files.len());
    let arc: Arc<StringPool> = Arc::new(string_pool);
    
    let mut error_reporter = ErrorReporter::new();
    
    let modules_with_ctrc: Vec<ModuleWithArena> = process_frontend(arc.clone(), files, &mut error_reporter)
        .await?
        .into_iter()
        .filter(|arena| arena.valid)
        .collect::<Vec<_>>();
    
    error_reporter.report_all();
    
    if error_reporter.has_errors() {
        std::process::exit(1);
    }
    
    println!("Processed {} modules", modules_with_ctrc.len());

    // Extract HIR modules for dependency analysis
    let hir_modules: Vec<HirModule> = modules_with_ctrc.iter().map(|m| m.module).collect();
    let builder: ModuleBuilder = ModuleBuilder::run(&hir_modules, arc.clone());
    let indexes: Vec<usize> = topo_sort(&builder.modules);

    let mut backend: CraneliftBackend = CraneliftBackend::new(arc.clone());

    for idx in indexes {
        let hir_module: HirModule = hir_modules[idx];
        let mir_module_lowerer: MirModuleLowerer = MirModuleLowerer::new(arc.clone());
        let mir_module: Module = MirModuleLowerer::lower_module(mir_module_lowerer, hir_module);

        let _ = hir_module;
        backend.emit_module(&mir_module);
    }

    backend.finish();
    Ok(())
}

struct ModuleWithArena<'a, 'bump> {
    #[allow(dead_code)] // This simply prevents parser and HIR bump from UB, doesn't need to be used
    parse_and_hir_bump: Arc<GrowableAtomicBump<'bump>>,

    module: HirModule<'a, 'bump>,

    valid: bool
}

async fn process_frontend<'a, 'bump>(
    context: Arc<StringPool>,
    files: Vec<PathBuf>,
    _error_reporter: &mut ErrorReporter<'a, 'bump>,
) -> Result<Vec<ModuleWithArena<'a, 'bump>>, CompilerError> {
    // Bound concurrency to avoid blowing out memory
    let parallelism = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4);
    // choose a sane multiplier; tweak if you want more/less concurrency
    let max_concurrent = std::cmp::max(1, parallelism.saturating_mul(2));
    let sem = Arc::new(Semaphore::new(max_concurrent));

    let mut handles: Vec<JoinHandle<Result<ModuleWithArena, CompilerError>>> = Vec::new();

    for path in files {
        let context_clone = context.clone();
        let sem_clone = sem.clone();

        // Acquire permit before spawning so we don't create thousands of tasks.
        // This effectively limits the number of live tasks and helps keep memory bounded.
        let permit = sem_clone.acquire_owned().await
            .expect("Semaphore closed unexpectedly");

        // move the permit into the task so it is released when the task ends
        handles.push(tokio::spawn(async move {
            let _permit = permit;
            process_single_file(context_clone, path).await
        }));
    }

    // Wait for all tasks to finish
    // Vec<tokio::runtime::task::Result<Result<ModuleWithArena, CompilerError>>>
    let results = join_all(handles).await;
    let mut modules: Vec<ModuleWithArena> = Vec::new();

    for result in results {
        let module_with_arena = result
            .map_err(|e| CompilerError::TaskJoinError(e.to_string()))??;
        modules.push(module_with_arena);
    }

    Ok(modules)
}

async fn process_single_file<'a, 'bump>(
    context: Arc<StringPool>,
    path: PathBuf,
) -> Result<ModuleWithArena<'a, 'bump>, CompilerError>
where 'a: 'bump, 'bump: 'a {
    let contents = tokio::fs::read_to_string(&path)
        .await
        .map_err(|e| CompilerError::FailedToReadFile(path.clone(), e))?;

    let contents_bytes = contents.as_bytes();
    if contents_bytes.is_empty() {
        return Ok(ModuleWithArena {
            parse_and_hir_bump: Arc::new(GrowableAtomicBump::new()),
            module: HirModule::default(),
            valid: false
        });
    }

    let initial_capacity = {
        let file_len = contents_bytes.len();
        let base = 16 * 1024;
        // Give a modest headroom for parser allocations
        std::cmp::max(base, file_len.saturating_mul(2) + 4096)
    };

    let bump: Arc<GrowableAtomicBump<'bump>> = Arc::new(
        GrowableAtomicBump::with_capacity_and_aligned(initial_capacity, 8)
            .map_err(|_| CompilerError::FailedToAllocateBump)?);

    // File name handling
    let path_file_name: &OsStr = path.file_name().unwrap();
    let file_name_string = path_file_name
        .to_str()
        .ok_or_else(|| CompilerError::InvalidFileName(
            path_file_name.to_string_lossy().as_bytes().to_vec(),
        ))?;

    // Allocate file_name in the bump allocator to get 'static lifetime
    let file_name_bytes = file_name_string.as_bytes();
    let file_name_static = {
        let bytes_static = bump.alloc_many(file_name_bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;

        // SAFETY: we know file_name_string is valid UTF-8
        std::str::from_utf8(unsafe {
                std::slice::from_raw_parts(bytes_static.as_ptr(), file_name_bytes.len())
            })
            .map_err(|_| CompilerError::InvalidFileName(file_name_bytes.to_vec()))?
    };

    // Allocate contents in the bump allocator to get 'static lifetime
    let contents_static = {
        let bytes_static = bump.alloc_many(contents_bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;
        // Safe: contents came from valid UTF-8 string
        std::str::from_utf8(
            unsafe { std::slice::from_raw_parts(bytes_static.as_ptr(), contents_bytes.len()) }
        ).map_err(|_| CompilerError::InvalidFileName(contents_bytes.to_vec()))?
    };

    let stmts: Vec<Stmt, &'_ GrowableBump> = scribe_parser::parser::parse_program(
        contents_static,
        file_name_static,
        context.clone(),
        bump.clone(),
    ).map_err(CompilerError::ParserError)?;

    let lowerer = HirLowerer::new(context.clone(), bump.clone());
    let module: HirModule = lowerer.lower_module(stmts);

    let temp_bump = GrowableBump::new(4096, 8);
    let ctrc_result = ctrc_graph::analyze_hir_for_ctrc(&module, &temp_bump);
    
    let mut temp_ctrc_reporter = ErrorReporter::new();
    temp_ctrc_reporter.add_source_file(file_name_static.to_string(), contents_static.to_string());
    
    analyze_ctrc_and_report(&ctrc_result, &*context, &mut temp_ctrc_reporter, file_name_static);

    if temp_ctrc_reporter.has_errors() {
        temp_ctrc_reporter.report_all();
    }

    Ok(ModuleWithArena {
        parse_and_hir_bump: bump,
        module,
        valid: true
    })
}

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Failed to read file {0}: {1}")]
    FailedToReadFile(PathBuf, io::Error),

    #[error("Failed to allocate string pool.")]
    FailedToAllocateStringPool,

    #[error("Failed to allocate bump allocator")]
    FailedToAllocateBump,

    #[error("Failed to compile module due to type errors: {0:?}")]
    TypeCheckerErrors(Vec<String>),

    #[error("Invalid UTF-8 filename: {0:?}")]
    InvalidFileName(Vec<u8>),

    #[error("Parser error: {0:#?}")]
    ParserError(Vec<ParserError>),

    #[error("Join error: {0}")]
    TaskJoinError(String),
}