#![feature(str_as_str)]
#![feature(allocator_api)]

use codex_dependency_graph::module_collection_builder::ModuleBuilder;
use codex_dependency_graph::topo::topo_sort;
use ctrc_graph::ctrc_diagnostics::analyze_ctrc_and_report;
use emberforge_compiler::midend::ir::module_lowerer::MirModuleLowerer;
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::{CraneliftBackend, EmitError};
use ir::errors::reporter::ErrorReporter;
use ir::hir::HirModule;
use scribe_parser::hir_lowerer::HirLowerer;
use scribe_parser::parser::descent_parser::ParserError;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

use std::io;
use std::path::PathBuf;
use std::process::exit;
use std::sync::Arc;
use std::time::Instant;

use futures::FutureExt;
use snmalloc_rs::SnMalloc;
use thiserror::Error;
use walkdir::{DirEntry, WalkDir};

#[global_allocator]
static ALLOCATOR: SnMalloc = SnMalloc;

use rayon::prelude::*;
use ctrc_graph::CTRCAnalysisResult;
use ctrc_graph::hir_integration::convenience::analyze_and_pretty_print;

// entry point
fn main() -> Result<(), CompilerError> {
    let start = Instant::now();
    let result = std::panic::catch_unwind(|| run_compiler());

    let duration = start.elapsed();
    let millis = duration.as_millis();
    let nanos = duration.as_nanos();

    match result {
        Ok(Ok(_)) => {
            println!("Finished build in {}ms (or {}ns)", millis, nanos);
            Ok(())
        }
        Ok(Err(e)) => {
            eprintln!("Compilation failed after {}ms (or {}ns) \nError: {:?}", millis, nanos, e);
            Err(e)
        }
        Err(e) => {
            eprintln!("Panic occurred after {}ms (or {}ns)", millis, nanos);
            exit(1);
        }
    }
}

fn run_compiler() -> Result<(), CompilerError> {
    let string_pool = StringPool::new()
        .map_err(|_| CompilerError::FailedToAllocateStringPool)?;

    let files: Vec<PathBuf> = WalkDir::new("src")
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().is_file() && entry.path()
                .extension()
                .and_then(|ext| ext.to_str()) == Some("zeta")
        })
        .map(DirEntry::into_path)
        .collect();

    if files.is_empty() {
        eprintln!("No .zeta files found");
        return Ok(());
    }

    let arc = Arc::new(string_pool);
    let error_reporter = ErrorReporter::new();

    let modules_with_ctrc: Vec<ModuleWithArena> = files
        .iter()
        .map(|path| process_single_file(arc.clone(), path.clone()))
        .filter_map(|result| match result {
            Ok(v) if v.valid => Some(v),
            Err(e) => {
                eprintln!("Error: {:?}", e);
                None
            }
            _ => None
        })
        .collect();

    error_reporter.report_all();
    if error_reporter.has_errors() {
        exit(1);
    }

    println!("Processed {} modules", modules_with_ctrc.len());

    let hir_modules: Vec<HirModule> = modules_with_ctrc.iter().map(|m| m.module).collect();
    let builder = ModuleBuilder::run(&hir_modules, arc.clone());
    let indexes: Vec<usize> = topo_sort(&builder.modules);

    let mut backend = CraneliftBackend::new(arc.clone());

    for idx in indexes {
        let hir_module = hir_modules[idx];
        let mir_module = MirModuleLowerer::new(arc.clone()).lower_module(hir_module);
        backend.emit_module(&mir_module);
    }

    backend.finish().map_err(|e| CompilerError::FinishError(e))
}

fn process_single_file<'a, 'bump>(
    context: Arc<StringPool>,
    path: PathBuf,
) -> Result<ModuleWithArena<'a, 'bump>, CompilerError>
where
    'a: 'bump,
    'bump: 'a,
{
    let contents = std::fs::read_to_string(&path)
        .map_err(|e| CompilerError::FailedToReadFile(path.clone(), e))?;

    if contents.is_empty() {
        return Ok(ModuleWithArena {
            parse_and_hir_bump: Arc::new(GrowableAtomicBump::new()),
            module: HirModule::default(),
            valid: false,
        });
    }

    let contents_bytes = contents.as_bytes();

    let initial_capacity = {
        let base = 16 * 1024;
        std::cmp::max(base, contents_bytes.len() * 2 + 4096)
    };

    let bump: Arc<GrowableAtomicBump<'bump>> =
        Arc::new(GrowableAtomicBump::with_capacity_and_aligned(initial_capacity, 8)
            .map_err(|_| CompilerError::FailedToAllocateBump)?);

    let file_name_static = {
        let file_name_string = path.file_name()
            .and_then(|s| s.to_str())
            .ok_or_else(|| CompilerError::InvalidFileName(Vec::new()))?;

        let bytes = file_name_string.as_bytes();
        let stored = bump
            .alloc_many(bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;

        std::str::from_utf8(unsafe {
            std::slice::from_raw_parts(stored.as_ptr(), bytes.len())
        }).unwrap()
    };

    let contents_static = {
        let stored = bump
            .alloc_many(contents_bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;
        std::str::from_utf8(unsafe {
            std::slice::from_raw_parts(stored.as_ptr(), contents_bytes.len())
        }).unwrap()
    };

    let stmts = scribe_parser::parser::parse_program(
        contents_static,
        file_name_static,
        context.clone(),
        bump.clone(),
    ).map_err(CompilerError::ParserError)?;

    let mut lowerer = HirLowerer::new(context.clone(), bump.clone());
    let module = lowerer.lower_module(stmts);

    let temp_bump = GrowableBump::new(4096, 8);
    let ctrc: CTRCAnalysisResult = ctrc_graph::analyze_hir_for_ctrc(&module, &temp_bump);

    let mut temp_reporter = ErrorReporter::new();
    temp_reporter.add_source_file(file_name_static.into(), contents_static.into());
    analyze_ctrc_and_report(&ctrc, &*context, &mut temp_reporter, file_name_static);

    /*let string = analyze_and_pretty_print(module, &temp_bump, context.clone()).unwrap();
    println!("{}", string);*/

    if temp_reporter.has_errors() {
        temp_reporter.report_all();
    }

    Ok(ModuleWithArena {
        parse_and_hir_bump: bump,
        module,
        valid: true,
    })
}

struct ModuleWithArena<'a, 'bump> {
    #[allow(dead_code)] // This simply prevents parser and HIR bump from UB, doesn't need to be used
    parse_and_hir_bump: Arc<GrowableAtomicBump<'bump>>,

    module: HirModule<'a, 'bump>,

    valid: bool
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

    #[error("Failed to emit module: {0}")]
    FinishError(EmitError)
}