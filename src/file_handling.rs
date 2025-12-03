use engraver_assembly_emit::backend::Backend;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use walkdir::WalkDir;
use codex_dependency_graph::module_collection_builder::ModuleBuilder;
use codex_dependency_graph::topo::topo_sort;
use codex_dependency_graph::DepGraph;
use ctrc_graph::{analyze_ctrc_and_report, CTRCAnalysisResult};
use emberforge_compiler::midend::ir::module_lowerer::MirModuleLowerer;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::errors::reporter::ErrorReporter;
use ir::hir::HirModule;
use ir::ssa_ir::Module;
use scribe_parser::hir_lowerer::HirLowerer;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;
use crate::main_structs::{CompilerError, ModuleWithArena};

pub(crate) fn compiler_lib_path() -> Result<PathBuf, CompilerError> {
    let exe = std::env::current_exe()
        .map_err(|_| CompilerError::SourceNotFound(PathBuf::from("current_exe()")))?;

    let root = exe
        .parent()
        .ok_or_else(|| CompilerError::SourceNotFound(PathBuf::from("compiler root")))?
        .to_path_buf();

    Ok(root.join("lib"))
}

pub(crate) fn collect_zeta_files(dir: &Path) -> Result<Vec<PathBuf>, CompilerError> {
    if !dir.exists() {
        return Err(CompilerError::SourceNotFound(dir.to_path_buf()));
    }

    let files = WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().and_then(|e| e.to_str()) == Some("zeta"))
        .map(|e| e.into_path())
        .collect::<Vec<_>>();

    if files.is_empty() {
        return Err(CompilerError::NoSourceFiles);
    }

    Ok(files)
}

pub(crate) fn compile_files<'a, 'bump>(
    files: &[PathBuf],
    pool: Arc<StringPool>
) -> Result<Vec<ModuleWithArena<'a, 'bump>>, CompilerError> {
    let modules: Vec<ModuleWithArena> = files
        .iter()
        .map(|f| process_single_file(pool.clone(), f.clone()))
        .filter_map(|res| res.ok())
        .collect::<Vec<_>>();

    if modules.is_empty() {
        return Err(CompilerError::NoSourceFiles);
    }

    Ok(modules)
}

pub(crate) fn emit_all(
    modules: Vec<HirModule>,
    backend: &mut CraneliftBackend,
    pool: Arc<StringPool>
) {
    let mut dep_graph = DepGraph::new();
    
    dep_graph.build_from_hir(&modules, &pool);

    let len = modules.len();
    if len > 1 {
        let stdlib_idx: usize = 0;
        let user_indices: Vec<usize> = (1..len).collect();
        dep_graph.link_stdlib_to_user(stdlib_idx, &user_indices);
    }
    
    let compilation_order: Vec<usize> = dep_graph.get_module_compilation_order();
    
    for module_idx in compilation_order {
        if module_idx < len {
            let hir_module: HirModule = modules[module_idx];
            let mir_module: Module = MirModuleLowerer::new(pool.clone()).lower_module(hir_module);
            CraneliftBackend::emit_module(backend, &mir_module);
        } else {
            eprintln!("Warning: index {} is out of bounds for module list of length {}", module_idx, len);
        }
    }
}

pub(crate) fn process_single_file<'a, 'bump>(
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
            parser_diagnostics: scribe_parser::parser::ParserDiagnostics::new(),
            valid: false,
        });
    }

    let contents_bytes = contents.as_bytes();

    const BASE: usize = 16 * 1024;
    const FOUR_KILOBYTES: usize = 4 * 1024;
    let initial_capacity: usize = std::cmp::max(BASE, contents_bytes.len() * 2 + FOUR_KILOBYTES);

    let atomic_bump = GrowableAtomicBump::with_capacity_and_aligned(initial_capacity, 8)
        .map_err(|_| CompilerError::FailedToAllocateBump)?;

    let bump: Arc<GrowableAtomicBump<'bump>> = Arc::new(atomic_bump);

    let file_name_static: &str = {
        let file_name_string = path.file_name()
            .and_then(|s| s.to_str())
            .ok_or_else(|| CompilerError::InvalidFileName(Vec::new()))?;

        let bytes = file_name_string.as_bytes();
        let stored: &mut [u8] = bump
            .alloc_many(bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;

        std::str::from_utf8(unsafe { std::slice::from_raw_parts(stored.as_ptr(), bytes.len()) })
            .unwrap()
    };

    let contents_static: &str = {
        let stored: &mut [u8] = bump
            .alloc_many(contents_bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;
        std::str::from_utf8(unsafe { std::slice::from_raw_parts(stored.as_ptr(), contents_bytes.len()) })
            .unwrap()
    };

    println!("{}", contents_static);

    let parse_result = scribe_parser::parser::parse_program(
        contents_static,
        file_name_static,
        context.clone(),
        bump.clone(),
    );

    //println!("{:#?}", parse_result.statements);

    let mut lowerer = HirLowerer::new(context.clone(), bump.clone());
    let module = lowerer.lower_module(parse_result.statements);

    let temp_bump = GrowableBump::new(4096, 8);
    let ctrc: CTRCAnalysisResult = ctrc_graph::analyze_hir_for_ctrc(&module, &temp_bump);

    let mut temp_reporter: ErrorReporter = ErrorReporter::new();
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
        parser_diagnostics: parse_result.diagnostics,
        valid: true,
    })
}