use crate::main_structs::{CompilerError, ModuleWithArena};
use codex_dependency_graph::dep_graph::DepGraph;
use emberforge_compiler::midend::copy_analysis::drop_glue::DropGlueRegistry;
use emberforge_compiler::midend::ir::module_lowerer::MirModuleLowerer;
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::hir::{HirModule, StrId};
use ir::ir_hasher::HashSet;
use ir::registry::global_registry::GlobalRegistry;
use ir::ssa_ir::Module;
use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;
use walkdir::WalkDir;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

pub fn compiler_lib_path<'a>() -> Result<PathBuf, CompilerError<'a>> {
    let exe = std::env::current_exe()
        .map_err(|_| CompilerError::SourceNotFound(PathBuf::from("current_exe()")))?;

    let root = exe
        .parent()
        .ok_or_else(|| CompilerError::SourceNotFound(PathBuf::from("compiler root")))?
        .to_path_buf();

    Ok(root.join("lib"))
}

pub(crate) fn collect_zeta_files<'a>(dir: &Path) -> Result<Vec<PathBuf>, CompilerError<'a>> {
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
        eprintln!(
            "hit `files.is_empty()`, iter: {:?}",
            WalkDir::new(dir).into_iter().collect::<Vec<_>>()
        );
        return Err(CompilerError::NoSourceFiles);
    }

    Ok(files)
}

pub fn collect_extern_c_names<'a, 'bump>(
    hir_modules: &[ir::hir::HirModule<'a, 'bump>],
) -> HashSet<StrId> {
    let mut externs = HashSet::default();
    for module in hir_modules {
        for item in module.items {
            if let ir::hir::Hir::Func(f) = item {
                if matches!(
                    f.function_metadata.extern_modifier,
                    ir::ast::ExternModifier::Abi(_)
                ) {
                    externs.insert(f.name);
                }
            }
        }
    }
    externs
}

pub fn parse_single_file<'a, 'bump>(
    pool: Arc<StringPool>,
    path: PathBuf,
) -> Result<ModuleWithArena<'a, 'bump>, CompilerError<'a>>
where
    'a: 'bump,
    'bump: 'a,
{
    let contents = std::fs::read_to_string(&path)
        .map_err(|e| CompilerError::FailedToReadFile(path.clone(), e))?;

    parse_single_module(pool, path, contents)
}

pub fn parse_single_file_from_source<'a, 'bump>(
    pool: Arc<StringPool>,
    name: PathBuf,
    source: String,
) -> Result<ModuleWithArena<'a, 'bump>, CompilerError<'a>>
where
    'a: 'bump,
    'bump: 'a,
{
    parse_single_module(pool, name, source)
}

fn parse_single_module<'a, 'bump>(
    pool: Arc<StringPool>,
    path: PathBuf,
    contents: String,
) -> Result<ModuleWithArena<'a, 'bump>, CompilerError<'a>>
where
    'a: 'bump,
    'bump: 'a,
{
    let file_name_str = path
        .to_str()
        .ok_or_else(|| CompilerError::InvalidFileName(Vec::new()))?;

    let name = StrId(pool.intern(file_name_str));

    if contents.is_empty() {
        return Ok(ModuleWithArena {
            bump: Arc::new(GrowableAtomicBump::new()),
            name,
            path,
            stmts: &[],
            parser_diagnostics: scribe_parser::parser::ParserDiagnostics::new(),
            source: StrId(pool.intern("")),
        });
    }

    let contents_bytes = contents.as_bytes();

    const BASE: usize = 16 * 1024;
    const FOUR_KB: usize = 4 * 1024;
    let initial_capacity = std::cmp::max(BASE, contents_bytes.len() * 2 + FOUR_KB);

    let atomic_bump = GrowableAtomicBump::with_capacity_and_aligned(initial_capacity, 8)
        .map_err(|_| CompilerError::FailedToAllocateBump)?;
    let bump = Arc::new(atomic_bump);

    let file_name_static: &str = {
        let bytes = file_name_str.as_bytes();
        let stored = bump
            .alloc_many(bytes)
            .ok_or(CompilerError::FailedToAllocateBump)?;
        std::str::from_utf8(unsafe { std::slice::from_raw_parts(stored.as_ptr(), bytes.len()) })
            .unwrap()
    };

    let source = StrId(pool.intern_bytes(contents_bytes));

    let parse_result =
        scribe_parser::parser::parse_program(source, file_name_static, pool.clone(), bump.clone());

    let stmts: &'bump [ir::ast::Stmt<'a, 'bump>] = bump.alloc_slice(&parse_result.statements);

    Ok(ModuleWithArena {
        bump,
        name,
        path,
        stmts,
        parser_diagnostics: parse_result.diagnostics,
        source,
    })
}

pub(crate) fn emit_all<'a, 'bump>(
    hir_modules: &[HirModule<'a, 'bump>],
    compilation_order: &[usize],
    backend: &mut CraneliftBackend,
    pool: Arc<StringPool>,
    extern_c_names: Rc<HashSet<StrId>>,
    dep_graph: &'a RefCell<DepGraph>,
    registry: GlobalRegistry<'a, 'bump>,
) where
    'bump: 'a,
{
    let glue_registry = DropGlueRegistry::new(&registry, pool.clone());

    let mir_module: Module = MirModuleLowerer::new(
        pool.clone(),
        extern_c_names.clone(),
        dep_graph,
        0, // sentinel, it's overwritten
        &glue_registry,
    )
    .lower_all_modules(hir_modules, compilation_order);

    CraneliftBackend::emit_module(backend, &mir_module);
}
