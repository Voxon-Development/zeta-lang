#![feature(allocator_api)]

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use codex_dependency_graph::dep_graph::{AstModule, DepGraph};
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::analysis_context::CopyAnalysisCtx;
use ir::ast::Stmt;
use ir::errors::reporter::ErrorReporter;
use ir::hir::{HirModule, StrId};
use ir::ir_hasher::{HashMap, HashSet};
use ir::registry::global_registry::GlobalRegistry;
use scribe_parser::hir_lowerer::HirLowerer;
use scribe_parser::hir_lowerer::lambda_hoisting::LambdaHoister;
use scribe_parser::hir_lowerer::monomorphization::Monomorphizer;
use sentinel_typechecker::TypeChecker;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

pub mod compilation_passes;
pub mod file_handling;
pub mod file_loader;
pub mod io_uring_file_loader;
pub mod link;
pub mod main_structs;
pub mod std_file_loader;

use crate::compilation_passes::pass_hir_lowering;
use crate::file_loader::FileLoader;
use crate::main_structs::{CompilerError as BuildError, ModuleWithArena};

pub struct Compiler<'a, 'bump> {
    pool: Arc<StringPool>,
    registry: GlobalRegistry<'a, 'bump>,

    dep_graph: &'a RefCell<DepGraph>,
    #[allow(unused)] // Avoids a UB
    dep_graph_storage: Box<RefCell<DepGraph>>,

    lowerer_bump: Box<GrowableBump<'bump>>,

    type_checker: Rc<RefCell<TypeChecker<'a, 'bump>>>,
    cpy_ctx: Rc<RefCell<CopyAnalysisCtx<'a, 'bump>>>,

    module_ids: HashMap<StrId, usize>,
    next_module_idx: usize,
    stdlib_module_ids: HashSet<usize>,
    modules: HashMap<usize, ModuleWithArena<'a, 'bump>>,
    hir_modules: HashMap<usize, HirModule<'a, 'bump>>, // pre-monomorphization
    codegen_hir_modules: HashMap<usize, HirModule<'a, 'bump>>, // post-monomorphization
    loaded_sources: HashMap<String, String>,
}

impl<'a, 'bump> Compiler<'a, 'bump>
where
    'bump: 'a,
    'a: 'bump,
{
    pub fn new() -> Result<Self, BuildError<'a>> {
        let pool = Arc::new(StringPool::new().map_err(|_| BuildError::FailedToAllocateStringPool)?);
        let registry = GlobalRegistry::new();

        let dep_graph_storage = Box::new(RefCell::new(DepGraph::new()));
        // SAFETY: Box's heap allocation is stable across Compiler moves.
        // dep_graph_storage's contents are only ever mutated through
        // `.borrow_mut()` on this erased reference or on the boxed value
        // directly, never replaced wholesale. RefCell's runtime borrow
        // tracking is what makes this genuinely sound (not just
        // borrow-checker-satisfied): any illegal aliasing that would occur
        // panics at the .borrow()/.borrow_mut() call site instead of
        // silently producing UB
        let dep_graph: &'a RefCell<DepGraph> =
            unsafe { &*(dep_graph_storage.as_ref() as *const RefCell<DepGraph>) };

        let lowerer_bump = Box::new(GrowableBump::new(4096, 8));
        let lowerer_bump_ref: &'bump GrowableBump<'bump> =
            unsafe { &*(lowerer_bump.as_ref() as *const GrowableBump<'bump>) };

        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            pool.clone(),
        )));
        let type_checker = Rc::new(RefCell::new(TypeChecker::new(
            dep_graph,
            lowerer_bump_ref,
            cpy_ctx.clone(),
            pool.clone(),
        )));

        Ok(Self {
            pool,
            registry,
            dep_graph,
            dep_graph_storage,
            lowerer_bump,
            type_checker,
            cpy_ctx,
            module_ids: HashMap::default(),
            next_module_idx: 0,
            stdlib_module_ids: HashSet::default(),
            modules: HashMap::default(),
            hir_modules: HashMap::default(),
            codegen_hir_modules: HashMap::default(),
            loaded_sources: HashMap::default(),
        })
    }

    pub fn dep_graph(&self) -> &'a RefCell<DepGraph> {
        self.dep_graph
    }

    pub fn path_for_module(&self, module_idx: usize) -> Option<&Path> {
        self.modules.get(&module_idx).map(|m| m.path.as_path())
    }

    pub fn module_idx_for_path(&self, path: &Path) -> Option<usize> {
        let name = StrId(self.pool.intern(path.to_string_lossy().as_ref()));
        self.module_ids.get(&name).copied()
    }

    pub fn type_checker(&self) -> Rc<RefCell<TypeChecker<'a, 'bump>>> {
        self.type_checker.clone()
    }

    pub fn dep_graph_ref(&self) -> &'a RefCell<DepGraph> {
        self.dep_graph
    }

    pub fn source_text(&self, module_idx: usize) -> Option<String> {
        Some(self.modules.get(&module_idx)?.source.to_string())
    }

    pub fn load_directory<L: FileLoader>(
        &mut self,
        loader: &L,
        dir: &Path,
        is_stdlib: bool,
    ) -> Result<ErrorReporter<'a>, BuildError<'a>> {
        let files = crate::file_handling::collect_zeta_files(dir)?;

        let sources = loader
            .load_files(&files)
            .map_err(|e| BuildError::FailedToReadFile(dir.to_path_buf(), e))?;

        let mut reporter = ErrorReporter::new();

        for file in sources {
            reporter.merge(self.load_module(&file.path, file.source, is_stdlib)?);
        }

        Ok(reporter)
    }

    fn load_module(
        &mut self,
        path: &Path,
        source: String,
        is_stdlib: bool,
    ) -> Result<ErrorReporter<'a>, BuildError<'a>> {
        let canonical_name = path
            .to_str()
            .ok_or_else(|| BuildError::InvalidFileName(Vec::new()))?;

        let parsed = crate::file_handling::parse_single_file_from_source(
            self.pool.clone(),
            path.to_path_buf(),
            source,
        )?;
        let reporter = self.ingest_parsed_module(canonical_name, parsed, is_stdlib);

        Ok(reporter)
    }

    fn load_module_from_disk(
        &mut self,
        path: &Path,
        is_stdlib: bool,
    ) -> Result<ErrorReporter<'a>, BuildError<'a>> {
        let canonical_name = path
            .to_str()
            .ok_or_else(|| BuildError::InvalidFileName(Vec::new()))?;

        let parsed =
            crate::file_handling::parse_single_file(self.pool.clone(), path.to_path_buf())?;
        let reporter = self.ingest_parsed_module(canonical_name, parsed, is_stdlib);

        Ok(reporter)
    }

    pub fn emit(
        &mut self,
        out_dir: &Path,
        optimize: bool,
        verbose: bool,
        emit_obj: bool,
    ) -> Result<PathBuf, BuildError<'a>> {
        let compilation_order = self.dep_graph.borrow().get_module_compilation_order();

        let ordered_hir: Vec<HirModule<'a, 'bump>> = (0..self.next_module_idx)
            .filter_map(|i| self.codegen_hir_modules.get(&i).copied())
            .collect();

        let mut backend: CraneliftBackend =
            CraneliftBackend::new(self.pool.clone(), optimize, verbose);

        crate::file_handling::emit_all(
            &ordered_hir,
            &compilation_order,
            &mut backend,
            self.pool.clone(),
            Rc::new(crate::file_handling::collect_extern_c_names(&ordered_hir)),
            self.dep_graph,
            self.registry.clone(),
        );

        let out_obj = backend
            .finish(&out_dir.to_path_buf())
            .map_err(|e| BuildError::FinishError(Box::new(e)))?;

        if emit_obj {
            Ok(out_obj)
        } else {
            let program_path = out_dir.join("program");
            crate::link::link(
                &[out_obj.to_str().unwrap()],
                program_path.to_str().unwrap(),
                true,
            )?;
            Ok(program_path)
        }
    }

    pub fn ast_stmts(&self, module_idx: usize) -> Option<&'bump [Stmt<'a, 'bump>]> {
        self.modules.get(&module_idx).map(|m| m.stmts)
    }

    fn module_idx_for(&mut self, canonical_name: StrId) -> usize {
        if let Some(&idx) = self.module_ids.get(&canonical_name) {
            return idx;
        }
        let idx = self.next_module_idx;
        self.next_module_idx += 1;
        self.module_ids.insert(canonical_name, idx);
        idx
    }

    pub fn open_module(&mut self, path: &Path, source: String) -> ErrorReporter<'a> {
        self.update_module(path, source)
    }

    pub fn update_module(&mut self, path: &Path, source: String) -> ErrorReporter<'a> {
        let canonical_name = StrId(self.pool.intern(&path.to_string_lossy()));
        let module_idx = self.module_idx_for(canonical_name);

        let mut reporter = self.make_reporter();

        let parsed = match crate::file_handling::parse_single_file_from_source(
            self.pool.clone(),
            PathBuf::from(canonical_name.as_str()),
            source,
        ) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("error: {}", e);
                return reporter;
            }
        };

        for perr in &parsed.parser_diagnostics.errors {
            reporter.add_parser_error(perr.clone());
        }
        reporter.add_source_file(canonical_name.to_string(), parsed.source.to_string());
        if parsed.parser_diagnostics.has_errors() {
            return reporter;
        }

        let bump = GrowableBump::new(4096, 8);
        let mut stmt_vec: Vec<Stmt<'a, 'bump>, &GrowableBump<'bump>> = Vec::new_in(&bump);
        for stmt in parsed.stmts {
            stmt_vec.push(*stmt);
        }

        let hir = match pass_hir_lowering(
            stmt_vec,
            self.pool.clone(),
            parsed.bump.clone(),
            self.dep_graph,
            module_idx,
            self.registry.clone(),
        ) {
            Ok(h) => h,
            Err(e) => {
                eprintln!("error: {}", e);
                return reporter;
            }
        };

        let ast_module = AstModule {
            name: canonical_name,
            path: parsed.path.clone(),
            stmts: parsed.stmts,
        };

        let importers = self.dep_graph.borrow().get_module_importers(module_idx);

        self.dep_graph
            .borrow_mut()
            .update_module_items(module_idx, &ast_module, &self.pool);

        for imp_idx in importers {
            if let Some(imp_module) = self.modules.get(&imp_idx) {
                let imp_ast = codex_dependency_graph::dep_graph::AstModule {
                    name: imp_module.name,
                    path: parsed.path.clone(),
                    stmts: imp_module.stmts,
                };
                self.dep_graph
                    .borrow_mut()
                    .extract_edges_for_module(imp_idx, &imp_ast, &self.pool);
            }
        }

        self.modules.insert(module_idx, parsed);
        self.hir_modules.insert(module_idx, hir);

        let invalidation_set = self
            .dep_graph
            .borrow()
            .reverse_deps_transitive_modules(module_idx);

        self.registry.unregister_module(canonical_name);
        {
            let mut checker = self.type_checker.borrow_mut();
            for &m in &invalidation_set {
                if let Some(hir) = self.hir_modules.get(&m) {
                    checker.register_module(hir, m);
                }
            }
        }

        let updated: Vec<(usize, &HirModule<'a, 'bump>)> = invalidation_set
            .iter()
            .filter_map(|&m| self.hir_modules.get(&m).map(|h| (m, h)))
            .collect();
        self.cpy_ctx.borrow_mut().recompute(&updated);

        {
            let mut checker = self.type_checker.borrow_mut();
            for &m in &invalidation_set {
                if let Some(hir) = self.hir_modules.get(&m) {
                    checker.check_module_body(hir, m);
                }
            }
            for err in checker.errors() {
                reporter.add_type_error(err.clone());
            }
        }
        reporter
    }

    pub fn bootstrap_stdlib(
        &mut self,
        stdlib_path: &Path,
    ) -> Result<ErrorReporter<'a>, BuildError<'a>> {
        let stdlib_files = crate::file_handling::collect_zeta_files(stdlib_path)?;

        let mut reporter = ErrorReporter::new();
        for file in stdlib_files {
            reporter.merge(self.load_module_from_disk(&file, true)?);
        }

        Ok(reporter)
    }

    fn make_reporter(&self) -> ErrorReporter<'a> {
        let mut reporter = ErrorReporter::new();

        for (path, source) in &self.loaded_sources {
            reporter.add_source_file(path.clone(), source.clone());
        }

        reporter
    }

    fn ingest_parsed_module(
        &mut self,
        name: &str,
        parsed: ModuleWithArena<'a, 'bump>,
        is_stdlib: bool,
    ) -> ErrorReporter<'a> {
        let canonical_name = StrId(self.pool.intern(name));
        let module_idx = self.module_idx_for(canonical_name);

        self.loaded_sources.insert(
            parsed.path.to_string_lossy().to_string(),
            parsed.source.to_string(),
        );
        let mut reporter = self.make_reporter();
        for perr in &parsed.parser_diagnostics.errors {
            reporter.add_parser_error(perr.clone());
        }

        if parsed.parser_diagnostics.has_errors() {
            return reporter;
        }

        if is_stdlib {
            self.stdlib_module_ids.insert(module_idx);
        }

        let mut stmt_vec = Vec::new_in(self.lowerer_bump.as_ref());
        for stmt in parsed.stmts {
            stmt_vec.push(*stmt);
        }

        let ast_module = codex_dependency_graph::dep_graph::AstModule {
            name: canonical_name,
            path: parsed.path.clone(),
            stmts: parsed.stmts,
        };

        // Register this module's import edges FIRST, so lower_module can see them.
        self.dep_graph
            .borrow_mut()
            .update_module_items(module_idx, &ast_module, &self.pool);

        let mut lowerer = HirLowerer::new(
            self.pool.clone(),
            parsed.bump.clone(),
            self.dep_graph,
            self.registry.clone(),
        );
        let hir = lowerer.lower_module(stmt_vec, module_idx);
        self.hir_modules.insert(module_idx, hir);

        let importers = { self.dep_graph.borrow().get_module_importers(module_idx) };
        for imp_idx in importers {
            if let Some(imp_module) = self.modules.get(&imp_idx) {
                let imp_ast = codex_dependency_graph::dep_graph::AstModule {
                    name: imp_module.name,
                    path: parsed.path.clone(),
                    stmts: imp_module.stmts,
                };
                self.dep_graph
                    .borrow_mut()
                    .extract_edges_for_module(imp_idx, &imp_ast, &self.pool);
            }
        }

        if is_stdlib {
            for &existing_idx in self.module_ids.values() {
                if existing_idx != module_idx && !self.stdlib_module_ids.contains(&existing_idx) {
                    self.dep_graph
                        .borrow_mut()
                        .register_import(existing_idx, module_idx);
                }
            }
        } else {
            for &stdlib_idx in &self.stdlib_module_ids {
                self.dep_graph
                    .borrow_mut()
                    .register_import(module_idx, stdlib_idx);
            }
        }

        let invalidation_set = {
            self.dep_graph
                .borrow()
                .reverse_deps_transitive_modules(module_idx)
        };

        self.registry.unregister_module(canonical_name);
        {
            let mut checker = self.type_checker.borrow_mut();
            for &m in &invalidation_set {
                if let Some(hir) = self.hir_modules.get(&m) {
                    checker.register_module(hir, m);
                }
            }
        }

        let updated: Vec<(usize, &HirModule<'a, 'bump>)> = invalidation_set
            .iter()
            .filter_map(|&m| self.hir_modules.get(&m).map(|h| (m, h)))
            .collect();
        self.cpy_ctx.borrow_mut().recompute(&updated);

        {
            {
                let mut checker = self.type_checker.borrow_mut();
                for &m in &invalidation_set {
                    if let Some(hir) = self.hir_modules.get(&m) {
                        checker.check_module_body(hir, m);
                    }
                }
            }
            let mut checker = self.type_checker.borrow_mut();
            for err in checker.take_errors() {
                reporter.add_type_error(err);
            }
        }

        let checked_hir = self.hir_modules[&module_idx];

        let hoister = LambdaHoister::new(parsed.bump.clone(), self.pool.clone(), checked_hir.name);
        let hoisted_module = hoister.run(checked_hir);

        let monomorphizer = Monomorphizer::new(
            self.pool.clone(),
            parsed.bump.clone(),
            lowerer.ctx.functions.clone(),
            &lowerer.ctx,
            self.registry.instantiated_functions.clone(),
            self.registry.instantiated_structs.clone(),
            self.registry.instantiated_struct_origins.clone(),
        );
        let monomorphized_module = monomorphizer.run(hoisted_module);

        self.modules.insert(module_idx, parsed);
        self.codegen_hir_modules
            .insert(module_idx, monomorphized_module);
        reporter
    }

    pub fn close_module(&mut self, name: &str) {
        let canonical_name = StrId(self.pool.intern(name));
        if let Some(&idx) = self.module_ids.get(&canonical_name) {
            self.modules.remove(&idx);
            self.hir_modules.remove(&idx);
            self.cpy_ctx.borrow_mut().remove_module(idx);
        }
    }
}
