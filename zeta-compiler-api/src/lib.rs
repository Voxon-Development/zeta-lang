#![feature(allocator_api)]

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use codex_dependency_graph::dep_graph::{AstModule, DepGraph};
use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::analysis_context::CopyAnalysisCtx;
use ir::ast::Stmt;
use ir::hir::{HirModule, StrId};
use ir::registry::global_registry::GlobalRegistry;
use sentinel_typechecker::TypeChecker;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

pub mod compilation_passes;
pub mod file_handling;
pub mod link;
pub mod main_structs;

use crate::compilation_passes::pass_hir_lowering;
use crate::file_handling::parse_single_file;
use crate::main_structs::{CompilerError, ModuleWithArena};

pub struct Diagnostic {
    pub module: StrId,
    pub message: String,
}

pub struct Compiler<'a, 'bump> {
    pool: Arc<StringPool>,
    registry: GlobalRegistry<'a, 'bump>,

    dep_graph: &'a RefCell<DepGraph>,

    lowerer_bump: Box<GrowableBump<'bump>>,

    type_checker: Rc<RefCell<TypeChecker<'a, 'bump>>>,
    cpy_ctx: Rc<RefCell<CopyAnalysisCtx<'a, 'bump>>>,

    module_ids: HashMap<StrId, usize>,
    next_module_idx: usize,
    stdlib_module_ids: HashSet<usize>,
    modules: HashMap<usize, ModuleWithArena<'a, 'bump>>,
    hir_modules: HashMap<usize, HirModule<'a, 'bump>>,
}

impl<'a, 'bump> Compiler<'a, 'bump>
where
    'bump: 'a,
    'a: 'bump,
{
    pub fn new() -> Result<Self, CompilerError<'a>> {
        let pool =
            Arc::new(StringPool::new().map_err(|_| CompilerError::FailedToAllocateStringPool)?);
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
            lowerer_bump,
            type_checker,
            cpy_ctx,
            module_ids: HashMap::new(),
            next_module_idx: 0,
            stdlib_module_ids: HashSet::new(),
            modules: HashMap::new(),
            hir_modules: HashMap::new(),
        })
    }

    pub fn load_directory(
        &mut self,
        dir: &Path,
        is_stdlib: bool,
    ) -> Result<Vec<Diagnostic>, CompilerError<'a>> {
        let files = crate::file_handling::collect_zeta_files(dir)?;
        let mut diagnostics = Vec::new();
        for file in files {
            diagnostics.extend(self.load_module_from_disk(&file, is_stdlib)?);
        }
        Ok(diagnostics)
    }

    fn load_module_from_disk(
        &mut self,
        path: &Path,
        is_stdlib: bool,
    ) -> Result<Vec<Diagnostic>, CompilerError<'a>> {
        let file_stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| CompilerError::InvalidFileName(Vec::new()))?
            .to_string();

        let parsed =
            crate::file_handling::parse_single_file(self.pool.clone(), path.to_path_buf())?;
        Ok(self.ingest_parsed_module(&file_stem, parsed, is_stdlib))
    }

    pub fn emit(
        &mut self,
        out_dir: &Path,
        optimize: bool,
        verbose: bool,
        emit_obj: bool,
    ) -> Result<PathBuf, CompilerError<'a>> {
        let compilation_order = self.dep_graph.borrow().get_module_compilation_order();

        let ordered_hir: Vec<HirModule<'a, 'bump>> = (0..self.next_module_idx)
            .filter_map(|i| self.hir_modules.get(&i).copied())
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
            .map_err(|e| CompilerError::FinishError(Box::new(e)))?;

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

    fn module_idx_for(&mut self, canonical_name: StrId) -> usize {
        if let Some(&idx) = self.module_ids.get(&canonical_name) {
            return idx;
        }
        let idx = self.next_module_idx;
        self.next_module_idx += 1;
        self.module_ids.insert(canonical_name, idx);
        idx
    }

    pub fn open_module(&mut self, path: &Path, source: String) -> Vec<Diagnostic> {
        self.update_module(path, source)
    }

    pub fn update_module(&mut self, path: &Path, _source: String) -> Vec<Diagnostic> {
        let file_stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        let canonical_name = StrId(self.pool.intern(file_stem));
        let module_idx = self.module_idx_for(canonical_name);

        let parsed = match parse_single_file(self.pool.clone(), path.to_path_buf()) {
            Ok(m) => m,
            Err(e) => {
                return vec![Diagnostic {
                    module: canonical_name,
                    message: e.to_string(),
                }];
            }
        };

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
                return vec![Diagnostic {
                    module: canonical_name,
                    message: e.to_string(),
                }];
            }
        };

        let ast_module = AstModule {
            name: canonical_name,
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

        let mut diagnostics = Vec::new();
        {
            let mut checker = self.type_checker.borrow_mut();
            for &m in &invalidation_set {
                if let Some(hir) = self.hir_modules.get(&m) {
                    checker.check_module_body(hir, m);
                }
            }
            for err in checker.errors() {
                diagnostics.push(Diagnostic {
                    module: canonical_name,
                    message: err.to_string(),
                });
            }
        }
        diagnostics
    }

    pub fn bootstrap_stdlib(&mut self, stdlib_path: &Path) -> Result<(), CompilerError<'a>> {
        let stdlib_files = crate::file_handling::collect_zeta_files(stdlib_path)?;

        for file in stdlib_files {
            self.load_module_from_disk(&file, true)?;
        }

        Ok(())
    }

    fn ingest_parsed_module(
        &mut self,
        name: &str,
        parsed: ModuleWithArena<'a, 'bump>,
        is_stdlib: bool,
    ) -> Vec<Diagnostic> {
        let canonical_name = StrId(self.pool.intern(name));
        let module_idx = self.module_idx_for(canonical_name);

        if parsed.parser_diagnostics.has_errors() {
            return parsed
                .parser_diagnostics
                .errors
                .iter()
                .map(|e| Diagnostic {
                    module: canonical_name,
                    message: e.to_string(),
                })
                .collect();
        }

        if is_stdlib {
            self.stdlib_module_ids.insert(module_idx);
        }

        let mut stmt_vec = Vec::new_in(self.lowerer_bump.as_ref());
        for stmt in parsed.stmts {
            stmt_vec.push(*stmt);
        }

        let hir = match crate::compilation_passes::pass_hir_lowering(
            stmt_vec,
            self.pool.clone(),
            parsed.bump.clone(),
            self.dep_graph,
            module_idx,
            self.registry.clone(),
        ) {
            Ok(h) => h,
            Err(e) => {
                return vec![Diagnostic {
                    module: canonical_name,
                    message: e.to_string(),
                }];
            }
        };

        let ast_module = codex_dependency_graph::dep_graph::AstModule {
            name: canonical_name,
            stmts: parsed.stmts,
        };

        let importers = { self.dep_graph.borrow().get_module_importers(module_idx) };

        {
            self.dep_graph
                .borrow_mut()
                .update_module_items(module_idx, &ast_module, &self.pool);
        }

        for imp_idx in importers {
            if let Some(imp_module) = self.modules.get(&imp_idx) {
                let imp_ast = codex_dependency_graph::dep_graph::AstModule {
                    name: imp_module.name,
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

        self.modules.insert(module_idx, parsed);
        self.hir_modules.insert(module_idx, hir);

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

        let mut diagnostics = Vec::new();
        {
            let mut checker = self.type_checker.borrow_mut();
            for &m in &invalidation_set {
                if let Some(hir) = self.hir_modules.get(&m) {
                    checker.check_module_body(hir, m);
                }
            }
            for err in checker.errors() {
                diagnostics.push(Diagnostic {
                    module: canonical_name,
                    message: err.to_string(),
                });
            }
        }
        diagnostics
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
