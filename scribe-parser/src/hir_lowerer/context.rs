use codex_dependency_graph::dep_graph::DepGraph;
use ir::auto_imports::AutoImportRegistry;
use ir::errors::reporter::ErrorReporter;
use ir::hir::{HirEnum, HirFuncProto};
use ir::hir::{HirFunc, HirInterface, HirStruct, HirType, StrId};
use ir::ir_hasher::{FxHashBuilder, FxHashMap};
use ir::registry::global_registry::GlobalRegistry;
use ir::span::SourceSpan;
use std::cell::RefCell;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

use crate::optimized_string_buffering::build_module_scoped_name;

pub type FxHashSet<T> = HashSet<T, FxHashBuilder>;

pub struct LoweringCtx<'a, 'bump> {
    pub structs: Rc<RefCell<FxHashMap<StrId, HirStruct<'a, 'bump>>>>,
    pub interfaces: Rc<RefCell<FxHashMap<StrId, HirInterface<'a, 'bump>>>>,
    pub enums: Rc<RefCell<FxHashMap<StrId, HirEnum<'a, 'bump>>>>,
    pub functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    pub func_protos: RefCell<FxHashMap<StrId, HirFuncProto<'a, 'bump>>>,
    pub type_bindings: RefCell<FxHashMap<StrId, HirType<'a, 'bump>>>,
    pub variable_types: RefCell<FxHashMap<StrId, HirType<'a, 'bump>>>,
    pub generic_params: RefCell<HashSet<StrId>>,
    pub context: Arc<StringPool>,
    pub dep_graph: &'a RefCell<DepGraph>,
    pub imported_modules: RefCell<FxHashMap<StrId, usize>>,
    pub bump: Arc<GrowableAtomicBump<'bump>>,
    pub module_idx: usize,
    pub struct_interfaces: Rc<RefCell<FxHashMap<StrId, Vec<StrId>>>>,
    pub struct_methods: Rc<RefCell<FxHashMap<StrId, FxHashMap<StrId, StrId>>>>,
    pub instantiated_structs: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    pub instantiated_struct_origins:
        Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,

    /// Concrete type of `this` while lowering the current method's
    /// signature/body. Set/cleared per-method in `lower_impl_decl`.
    pub current_self_type: RefCell<Option<HirType<'a, 'bump>>>,
    pub named_imports: RefCell<FxHashMap<StrId, usize>>,
    pub auto_imports: Rc<RefCell<AutoImportRegistry>>,
    pub lowering_errors: RefCell<Vec<(String, SourceSpan<'a>)>>,
}

impl<'a, 'bump> LoweringCtx<'a, 'bump> {
    pub(super) fn mangle_type_name(&self, name: StrId) -> StrId {
        let Some(pkg) = self.dep_graph.borrow().get_module_package(self.module_idx) else {
            return name;
        };

        let pkg_str = self.context.resolve_string(&pkg);
        let segments: Vec<StrId> = pkg_str
            .split("::")
            .map(|seg| StrId(self.context.intern(seg)))
            .collect();

        build_module_scoped_name(&segments, name, None, self.context.clone())
    }

    pub(super) fn resolve_type_path_name(
        &self,
        path: &[StrId],
        name: StrId,
        span: SourceSpan<'a>,
    ) -> StrId {
        if path.is_empty() {
            let s = name.as_str();
            if matches!(s, "Drop" | "Copy" | "Clone" | "Allocator" | "RawAllocator") {
                return name;
            }
            if let Some(&target_module_idx) = self.named_imports.borrow().get(&name) {
                return self.mangle_via_module(target_module_idx, name);
            }
            if let Some(&target_module_idx) = self.imported_modules.borrow().get(&name) {
                return self.mangle_via_module(target_module_idx, name);
            }

            let own_candidate = self.mangle_type_name(name);
            if self.structs.borrow().contains_key(&own_candidate)
                || self.enums.borrow().contains_key(&own_candidate)
                || self.interfaces.borrow().contains_key(&own_candidate)
            {
                return own_candidate;
            }

            let mut found: Option<(usize, StrId)> = None;
            for auto_path in self.auto_imports.borrow().paths() {
                let segments: Vec<StrId> = auto_path
                    .iter()
                    .map(|seg| StrId(self.context.intern(seg)))
                    .collect();
                let Some(target_module_idx) =
                    self.dep_graph.borrow().resolve_module_path(&segments)
                else {
                    continue;
                };
                if target_module_idx == self.module_idx {
                    continue; // already checked above as `own_candidate`
                }
                let candidate = self.mangle_via_module(target_module_idx, name);
                let exists = self.structs.borrow().contains_key(&candidate)
                    || self.enums.borrow().contains_key(&candidate)
                    || self.interfaces.borrow().contains_key(&candidate);
                if !exists {
                    continue;
                }
                match found {
                    None => found = Some((target_module_idx, candidate)),
                    Some((existing_idx, _)) if existing_idx != target_module_idx => {
                        let existing_pkg = self
                            .dep_graph
                            .borrow()
                            .get_module_package(existing_idx)
                            .map(|p| p.to_string())
                            .unwrap_or_default();
                        let new_pkg = self
                            .dep_graph
                            .borrow()
                            .get_module_package(target_module_idx)
                            .map(|p| p.to_string())
                            .unwrap_or_default();
                        self.record_error(
                            format!(
                                "`{}` is ambiguous: auto-imported from multiple packages \
                                 ({}, {}); add an explicit `import` to disambiguate",
                                s, existing_pkg, new_pkg,
                            ),
                            span,
                        );
                    }
                    _ => {}
                }
            }

            return found
                .map(|(_, candidate)| candidate)
                .unwrap_or(own_candidate);
        }

        let alias = *path.last().unwrap();
        if let Some(&target_module_idx) = self.imported_modules.borrow().get(&alias) {
            return self.mangle_via_module(target_module_idx, name);
        }

        if let Some(target_module_idx) = self.dep_graph.borrow().resolve_module_path(path) {
            return self.mangle_via_module(target_module_idx, name);
        }

        let path_str = path
            .iter()
            .map(|s| self.context.resolve_string(s).to_string())
            .collect::<Vec<_>>()
            .join("::");
        self.record_error(
            format!(
                "cannot resolve type path `{}::{}`: no module is registered for path `{}`, \
                 and `{}` is not an imported module alias. Check for a missing `import` or \
                 a module that hasn't been compiled yet.",
                path_str,
                self.context.resolve_string(&name),
                path_str,
                self.context.resolve_string(&alias),
            ),
            span,
        );
        name
    }

    fn mangle_via_module(&self, target_module_idx: usize, name: StrId) -> StrId {
        let Some(pkg) = self
            .dep_graph
            .borrow()
            .get_module_package(target_module_idx)
        else {
            return name;
        };
        let pkg_str = self.context.resolve_string(&pkg);
        let segments: Vec<StrId> = pkg_str
            .split("::")
            .map(|seg| StrId(self.context.intern(seg)))
            .collect();
        build_module_scoped_name(&segments, name, None, self.context.clone())
    }

    pub(crate) fn record_error(&self, message: impl Into<String>, span: SourceSpan<'a>) {
        self.lowering_errors
            .borrow_mut()
            .push((message.into(), span));
    }
}

pub struct HirLowerer<'a, 'bump> {
    pub ctx: LoweringCtx<'a, 'bump>,
    pub error_reporter: ErrorReporter<'a>,
    _phantom: PhantomData<&'bump ()>,
}

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn new(
        context: Arc<StringPool>,
        bump: Arc<GrowableAtomicBump<'bump>>,
        dep_graph: &'a RefCell<DepGraph>,
        registry: GlobalRegistry<'a, 'bump>,
        auto_imports: Rc<RefCell<AutoImportRegistry>>,
    ) -> Self {
        Self {
            ctx: LoweringCtx {
                structs: registry.structs,
                enums: registry.enums,
                functions: registry.functions.clone(),
                func_protos: RefCell::new(FxHashMap::default()),
                interfaces: registry.interfaces,
                type_bindings: RefCell::new(FxHashMap::default()),
                variable_types: RefCell::new(FxHashMap::default()),
                generic_params: RefCell::new(HashSet::default()),
                context: context.clone(),
                bump: bump.clone(),
                imported_modules: RefCell::new(FxHashMap::default()),
                dep_graph,
                module_idx: usize::MAX,
                struct_interfaces: registry.struct_interfaces,
                struct_methods: registry.struct_methods,
                current_self_type: RefCell::new(None),
                instantiated_structs: registry.instantiated_structs.clone(),
                instantiated_struct_origins: registry.instantiated_struct_origins.clone(),
                named_imports: RefCell::new(FxHashMap::default()),
                lowering_errors: RefCell::new(Vec::default()),
                auto_imports,
            },

            error_reporter: ErrorReporter::new(),
            _phantom: PhantomData,
        }
    }

    pub fn lowering_errors(&self) -> std::cell::Ref<'_, Vec<(String, SourceSpan<'a>)>> {
        self.ctx.lowering_errors.borrow()
    }

    pub fn has_lowering_errors(&self) -> bool {
        !self.ctx.lowering_errors.borrow().is_empty()
    }

    pub fn is_generic_param(&self, name: StrId) -> bool {
        self.ctx.generic_params.borrow().contains(&name)
    }

    pub fn add_generic_param(&self, name: StrId) {
        self.ctx.generic_params.borrow_mut().insert(name);
    }

    pub fn remove_generic_param(&self, name: StrId) {
        self.ctx.generic_params.borrow_mut().remove(&name);
    }

    pub fn get_generic_params(&self) -> HashSet<StrId> {
        self.ctx.generic_params.borrow().clone()
    }
}
