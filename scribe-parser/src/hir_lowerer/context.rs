use codex_dependency_graph::dep_graph::DepGraph;
use ir::errors::reporter::ErrorReporter;
use ir::hir::HirFuncProto;
use ir::hir::{HirFunc, HirInterface, HirStruct, HirType, StrId};
use ir::ir_hasher::{FxHashBuilder, FxHashMap};
use ir::registry::global_registry::GlobalRegistry;
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

    pub(super) fn resolve_type_path_name(&self, path: &[StrId], name: StrId) -> StrId {
        if path.is_empty() {
            // TODO: probably add `builtin_interfaces` or something
            let s = self.context.resolve_string(&name);
            if matches!(s, "Drop" | "Copy" | "Clone") {
                return name;
            }
            if let Some(&target_module_idx) = self.imported_modules.borrow().get(&name) {
                return self.mangle_via_module(target_module_idx, name);
            }
            return self.mangle_type_name(name);
        }

        let alias = *path.last().unwrap();
        if let Some(&target_module_idx) = self.imported_modules.borrow().get(&alias) {
            return self.mangle_via_module(target_module_idx, name);
        }

        if let Some(target_module_idx) = self.dep_graph.borrow().resolve_module_path(path) {
            return self.mangle_via_module(target_module_idx, name);
        }

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
    ) -> Self {
        Self {
            ctx: LoweringCtx {
                structs: registry.structs,
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
            },
            error_reporter: ErrorReporter::new(),
            _phantom: PhantomData,
        }
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
