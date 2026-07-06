use crate::hir_lowerer::monomorphization::Monomorphizer;
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

pub type FxHashSet<T> = HashSet<T, FxHashBuilder>;

pub struct LoweringCtx<'a, 'bump> {
    pub classes: Rc<RefCell<FxHashMap<StrId, HirStruct<'a, 'bump>>>>,
    pub interfaces: Rc<RefCell<FxHashMap<StrId, HirInterface<'a, 'bump>>>>,
    pub functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    pub func_protos: RefCell<FxHashMap<StrId, HirFuncProto<'a, 'bump>>>,
    pub type_bindings: RefCell<FxHashMap<StrId, HirType<'a, 'bump>>>,
    pub variable_types: RefCell<FxHashMap<StrId, HirType<'a, 'bump>>>,
    pub generic_params: RefCell<HashSet<StrId>>,
    pub context: Arc<StringPool>,
    pub dep_graph: &'a DepGraph,
    pub imported_modules: RefCell<FxHashMap<StrId, usize>>,
    pub bump: Arc<GrowableAtomicBump<'bump>>,
    pub module_idx: usize,
    pub struct_interfaces: Rc<RefCell<FxHashMap<StrId, Vec<StrId>>>>,
    pub struct_methods: Rc<RefCell<FxHashMap<StrId, FxHashMap<StrId, StrId>>>>,
}

pub struct HirLowerer<'a, 'bump> {
    pub ctx: LoweringCtx<'a, 'bump>,
    pub error_reporter: ErrorReporter<'a>,
    pub(crate) mono: Monomorphizer<'a, 'bump>,
    _phantom: PhantomData<&'bump ()>,
}

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn new(
        context: Arc<StringPool>,
        bump: Arc<GrowableAtomicBump<'bump>>,
        dep_graph: &'a DepGraph,
        registry: GlobalRegistry<'a, 'bump>,
    ) -> Self {
        Self {
            ctx: LoweringCtx {
                classes: registry.classes,
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
            },
            error_reporter: ErrorReporter::new(),
            mono: Monomorphizer::new(context, bump, registry.functions),
            _phantom: PhantomData,
        }
    }

    /// Get a reference to the monomorphizer
    pub fn monomorphizer(&self) -> &Monomorphizer<'a, 'bump> {
        &self.mono
    }

    /// Check if an identifier is a generic type parameter
    pub fn is_generic_param(&self, name: StrId) -> bool {
        self.ctx.generic_params.borrow().contains(&name)
    }

    /// Add a generic parameter to the current context
    pub fn add_generic_param(&self, name: StrId) {
        self.ctx.generic_params.borrow_mut().insert(name);
    }

    /// Remove a generic parameter from the current context
    pub fn remove_generic_param(&self, name: StrId) {
        self.ctx.generic_params.borrow_mut().remove(&name);
    }

    /// Get all generic parameters in the current context
    pub fn get_generic_params(&self) -> HashSet<StrId> {
        self.ctx.generic_params.borrow().clone()
    }
}
