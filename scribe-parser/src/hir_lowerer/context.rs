use ir::hir::HirFuncProto;
use crate::hir_lowerer::monomorphization::Monomorphizer;
use ir::errors::reporter::ErrorReporter;
use ir::hir::{HirFunc, HirInterface, HirStruct, HirType, StrId};
use ir::ir_hasher::{FxHashBuilder, FxHashMap};
use std::cell::RefCell;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

pub type FxHashSet<T> = HashSet<T, FxHashBuilder>;

// ===============================
// Lowering Context
// ===============================
pub struct LoweringCtx<'a, 'bump> {
    pub classes: RefCell<FxHashMap<StrId, HirStruct<'a, 'bump>>>,
    pub interfaces: RefCell<FxHashMap<StrId, HirInterface<'a, 'bump>>>,

    pub functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    pub func_protos: RefCell<FxHashMap<StrId, HirFuncProto<'a, 'bump>>>,

    pub type_bindings: RefCell<FxHashMap<StrId, HirType<'a, 'bump>>>,
    pub variable_types: RefCell<FxHashMap<StrId, HirType<'a, 'bump>>>,
    pub generic_params: RefCell<HashSet<StrId>>,

    pub context: Arc<StringPool>,
    pub bump: Arc<GrowableAtomicBump<'bump>>,
}

pub struct HirLowerer<'a, 'bump> {
    pub ctx: LoweringCtx<'a, 'bump>,
    pub error_reporter: ErrorReporter<'a, 'bump>,
    pub(crate) mono: Monomorphizer<'a, 'bump>,
    _phantom: PhantomData<&'bump ()>,
}

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn new(context: Arc<StringPool>, bump: Arc<GrowableAtomicBump<'bump>>) -> Self {
        let functions: Rc<RefCell<FxHashMap<StrId, HirFunc>>> = Rc::new(RefCell::new(FxHashMap::default()));
        Self {
            ctx: LoweringCtx {
                classes: RefCell::new(FxHashMap::default()),
                functions: functions.clone(),
                func_protos: RefCell::new(FxHashMap::default()),
                interfaces: RefCell::new(FxHashMap::default()),
                type_bindings: RefCell::new(FxHashMap::default()),
                variable_types: RefCell::new(FxHashMap::default()),
                generic_params: RefCell::new(HashSet::default()),
                context: context.clone(),
                bump: bump.clone(),
            },
            error_reporter: ErrorReporter::new(),
            mono: Monomorphizer::new(
                context,
                bump,
                functions.clone()
            ),
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
