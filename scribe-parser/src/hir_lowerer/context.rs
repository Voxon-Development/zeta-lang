use std::sync::Arc;
use dashmap::DashMap;
use ir::errors::reporter::ErrorReporter;
use ir::hir::{HirFunc, HirInterface, HirStruct, HirType, StrId};
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;
use crate::hir_lowerer::monomorphization::Monomorphizer;
// ===============================
// Lowering Context
// ===============================

pub struct LoweringCtx<'a, 'bump> {
    pub classes: DashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
    pub interfaces: DashMap<StrId, HirInterface<'a, 'bump>, FxHashBuilder>,
    pub functions: DashMap<StrId, HirFunc<'a, 'bump>, FxHashBuilder>,
    pub type_bindings: DashMap<StrId, HirType<'a, 'bump>, FxHashBuilder>,
    pub variable_types: DashMap<StrId, HirType<'a, 'bump>, FxHashBuilder>,

    // caches for instantiated generics:
    pub instantiated_classes: DashMap<(StrId, StrId), StrId, FxHashBuilder>, // (orig_name, suffix) -> new_name
    pub instantiated_functions: DashMap<(StrId, StrId), StrId, FxHashBuilder>, // (orig_name, suffix) -> new_name

    pub context: Arc<StringPool>,
    pub bump: Arc<GrowableAtomicBump<'bump>>,
}

pub struct HirLowerer<'a, 'bump> {
    pub ctx: Arc<LoweringCtx<'a, 'bump>>,
    pub error_reporter: ErrorReporter<'a, 'bump>,
    pub mono: Monomorphizer<'a, 'bump>
}

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn new(context: Arc<StringPool>, bump: Arc<GrowableAtomicBump<'bump>>) -> Self {
        let sea_hasher = FxHashBuilder;
        
        let ctx: Arc<LoweringCtx<'a, 'bump>> = Arc::new(LoweringCtx {
            classes: DashMap::with_hasher(sea_hasher),
            functions: DashMap::with_hasher(sea_hasher),
            interfaces: DashMap::with_hasher(sea_hasher),
            type_bindings: DashMap::with_hasher(sea_hasher),
            variable_types: DashMap::with_hasher(sea_hasher),

            instantiated_classes: DashMap::with_hasher(sea_hasher),
            instantiated_functions: DashMap::with_hasher(sea_hasher),

            context,
            bump,
        });

        Self {
            mono: Monomorphizer::new(Arc::clone(&ctx)),
            ctx: Arc::clone(&ctx),
            error_reporter: ErrorReporter::new(),
        }
    }
}
