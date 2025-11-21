use super::context::HirLowerer;
use super::monomorphization::{
    class_instantiation::{instantiate_class_for_types},
};
use ir::hir::{HirFunc, HirType, StrId};
use ir::ir_hasher::FxHashBuilder;
use std::collections::HashMap;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Monomorphization (delegates to dedicated monomorphization module)
    // ===============================
    
    /// Monomorphize a function with substitutions mapping generic-name -> concrete HirType
    pub fn monomorphize_function(
        &self,
        func: &HirFunc<'a, 'bump>,
        substitutions: &HashMap<StrId, HirType<'a, 'bump>, FxHashBuilder>,
    ) -> HirFunc<'a, 'bump> {
        self.mono.monomorphize_function(func, substitutions)
    }

    /// Instantiate a generic class for concrete args
    pub fn instantiate_class_for_types(&self, base: StrId, concrete_args: &[HirType<'a, '_>]) -> StrId {
        instantiate_class_for_types(&self.ctx, base, concrete_args, self.ctx.bump.clone())
    }
}
