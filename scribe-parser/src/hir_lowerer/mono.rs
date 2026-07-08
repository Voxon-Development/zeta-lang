use super::context::HirLowerer;
use super::monomorphization::class_instantiation::instantiate_class_for_types;
use ir::hir::{HirFunc, HirType, StrId};
use ir::ir_hasher::FxHashMap;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn monomorphize_function(
        &mut self,
        func: &HirFunc<'a, 'bump>,
        substitutions: &FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> HirFunc<'a, 'bump> {
        self.ctx
            .functions
            .borrow()
            .get(
                &self
                    .mono
                    .monomorphize_function(func, substitutions)
                    .unwrap(),
            )
            .copied()
            .unwrap()
    }

    pub fn instantiate_class_for_types(
        &self,
        base: StrId,
        concrete_args: &[HirType<'a, 'bump>],
    ) -> StrId {
        instantiate_class_for_types(
            &self.ctx,
            &self.mono,
            base,
            concrete_args,
            self.ctx.bump.clone(),
        )
        .map(|c| c.name)
        .unwrap()
    }
}
