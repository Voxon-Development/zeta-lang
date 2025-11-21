use super::naming::instantiate_class_name;
use super::type_substitution::substitute_type;
use crate::hir_lowerer::context::LoweringCtx;
use ir::hir::{HirField, HirStruct, HirType, StrId};
use ir::ir_hasher::FxHashBuilder;
use std::collections::HashMap;
use std::sync::Arc;
use dashmap::mapref::one::Ref;
use zetaruntime::arena::GrowableAtomicBump;

/// Instantiate a generic class for concrete args, caching the result and registering the new class
// Removed unsafe patterns - now using safe field reconstruction
pub fn instantiate_class_for_types<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    base_id: StrId,
    concrete_args: &[HirType<'a, 'bump>],
    bump: Arc<GrowableAtomicBump<'bump>>
) -> StrId {
    let base: Ref<StrId, HirStruct> = ctx.classes.get(&base_id).unwrap();
    
    let mut map: HashMap<StrId, HirType, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder);
    for (i, generic) in base.generics.unwrap().iter().enumerate() {
        let ty = concrete_args
            .get(i)
            .copied()
            .unwrap_or_else(|| HirType::Generic(generic.name));
        map.insert(generic.name, ty);
    }

    let mut new_class = base.clone();

    let new_fields: Vec<HirField> = new_class.fields.iter().map(|field| {
        HirField {
            name: field.name,
            visibility: field.visibility,
            field_type: substitute_type(&field.field_type, &map, bump.clone()),
        }
    }).collect();
    new_class.fields = bump.alloc_slice(&new_fields);

    new_class.generics = None;

    let interned = instantiate_class_name(concrete_args, base.value(), ctx.context.clone());
    new_class.name = interned;

    ctx.classes.insert(interned, new_class.clone());

    interned
}

/// Tries to resolve an interface call into a direct call to the implementing class' method
pub fn direct_method_lookup<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    class_name: &StrId,
    class_args: &[HirType<'a, 'bump>],
    method_name: &StrId,
    bump: Arc<GrowableAtomicBump<'bump>>
) -> Option<StrId> {
    let concrete_name = if class_args.is_empty() {
        *class_name
    } else {
        instantiate_class_for_types(ctx, *class_name, class_args, bump.clone())
    };

    let class = ctx.classes.get(&concrete_name)?;

    let Some(methods) = class.methods else {
        return None;
    };

    methods
        .iter()
        .find(|m| m.name == *method_name)
        .map(|m| m.name)
}
