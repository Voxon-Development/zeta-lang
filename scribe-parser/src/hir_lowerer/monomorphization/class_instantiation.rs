use super::naming::instantiate_class_name;
use super::type_substitution::substitute_type;
use crate::hir_lowerer::context::LoweringCtx;
use crate::hir_lowerer::monomorphization::suffix_for_subs;
use ir::hir::{HirField, HirStruct, HirType, StrId};
use ir::ir_hasher::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;

pub fn instantiate_class_for_types<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    instantiated_classes: &Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    instantiated_class_origins: &Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,
    base_id: StrId,
    concrete_args: &[HirType<'a, 'bump>],
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> Option<&'bump HirStruct<'a, 'bump>> {
    if concrete_args.is_empty() {
        return ctx
            .classes
            .borrow()
            .get(&base_id)
            .map(|c| unsafe { std::mem::transmute::<_, &'bump _>(c) });
    }

    if concrete_args
        .iter()
        .any(|t| matches!(t, HirType::Generic(_)))
    {
        return None;
    }

    let binding = ctx.classes.borrow();
    let base = binding.get(&base_id)?;
    let type_map = if let Some(generics) = &base.generics {
        let mut map = FxHashMap::default();
        for (param, arg) in generics.iter().zip(concrete_args) {
            map.insert(param.name, arg.clone());
        }
        map
    } else {
        FxHashMap::default()
    };
    let suffix = suffix_for_subs(ctx.context.clone(), &type_map);
    let key = (base_id, suffix);
    {
        let cache = instantiated_classes.borrow();
        if let Some(cached) = cache.get(&key) {
            let ptr = ctx
                .classes
                .borrow()
                .get(cached)
                .map(|c| unsafe { std::mem::transmute::<_, &'bump _>(c) });
            if ptr.is_some() {
                return ptr;
            }
        }
    }
    drop(binding);
    let base = {
        let classes = ctx.classes.borrow();
        classes.get(&base_id)?.clone()
    };
    let mut new_class = base.clone();
    let interned = instantiate_class_name(concrete_args, &base, ctx.context.clone());
    new_class.name = interned;
    if let Some(generics) = &base.generics {
        let mut type_map = FxHashMap::default();
        for (param, arg) in generics.iter().zip(concrete_args) {
            type_map.insert(param.name, arg.clone());
        }
        let mut new_fields = Vec::new();
        for field in new_class.fields {
            let field_generics = match field.generics {
                Some(generics) => {
                    let mut new_generics = Vec::with_capacity(generics.len());
                    for ty in generics.iter() {
                        new_generics.push(substitute_type(ty, &type_map, bump.clone()));
                    }
                    Some(bump.alloc_slice_immutable(&new_generics))
                }
                None => None,
            };
            let new_field_type = substitute_type(&field.field_type, &type_map, bump.clone());
            new_fields.push(HirField {
                name: field.name,
                visibility: field.visibility,
                field_type: new_field_type,
                generics: field_generics,
            });
        }
        new_class.fields = bump.alloc_slice(&new_fields);
        new_class.generics = None;
    }
    let new_class_ptr = bump.alloc_value(new_class);
    {
        let mut classes = ctx.classes.borrow_mut();
        classes.insert(interned, *new_class_ptr);
        instantiated_classes
            .borrow_mut()
            .insert(key, new_class_ptr.name);
        instantiated_class_origins
            .borrow_mut()
            .insert(interned, (base_id, concrete_args.to_vec()));
    }
    Some(new_class_ptr)
}

pub fn direct_method_lookup<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    instantiated_classes: &Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    instantiated_class_origins: &Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,
    class_name: &StrId,
    class_args: &[HirType<'a, 'bump>],
    method_name: &StrId,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> Option<StrId> {
    let concrete_name = if class_args.is_empty() {
        *class_name
    } else {
        instantiate_class_for_types(
            ctx,
            instantiated_classes,
            instantiated_class_origins,
            *class_name,
            class_args,
            bump.clone(),
        )?
        .name
    };
    let struct_methods = ctx.struct_methods.borrow();
    let methods = struct_methods.get(&concrete_name)?;
    methods.get(method_name).map(|m| *m)
}
