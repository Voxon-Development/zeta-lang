use super::naming::instantiate_struct_name;
use super::type_substitution::substitute_type;
use crate::hir_lowerer::context::LoweringCtx;
use crate::hir_lowerer::monomorphization::suffix_for_subs;
use ir::hir::{HirField, HirStruct, HirType, StrId};
use ir::ir_hasher::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;

pub fn instantiate_struct_for_types<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    instantiated_structs: &Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    instantiated_struct_origins: &Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,
    base_id: StrId,
    concrete_args: &[HirType<'a, 'bump>],
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> Option<&'bump HirStruct<'a, 'bump>> {
    if concrete_args.is_empty() {
        return ctx
            .structs
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

    let binding = ctx.structs.borrow();
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
        let cache = instantiated_structs.borrow();
        if let Some(cached) = cache.get(&key) {
            let ptr = ctx
                .structs
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
        let structs = ctx.structs.borrow();
        structs.get(&base_id)?.clone()
    };
    let mut new_struct = base.clone();
    let interned = instantiate_struct_name(concrete_args, &base, ctx.context.clone());
    new_struct.name = interned;
    if let Some(generics) = &base.generics {
        let mut type_map = FxHashMap::default();
        for (param, arg) in generics.iter().zip(concrete_args) {
            type_map.insert(param.name, arg.clone());
        }
        let mut new_fields = Vec::new();
        for field in new_struct.fields {
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
        new_struct.fields = bump.alloc_slice(&new_fields);
        new_struct.generics = None;
    }
    let new_struct_ptr = bump.alloc_value(new_struct);
    {
        let mut structs = ctx.structs.borrow_mut();
        structs.insert(interned, *new_struct_ptr);
        instantiated_structs
            .borrow_mut()
            .insert(key, new_struct_ptr.name);
        instantiated_struct_origins
            .borrow_mut()
            .insert(interned, (base_id, concrete_args.to_vec()));
    }
    {
        if ctx.struct_interfaces.borrow().get(&base_id).is_none() {
            return Some(new_struct_ptr);
        }
    }
    let interfaces = { ctx.struct_interfaces.borrow().get(&base_id).cloned() };

    if let Some(interfaces) = interfaces {
        ctx.struct_interfaces
            .borrow_mut()
            .insert(interned, interfaces);
    }

    let methods = { ctx.struct_methods.borrow().get(&base_id).cloned() };

    if let Some(methods) = methods {
        ctx.struct_methods.borrow_mut().insert(interned, methods);
    }

    Some(new_struct_ptr)
}

pub fn direct_method_lookup<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    instantiated_structs: &Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    instantiated_struct_origins: &Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,
    struct_name: &StrId,
    struct_args: &[HirType<'a, 'bump>],
    method_name: &StrId,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> Option<StrId> {
    let concrete_name = if struct_args.is_empty() {
        *struct_name
    } else {
        instantiate_struct_for_types(
            ctx,
            instantiated_structs,
            instantiated_struct_origins,
            *struct_name,
            struct_args,
            bump.clone(),
        )?
        .name
    };
    let struct_methods = ctx.struct_methods.borrow();
    let methods = struct_methods.get(&concrete_name)?;
    methods.get(method_name).map(|m| *m)
}
