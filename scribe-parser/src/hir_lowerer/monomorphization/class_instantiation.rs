use super::naming::instantiate_class_name;
use super::type_substitution::substitute_type;
use crate::hir_lowerer::context::LoweringCtx;
use ir::hir::{HirField, HirStruct, HirType, StrId};
use ir::ir_hasher::FxHashMap;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use crate::hir_lowerer::monomorphization::{suffix_for_subs, Monomorphizer};

/// Instantiate a generic class for concrete args, caching the result and registering the new class
pub fn instantiate_class_for_types<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    monomorphizer: &Monomorphizer<'a, 'bump>,
    base_id: StrId,
    concrete_args: &[HirType<'a, 'bump>],
    bump: Arc<GrowableAtomicBump<'bump>>
) -> Option<&'bump HirStruct<'a, 'bump>> {
    // Early return if no generic parameters are provided
    if concrete_args.is_empty() {
        return ctx.classes.borrow().get(&base_id).map(|c| {
            // Safety: The bump allocator ensures the reference is valid for 'bump
            unsafe { std::mem::transmute::<_, &'bump _>(c) }
        });
    }

    let binding = ctx.classes.borrow();
    let base = binding.get(&base_id)?;

    // Create a type substitution map for the concrete arguments
    let type_map = if let Some(generics) = &base.generics {
        let mut map = FxHashMap::default();
        for (param, arg) in generics.iter().zip(concrete_args) {
            map.insert(param.name, arg.clone());
        }
        map
    } else {
        FxHashMap::default()
    };
    
    // Create a proper StrId key for the concrete arguments
    let suffix = suffix_for_subs(ctx.context.clone(), &type_map);
    let key = (base_id, suffix);
    
    // Check cache with minimal borrow scope
    {
        let instantiated_classes = monomorphizer.instantiated_classes.borrow();
        if let Some(cached) = instantiated_classes.get(&key) {
            return Some(unsafe { std::mem::transmute::<_, &'bump _>(cached) });
        }
    }

    // Get the base class definition
    let base = {
        let classes = ctx.classes.borrow();
        classes.get(&base_id)?.clone()
    };
    
    let mut new_class = base.clone();
    
    // Create the new class name early to avoid borrow issues
    let interned = instantiate_class_name(concrete_args, &base, ctx.context.clone());
    new_class.name = interned;

    // Apply type substitutions if there are generics
    if let Some(generics) = &base.generics {
        let mut type_map = FxHashMap::default();
        for (param, arg) in generics.iter().zip(concrete_args) {
            type_map.insert(param.name, arg.clone());
        }
        
        // Apply substitutions to fields
        let new_fields: Vec<_> = new_class.fields.iter()
            .map(|field| {
                // Handle generic type parameters for the field
                let field_generics = field.generics.map(|generics| {
                    let mut new_generics = Vec::with_capacity(generics.len());
                    for ty in generics.iter() {
                        new_generics.push(substitute_type(ty, &type_map, bump.clone()));
                    }
                    bump.alloc_slice_immutable(&new_generics)
                });
                
                // Apply substitutions to the field type
                let new_field_type = substitute_type(&field.field_type, &type_map, bump.clone());
                
                HirField {
                    name: field.name,
                    visibility: field.visibility,
                    field_type: new_field_type,
                    generics: field_generics,
                }
            })
            .collect();
            
        new_class.fields = bump.alloc_slice(&new_fields);
        new_class.generics = None;
    }

    // Allocate the new class in the bump allocator
    let new_class_ptr = bump.alloc_value(new_class);
    
    // Insert into the context and cache
    {
        let mut classes = ctx.classes.borrow_mut();
        classes.insert(interned, *new_class_ptr);
        
        // Cache the instantiated class
        monomorphizer.instantiated_classes.borrow_mut()
            .insert(key, new_class_ptr.name);
    }
    
    Some(new_class_ptr)
}

/// Tries to resolve an interface call into a direct call to the implementing class' method
pub fn direct_method_lookup<'a, 'bump>(
    ctx: &LoweringCtx<'a, 'bump>,
    monomorphizer: &Monomorphizer<'a, 'bump>,
    class_name: &StrId,
    class_args: &[HirType<'a, 'bump>],
    method_name: &StrId,
    bump: Arc<GrowableAtomicBump<'bump>>
) -> Option<StrId> {
    let concrete_name = if class_args.is_empty() {
        *class_name
    } else {
        instantiate_class_for_types(ctx, monomorphizer, *class_name, class_args, bump.clone())?.name
    };

    let classes_binding = ctx.classes.borrow();
    let class = classes_binding.get(&concrete_name)?;

    let Some(methods) = class.methods else {
        return None;
    };

    methods
        .iter()
        .find(|m| m.name == *method_name)
        .map(|m| m.name)
}
