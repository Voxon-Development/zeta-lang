use ir::hir::{HirType, StrId};
use ir::ir_hasher::HashMap;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;

pub fn substitute_type<'a, 'subs, 'bump>(
    ty: &HirType<'a, 'bump>,
    subs: &'subs HashMap<StrId, HirType<'a, 'bump>>,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> HirType<'a, 'bump> {
    match ty {
        HirType::Generic(name) => subs.get(name).copied().unwrap_or(*ty),
        HirType::Ref {
            inner,
            mutability_state,
            provenance,
        } => HirType::Ref {
            inner: bump.alloc_value_immutable(substitute_type(inner, subs, bump.clone())),
            mutability_state: *mutability_state,
            provenance: *provenance,
        },
        HirType::SafePointer(inner) => HirType::SafePointer(
            bump.alloc_value_immutable(substitute_type(inner, subs, bump.clone())),
        ),
        HirType::UnsafePointer(inner) => HirType::UnsafePointer(
            bump.alloc_value_immutable(substitute_type(inner, subs, bump.clone())),
        ),
        HirType::OwnedPointer(inner) => HirType::OwnedPointer(
            bump.alloc_value_immutable(substitute_type(inner, subs, bump.clone())),
        ),
        HirType::Struct {
            name,
            field_types,
            type_args,
        } => {
            let new_fields: Vec<_> = field_types
                .iter()
                .map(|f| substitute_type(f, subs, bump.clone()))
                .collect();
            let new_args: Vec<_> = type_args
                .iter()
                .map(|a| substitute_type(a, subs, bump.clone()))
                .collect();
            HirType::Struct {
                name: *name,
                field_types: bump.alloc_slice_immutable(&new_fields),
                type_args: bump.alloc_slice_immutable(&new_args),
            }
        }

        HirType::DynInterface(name, args) => {
            let new_args: Vec<HirType<'a, 'bump>> = args
                .iter()
                .map(|a| substitute_type(a, subs, bump.clone()))
                .collect();
            HirType::DynInterface(*name, bump.alloc_slice(&new_args))
        }

        HirType::Enum(name, args) => {
            let new_args: Vec<HirType<'a, 'bump>> = args
                .iter()
                .map(|a| substitute_type(a, subs, bump.clone()))
                .collect();
            HirType::Enum(*name, bump.alloc_slice(&new_args))
        }

        HirType::Lambda {
            params,
            return_type,
        } => {
            let new_params: Vec<HirType<'a, 'bump>> = params
                .iter()
                .map(|p| substitute_type(p, subs, bump.clone()))
                .collect();
            let new_return = substitute_type(return_type, subs, bump.clone());
            HirType::Lambda {
                params: bump.alloc_slice(&new_params),
                return_type: bump.alloc_value_immutable(new_return),
            }
        }

        other => *other,
    }
}
