use ir::hir::{HirType, StrId};
use std::sync::Arc;
use ir::ir_hasher::HashMap;
use zetaruntime::arena::GrowableAtomicBump;

/// Substitute occurrences of Generic types with concrete ones
pub fn substitute_type<'a, 'bump>(
    ty: &HirType<'a, 'bump>,
    subs: &HashMap<StrId, HirType<'a, 'bump>>,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> HirType<'a, 'bump> {
    match ty {
        HirType::Generic(name) => subs.get(name).copied().unwrap_or(*ty),
        HirType::Struct(name, args) => {
            if args.is_empty() {
                HirType::Struct(*name, &[])
            } else {
                let new_args: Vec<HirType<'a, 'bump>> = args.iter().map(|a| substitute_type(a, subs, bump.clone())).collect();
                HirType::Struct(*name, bump.alloc_slice(&new_args))
            }
        }

        HirType::Interface(name, args) => {
            let new_args: Vec<HirType<'a, 'bump>> = args.iter().map(|a| substitute_type(a, subs, bump.clone())).collect();
            HirType::Interface(*name, bump.alloc_slice(&new_args))
        }

        HirType::Enum(name, args) => {
            let new_args: Vec<HirType<'a, 'bump>> = args.iter().map(|a| substitute_type(a, subs, bump.clone())).collect();
            HirType::Enum(*name, bump.alloc_slice(&new_args))
        }

        HirType::Lambda {
            params,
            return_type,
            concurrent,
        } => {
            let new_params: Vec<HirType<'a, 'bump>> = params.iter().map(|p| substitute_type(p, subs, bump.clone())).collect();
            let new_return = substitute_type(return_type, subs, bump.clone());
            HirType::Lambda {
                params: bump.alloc_slice(&new_params),
                return_type: bump.alloc_value_immutable(new_return),
                concurrent: *concurrent,
            }
        }

        other => *other,
    }
}
