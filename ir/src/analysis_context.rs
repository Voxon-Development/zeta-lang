use crate::hir::{Hir, HirEnum, HirModule, HirType, StrId};
use crate::ir_hasher::FxHashMap;
use crate::registry::global_registry::GlobalRegistry;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

/// Whole-program context for the `is_copy` fixpoint. Built
/// once, after every module has finished HIR lowering, before typechecking
/// begins
pub struct CopyAnalysisCtx<'a, 'bump> {
    registry: GlobalRegistry<'a, 'bump>,
    enums: FxHashMap<StrId, HirEnum<'a, 'bump>>,

    #[allow(unused)] // May be used in the future
    copy_iface: StrId,
    drop_iface: StrId,

    is_copy: FxHashMap<StrId, bool>,
}

impl<'a, 'bump> CopyAnalysisCtx<'a, 'bump> {
    pub fn new(
        hir_modules: &[HirModule<'a, 'bump>],
        registry: GlobalRegistry<'a, 'bump>,
        context: Arc<StringPool>,
    ) -> Self {
        let mut enums: FxHashMap<StrId, HirEnum<'a, 'bump>> = FxHashMap::default();
        for module in hir_modules {
            for item in module.items {
                if let Hir::Enum(e) = item {
                    enums.insert(e.name, **e);
                }
            }
        }

        Self {
            registry,
            enums,
            copy_iface: StrId(context.intern("Copy")),
            drop_iface: StrId(context.intern("Drop")),
            is_copy: FxHashMap::default(),
        }
    }

    pub fn run(&mut self) {
        for name in self.struct_names() {
            self.is_copy.insert(name, true);
        }
        for &name in self.enums.keys() {
            self.is_copy.insert(name, true);
        }

        let mut changed = true;
        while changed {
            changed = false;
            for name in self.struct_names() {
                let computed = self.compute_struct_copy(name);
                if self.is_copy.get(&name).copied() != Some(computed) {
                    self.is_copy.insert(name, computed);
                    changed = true;
                }
            }
            for &name in &self.enums.keys().copied().collect::<Vec<_>>() {
                let computed = self.compute_enum_copy(name);
                if self.is_copy.get(&name).copied() != Some(computed) {
                    self.is_copy.insert(name, computed);
                    changed = true;
                }
            }
        }
    }

    pub fn is_copy(&self, name: StrId) -> bool {
        *self
            .is_copy
            .get(&name)
            .unwrap_or_else(|| panic!("is_copy queried before run() or for unknown type {}", name))
    }

    fn struct_names(&self) -> Vec<StrId> {
        self.registry.classes.borrow().keys().copied().collect()
    }

    fn implements(&self, struct_name: StrId, iface: StrId) -> bool {
        self.registry
            .struct_interfaces
            .borrow()
            .get(&struct_name)
            .map(|ifaces| ifaces.contains(&iface))
            .unwrap_or(false)
    }

    fn compute_struct_copy(&self, name: StrId) -> bool {
        let classes = self.registry.classes.borrow();
        let Some(hir_struct) = classes.get(&name) else {
            return false;
        };

        // `impl X by Drop` forces non-Copy unconditionally. Whether
        // this conflicts with an explicit `impl X by Copy` on the same type
        // is a typechecker validation concern, not this analysis's problem,
        // if both are present, this simply reports non-Copy (Drop wins),
        // and the typechecker separately flags the conflict as an error.
        if self.implements(name, self.drop_iface) {
            return false;
        }

        hir_struct
            .fields
            .iter()
            .all(|f| self.type_is_copy(&f.field_type))
    }

    fn compute_enum_copy(&self, name: StrId) -> bool {
        let Some(hir_enum) = self.enums.get(&name) else {
            return false;
        };
        hir_enum.variants.iter().all(|variant| {
            variant
                .fields
                .iter()
                .all(|f| self.type_is_copy(&f.field_type))
        })
    }

    /// Type-level Copy-ness. `Ref`/`SafePointer`/`UnsafePointer`
    /// are unconditionally Copy regardless of `inner`. Struct/Enum cases read the current fixpoint value rather
    /// than recursing structurally, since it may not be finalized mid-loop.
    pub fn type_is_copy(&self, ty: &HirType<'a, 'bump>) -> bool {
        match ty {
            HirType::I8
            | HirType::I16
            | HirType::I32
            | HirType::I64
            | HirType::U8
            | HirType::U16
            | HirType::U32
            | HirType::U64
            | HirType::I128
            | HirType::U128
            | HirType::F32
            | HirType::F64
            | HirType::Boolean
            | HirType::Char
            | HirType::Void
            | HirType::This
            | HirType::Null
            | HirType::Lambda { .. } => true,

            HirType::Ref { .. } | HirType::SafePointer(_) | HirType::UnsafePointer(_) => true,

            HirType::Nullable(inner) => self.type_is_copy(inner),

            HirType::Struct(name, _) => self.is_copy.get(name).copied().unwrap_or(true),
            HirType::Enum(name, _) => self.is_copy.get(name).copied().unwrap_or(true),

            HirType::Dyn { .. } | HirType::DynInterface(..) => false,

            HirType::String | HirType::Generic(_) | HirType::Unknown => false,
            HirType::Tuple(args) => args.iter().all(|arg| self.type_is_copy(arg)),
        }
    }

    pub fn implements_drop(&self, struct_name: StrId) -> bool {
        self.registry
            .struct_interfaces
            .borrow()
            .get(&struct_name)
            .map(|ifaces| ifaces.contains(&self.drop_iface))
            .unwrap_or(false)
    }
}
