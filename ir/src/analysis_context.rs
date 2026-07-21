use crate::borrow_checker::{Bound, Interval};
use crate::hir::{
    AssignmentOperator, Hir, HirEnum, HirExpr, HirModule, HirParam, HirType, Operator, StrId,
};
use crate::ir_hasher::FxHashMap;
use crate::registry::global_registry::GlobalRegistry;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

use crate::hir::{HirFunc, HirStmt};
use std::cell::RefCell;

use std::marker::PhantomData;

/// Cached analysis of one method.
///
/// We analyze every method at most once. Recursive analysis is guarded by
/// `InProgress`; recursive cycles simply become `Opaque`.
pub enum MethodAnalysisState {
    InProgress,

    Done(MethodSummary),
}

/// Result of symbolically analyzing one method.
#[derive(Clone, Debug)]
pub struct MethodSummary {
    /// Symbolic memory region returned by this method.
    ///
    /// For example:
    ///
    ///     return &self.data[i];
    ///
    /// becomes roughly
    ///
    ///     Interval {
    ///         lower: i * sizeof(T),
    ///         upper: i * sizeof(T) + sizeof(T),
    ///     }
    ///
    pub returned_region: Interval,

    /// Whether every invocation is guaranteed to return a unique region
    /// regardless of its inputs.
    ///
    /// Example:
    ///
    ///     alloc_place_id()
    ///
    pub fresh_per_call: bool,
}

pub struct IndexDisjointCtx<'a, 'bump> {
    analyses: RefCell<FxHashMap<StrId, MethodAnalysisState>>,

    _marker: PhantomData<&'a &'bump ()>,
}

impl<'a, 'bump> IndexDisjointCtx<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            analyses: RefCell::new(FxHashMap::default()),
            _marker: PhantomData,
        }
    }

    /// Analyze one indexing method.
    ///
    /// The returned summary is expressed purely in terms of the method's own
    /// parameters (`Bound::Symbol`) and `SelfField`s.
    pub fn analyze_method(
        &self,
        method: &HirFunc<'a, 'bump>,
        resolve: &impl Fn(StrId) -> Option<HirFunc<'a, 'bump>>,
    ) -> MethodSummary {
        if let Some(state) = self.analyses.borrow().get(&method.name) {
            return match state {
                MethodAnalysisState::Done(summary) => summary.clone(),

                MethodAnalysisState::InProgress => MethodSummary {
                    returned_region: Interval {
                        lower: Bound::Opaque(0),
                        upper: Bound::Opaque(0),
                    },
                    fresh_per_call: false,
                },
            };
        }

        self.analyses
            .borrow_mut()
            .insert(method.name, MethodAnalysisState::InProgress);

        let summary = if Self::is_fresh_per_call_pattern(method) {
            MethodSummary {
                returned_region: Interval {
                    lower: Bound::FreshPerCall,
                    upper: Bound::FreshPerCall,
                },
                fresh_per_call: true,
            }
        } else {
            Self::analyze_body(method, resolve, self)
        };

        self.analyses
            .borrow_mut()
            .insert(method.name, MethodAnalysisState::Done(summary.clone()));

        summary
    }

    fn analyze_body(
        method: &HirFunc<'a, 'bump>,
        resolve: &impl Fn(StrId) -> Option<HirFunc<'a, 'bump>>,
        ctx: &Self,
    ) -> MethodSummary {
        let Some(HirStmt::Block { body }) = method.body else {
            return MethodSummary {
                returned_region: Interval {
                    lower: Bound::Opaque(0),
                    upper: Bound::Opaque(0),
                },
                fresh_per_call: false,
            };
        };

        let mut locals = FxHashMap::<StrId, Interval>::default();

        Self::analyze_stmts(body, resolve, ctx, &mut locals)
    }

    fn opaque_summary() -> MethodSummary {
        MethodSummary {
            returned_region: Interval {
                lower: Bound::Opaque(0),
                upper: Bound::Opaque(0),
            },
            fresh_per_call: false,
        }
    }

    fn analyze_stmts(
        stmts: &[HirStmt<'a, 'bump>],
        resolve: &impl Fn(StrId) -> Option<HirFunc<'a, 'bump>>,
        ctx: &Self,
        locals: &mut FxHashMap<StrId, Interval>,
    ) -> MethodSummary {
        for stmt in stmts {
            match stmt {
                HirStmt::Let { name, value, .. } => {
                    let interval = Self::expr_to_interval(value, resolve, ctx, locals);
                    locals.insert(*name, interval);
                }

                HirStmt::Expr(_) => {
                    // Ignore pure expressions.
                    // Any side-effecting expressions should eventually be rejected
                    // by expr_to_bound returning Opaque.
                }

                HirStmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    if !Self::is_early_exit_only(then_block) {
                        return Self::opaque_summary();
                    }

                    if let Some(stmt) = else_block {
                        match stmt {
                            HirStmt::Block { body } => {
                                return Self::analyze_stmts(body, resolve, ctx, locals);
                            }
                            HirStmt::Return(Some(expr)) => {
                                return MethodSummary {
                                    returned_region: Self::expr_to_interval(
                                        expr, resolve, ctx, locals,
                                    ),
                                    fresh_per_call: false,
                                };
                            }
                            HirStmt::Return(None) => {
                                return Self::opaque_summary();
                            }
                            _ => return Self::opaque_summary(),
                        }
                    }

                    return Self::opaque_summary();
                }

                HirStmt::Return(Some(expr)) => {
                    return MethodSummary {
                        returned_region: Self::expr_to_interval(expr, resolve, ctx, locals),
                        fresh_per_call: false,
                    };
                }

                HirStmt::Return(None) => {
                    return Self::opaque_summary();
                }

                _ => return Self::opaque_summary(),
            }
        }

        Self::opaque_summary()
    }

    fn is_fresh_per_call_pattern(method: &HirFunc<'a, 'bump>) -> bool {
        let has_normal_params = method.params.map_or(false, |params| {
            params.iter().any(|p| matches!(p, HirParam::Normal { .. }))
        });

        if has_normal_params {
            return false;
        }

        let Some(HirStmt::Block { body }) = method.body else {
            return false;
        };

        let [
            HirStmt::Expr(HirExpr::Assignment {
                target:
                    HirExpr::FieldAccess {
                        object: target_obj,
                        field: target_field,
                        ..
                    },
                op: AssignmentOperator::AddAssign,
                value: HirExpr::Number(amount, _),
                ..
            }),
            HirStmt::Return(Some(HirExpr::FieldAccess {
                object: return_obj,
                field: return_field,
                ..
            })),
        ] = body
        else {
            return false;
        };

        *amount > 0
            && matches!(target_obj, HirExpr::This { .. })
            && matches!(return_obj, HirExpr::This { .. })
            && target_field == return_field
    }

    fn expr_to_interval(
        expr: &HirExpr<'a, 'bump>,
        resolve: &impl Fn(StrId) -> Option<HirFunc<'a, 'bump>>,
        ctx: &Self,
        locals: &FxHashMap<StrId, Interval>,
    ) -> Interval {
        match expr {
            HirExpr::Number(value, _) => Interval {
                lower: Bound::Const(*value),
                upper: Bound::Const(*value),
            },

            HirExpr::Ident(name, _) => locals.get(name).cloned().unwrap_or(Interval {
                lower: Bound::Symbol(*name),
                upper: Bound::Symbol(*name),
            }),

            HirExpr::This { .. } => Interval {
                lower: Bound::Symbol(StrId::default()),
                upper: Bound::Symbol(StrId::default()),
            },

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                if matches!(&**object, HirExpr::This { .. }) {
                    Interval {
                        lower: Bound::SelfField(*field),
                        upper: Bound::SelfField(*field),
                    }
                } else {
                    Interval {
                        lower: Bound::Opaque(0),
                        upper: Bound::Opaque(0),
                    }
                }
            }

            HirExpr::Binary {
                left, op, right, ..
            } => {
                let lhs = Self::expr_to_interval(left, resolve, ctx, locals);
                let rhs = Self::expr_to_interval(right, resolve, ctx, locals);

                match (op, &rhs.lower, &rhs.upper) {
                    (Operator::Add, Bound::Const(offset), Bound::Const(_)) => Interval {
                        lower: Bound::Offset {
                            base: Box::new(lhs.lower),
                            offset: *offset,
                        },
                        upper: Bound::Offset {
                            base: Box::new(lhs.upper),
                            offset: *offset,
                        },
                    },

                    (Operator::Subtract, Bound::Const(offset), Bound::Const(_)) => Interval {
                        lower: Bound::Offset {
                            base: Box::new(lhs.lower),
                            offset: -*offset,
                        },
                        upper: Bound::Offset {
                            base: Box::new(lhs.upper),
                            offset: -*offset,
                        },
                    },

                    (Operator::Multiply, Bound::Const(factor), Bound::Const(_)) => Interval {
                        lower: Bound::Scale {
                            base: Box::new(lhs.lower),
                            factor: *factor,
                        },
                        upper: Bound::Scale {
                            base: Box::new(lhs.upper),
                            factor: *factor,
                        },
                    },

                    _ => Interval {
                        lower: Bound::Opaque(0),
                        upper: Bound::Opaque(0),
                    },
                }
            }

            HirExpr::Ref { expr, .. } | HirExpr::Deref { expr, .. } => {
                Self::expr_to_interval(expr, resolve, ctx, locals)
            }

            HirExpr::Call {
                callee: HirExpr::Ident(name, _),
                args,
                ..
            } => {
                let Some(func) = resolve(*name) else {
                    return Interval {
                        lower: Bound::Opaque(0),
                        upper: Bound::Opaque(0),
                    };
                };

                let summary = ctx.analyze_method(&func, resolve);

                if summary.fresh_per_call {
                    return Interval {
                        lower: Bound::FreshPerCall,
                        upper: Bound::FreshPerCall,
                    };
                }

                let mut substitutions = FxHashMap::<StrId, Interval>::default();

                if let Some(params) = func.params {
                    let mut arg_iter = args.iter();

                    for param in params {
                        if let HirParam::Normal { name, .. } = param {
                            let Some(arg) = arg_iter.next() else {
                                return Interval {
                                    lower: Bound::Opaque(0),
                                    upper: Bound::Opaque(0),
                                };
                            };

                            substitutions
                                .insert(*name, Self::expr_to_interval(arg, resolve, ctx, locals));
                        }
                    }
                }

                Self::substitute(&summary.returned_region, &substitutions)
            }

            HirExpr::Call { .. } => Interval {
                lower: Bound::Opaque(0),
                upper: Bound::Opaque(0),
            },

            _ => Interval {
                lower: Bound::Opaque(0),
                upper: Bound::Opaque(0),
            },
        }
    }

    fn is_early_exit_only(block: &[HirStmt<'a, 'bump>]) -> bool {
        block.iter().all(|stmt| match stmt {
            HirStmt::Return(_) => true,

            HirStmt::Block { body } => Self::is_early_exit_only(body),

            HirStmt::If {
                then_block,
                else_block,
                ..
            } => {
                Self::is_early_exit_only(then_block)
                    && else_block
                        .as_ref()
                        .map(|stmt| match stmt {
                            HirStmt::Block { body } => Self::is_early_exit_only(body),
                            HirStmt::Return(_) => true,
                            _ => false,
                        })
                        .unwrap_or(true)
            }

            _ => false,
        })
    }

    fn substitute_lower(bound: &Bound, subst: &FxHashMap<StrId, Interval>) -> Bound {
        match bound {
            Bound::Const(_) => bound.clone(),

            Bound::Symbol(sym) => subst
                .get(sym)
                .map(|i| i.lower.clone())
                .unwrap_or_else(|| bound.clone()),

            Bound::SelfField(_) => bound.clone(),
            Bound::FreshPerCall => bound.clone(),
            Bound::Opaque(_) => bound.clone(),

            Bound::Offset { base, offset } => Bound::Offset {
                base: Box::new(Self::substitute_lower(base, subst)),
                offset: *offset,
            },

            Bound::Scale { base, factor } => Bound::Scale {
                base: Box::new(Self::substitute_lower(base, subst)),
                factor: *factor,
            },

            Bound::Min(bounds) => Bound::Min(
                bounds
                    .iter()
                    .map(|b| Self::substitute_lower(b, subst))
                    .collect(),
            ),

            Bound::Max(bounds) => Bound::Max(
                bounds
                    .iter()
                    .map(|b| Self::substitute_lower(b, subst))
                    .collect(),
            ),

            Bound::Sum(first, second) => Bound::Sum(
                Box::new(Self::substitute_lower(first, subst)),
                Box::new(Self::substitute_lower(second, subst)),
            ),
        }
    }

    fn substitute_upper(bound: &Bound, subst: &FxHashMap<StrId, Interval>) -> Bound {
        match bound {
            Bound::Const(_) => bound.clone(),

            Bound::Symbol(sym) => subst
                .get(sym)
                .map(|i| i.upper.clone())
                .unwrap_or_else(|| bound.clone()),

            Bound::SelfField(_) => bound.clone(),
            Bound::FreshPerCall => bound.clone(),
            Bound::Opaque(_) => bound.clone(),

            Bound::Offset { base, offset } => Bound::Offset {
                base: Box::new(Self::substitute_upper(base, subst)),
                offset: *offset,
            },

            Bound::Scale { base, factor } => Bound::Scale {
                base: Box::new(Self::substitute_upper(base, subst)),
                factor: *factor,
            },

            Bound::Min(bounds) => Bound::Min(
                bounds
                    .iter()
                    .map(|b| Self::substitute_upper(b, subst))
                    .collect(),
            ),

            Bound::Max(bounds) => Bound::Max(
                bounds
                    .iter()
                    .map(|b| Self::substitute_upper(b, subst))
                    .collect(),
            ),
            Bound::Sum(first, second) => Bound::Sum(
                Box::new(Self::substitute_upper(first, subst)),
                Box::new(Self::substitute_upper(second, subst)),
            ),
        }
    }

    fn substitute(interval: &Interval, subst: &FxHashMap<StrId, Interval>) -> Interval {
        Interval {
            lower: Self::substitute_lower(&interval.lower, subst),
            upper: Self::substitute_upper(&interval.upper, subst),
        }
    }
}

/// Whole-program context for the `is_copy` fixpoint. Built
/// once, after every module has finished HIR lowering, before typechecking
/// begins
pub struct CopyAnalysisCtx<'a, 'bump> {
    registry: GlobalRegistry<'a, 'bump>,
    enums: FxHashMap<StrId, HirEnum<'a, 'bump>>,
    enums_by_module: FxHashMap<usize, Vec<StrId>>,

    #[allow(unused)] // May be used in the future
    copy_iface: StrId,
    drop_iface: StrId,

    is_copy: FxHashMap<StrId, bool>,
}

impl<'a, 'bump> CopyAnalysisCtx<'a, 'bump> {
    pub fn new(
        hir_modules: &[(usize, HirModule<'a, 'bump>)],
        registry: GlobalRegistry<'a, 'bump>,
        context: Arc<StringPool>,
    ) -> Self {
        let mut enums: FxHashMap<StrId, HirEnum<'a, 'bump>> = FxHashMap::default();
        let mut enums_by_module: FxHashMap<usize, Vec<StrId>> = FxHashMap::default();
        for (module_idx, module) in hir_modules {
            let mut names = Vec::new();
            for item in module.items {
                if let Hir::Enum(e) = item {
                    enums.insert(e.name, **e);
                    names.push(e.name);
                }
            }
            enums_by_module.insert(*module_idx, names);
        }

        Self {
            registry,
            enums,
            enums_by_module,
            copy_iface: StrId(context.intern("Copy")),
            drop_iface: StrId(context.intern("Drop")),
            is_copy: FxHashMap::default(),
        }
    }

    /// Incrementally refresh the enum cache for the given modules (an edit
    /// may add/remove/rename enums), then rerun the whole-program fixpoint.
    /// The fixpoint itself can't be scoped to just `updated_modules`, a
    /// struct anywhere in the program may hold a field of an enum/struct
    /// that just changed, so Copy-ness has to be re-derived globally. This
    /// only avoids the O(program) rescan `new()` used to require on every edit.
    pub fn recompute(&mut self, updated_modules: &[(usize, &HirModule<'a, 'bump>)]) {
        for &(module_idx, module) in updated_modules {
            self.upsert_module_enums(module_idx, module);
        }
        self.run();
    }

    /// Call when a module is closed/removed entirely (not just edited).
    pub fn remove_module(&mut self, module_idx: usize) {
        if let Some(old) = self.enums_by_module.remove(&module_idx) {
            for name in old {
                self.enums.remove(&name);
                self.is_copy.remove(&name);
            }
        }
    }

    fn upsert_module_enums(&mut self, module_idx: usize, module: &HirModule<'a, 'bump>) {
        if let Some(old) = self.enums_by_module.remove(&module_idx) {
            for name in old {
                self.enums.remove(&name);
            }
        }
        let mut names = Vec::new();
        for item in module.items {
            if let Hir::Enum(e) = item {
                self.enums.insert(e.name, **e);
                names.push(e.name);
            }
        }
        self.enums_by_module.insert(module_idx, names);
    }

    pub fn run(&mut self) {
        let struct_names = self.struct_names();
        for name in &struct_names {
            self.is_copy.insert(*name, true);
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
            for &name in self.enums.keys() {
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
        self.registry.structs.borrow().keys().copied().collect()
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
        let structs = self.registry.structs.borrow();
        let Some(hir_struct) = structs.get(&name) else {
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

            HirType::Struct { name, .. } => self
                .is_copy
                .get(name)
                .copied()
                .unwrap_or_else(|| panic!("Copy analysis queried for unanalyzed type {}", name)),
            HirType::Enum(name, _) => self
                .is_copy
                .get(name)
                .copied()
                .unwrap_or_else(|| panic!("Copy analysis queried for unanalyzed type {}", name)),

            HirType::Dyn { .. } | HirType::DynInterface(..) => false,

            HirType::String | HirType::Generic(_) | HirType::Unknown => false,
            HirType::Tuple(args) => args.iter().all(|arg| self.type_is_copy(arg)),

            // A kind of pointer known to be able to store multiple elements
            HirType::Array(_, _) => false,
            // A kind of reference to an array of compile-time-unknown length, known to be able to store multiple elements
            HirType::Slice(_) => true,
            HirType::OwnedPointer(_) => false,
            HirType::Usize => true,
            HirType::Isize => true,
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
