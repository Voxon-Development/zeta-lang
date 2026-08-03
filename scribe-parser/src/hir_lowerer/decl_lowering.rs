use std::sync::Arc;

use crate::hir_lowerer::module_lowering::{ImplTargetKind, primitive_hir_type};

use super::context::HirLowerer;
use ir::ast::{FuncDecl, Generic, InterfaceDecl, Param, ParamPassingKind, StructDecl};
use ir::hir::{
    ConstStmt, HirEnum, HirEnumVariant, HirField, HirFunc, HirGeneric, HirImpl, HirInterface,
    HirParam, HirStmt, HirStruct, HirType, StrId, ThisPassingKind,
};
use ir::hir_utils::lower_visibility;
use ir::span::SourceSpan;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::intern_fmt;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub(super) fn lower_func_body_from_proto(
        &mut self,
        func: FuncDecl<'a, 'bump>,
        struct_name: Option<StrId>,
    ) -> HirFunc<'a, 'bump> {
        let is_extern = matches!(
            func.function_metadata.extern_modifier,
            ir::ast::ExternModifier::Abi(_)
        );
        let unmangled_name = func.name;
        let is_main = self.ctx.context.resolve_string(&unmangled_name) == "main";
        let lookup_name = if is_extern || is_main {
            unmangled_name
        } else {
            self.mangle_function_name(self.ctx.module_idx, struct_name, unmangled_name)
        };
        let binding = self.ctx.functions.borrow();
        let proto = match binding.get(&lookup_name) {
            Some(p) => p,
            None => {
                drop(binding);
                return self.make_error_function(func);
            }
        };
        let generics = self.lower_generics_slice(func.generics.unwrap_or_default());

        if let Some(gs) = generics {
            for g in gs {
                self.add_generic_param(g.name);
            }
        }

        let body = func.body.map(|b| {
            let stmts: Vec<HirStmt<'a, 'bump>> =
                b.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
            HirStmt::Block {
                body: self.ctx.bump.alloc_slice(&stmts),
            }
        });

        if let Some(gs) = generics {
            for g in gs {
                self.remove_generic_param(g.name);
            }
        }

        HirFunc {
            name: lookup_name,
            function_metadata: func.function_metadata,
            params: proto.params,
            return_type: proto.return_type,
            generics,
            body,
            unmangled_name,
            declaring_module_idx: self.ctx.module_idx,
            impl_target: struct_name,
        }
    }

    pub(crate) fn lower_generics_slice(
        &self,
        generics: &[Generic<'a, 'bump>],
    ) -> Option<&'bump [HirGeneric<'a, 'bump>]> {
        if generics.is_empty() {
            return None;
        }

        // Pre-register every name in this list so constraints can reference
        // siblings declared later in the same generic parameter list
        // (e.g. `map<U, F: func(T): U>`, where F's constraint mentions U).
        for g in generics {
            self.add_generic_param(g.type_name);
        }

        let lowered: Vec<_> = generics
            .iter()
            .map(|g| {
                let constraints_vec: Vec<_> = g
                    .constraints
                    .iter()
                    .map(|ty| self.lower_type(ty, g.span))
                    .collect();

                HirGeneric {
                    name: g.type_name,
                    constraints: self.ctx.bump.alloc_slice_immutable(&constraints_vec),
                }
            })
            .collect();

        for g in generics {
            self.remove_generic_param(g.type_name);
        }

        Some(self.ctx.bump.alloc_slice_immutable(&lowered))
    }

    pub fn lower_params(&self, params: &[Param<'a, 'bump>]) -> Vec<HirParam<'a, 'bump>> {
        params
            .iter()
            .map(|p| match p {
                Param::Normal(p) => HirParam::Normal {
                    name: p.name,
                    param_type: self.lower_type(&p.type_annotation, p.span),
                    span: p.span,
                },
                Param::This(tp) => HirParam::This {
                    kind: match tp.passing_kind {
                        ParamPassingKind::RefConst => ThisPassingKind::RefConst,
                        ParamPassingKind::RefMut => ThisPassingKind::RefMut,
                        ParamPassingKind::MutSafePtr => ThisPassingKind::MutSafePtr,
                        ParamPassingKind::MutUnsafePtr => ThisPassingKind::MutUnsafePtr,
                        ParamPassingKind::ConstUnsafePtr => ThisPassingKind::ConstUnsafePtr,
                        ParamPassingKind::ConstSafePtr => ThisPassingKind::ConstSafePtr,
                        ParamPassingKind::Move => ThisPassingKind::Move,
                        ParamPassingKind::MoveMut => ThisPassingKind::MoveMut,
                    },
                    span: tp.span,
                },
            })
            .collect()
    }

    pub(super) fn lower_struct_decl(&mut self, c: StructDecl<'a, 'bump>) -> HirStruct<'a, 'bump> {
        let generics: Option<&[HirGeneric]> =
            self.lower_generics_slice(c.generics.unwrap_or_default());

        if let Some(gs) = generics {
            for g in gs {
                self.add_generic_param(g.name);
            }
        }

        let fields_vec: Vec<HirField<'a, 'bump>> = c
            .params
            .unwrap_or_default()
            .iter()
            .map(|p| self.lower_field(p))
            .collect();
        let fields = self.ctx.bump.alloc_slice(&fields_vec);

        if let Some(gs) = generics {
            for g in gs {
                self.remove_generic_param(g.name);
            }
        }

        HirStruct {
            name: self.ctx.mangle_type_name(c.name),
            visibility: lower_visibility(&c.visibility),
            generics,
            fields,
        }
    }

    fn lower_field(&self, p: &Param<'a, 'bump>) -> HirField<'a, 'bump> {
        match p {
            Param::Normal(p) => HirField {
                name: p.name,
                field_type: self.lower_type(&p.type_annotation, p.span),
                visibility: lower_visibility(&p.visibility),
            },
            Param::This(_) => panic!("`this` parameter is not allowed in a struct"),
        }
    }

    pub fn lower_interface_decl(&mut self, i: InterfaceDecl<'a, 'bump>) -> HirInterface<'a, 'bump> {
        let is_builtin = matches!(
            i.name.as_str(),
            "Drop" | "Copy" | "Clone" | "Allocator" | "RawAllocator"
        );

        let interface_name = if is_builtin && !self.ctx.interfaces.borrow().contains_key(&i.name) {
            i.name
        } else {
            self.mangle_with_module_path(i.name)
        };

        let methods_vec: Vec<HirFunc<'a, 'bump>> = i
            .methods
            .unwrap_or_default()
            .into_iter()
            .map(|f| self.lower_func_body_from_proto(*f, Some(i.name)))
            .collect();
        let methods = self.ctx.bump.alloc_slice(&methods_vec);
        let generics: Option<&[HirGeneric]> =
            self.lower_generics_slice(i.generics.unwrap_or_default());

        HirInterface {
            name: interface_name,
            visibility: lower_visibility(&i.visibility),
            methods: if methods.is_empty() {
                None
            } else {
                Some(methods)
            },
            generics: if generics.is_none() {
                None
            } else {
                Some(generics.unwrap())
            },
        }
    }

    pub(super) fn lower_impl_decl(
        &mut self,
        i: ir::ast::ImplDecl<'a, 'bump>,
    ) -> HirImpl<'a, 'bump> {
        let generics: Option<&[HirGeneric]> =
            self.lower_generics_slice(i.generics.unwrap_or_default());

        if let Some(gs) = generics {
            for g in gs {
                self.add_generic_param(g.name);
            }
        }

        let (target_key, target_kind) = match self.resolve_impl_target(&i.target, i.span) {
            Some(found) => found,
            None => {
                self.ctx.record_error(
                    format!(
                        "invalid `impl` target `{:?}`: expected a struct, primitive, or slice/array type",
                        i.target.kind
                    ),
                    i.span,
                );
                (
                    StrId(self.ctx.context.intern("<error>")),
                    ImplTargetKind::UserType,
                )
            }
        };
        let target_generics = self.lower_impl_type_args(&i.target, i.span);

        let (interface_key, interface_generics) = match &i.interface {
            Some(iface_ty) => match iface_ty.struct_name_path() {
                Some((iface_name, iface_path)) => {
                    let iface_key = self
                        .ctx
                        .resolve_type_path_name(iface_path, iface_name, i.span);
                    (Some(iface_key), self.lower_impl_type_args(iface_ty, i.span))
                }
                None => {
                    self.ctx.record_error(
                        format!(
                            "`by` clause must name an interface type, found `{:?}`",
                            iface_ty.kind
                        ),
                        i.span,
                    );
                    (None, None)
                }
            },
            None => (None, None),
        };

        let self_type_args: Vec<HirType<'a, 'bump>> = generics
            .map(|gs| gs.iter().map(|g| HirType::Generic(g.name)).collect())
            .unwrap_or_default();

        let self_ty = self.impl_target_self_type(target_key, target_kind, &self_type_args);

        let methods_vec: Vec<HirFunc<'a, 'bump>> = i
            .methods
            .unwrap_or_default()
            .into_iter()
            .map(|f| {
                let prev_self = self.ctx.current_self_type.replace(Some(self_ty));
                let mut func = self.lower_func_body_from_proto(*f, Some(target_key));
                self.ctx.current_self_type.replace(prev_self);
                func.generics =
                    Self::merge_generics(generics, func.generics, self.ctx.bump.clone());
                func
            })
            .collect();
        let methods = self.ctx.bump.alloc_slice(&methods_vec);

        for m in methods.iter() {
            self.ctx.functions.borrow_mut().insert(m.name, *m);
        }

        if let Some(gs) = generics {
            for g in gs {
                self.remove_generic_param(g.name);
            }
        }

        HirImpl {
            generics: generics.filter(|g| !g.is_empty()),
            interface: interface_key,
            interface_generics,
            target: target_key,
            target_generics,
            methods: if methods.is_empty() {
                None
            } else {
                Some(methods)
            },
        }
    }

    /// The `HirType` that `this`/`Self` resolves to while lowering an impl
    /// block's method bodies.
    fn impl_target_self_type(
        &self,
        target_key: StrId,
        target_kind: ImplTargetKind,
        self_type_args: &[HirType<'a, 'bump>],
    ) -> HirType<'a, 'bump> {
        match target_kind {
            ImplTargetKind::UserType => {
                let field_types: Vec<HirType<'a, 'bump>> = self
                    .ctx
                    .structs
                    .borrow()
                    .get(&target_key)
                    .map(|c| c.fields.iter().map(|f| f.field_type).collect())
                    .unwrap_or_default();
                HirType::Struct {
                    name: target_key,
                    field_types: self.ctx.bump.alloc_slice_immutable(&field_types),
                    type_args: self.ctx.bump.alloc_slice_immutable(self_type_args),
                }
            }
            ImplTargetKind::Primitive => primitive_hir_type(target_key, &self.ctx.context)
                .expect("registered primitive impl target must map to a primitive HirType"),
            ImplTargetKind::Slice { element } => {
                let elem_ty = match element {
                    Some(e) => {
                        primitive_hir_type(e, &self.ctx.context).unwrap_or(HirType::Generic(e))
                    }

                    None => HirType::Void,
                };
                HirType::Slice(self.ctx.bump.alloc_value(elem_ty))
            }
        }
    }

    fn lower_impl_type_args(
        &self,
        ty: &ir::ast::Type<'a, 'bump>,
        span: SourceSpan<'a>,
    ) -> Option<&'bump [HirType<'a, 'bump>]> {
        let ir::ast::TypeKind::Struct { generics, .. } = ty.kind else {
            return None;
        };
        if generics.is_empty() {
            return None;
        }
        let lowered: Vec<HirType> = generics.iter().map(|g| self.lower_type(g, span)).collect();
        Some(self.ctx.bump.alloc_slice_immutable(&lowered))
    }

    fn merge_generics<'x>(
        impl_generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
        method_generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
        bump: Arc<GrowableAtomicBump<'bump>>,
    ) -> Option<&'bump [HirGeneric<'a, 'bump>]> {
        match (impl_generics, method_generics) {
            (None, m) => m,
            (i, None) => i,
            (Some(i), Some(m)) => {
                let mut combined: Vec<HirGeneric> = Vec::with_capacity(i.len() + m.len());
                combined.extend_from_slice(i);
                combined.extend_from_slice(m);
                Some(bump.alloc_slice_immutable(&combined))
            }
        }
    }

    pub(super) fn lower_enum_decl(&self, e: ir::ast::EnumDecl<'a, '_>) -> HirEnum<'a, 'bump> {
        let generics: Option<&[HirGeneric]> =
            self.lower_generics_slice(e.generics.unwrap_or_default());

        if let Some(gs) = generics {
            for g in gs {
                self.add_generic_param(g.name);
            }
        }

        let variants_vec: Vec<HirEnumVariant<'a, 'bump>> = e
            .variants
            .into_iter()
            .map(|v| {
                let fields_vec: Vec<HirField<'a, 'bump>> = v
                    .fields
                    .into_iter()
                    .map(|f| {
                        let field_type = self.lower_type(&f.field_type, f.span);
                        HirField {
                            name: f.name,
                            field_type,
                            visibility: lower_visibility(&f.visibility),
                        }
                    })
                    .collect();
                let fields = self.ctx.bump.alloc_slice(&fields_vec);
                HirEnumVariant {
                    name: v.name,
                    fields,
                }
            })
            .collect();
        let variants: &mut [HirEnumVariant<'a, 'bump>] = self.ctx.bump.alloc_slice(&variants_vec);

        if let Some(gs) = generics {
            for g in gs {
                self.remove_generic_param(g.name);
            }
        }

        HirEnum {
            name: self.ctx.mangle_type_name(e.name),
            visibility: lower_visibility(&e.visibility),
            generics,
            variants,
        }
    }

    pub(super) fn lower_const_stmt(
        &self,
        const_stmt: ir::ast::ConstStmt<'a, '_>,
    ) -> ConstStmt<'a, 'bump> {
        let value = self.lower_expr(&const_stmt.value);
        let final_type = self.lower_type(&const_stmt.type_annotation, const_stmt.span);

        self.ctx
            .variable_types
            .borrow_mut()
            .insert(const_stmt.ident, final_type);

        ConstStmt {
            name: const_stmt.ident,
            ty: final_type,
            value,
        }
    }

    pub(super) fn make_error_function(&self, func: FuncDecl<'a, 'bump>) -> HirFunc<'a, 'bump> {
        let name = StrId(intern_fmt!(self.ctx.context, "{}_error", func.name));
        HirFunc {
            name,
            function_metadata: func.function_metadata,
            generics: None,
            params: Some(self.ctx.bump.alloc_slice(&[])),
            return_type: None,
            body: None,
            unmangled_name: name, // no one cares if an error function's name is mangled
            declaring_module_idx: self.ctx.module_idx,
            impl_target: None,
        }
    }
}
