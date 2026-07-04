use super::context::HirLowerer;
use super::utils::lower_visibility;
use ir::ast::{FuncDecl, Generic, Param, ParamPassingKind, StructDecl};
use ir::hir::{
    ConstStmt, HirEnum, HirEnumVariant, HirField, HirFunc, HirGeneric, HirImpl, HirInterface,
    HirParam, HirStmt, HirStruct, StrId, ThisPassingKind,
};

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub(super) fn lower_func_body_from_proto(
        &mut self,
        func: FuncDecl<'a, 'bump>,
        class_name: Option<StrId>,
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
            self.mangle_function_name(class_name, unmangled_name)
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

        let body = func.body.map(|b| {
            let stmts: Vec<HirStmt<'a, 'bump>> =
                b.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
            HirStmt::Block {
                body: self.ctx.bump.alloc_slice(&stmts),
            }
        });

        HirFunc {
            name: lookup_name,
            function_metadata: func.function_metadata,
            params: proto.params,
            return_type: proto.return_type,
            generics,
            body,
            unmangled_name,
        }
    }

    pub(crate) fn lower_generics_slice(
        &self,
        generics: &[Generic<'a, 'bump>],
    ) -> Option<&'bump [HirGeneric<'a, 'bump>]> {
        if generics.is_empty() {
            return None;
        }

        let lowered: Vec<_> = generics
            .iter()
            .map(|g| {
                let constraints_vec: Vec<_> =
                    g.constraints.iter().map(|ty| self.lower_type(ty)).collect();

                HirGeneric {
                    name: g.type_name,
                    constraints: self.ctx.bump.alloc_slice_immutable(&constraints_vec),
                }
            })
            .collect();

        Some(self.ctx.bump.alloc_slice_immutable(&lowered))
    }

    pub fn lower_params(&self, params: &[Param<'a, 'bump>]) -> Vec<HirParam<'a, 'bump>> {
        params
            .iter()
            .map(|p| match p {
                Param::Normal(p) => HirParam::Normal {
                    name: p.name,
                    param_type: self.lower_type(&p.type_annotation),
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
                },
            })
            .collect()
    }

    pub(super) fn lower_struct_decl(&mut self, c: StructDecl<'a, 'bump>) -> HirStruct<'a, 'bump> {
        let generics: Option<&[HirGeneric]> =
            self.lower_generics_slice(c.generics.unwrap_or_default());

        let fields_vec: Vec<HirField<'a, 'bump>> = c
            .params
            .unwrap_or_default()
            .iter()
            .map(|p| self.lower_field(p))
            .collect();

        let fields = self.ctx.bump.alloc_slice(&fields_vec);
        let methods_vec: Vec<HirFunc<'a, 'bump>> = c
            .body
            .into_iter()
            .map(|s| self.lower_func_body_from_proto(*s, Some(c.name)))
            .collect();

        let methods = self.ctx.bump.alloc_slice(&methods_vec);

        let constants_vec: Vec<ConstStmt<'a, 'bump>> = c
            .constants
            .into_iter()
            .map(|b| self.lower_const_stmt(*b))
            .collect();
        let constants: &mut [ConstStmt] = self.ctx.bump.alloc_slice(&constants_vec);

        HirStruct {
            name: c.name,
            visibility: lower_visibility(&c.visibility),
            generics: if generics.is_none() {
                None
            } else {
                Some(generics.unwrap())
            },
            fields,
            interfaces: None,
            methods: if methods.is_empty() {
                None
            } else {
                Some(methods)
            },
            constants: if constants.is_empty() {
                None
            } else {
                Some(constants)
            },
        }
    }

    fn lower_field(&self, p: &Param<'a, 'bump>) -> HirField<'a, 'bump> {
        match p {
            Param::Normal(p) => HirField {
                name: p.name,
                field_type: self.lower_type(&p.type_annotation),
                visibility: lower_visibility(&p.visibility),
                generics: None,
            },
            Param::This(_) => panic!("`this` parameter is not allowed in a class"),
        }
    }

    pub(super) fn lower_interface_decl(
        &mut self,
        i: ir::ast::InterfaceDecl<'a, '_>,
    ) -> HirInterface<'a, 'bump> {
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
            name: i.name,
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

        let methods_vec: Vec<HirFunc<'a, 'bump>> = i
            .methods
            .unwrap_or_default()
            .into_iter()
            .map(|f| self.lower_func_body_from_proto(*f, Some(i.target)))
            .collect();
        let methods = self.ctx.bump.alloc_slice(&methods_vec);

        let hir_impl = HirImpl {
            generics: if generics.is_none() || generics.unwrap().is_empty() {
                None
            } else {
                Some(generics.unwrap())
            },
            interface: i.interface,
            target: i.target,
            methods: if methods.is_empty() {
                None
            } else {
                Some(methods)
            },
        };

        hir_impl
    }

    pub(super) fn lower_enum_decl(&self, e: ir::ast::EnumDecl<'a, '_>) -> HirEnum<'a, 'bump> {
        let variants_vec: Vec<HirEnumVariant<'a, 'bump>> = e
            .variants
            .into_iter()
            .map(|v| {
                let fields_vec: Vec<HirField<'a, 'bump>> = v
                    .fields
                    .into_iter()
                    .map(|f| {
                        let field_type = self.lower_type(&f.field_type);
                        let generics = f.generics.map(|generics| {
                            let lowered_generics: Vec<_> =
                                generics.iter().map(|ty| self.lower_type(ty)).collect();
                            self.ctx.bump.alloc_slice_immutable(&lowered_generics)
                        });

                        HirField {
                            name: f.name,
                            field_type,
                            visibility: lower_visibility(&f.visibility),
                            generics,
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

        let generics: Option<&[HirGeneric]> =
            self.lower_generics_slice(e.generics.unwrap_or_default());

        HirEnum {
            name: e.name,
            visibility: lower_visibility(&e.visibility),
            generics: if generics.is_none() {
                None
            } else {
                Some(generics.unwrap())
            },
            variants,
        }
    }

    pub(super) fn lower_const_stmt(&self, l: ir::ast::ConstStmt<'a, '_>) -> ConstStmt<'a, 'bump> {
        let value = self.lower_expr(&l.value);
        let final_type = self.lower_type(&l.type_annotation);

        self.ctx
            .variable_types
            .borrow_mut()
            .insert(l.ident, final_type);

        ConstStmt {
            name: l.ident,
            ty: final_type,
            value,
        }
    }

    pub(super) fn make_error_function(&self, func: FuncDecl<'a, 'bump>) -> HirFunc<'a, 'bump> {
        HirFunc {
            name: StrId::default(),
            function_metadata: func.function_metadata,
            generics: None,
            params: Some(self.ctx.bump.alloc_slice(&[])),
            return_type: None,
            body: None,
            unmangled_name: StrId::default(), // Not mangled in the first place
        }
    }
}
