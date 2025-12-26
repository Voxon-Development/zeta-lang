use super::context::HirLowerer;
use super::utils::lower_visibility;
use ir::ast::{StructDecl, FuncDecl, Param, Generic};
use ir::hir::{self, ConstStmt, HirEnum, HirEnumVariant, HirField, HirFunc, HirGeneric, HirImpl, HirInterface, HirParam, HirStmt, HirStruct};

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Function/Class/Interface/Enum Lowering
    // ===============================
    pub(super) fn lower_func_body_from_proto(
        &self,
        func: FuncDecl<'a, 'bump>,
    ) -> HirFunc<'a, 'bump> {
        // 1. Fetch prototype (must exist)
        let binding = self.ctx.func_protos
            .borrow();
        let proto = binding
            .get(&func.name)
            .expect("function prototype missing");

        // 2. Lower generics (body-time, not proto-time)
        let generics_vec = self.lower_generics(func.generics.unwrap_or_default());
        let generics = if generics_vec.is_empty() {
            None
        } else {
            Some(self.ctx.bump.alloc_slice_immutable(&generics_vec))
        };

        // 3. Lower body ONLY
        let body = func.body.map(|b| {
            let stmts: Vec<HirStmt<'a, 'bump>> =
                b.block.into_iter().map(|s| self.lower_stmt(*s)).collect();

            HirStmt::Block {
                body: self.ctx.bump.alloc_slice(&stmts),
            }
        });

        HirFunc {
            name: func.name,
            visibility: lower_visibility(&func.visibility),
            is_unsafe: func.is_unsafe,
            inline: proto.inline,
            noinline: func.noinline,

            params: proto.params,
            return_type: Some(proto.return_type),

            generics,
            body,
        }
    }

    pub fn lower_generics(&self, generics: &[Generic<'a, 'bump>]) -> Vec<HirGeneric<'a, 'bump>> {
        generics
            .into_iter()
            .map(|g| HirGeneric {
                name: g.type_name,
                constraints: self.ctx.bump.alloc_slice_immutable(g.constraints.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>().as_slice()),
            })
            .collect()
    }

    pub fn lower_params(&self, params: &[Param<'a, 'bump>]) -> Vec<HirParam<'a, 'bump>> {
        params
            .into_iter()
            .enumerate()
            .map(|(idx, p)| match p {
                Param::Normal(p) => {
                    // Check if this is the first parameter and it's named "this"
                    let param_name = self.ctx.context.resolve_string(&p.name);
                    if idx == 0 && param_name == "this" {
                        // Convert to This parameter
                        hir::HirParam::This {
                            param_type: Some(self.lower_type(&p.type_annotation)),
                        }
                    } else {
                        hir::HirParam::Normal {
                            name: p.name,
                            param_type: self.lower_type(&p.type_annotation),
                        }
                    }
                }
                Param::This(p) => hir::HirParam::This {
                    param_type: p.type_annotation.map(|t| self.lower_type(&t)),
                },
            })
            .collect()
    }

    pub(super) fn lower_struct_decl(&mut self, c: StructDecl<'a, 'bump>) -> HirStruct<'a, 'bump> {
        let generics_vec: Vec<HirGeneric> = c.generics
            .unwrap_or_default()
            .into_iter()
            .map(|g| HirGeneric {
                name: g.type_name,
                constraints: self.ctx.bump.alloc_slice_immutable(g.constraints.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>().as_slice()),
            })
            .collect();
        
        let generics = self.ctx.bump.alloc_slice(&generics_vec);

        let fields_vec: Vec<HirField<'a, 'bump>> = c.params
            .unwrap_or_default()
            .iter()
            .map(|p| self.lower_field(p))
            .collect();
        
        let fields = self.ctx.bump.alloc_slice(&fields_vec);
        let methods_vec: Vec<HirFunc<'a, 'bump>> = c
            .body
            .into_iter()
            .map(|s| self.lower_func_body_from_proto(*s))
            .collect();
        
        let methods = self.ctx.bump.alloc_slice(&methods_vec);

        let constants_vec: Vec<ConstStmt<'a, 'bump>> = c.constants.into_iter().map(|b| self.lower_const_stmt(*b)).collect();
        let constants: &mut [ConstStmt] = self.ctx.bump.alloc_slice(&constants_vec);

        let destructor: Option<&HirStmt> = c.destructor.map(|block| {
            let stmts_vec: Vec<HirStmt<'a, 'bump>> = block.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
            let block_stmt = HirStmt::Block { body: self.ctx.bump.alloc_slice(&stmts_vec) };
            &*self.ctx.bump.alloc_value(block_stmt)
        });

        HirStruct {
            name: c.name,
            visibility: lower_visibility(&c.visibility),
            generics: if generics.is_empty() { None } else { Some(generics) },
            fields,
            interfaces: None,
            methods: if methods.is_empty() { None } else { Some(methods) },
            constants: if constants.is_empty() { None } else { Some(constants) },
            destructor,
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

    pub(super) fn lower_interface_decl(&self, i: ir::ast::InterfaceDecl<'a, '_>) -> HirInterface<'a, 'bump> {
        let methods_vec: Vec<HirFunc<'a, 'bump>> = i
            .methods
            .unwrap_or_default()
            .into_iter()
            .map(|f| self.lower_func_body_from_proto(*f))
            .collect();
        let methods = self.ctx.bump.alloc_slice(&methods_vec);

        let generics_vec: Vec<HirGeneric> = i
            .generics
            .unwrap_or_default()
            .into_iter()
            .map(|g| HirGeneric {
                name: g.type_name,
                constraints: self.ctx.bump.alloc_slice_immutable(g.constraints.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>().as_slice()),
            })
            .collect();
        let generics = self.ctx.bump.alloc_slice(&generics_vec);

        HirInterface {
            name: i.name,
            visibility: lower_visibility(&i.visibility),
            methods: if methods.is_empty() { None } else { Some(methods) },
            generics: if generics.is_empty() { None } else { Some(generics) },
        }
    }

    pub(super) fn lower_impl_decl(&self, i: ir::ast::ImplDecl<'a, '_>) -> HirImpl<'a, 'bump> {
        let generics_vec: Vec<HirGeneric> = i
            .generics
            .unwrap_or_default()
            .into_iter()
            .map(|g| HirGeneric {
                name: g.type_name,
                constraints: self.ctx.bump.alloc_slice_immutable(g.constraints.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>().as_slice()),
            })
            .collect();
        let generics = self.ctx.bump.alloc_slice(&generics_vec);

        let methods_vec: Vec<HirFunc<'a, 'bump>> = i
            .methods
            .unwrap_or_default()
            .into_iter()
            .map(|f| self.lower_func_body_from_proto(*f))
            .collect();
        let methods = self.ctx.bump.alloc_slice(&methods_vec);

        let hir_impl = HirImpl {
            generics: if generics.is_empty() { None } else { Some(generics) },
            interface: i.interface,
            target: i.target,
            methods: if methods.is_empty() { None } else { Some(methods) },
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
                            let lowered_generics: Vec<_> = generics.iter()
                                .map(|ty| self.lower_type(ty))
                                .collect();
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

        let generics_vec: Vec<HirGeneric> = e.generics
            .unwrap_or_default()
            .into_iter()
            .map(|g| HirGeneric {
                name: g.type_name,
                constraints: self.ctx.bump.alloc_slice_immutable(g.constraints.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>().as_slice()),
            })
            .collect();
        let generics: &mut [HirGeneric<'a, 'bump>] = self.ctx.bump.alloc_slice(&generics_vec);

        HirEnum {
            name: e.name,
            visibility: lower_visibility(&e.visibility),
            generics: if generics.is_empty() { None } else { Some(generics) },
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
}
