use super::context::HirLowerer;
use ir::ast::Stmt;
use ir::ast::{FuncDecl, Path};
use ir::hir::HirFunc;
use ir::hir::HirFuncProto;
use ir::hir::{Hir, HirModule, HirStmt, StrId};
use ir::hir::{HirParam, HirType};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn lower_module(
        &mut self,
        stmts: Vec<Stmt<'a, 'bump>, &GrowableBump<'bump>>,
    ) -> HirModule<'a, 'bump> {
        self.collect_prototypes(&stmts);
        let (imports, items, pkg_name) = self.lower_function_bodies(stmts);

        HirModule {
            name: pkg_name.unwrap_or_else(|| StrId(self.ctx.context.intern("root"))),
            imports: self.ctx.bump.alloc_slice(&imports),
            items: self.ctx.bump.alloc_slice(&items),
        }
    }

    pub fn collect_prototypes(&mut self, stmts: &[Stmt<'a, '_>]) {
        for stmt in stmts {
            if let Stmt::FuncDecl(f) = stmt {
                self.lower_func_as_proto(f);
            }
            if let Stmt::StructDecl(struct_decl) = stmt {
                for x in struct_decl.body {
                    self.lower_func_as_proto(x);
                }
            }
            if let Stmt::InterfaceDecl(interface_decl) = stmt {
                let Some(methods) = interface_decl.methods else {
                    continue;
                };
                for x in methods {
                    self.lower_func_as_proto(x);
                }
            }
            if let Stmt::ImplDecl(impl_decl) = stmt {
                let Some(methods) = impl_decl.methods else {
                    continue;
                };
                for x in methods {
                    self.lower_func_as_proto(x);
                }
            }
            // Recurse into inline module declarations so nested functions
            // get their prototypes registered before bodies are lowered.
            if let Stmt::Module(module_decl) = stmt {
                self.collect_prototypes(module_decl.body);
            }
        }
    }

    fn lower_func_as_proto(&mut self, f: &FuncDecl<'a, '_>) {
        let proto: HirFuncProto = self.lower_func_proto(f);
        self.ctx.functions.borrow_mut().insert(
            proto.name,
            HirFunc {
                name: proto.name,
                params: proto.params,
                return_type: Some(proto.return_type),
                body: None,
                function_metadata: proto.function_metadata,
                generics: None,
            },
        );
    }

    pub fn lower_function_bodies(
        &mut self,
        stmts: Vec<ir::ast::Stmt<'a, '_>, &GrowableBump<'_>>,
    ) -> (Vec<Path<'a, 'bump>>, Vec<Hir<'a, 'bump>>, Option<StrId>) {
        let mut imports: Vec<Path<'a, 'bump>> = Vec::with_capacity(64);
        let mut items: Vec<Hir<'a, 'bump>> = Vec::with_capacity(64);
        let mut pkg_name: Option<StrId> = None;

        for stmt in stmts {
            match stmt {
                Stmt::Import(import_stmt) => {
                    imports.push(*import_stmt.path);
                }
                Stmt::Package(package_stmt) => {
                    // Format the dot-joined path string and intern it as the
                    // module's canonical name (e.g. "com.example.myapp").
                    let joined = format!("{}", package_stmt.path);
                    pkg_name = Some(StrId(self.ctx.context.intern(&joined)));
                }
                Stmt::Module(module_decl) => {
                    // Flatten the inline module's declarations into the parent
                    // module's item list (they share the same HIR module node).
                    for &body_stmt in module_decl.body {
                        match body_stmt {
                            Stmt::FuncDecl(f) => {
                                // Lower body first (needs immutable borrow of functions)
                                let lowered_body = f.body.map(|b| self.lower_block(b));
                                // Now do mutable borrow to update function
                                let mut func_binding = self.ctx.functions.borrow_mut();
                                let func = func_binding.get_mut(&f.name).unwrap();
                                func.body = lowered_body;
                                items.push(Hir::Func(self.ctx.bump.alloc_value(func.clone())));
                            }
                            other => items.push(self.lower_toplevel(other)),
                        }
                    }
                }
                Stmt::FuncDecl(f) => {
                    // Lower body first (needs immutable borrow of functions)
                    let lowered_body = f.body.map(|b| self.lower_block(b));
                    // Now do mutable borrow to update function
                    let mut func_binding = self.ctx.functions.borrow_mut();
                    let func = func_binding.get_mut(&f.name).unwrap();
                    func.body = lowered_body;
                    items.push(Hir::Func(self.ctx.bump.alloc_value(func.clone())));
                }
                other => items.push(self.lower_toplevel(other)),
            }
        }

        (imports, items, pkg_name)
    }

    pub fn lower_func_proto(&self, f: &FuncDecl<'a, 'bump>) -> HirFuncProto<'a, 'bump> {
        let params = f.params.map(|ps| {
            let lowered: Vec<HirParam<'a, 'bump>> = self.lower_params(ps);
            self.ctx.bump.alloc_slice_immutable(&lowered)
        });

        let return_type = match f.return_type {
            Some(ty) => self.lower_type(&ty),
            None => HirType::Void,
        };

        HirFuncProto {
            name: f.name,
            params,
            return_type,
            function_metadata: f.function_metadata,
            generics: if f.generics.is_none() {
                None
            } else {
                self.lower_generics_slice(f.generics.unwrap())
            },
        }
    }

    pub fn lower_block(&self, block: &'a ir::ast::Block<'a, 'bump>) -> HirStmt<'a, 'bump> {
        let mut stmts = Vec::with_capacity(block.block.len());

        for stmt in block {
            stmts.push(self.lower_stmt(*stmt));
        }

        HirStmt::Block {
            body: self.ctx.bump.alloc_slice(&stmts),
        }
    }

    pub(super) fn lower_toplevel(&mut self, stmt: Stmt<'a, 'bump>) -> Hir<'a, 'bump> {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_body_from_proto(*f);

                // hydrate the global function table
                self.ctx.functions.borrow_mut().insert(func.name, func);

                Hir::Func(
                    self.ctx
                        .bump
                        .alloc_value(self.ctx.functions.borrow()[&func.name]),
                )
            }
            Stmt::StructDecl(c) => {
                let class = self.lower_struct_decl(*c);
                Hir::Struct(self.ctx.bump.alloc_value(class))
            }
            Stmt::InterfaceDecl(i) => {
                let interface = self.lower_interface_decl(*i);
                Hir::Interface(self.ctx.bump.alloc_value(interface))
            }
            Stmt::ImplDecl(i) => {
                let impl_decl = self.lower_impl_decl(*i);
                Hir::Impl(self.ctx.bump.alloc_value(impl_decl))
            }
            Stmt::EnumDecl(e) => {
                let enum_decl = self.lower_enum_decl(*e);
                Hir::Enum(self.ctx.bump.alloc_value(enum_decl))
            }
            Stmt::UnsafeBlock(b) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> =
                    b.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                let body_slice = self.ctx.bump.alloc_slice(&body_vec);
                let inner_block = self
                    .ctx
                    .bump
                    .alloc_value_immutable(HirStmt::Block { body: body_slice });
                let stmt = HirStmt::UnsafeBlock { body: inner_block };
                Hir::Stmt(self.ctx.bump.alloc_value_immutable(stmt))
            }
            Stmt::Const(c) => Hir::Const(
                self.ctx
                    .bump
                    .alloc_value_immutable(self.lower_const_stmt(*c)),
            ),
            other => {
                let stmt = self.lower_stmt(other);
                Hir::Stmt(self.ctx.bump.alloc_value(stmt))
            }
        }
    }
}
