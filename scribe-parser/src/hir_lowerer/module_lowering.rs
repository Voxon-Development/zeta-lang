use ir::hir::HirGeneric;
use ir::hir::HirFunc;
use ir::ast;
use ir::hir::{HirParam, HirType};
use ir::hir::HirFuncProto;
use super::context::HirLowerer;
use ir::ast::Path;
use ir::ast::Stmt;
use ir::hir::{Hir, HirModule, HirStmt, StrId};
use zetaruntime::bump::GrowableBump;
use crate::hir_lowerer::lower_visibility;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Module Lowering
    // ===============================
    /*pub fn lower_module(
        &mut self,
        stmts: Vec<Stmt<'a, '_>, &'_ GrowableBump>,
    ) -> HirModule<'a, 'bump> {
        let mut items: Vec<Hir<'a, 'bump>> = Vec::new();
        let mut imports: Vec<Path<'bump>> = Vec::new();

        for stmt in stmts {
            match stmt {
                // Handle Import statements - add to imports list for dependency tracking
                Stmt::Import(import_stmt) => {
                    imports.push(*import_stmt.path);
                    // Import statements are tracked but don't generate HIR items
                    // They will be used by the dependency graph for module resolution
                }
                // Handle Package statements - similar to imports but for package declarations
                Stmt::Package(package_stmt) => {
                    // Package statements define the current module's package
                    // Store as module metadata (could be used for namespace resolution)
                    let _ = package_stmt.path;
                    // Package statements are metadata, not HIR items
                }
                // All other statements are lowered normally
                other => {
                    let item = self.lower_toplevel(other);
                    match &item {
                        Hir::Func(f) => {
                            self.ctx.functions.borrow_mut().insert(f.name, **f);
                        }
                        Hir::Struct(c) => {
                            self.ctx.classes.borrow_mut().insert(c.name, **c);
                        }
                        Hir::Interface(i) => {
                            self.ctx.interfaces.borrow_mut().insert(i.name, **i);
                        }
                        _ => {}
                    }
                    items.push(item);
                }
            }
        }

        let items_slice = self.ctx.bump.alloc_slice(&items);
        let imports_slice = self.ctx.bump.alloc_slice(&imports);

        HirModule {
            name: StrId(self.ctx.context.intern("root")),
            imports: imports_slice,
            items: items_slice,
        }
    }*/

    pub fn lower_module(&mut self, stmts: Vec<Stmt<'a, '_>, &'_ GrowableBump>) -> HirModule<'a, 'bump> {
        self.collect_prototypes(&stmts);
        let (imports, items): (Vec<Path>, Vec<Hir>) = self.lower_function_bodies(stmts);



        HirModule {
            name: StrId(self.ctx.context.intern("root")),
            imports: self.ctx.bump.alloc_slice(&imports),
            items: self.ctx.bump.alloc_slice(&items),
        }
    }

    pub fn collect_prototypes(
        &mut self,
        stmts: &[Stmt<'a, '_>],
    ) {
        for stmt in stmts {
            if let Stmt::FuncDecl(f) = stmt {
                let proto: HirFuncProto = self.lower_func_proto(f);
                self.ctx.functions.borrow_mut().insert(proto.name, HirFunc {
                    name: proto.name,
                    params: proto.params,
                    return_type: Some(proto.return_type),
                    body: None,
                    inline: proto.inline,
                    noinline: proto.noinline,
                    is_unsafe: proto.is_unsafe,
                    visibility: proto.visibility,
                    generics: None,
                });
            }
        }
    }

    pub fn lower_function_bodies(
        &mut self,
        stmts: Vec<Stmt<'a, '_>, &'_ GrowableBump>,
    ) -> (Vec<Path<'bump>>, Vec<Hir<'a, 'bump>>) {
        let mut imports: Vec<Path<'bump>> = Vec::with_capacity(64);
        let mut items: Vec<Hir<'a, 'bump>> = Vec::with_capacity(64);

        for stmt in stmts {
            match stmt {
                Stmt::Import(import_stmt) => {
                    imports.push(*import_stmt.path);
                    // Import statements are tracked but don't generate HIR items
                    // They will be used by the dependency graph for module resolution
                },
                Stmt::Package(package_stmt) => {
                    // Package statements define the current module's package
                    // Store as module metadata (could be used for namespace resolution)
                    let _ = package_stmt.path;
                    // Package statements are metadata, not HIR items
                },
                Stmt::FuncDecl(f) => {
                    let mut func_binding = self.ctx.functions.borrow_mut();
                    let func = func_binding.get_mut(&f.name).unwrap();
                    if let Some(f_body) = f.body {
                        func.body = Some(self.lower_block(f_body));
                    }

                    items.push(Hir::Func(self.ctx.bump.alloc_value(func.clone())));
                }
                other => items.push(self.lower_toplevel(other)),
            }
        }

        (imports, items)
    }

    pub fn lower_func_proto(
        &self,
        f: &ast::FuncDecl<'a, 'bump>,
    ) -> HirFuncProto<'a, 'bump> {
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
            inline: f.inline,
            noinline: f.noinline,
            is_unsafe: f.is_unsafe,
            visibility: lower_visibility(&f.visibility),
            generics: f.generics.map(|gs| {
                let lowered: Vec<HirGeneric<'a, 'bump>> = self.lower_generics(gs);
                self.ctx.bump.alloc_slice_immutable(&lowered)
            }),
            extern_string: f.extern_string,
            is_extern: f.is_extern,
        }
    }

    pub fn lower_block(
        &self,
        block: &'a ir::ast::Block<'a, 'bump>,
    ) -> HirStmt<'a, 'bump> {
        let mut stmts = Vec::with_capacity(block.block.len());

        for stmt in block {
            stmts.push(self.lower_stmt(*stmt));
        }

        HirStmt::Block {
            body: self.ctx.bump.alloc_slice(&stmts),
        }
    }


    pub(super) fn lower_toplevel(&mut self, stmt: Stmt<'_, 'a>) -> Hir<'a, 'bump> {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_body_from_proto(*f);

                // hydrate the global function table
                self.ctx.functions
                    .borrow_mut()
                    .insert(func.name, func);

                Hir::Func(self.ctx.bump.alloc_value(
                    self.ctx.functions.borrow()[&func.name]
                ))
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
