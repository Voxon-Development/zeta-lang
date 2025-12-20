use super::context::HirLowerer;
use ir::ast::Path;
use ir::ast::Stmt;
use ir::hir::{Hir, HirModule, HirStmt, StrId};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Module Lowering
    // ===============================
    pub fn lower_module(
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
    }

    pub(super) fn lower_toplevel(&mut self, stmt: Stmt<'_, 'a>) -> Hir<'a, 'bump> {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_decl(*f);
                Hir::Func(self.ctx.bump.alloc_value(func))
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
