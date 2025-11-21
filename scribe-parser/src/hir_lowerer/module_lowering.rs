use super::context::HirLowerer;
use ir::ast::Stmt;
use ir::hir::{Hir, HirModule, HirStmt, StrId};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Module Lowering
    // ===============================
    pub fn lower_module(&self, stmts: Vec<Stmt<'a, '_>, &'_ GrowableBump>) -> HirModule<'a, 'bump> {
        let mut items: Vec<Hir<'a, 'bump>> = Vec::new();
        for stmt in stmts {
            let item = self.lower_toplevel(stmt);
            // If we lowered a function/class/interface, register it for later resolution
            match &item {
                Hir::Func(f) => {
                    self.ctx.functions.insert(f.name, **f);
                }
                Hir::Struct(c) => {
                    self.ctx.classes.insert(c.name, **c);
                }
                Hir::Interface(i) => {
                    self.ctx.interfaces.insert(i.name, **i);
                }
                _ => {}
            }
            items.push(item);
        }
        
        let items_slice = self.ctx.bump.alloc_slice(&items);
        
        HirModule {
            name: StrId(self.ctx.context.intern("root")),
            imports: &[],
            items: items_slice,
        }
    }

    pub(super) fn lower_toplevel(&self, stmt: Stmt<'_, 'a>) -> Hir<'a, 'bump> {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_decl(*f);
                Hir::Func(self.ctx.bump.alloc_value(func))
            }
            Stmt::ClassDecl(c) => {
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
                let body_vec: Vec<HirStmt<'a, 'bump>> = b.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                let body_slice = self.ctx.bump.alloc_slice(&body_vec);
                let inner_block = self.ctx.bump.alloc_value_immutable(HirStmt::Block { body: body_slice });
                let stmt = HirStmt::UnsafeBlock { body: inner_block };
                Hir::Stmt(self.ctx.bump.alloc_value_immutable(stmt))
            }
            other => {
                let stmt = self.lower_stmt(other);
                Hir::Stmt(self.ctx.bump.alloc_value(stmt))
            }
        }
    }
}
