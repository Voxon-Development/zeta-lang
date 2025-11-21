use super::context::HirLowerer;
use ir::ast::{self, ElseBranch, LetStmt, Stmt, Type};
use ir::hir::{HirStmt, HirType};

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Statements
    // ===============================
    pub(super) fn lower_stmt(&self, stmt: Stmt<'a, '_>) -> HirStmt<'a, 'bump> {
        match stmt {
            Stmt::Let(l) => self.lower_let_stmt(l),
            Stmt::Return(r) => HirStmt::Return(
                r.value.map(|v| self.ctx.bump.alloc_value_immutable(self.lower_expr(v)))),
            Stmt::ExprStmt(e) => HirStmt::Expr(self.ctx.bump.alloc_value_immutable(self.lower_expr(e.expr))),
            Stmt::If(i) => self.lower_if_stmt(*i),

            Stmt::While(while_stmt) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = while_stmt
                    .block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_value_immutable(HirStmt::Block { body: self.ctx.bump.alloc_slice(&body_vec) });
                HirStmt::While {
                    cond: self.ctx.bump.alloc_value_immutable(self.lower_expr(&while_stmt.condition)),
                    body,
                }
            }

            Stmt::For(for_stmt) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = for_stmt
                    .block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_value_immutable(HirStmt::Block { body: self.ctx.bump.alloc_slice(&body_vec) });
                
                HirStmt::For {
                    init: for_stmt.let_stmt.map(|s| {
                        let stmt = self.lower_stmt(Stmt::Let(s));
                        self.ctx.bump.alloc_value_immutable(stmt)
                    }),
                    condition: for_stmt.condition.map(|e| self.ctx.bump.alloc_value_immutable(self.lower_expr(&e))),
                    increment: for_stmt.increment.map(|e| self.ctx.bump.alloc_value_immutable(self.lower_expr(&e))),
                    body,
                }
            }

            Stmt::Match(match_stmt) => {
                let arms_vec: Vec<ir::hir::HirMatchArm<'a, 'bump>> = match_stmt
                    .arms
                    .into_iter()
                    .map(|a| {
                        let body_vec: Vec<HirStmt<'a, 'bump>> = a.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                        let body = self.ctx.bump.alloc_value_immutable(HirStmt::Block { body: self.ctx.bump.alloc_slice(&body_vec) });
                        ir::hir::HirMatchArm {
                            pattern: self.lower_pattern(&a.pattern),
                            guard: a.guard.map(|guard| self.ctx.bump.alloc_value_immutable(self.lower_expr(guard))),
                            body,
                        }
                    })
                    .collect();
                let arms = self.ctx.bump.alloc_slice(&arms_vec);
                
                HirStmt::Match {
                    expr: self.ctx.bump.alloc_value_immutable(self.lower_expr(&match_stmt.expr)),
                    arms,
                }
            }

            Stmt::Break => HirStmt::Break,
            Stmt::Continue => HirStmt::Continue,
            Stmt::Block(block) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_slice(&body_vec);
                HirStmt::Block { body }
            }
            _ => panic!("unhandled stmt variant: {:?}", stmt),
        }
    }

    fn lower_if_stmt(&self, i: ast::IfStmt<'a, 'bump>) -> HirStmt<'a, 'bump> {
        let cond = self.lower_expr(&i.condition);
        let then_vec: Vec<HirStmt<'a, 'bump>> = i.then_branch
            .into_iter()
            .map(|s| self.lower_stmt(*s))
            .collect();
        let then_block: &[HirStmt] = self.ctx.bump.alloc_slice(&then_vec);
        
        let else_block = i.else_branch.map(|b| {
            let stmt = self.lower_else_branch(*b);
            self.ctx.bump.alloc_value_immutable(stmt)
        });
        
        HirStmt::If {
            cond,
            then_block,
            else_block,
        }
    }

    fn lower_else_branch(&self, branch: ElseBranch<'a, '_>) -> HirStmt<'a, 'bump> {
        match branch {
            ElseBranch::Else(else_block) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = else_block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                let body = self.ctx.bump.alloc_slice(&body_vec);
                HirStmt::Block { body }
            }

            ElseBranch::If(else_if) => self.lower_stmt(Stmt::If(else_if)),
        }
    }

    fn lower_let_stmt(&self, l: &LetStmt<'a, 'bump>) -> HirStmt<'a, 'bump> {
        let value = self.lower_expr(&l.value);
        let final_type: HirType<'a, 'bump> = if l.type_annotation != Type::Infer {
            self.lower_type(&l.type_annotation)
        } else {
            self.infer_type(&value)
        };

        self.ctx
            .variable_types
            .insert(l.ident, final_type);
        
        HirStmt::Let {
            name: l.ident,
            ty: final_type,
            value,
            mutable: l.mutability,
        }
    }
}
