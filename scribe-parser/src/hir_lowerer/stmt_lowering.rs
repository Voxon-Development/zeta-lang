use super::context::HirLowerer;
use ir::ast::{self, ElseBranch, LetStmt, Stmt, Type};
use ir::hir::{Hir, HirStmt, HirType};

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
                
                match &for_stmt.kind {
                    ir::ast::ForKind::CStyle { let_stmt, condition, increment } => {
                        HirStmt::For {
                            init: let_stmt.map(|s| {
                                let stmt = self.lower_stmt(Stmt::Let(s));
                                self.ctx.bump.alloc_value_immutable(stmt)
                            }),
                            condition: condition.map(|e| self.ctx.bump.alloc_value_immutable(self.lower_expr(&e))),
                            increment: increment.map(|e| self.ctx.bump.alloc_value_immutable(self.lower_expr(&e))),
                            body,
                        }
                    }
                    ir::ast::ForKind::RangeBased { variable, iterable } => {
                        // Convert for...in to C-style for loop
                        // for i in iterable => for (let i = 0; i < iterable.len(); i++)
                        // For now, just create a simple for loop
                        HirStmt::For {
                            init: None,
                            condition: None,
                            increment: None,
                            body,
                        }
                    }
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

            // Declarations should not appear as statements in function bodies
            // They are handled at module level, not statement level
            Stmt::FuncDecl(_) | Stmt::StructDecl(_) | Stmt::InterfaceDecl(_) | 
            Stmt::ImplDecl(_) | Stmt::EnumDecl(_) | Stmt::StateMachineDecl(_) | 
            Stmt::EffectDecl(_) => {
                panic!("Declaration statements should not appear in function bodies");
            }
            
            // Module-level statements that should be handled separately
            Stmt::Import(import) => {
                // Import statements are handled at module level for dependency tracking
                // Return a no-op statement here
                HirStmt::Block { body: &[] }
            }
            
            Stmt::Package(package) => {
                // Package statements are handled at module level for dependency tracking
                // Return a no-op statement here
                HirStmt::Block { body: &[] }
            }
            
            Stmt::Const(const_stmt) => {
                HirStmt::Const(self.ctx.bump.alloc_value_immutable(self.lower_const_stmt(*const_stmt)))
            }
            
            Stmt::UnsafeBlock(unsafe_block) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = unsafe_block
                    .block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_slice(&body_vec);
                HirStmt::UnsafeBlock { body: self.ctx.bump.alloc_value_immutable(HirStmt::Block { body }) }
            }
            
            Stmt::Defer(defer_stmt) => todo!()
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
        let final_type: HirType<'a, 'bump> = if l.type_annotation != Type::infer() {
            self.lower_type(&l.type_annotation)
        } else {
            self.infer_type(&value)
        };

        self.ctx
            .variable_types
            .borrow_mut()
            .insert(l.ident, final_type);
        
        HirStmt::Let {
            name: l.ident,
            ty: final_type,
            value,
        }
    }
}
