use super::context::HirLowerer;
use ir::ast::{self, Block, ElseBranch, ErrorHandlerPattern, LetStmt, MatchArm, Stmt, Type};
use ir::hir::{
    HirErrorHandlerBranch, HirErrorHandlerPattern, HirMatchArm, HirStmt, HirType, StrId,
};

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub(super) fn lower_stmt(&self, stmt: Stmt<'a, 'bump>) -> HirStmt<'a, 'bump> {
        match stmt {
            Stmt::Let(l) => self.lower_let_stmt(l),
            Stmt::Return(r) => HirStmt::Return(
                r.value
                    .map(|v| self.ctx.bump.alloc_value_immutable(self.lower_expr(v))),
            ),
            Stmt::ExprStmt(e) => {
                HirStmt::Expr(self.ctx.bump.alloc_value_immutable(self.lower_expr(e.expr)))
            }
            Stmt::If(i) => self.lower_if_stmt(*i),

            Stmt::While(while_stmt) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = while_stmt
                    .block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_value_immutable(HirStmt::Block {
                    body: self.ctx.bump.alloc_slice(&body_vec),
                });
                HirStmt::While {
                    cond: self
                        .ctx
                        .bump
                        .alloc_value_immutable(self.lower_expr(&while_stmt.condition)),
                    body,
                }
            }

            Stmt::For(for_stmt) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = for_stmt
                    .block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_value_immutable(HirStmt::Block {
                    body: self.ctx.bump.alloc_slice(&body_vec),
                });

                match &for_stmt.kind {
                    ir::ast::ForKind::CStyle {
                        let_stmt,
                        condition,
                        increment,
                    } => HirStmt::For {
                        init: let_stmt.map(|s| {
                            let stmt = self.lower_stmt(Stmt::Let(s));
                            self.ctx.bump.alloc_value_immutable(stmt)
                        }),
                        condition: condition
                            .map(|e| self.ctx.bump.alloc_value_immutable(self.lower_expr(&e))),
                        increment: increment
                            .map(|e| self.ctx.bump.alloc_value_immutable(self.lower_expr(&e))),
                        body,
                    },
                    ast::ForKind::RangeBased {
                        variable: _,
                        iterable: _,
                    } => {
                        // `iterable` MUST implement Iterable
                        // Convert for...in to `iter := elements.iter(); while (element := iter.next()) {}`
                        // For now, just create a simple for loop
                        // TODO: implement properly
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
                let arms_vec: Vec<HirMatchArm<'a, 'bump>> =
                    <&[MatchArm]>::into_iter(match_stmt.arms)
                        .map(|a| {
                            let body_vec: Vec<HirStmt<'a, 'bump>> =
                                a.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                            let body = self.ctx.bump.alloc_value_immutable(HirStmt::Block {
                                body: self.ctx.bump.alloc_slice(&body_vec),
                            });
                            HirMatchArm {
                                pattern: self.lower_pattern(&a.pattern),
                                guard: a.guard.map(|guard| {
                                    self.ctx.bump.alloc_value_immutable(self.lower_expr(guard))
                                }),
                                body,
                            }
                        })
                        .collect();
                let arms = self.ctx.bump.alloc_slice(&arms_vec);

                HirStmt::Match {
                    expr: self
                        .ctx
                        .bump
                        .alloc_value_immutable(self.lower_expr(&match_stmt.expr)),
                    arms,
                }
            }

            Stmt::Break(expr, span) => HirStmt::Break(
                expr.map(|expr| self.ctx.bump.alloc_value_immutable(self.lower_expr(expr))),
                span,
            ),
            Stmt::Continue(span) => HirStmt::Continue(span),
            Stmt::Block(block) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> =
                    block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                let body = self.ctx.bump.alloc_slice(&body_vec);
                HirStmt::Block { body }
            }

            Stmt::FuncDecl(_)
            | Stmt::StructDecl(_)
            | Stmt::InterfaceDecl(_)
            | Stmt::ImplDecl(_)
            | Stmt::EnumDecl(_)
            | Stmt::StateMachineDecl(_) => {
                panic!("Declaration statements should not appear in function bodies");
            }

            Stmt::Import(_import_stmt) => {
                // Import statements are handled at module level for dependency tracking
                // Return a no-op statement here
                HirStmt::Block { body: &[] }
            }

            Stmt::Package(_package_stmt) => {
                // Package statements are handled at module level for dependency tracking
                // Return a no-op statement here
                HirStmt::Block { body: &[] }
            }

            Stmt::Const(const_stmt) => HirStmt::Const(
                self.ctx
                    .bump
                    .alloc_value_immutable(self.lower_const_stmt(*const_stmt)),
            ),

            Stmt::UnsafeBlock(unsafe_block) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = unsafe_block
                    .block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_slice(&body_vec);
                HirStmt::UnsafeBlock {
                    body: self.ctx.bump.alloc_value_immutable(HirStmt::Block { body }),
                }
            }

            Stmt::Defer(defer_stmt) => {
                use ir::ast::DeferAction;
                let hir_body = match &defer_stmt.action {
                    DeferAction::Block(block) => {
                        let body_vec: Vec<HirStmt<'a, 'bump>> =
                            block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                        let body = self.ctx.bump.alloc_slice(&body_vec);
                        HirStmt::Block { body }
                    }
                    DeferAction::Stmt(stmt) => self.lower_stmt(**stmt),
                };
                HirStmt::Defer(self.ctx.bump.alloc_value_immutable(hir_body))
            }

            // A module declaration inside a function body is lowered as a block
            // of its contained statements (inline namespace / scope grouping).
            Stmt::Module(module_decl) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> = module_decl
                    .body
                    .iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
                let body = self.ctx.bump.alloc_slice(&body_vec);
                HirStmt::Block { body }
            }
        }
    }

    fn lower_if_stmt(&self, i: ast::IfStmt<'a, 'bump>) -> HirStmt<'a, 'bump> {
        let cond = self.lower_expr(&i.condition);
        let then_vec: Vec<HirStmt<'a, 'bump>> = i
            .then_branch
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
                let body_vec: Vec<HirStmt<'a, 'bump>> = else_block
                    .into_iter()
                    .map(|s| self.lower_stmt(*s))
                    .collect();
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

        let catch_pattern = l.catch_pattern.as_ref().map(|pat| {
            // `catch` is only legal when the value expression's callee declares `throws`.
            /*
            let declared_throws = self.ctx.throws_of(&l.value);
            if declared_throws.is_empty() {
                self.ctx
                    .diag
                    .borrow_mut()
                    .record_error("`catch` used on an expression that cannot throw", l.span);
            }
            */

            let lower_branch = |error_type: &Type<'a, 'bump>,
                                binding: Option<StrId>,
                                body: &'bump Block<'a, 'bump>| {
                let hir_error_type = self.lower_type(error_type);

                // Every named catch branch must correspond to a type the callee actually throws.
                /*
                if !declared_throws.iter().any(|t| *t == hir_error_type) {
                    self.ctx.diag.borrow_mut().record_error(
                        format!(
                            "`catch` branch handles `{}`, which this expression never throws",
                            hir_error_type
                        ),
                        l.span,
                    );
                }
                */

                if let Some(b) = binding {
                    self.ctx
                        .variable_types
                        .borrow_mut()
                        .insert(b, hir_error_type);
                }

                let body_stmts = self.lower_block(body);
                (hir_error_type, body_stmts)
            };

            match pat {
                ErrorHandlerPattern::Single {
                    error_type,
                    binding,
                    body,
                } => {
                    let (hir_error_type, body_stmts) = lower_branch(error_type, *binding, body);
                    let HirStmt::Block { body } = body_stmts else {
                        unreachable!()
                    };
                    HirErrorHandlerPattern::Single {
                        error_type: hir_error_type,
                        binding: *binding,
                        body,
                    }
                }
                ErrorHandlerPattern::Multiple { branches } => {
                    let lowered: Vec<_> = branches
                        .iter()
                        .map(|b| {
                            let (hir_error_type, body_stmts) =
                                lower_branch(&b.error_type, b.binding, b.body);
                            let HirStmt::Block { body } = body_stmts else {
                                unreachable!()
                            };
                            HirErrorHandlerBranch {
                                error_type: hir_error_type,
                                binding: b.binding,
                                body,
                            }
                        })
                        .collect();

                    /*
                    for thrown in &declared_throws {
                        if !lowered.iter().any(|br| br.error_type == *thrown) {
                            self.ctx.diag.borrow_mut().record_error(
                                format!("non-exhaustive `catch`: missing branch for `{}`", thrown),
                                l.span,
                            );
                        }
                    }
                    */

                    HirErrorHandlerPattern::Multiple {
                        branches: self.ctx.bump.alloc_slice(&lowered),
                    }
                }
            }
        });

        let else_block = l.else_block.map(|block| {
            // `? else` is only legal when final_type (or the catch-unwrapped type) is nullable.
            /*
            if final_type.inner_if_nullable().is_none() {
                self.ctx
                    .diag
                    .borrow_mut()
                    .record_error("`? else` used on a non-nullable type", l.span);
            }
            */
            self.ctx.bump.alloc_value_immutable(self.lower_block(block))
        });

        self.ctx
            .variable_types
            .borrow_mut()
            .insert(l.ident, final_type);

        HirStmt::Let {
            name: l.ident,
            ty: final_type,
            value,
            mutable: l.mutable,
            is_static: l.is_static,
            catch_pattern,
            else_block,
        }
    }
}
