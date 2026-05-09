use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    Block, DeferAction, DeferStmt, ElseBranch, Expr, ForKind, ForStmt, IfStmt, ImportStmt,
    InternalExprStmt, LetStmt, MatchArm, MatchStmt, ModuleDecl, PackageStmt, Pattern, ReturnStmt,
    Stmt, Type, Visibility, WhileStmt,
};
use ir::errors::error::ParserError;
use ir::hir::StrId;
use ir::tokens::TokenKind;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_let_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.parse_let_stmt_inner(false)
    }

    pub fn parse_static_let_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.parse_let_stmt_inner(true)
    }

    fn parse_let_stmt_inner(
        &mut self,
        is_static: bool,
    ) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Let);

        let is_mut = self.cursor.consume(TokenKind::Mut);
        let (name, _span) = self.cursor.expect_ident()?;

        let type_annotation = if self.cursor.consume(TokenKind::Colon) {
            self.parse_type()?
        } else {
            Type::infer()
        };

        self.cursor.expect(TokenKind::Assign)?;
        let value = self.parse_expr(0)?;
        self.cursor.consume(TokenKind::Semicolon);

        let let_stmt = LetStmt {
            ident: name,
            type_annotation,
            value: self.bump.alloc_value_immutable(value),
            mutable: is_mut,
            is_static,
        };

        Ok(Stmt::Let(self.bump.alloc_value_immutable(let_stmt)))
    }

    pub fn parse_shorthand_let_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        let (name, _span) = self.cursor.expect_ident()?;
        self.cursor.expect(TokenKind::ColonAssign)?;
        let value = self.parse_expr(0)?;
        self.cursor.consume(TokenKind::Semicolon);

        let let_stmt = LetStmt {
            ident: name,
            type_annotation: Type::infer(),
            value: self.bump.alloc_value_immutable(value),
            mutable: false,
            is_static: false,
        };

        Ok(Stmt::Let(self.bump.alloc_value_immutable(let_stmt)))
    }

    pub fn parse_if_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::If);
        self.cursor.expect(TokenKind::LParen)?;
        let condition = self.parse_expr(0)?;
        self.cursor.expect(TokenKind::RParen)?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.cursor.consume(TokenKind::Else) {
            if self.cursor.peek() == TokenKind::If {
                let inner_if = self.parse_if_stmt()?;
                match inner_if {
                    Stmt::If(if_stmt) => {
                        Some(self.bump.alloc_value_immutable(ElseBranch::If(if_stmt)))
                    }
                    _ => unreachable!(),
                }
            } else {
                let else_block = self.parse_block()?;
                Some(self.bump.alloc_value_immutable(ElseBranch::Else(
                    self.bump.alloc_value_immutable(else_block),
                )))
            }
        } else {
            None
        };

        let if_stmt = IfStmt {
            condition: self.bump.alloc_value_immutable(condition),
            then_branch: self.bump.alloc_value_immutable(then_branch),
            else_branch,
        };

        Ok(Stmt::If(self.bump.alloc_value_immutable(if_stmt)))
    }

    pub fn parse_while_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::While);
        self.cursor.expect(TokenKind::LParen)?;
        let condition = self.parse_expr(0)?;
        self.cursor.expect(TokenKind::RParen)?;

        let block = self.parse_block()?;

        let while_stmt = WhileStmt {
            condition: self.bump.alloc_value_immutable(condition),
            block: self.bump.alloc_value_immutable(block),
        };

        Ok(Stmt::While(self.bump.alloc_value_immutable(while_stmt)))
    }

    pub fn parse_for_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::For);
        self.cursor.expect(TokenKind::LParen)?;

        let let_stmt = if self.cursor.peek() == TokenKind::Semicolon {
            None
        } else {
            self.cursor.consume(TokenKind::Let);
            let is_mut = self.cursor.consume(TokenKind::Mut);
            let (name, _span) = self.cursor.expect_ident()?;
            let type_annotation = if self.cursor.consume(TokenKind::Colon) {
                self.parse_type()?
            } else {
                Type::infer()
            };
            self.cursor.expect(TokenKind::Assign)?;
            let value = self.parse_expr(0)?;

            let let_stmt = LetStmt {
                ident: name,
                type_annotation,
                value: self.bump.alloc_value_immutable(value),
                mutable: is_mut,
                is_static: false,
            };
            Some(self.bump.alloc_value_immutable(let_stmt))
        };

        self.cursor.expect(TokenKind::Semicolon)?;

        let condition = if self.cursor.peek() == TokenKind::Semicolon {
            None
        } else {
            Some(self.bump.alloc_value_immutable(self.parse_expr(0)?))
        };

        self.cursor.expect(TokenKind::Semicolon)?;

        let increment = if self.cursor.peek() == TokenKind::RParen {
            None
        } else {
            Some(self.bump.alloc_value_immutable(self.parse_expr(0)?))
        };

        self.cursor.expect(TokenKind::RParen)?;

        let block = self.parse_block()?;

        let for_stmt = ForStmt {
            kind: ForKind::CStyle {
                let_stmt,
                condition,
                increment,
            },
            block: self.bump.alloc_value_immutable(block),
        };

        Ok(Stmt::For(self.bump.alloc_value_immutable(for_stmt)))
    }

    pub fn parse_return_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Return);
        let value = if self.cursor.peek() != TokenKind::Semicolon {
            Some(self.bump.alloc_value_immutable(self.parse_expr(0)?))
        } else {
            None
        };
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::Return(
            self.bump.alloc_value_immutable(ReturnStmt { value }),
        ))
    }

    pub fn parse_expr_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        let expr = self.parse_expr(0)?;
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::ExprStmt(self.bump.alloc_value_immutable(
            InternalExprStmt {
                expr: self.bump.alloc_value_immutable(expr),
            },
        )))
    }

    pub fn parse_defer_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Defer);

        let action = if self.cursor.peek() == TokenKind::LBrace {
            // Block form: defer { ... }
            let block = self.parse_block()?;
            DeferAction::Block(self.bump.alloc_value_immutable(block))
        } else {
            // Inline form: defer expr_stmt;
            let stmt = self.parse_expr_stmt()?;
            DeferAction::Stmt(self.bump.alloc_value_immutable(stmt))
        };

        let defer_stmt = DeferStmt { action };
        Ok(Stmt::Defer(self.bump.alloc_value_immutable(defer_stmt)))
    }

    pub fn parse_match_stmt(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Match);

        // Parse subject without allowing struct-init syntax (to avoid ambiguity with `{`)
        let expr = Self::parse_expr_inner(&mut self.cursor, self.bump, 0, false)?;

        self.cursor.expect(TokenKind::LBrace)?;

        let mut arms = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            // Each arm: `case pattern [if guard] -> { block } | single_stmt`
            self.cursor.expect(TokenKind::Case)?;

            let pattern = self.parse_pattern()?;

            // Optional guard: `if condition`
            let guard = if self.cursor.consume(TokenKind::If) {
                let guard_expr = self.parse_expr(0)?;
                Some(self.bump.alloc_value_immutable(guard_expr))
            } else {
                None
            };

            self.cursor.expect(TokenKind::Arrow)?;

            // Arm body: block `{ }` or a single statement
            let block = if self.cursor.peek() == TokenKind::LBrace {
                self.parse_block()?
            } else {
                // Single expression/statement: wrap in a synthetic block
                let stmt = self.parse_expr_stmt()?;
                let stmt_ref = self.bump.alloc_value_immutable(stmt);
                Block {
                    block: self.bump.alloc_slice_copy(&[*stmt_ref]),
                }
            };

            arms.push(MatchArm {
                pattern,
                guard,
                block: self.bump.alloc_value_immutable(block),
            });

            // Optional trailing comma between arms
            self.cursor.consume(TokenKind::Comma);
        }

        self.cursor.expect(TokenKind::RBrace)?;

        let match_stmt = MatchStmt {
            expr: self.bump.alloc_value_immutable(expr),
            arms: self.bump.alloc_slice_copy(&arms),
        };

        Ok(Stmt::Match(self.bump.alloc_value_immutable(match_stmt)))
    }

    fn parse_pattern(&mut self) -> Result<Pattern<'bump>, ParserError<'a>> {
        match self.cursor.peek() {
            // Wildcard: _
            TokenKind::Underscore => {
                self.cursor.advance();
                Ok(Pattern::Wildcard)
            }

            // Number literal
            TokenKind::Number => {
                let tok = self.cursor.bump();
                let text = tok.text.unwrap_or_default();
                let n = text.as_str().parse::<i64>().unwrap_or(0);
                Ok(Pattern::Number(n))
            }

            // String literal
            TokenKind::String => {
                let tok = self.cursor.bump();
                let s = tok.text.unwrap_or_default();
                Ok(Pattern::String(s))
            }

            // Boolean
            TokenKind::BooleanTrue => {
                self.cursor.advance();
                Ok(Pattern::Boolean(true))
            }
            TokenKind::BooleanFalse => {
                self.cursor.advance();
                Ok(Pattern::Boolean(false))
            }

            // Identifier or EnumVariant
            TokenKind::Ident => {
                let tok = self.cursor.bump();
                let name = tok.text.unwrap_or_default();

                // EnumVariant: Name(binding, ...)
                if self.cursor.peek() == TokenKind::LParen {
                    self.cursor.advance(); // consume '('
                    let mut bindings = Vec::new_in(self.bump);
                    while self.cursor.peek() != TokenKind::RParen
                        && self.cursor.peek() != TokenKind::EOF
                    {
                        let sub = self.parse_pattern()?;
                        bindings.push(sub);
                        if !self.cursor.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.cursor.expect(TokenKind::RParen)?;
                    Ok(Pattern::EnumVariant {
                        name,
                        bindings: self.bump.alloc_slice_copy(&bindings),
                    })
                } else {
                    // Simple identifier binding
                    Ok(Pattern::Ident(name))
                }
            }

            // Tuple pattern: (a, b, c)
            TokenKind::LParen => {
                self.cursor.advance();
                let mut pats = Vec::new_in(self.bump);
                while self.cursor.peek() != TokenKind::RParen
                    && self.cursor.peek() != TokenKind::EOF
                {
                    pats.push(self.parse_pattern()?);
                    if !self.cursor.consume(TokenKind::Comma) {
                        break;
                    }
                }
                self.cursor.expect(TokenKind::RParen)?;
                Ok(Pattern::Tuple(self.bump.alloc_slice_copy(&pats)))
            }

            // Array pattern: [a, b, c]
            TokenKind::LBracket => {
                self.cursor.advance();
                let mut pats = Vec::new_in(self.bump);
                while self.cursor.peek() != TokenKind::RBracket
                    && self.cursor.peek() != TokenKind::EOF
                {
                    pats.push(self.parse_pattern()?);
                    if !self.cursor.consume(TokenKind::Comma) {
                        break;
                    }
                }
                self.cursor.expect(TokenKind::RBracket)?;
                Ok(Pattern::Array(self.bump.alloc_slice_copy(&pats)))
            }

            other => {
                let tok = self.cursor.peek_token();
                Err(ir::errors::error::ParserError::UnexpectedToken {
                    expected: TokenKind::Ident,
                    found: other,
                    span: tok.span,
                })
            }
        }
    }

    /// Parse `import foo.bar.Baz;`
    pub fn parse_import(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Import);
        let path = self.parse_path()?;
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::Import(
            self.bump.alloc_value_immutable(ImportStmt { path }),
        ))
    }

    /// Parse `package com.example.myapp;`
    pub fn parse_package(&mut self) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Package);
        let path = self.parse_path()?;
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::Package(
            self.bump.alloc_value_immutable(PackageStmt { path }),
        ))
    }

    /// Parse a dot-separated identifier path: `foo`, `foo.bar`, `foo.bar.Baz`
    fn parse_path(&mut self) -> Result<&'bump ir::ast::Path<'bump>, ParserError<'a>> {
        let mut segments = Vec::new_in(self.bump);
        let (first, _) = self.cursor.expect_ident()?;
        segments.push(first);
        // Consume `.ident` pairs as long as they follow
        while self.cursor.peek() == TokenKind::Dot
            && matches!(self.cursor.peek_n(1), TokenKind::Ident)
        {
            self.cursor.advance(); // consume '.'
            let (seg, _) = self.cursor.expect_ident()?;
            segments.push(seg);
        }
        Ok(self.bump.alloc_value_immutable(ir::ast::Path {
            path: self.bump.alloc_slice_copy(&segments),
        }))
    }

    /// Parse `[visibility] module Name { decl* }`
    ///
    /// The `module` keyword must already have been consumed by the caller.
    pub fn parse_module_decl(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        let (name, _) = self.cursor.expect_ident()?;
        self.cursor.expect(TokenKind::LBrace)?;

        let mut body = Vec::new_in(self.bump);
        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            body.push(self.parse_stmt(Visibility::Public)?);
        }
        self.cursor.expect(TokenKind::RBrace)?;

        Ok(Stmt::Module(self.bump.alloc_value_immutable(ModuleDecl {
            name,
            visibility,
            body: self.bump.alloc_slice_copy(&body),
        })))
    }

    pub fn parse_block(&mut self) -> Result<Block<'a, 'bump>, ParserError<'a>> {
        self.cursor.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new_in(self.bump);
        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            stmts.push(self.parse_stmt(ir::ast::Visibility::Public)?);
        }

        self.cursor.expect(TokenKind::RBrace)?;

        Ok(Block {
            block: self.bump.alloc_slice_copy(&stmts),
        })
    }
}
