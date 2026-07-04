use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    Block, DeferAction, DeferStmt, ElseBranch, Expr, ForKind, ForStmt, IfStmt, ImportStmt,
    InternalExprStmt, LetStmt, MatchArm, MatchStmt, ModuleDecl, PackageStmt, Pattern, ReturnStmt,
    Stmt, Type, Visibility, WhileStmt,
};
use ir::errors::error::{DiagnosticError, ParseErrorKind};
use ir::tokens::TokenKind;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_let_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        self.parse_let_stmt_inner(false)
    }

    pub fn parse_static_let_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        self.parse_let_stmt_inner(true)
    }

    fn parse_let_stmt_inner(
        &mut self,
        is_static: bool,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Let)?;

        let is_mut = self.cursor.consume(TokenKind::Mut);
        let (name, _span) = self.cursor.expect_ident()?;

        let type_annotation = if self.cursor.consume(TokenKind::Colon) {
            self.parse_type()?
        } else {
            self.diag.record(DiagnosticError::new(
                ParseErrorKind::ExpectedTypeAnnotation,
                self.cursor.peek_token().span,
            ));
            let kind = self.diag.synchronize(&mut self.cursor);
            if kind == TokenKind::EOF {
                return Err(DiagnosticError::new(
                    ParseErrorKind::UnexpectedEOF { expected: None },
                    self.cursor.peek_token().span,
                ));
            }

            Type::infer()
        };

        self.cursor.expect(TokenKind::Assign)?;
        let value = match self.parse_expr(0) {
            Ok(value) => value,
            Err(e) => {
                self.diag.record(e);
                let kind = self.diag.synchronize(&mut self.cursor);
                return if kind == TokenKind::EOF {
                    Err(DiagnosticError::new(
                        ParseErrorKind::UnexpectedEOF { expected: None },
                        self.cursor.peek_token().span,
                    ))
                } else {
                    Err(DiagnosticError::new(
                        ParseErrorKind::InvalidExpression { found: kind },
                        self.cursor.peek_token().span,
                    ))
                };
            }
        };

        let catch_pattern = if self.cursor.consume(TokenKind::Catch) {
            Some(self.parse_catch_pattern()?)
        } else {
            None
        };

        let else_block = if self.cursor.consume(TokenKind::Question) {
            self.cursor.expect(TokenKind::Else)?;
            let block = self.parse_block()?;
            Some(self.bump.alloc_value_immutable(block))
        } else {
            None
        };

        self.cursor.consume(TokenKind::Semicolon);

        let let_stmt = LetStmt {
            ident: name,
            type_annotation,
            value: self.bump.alloc_value_immutable(value),
            mutable: is_mut,
            is_static,
            catch_pattern,
            else_block,
            span: token.span,
        };

        Ok(Stmt::Let(self.bump.alloc_value_immutable(let_stmt)))
    }

    pub fn parse_shorthand_let_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let mutable = self.cursor.consume(TokenKind::Mut);
        let (name, span) = self.cursor.expect_ident()?;
        self.cursor.expect(TokenKind::ColonAssign)?;
        let value = self.parse_expr(0)?;

        let catch_pattern = if self.cursor.consume(TokenKind::Catch) {
            Some(self.parse_catch_pattern()?)
        } else {
            None
        };

        let else_block = if self.cursor.consume(TokenKind::Question) {
            self.cursor.expect(TokenKind::Else)?;
            let block = self.parse_block()?;
            Some(self.bump.alloc_value_immutable(block))
        } else {
            None
        };

        self.cursor.consume(TokenKind::Semicolon);

        let let_stmt = LetStmt {
            ident: name,
            type_annotation: Type::infer(),
            value: self.bump.alloc_value_immutable(value),
            mutable,
            is_static: false,
            catch_pattern,
            else_block,
            span,
        };

        Ok(Stmt::Let(self.bump.alloc_value_immutable(let_stmt)))
    }

    pub fn parse_if_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::If)?;
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
            span: token.span,
        };

        Ok(Stmt::If(self.bump.alloc_value_immutable(if_stmt)))
    }

    pub fn parse_break_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let break_token = self.cursor.expect(TokenKind::Break)?;
        match self.cursor.peek() {
            TokenKind::Semicolon => {
                self.cursor.consume(TokenKind::Semicolon);
                Ok(Stmt::Break(None, break_token.span))
            }
            _ => match self.parse_expr(0) {
                Ok(expr) => {
                    self.cursor.consume(TokenKind::Semicolon);
                    Ok(Stmt::Break(
                        Some(self.bump.alloc_value_immutable(expr)),
                        break_token.span,
                    ))
                }
                Err(error) => {
                    self.diag.record(error);
                    let kind = self.diag.synchronize(&mut self.cursor);
                    if kind == TokenKind::EOF {
                        Err(DiagnosticError::unexpected_eof(
                            Some(TokenKind::Semicolon),
                            self.cursor.peek_token().span,
                        ))
                    } else {
                        Ok(Stmt::Break(None, break_token.span))
                    }
                }
            },
        }
    }

    pub fn parse_continue_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Continue)?;
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::Continue(token.span))
    }

    pub fn parse_while_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::While)?;
        self.cursor.expect(TokenKind::LParen)?;
        let condition = self.parse_expr(0)?;
        self.cursor.expect(TokenKind::RParen)?;

        let block = self.parse_block()?;

        let while_stmt = WhileStmt {
            condition: self.bump.alloc_value_immutable(condition),
            block: self.bump.alloc_value_immutable(block),
            span: token.span,
        };

        Ok(Stmt::While(self.bump.alloc_value_immutable(while_stmt)))
    }

    pub fn parse_for_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::For)?;
        self.cursor.expect(TokenKind::LParen)?;

        let let_stmt = if self.cursor.peek() == TokenKind::Semicolon {
            None
        } else {
            // parse_let_stmt() automatically consumes Semicolon
            let Stmt::Let(let_stmt) = self.parse_let_stmt()? else {
                unreachable!()
            };
            Some(let_stmt)
        };

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
            span: token.span,
        };

        Ok(Stmt::For(self.bump.alloc_value_immutable(for_stmt)))
    }

    pub fn parse_return_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Return)?;
        let value = if self.cursor.peek() != TokenKind::Semicolon {
            Some(self.bump.alloc_value_immutable(self.parse_expr(0)?))
        } else {
            None
        };
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::Return(self.bump.alloc_value_immutable(ReturnStmt {
            value,
            span: token.span,
        })))
    }

    pub fn parse_expr_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.peek_token();
        let expr = self.parse_expr(0)?;
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::ExprStmt(self.bump.alloc_value_immutable(
            InternalExprStmt {
                expr: self.bump.alloc_value_immutable(expr),
                span: token.span,
            },
        )))
    }

    pub fn parse_defer_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Defer)?;

        let action = if self.cursor.peek() == TokenKind::LBrace {
            // Block form: defer { ... }
            let block = self.parse_block()?;
            DeferAction::Block(self.bump.alloc_value_immutable(block))
        } else {
            // Inline form: defer expr_stmt;
            let stmt = self.parse_expr_stmt()?;
            DeferAction::Stmt(self.bump.alloc_value_immutable(stmt))
        };

        let defer_stmt = DeferStmt {
            action,
            span: token.span,
        };
        Ok(Stmt::Defer(self.bump.alloc_value_immutable(defer_stmt)))
    }

    pub fn parse_match_stmt(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Match)?;

        // Parse subject without allowing struct-init syntax (to avoid ambiguity with `{`)
        let expr: Expr<'a, 'bump> = self.parse_expr_inner(0, false)?;

        self.cursor.expect(TokenKind::LBrace)?;

        let mut arms = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            // Each arm: `case pattern [if guard] -> { block } | single_stmt`
            let case_token = self.cursor.expect(TokenKind::Case)?;

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
                let span = self.cursor.peek_token().span;
                let stmt = self.parse_expr_stmt()?;
                let stmt_ref = self.bump.alloc_value_immutable(stmt);
                Block {
                    block: self.bump.alloc_slice_copy(&[*stmt_ref]),
                    span,
                }
            };

            arms.push(MatchArm {
                pattern,
                guard,
                block: self.bump.alloc_value_immutable(block),
                span: case_token.span,
            });

            self.cursor.consume(TokenKind::Comma);
        }

        self.cursor.expect(TokenKind::RBrace)?;

        let match_stmt = MatchStmt {
            expr: self.bump.alloc_value_immutable(expr),
            arms: self.bump.alloc_slice_copy(&arms),
            span: token.span,
        };

        Ok(Stmt::Match(self.bump.alloc_value_immutable(match_stmt)))
    }

    fn parse_pattern(&mut self) -> Result<Pattern<'bump>, DiagnosticError<'a>> {
        match self.cursor.peek() {
            TokenKind::Underscore => {
                self.cursor.advance();
                Ok(Pattern::Wildcard)
            }

            TokenKind::Number => {
                let tok = self.cursor.bump();
                let text = tok.text.unwrap_or_default();
                let n = text.as_str().parse::<i64>().unwrap_or(0);
                Ok(Pattern::Number(n))
            }

            TokenKind::String => {
                let tok = self.cursor.bump();
                let s = tok.text.unwrap_or_default();
                Ok(Pattern::String(s))
            }

            TokenKind::BooleanTrue => {
                self.cursor.advance();
                Ok(Pattern::Boolean(true))
            }
            TokenKind::BooleanFalse => {
                self.cursor.advance();
                Ok(Pattern::Boolean(false))
            }

            TokenKind::Ident => {
                let tok = self.cursor.bump();
                let name = tok.text.unwrap_or_default();

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
                Err(DiagnosticError::new(
                    ParseErrorKind::UnexpectedToken {
                        expected: TokenKind::Ident,
                        found: other,
                    },
                    tok.span,
                ))
            }
        }
    }

    /// Parse `import foo::bar.Baz;`
    pub fn parse_import(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Import)?;
        let path = self.parse_path()?;
        self.cursor.consume(TokenKind::Semicolon);
        Ok(Stmt::Import(self.bump.alloc_value_immutable(ImportStmt {
            path,
            span: token.span,
        })))
    }

    /// Parse `package com::example::myapp;`
    pub fn parse_package(&mut self) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Package)?;
        let path = self.parse_path()?;

        if path.member.is_some() {
            return Err(DiagnosticError::new(
                ParseErrorKind::PackageStmtCannotImport,
                path.span,
            ));
        }

        self.cursor.consume(TokenKind::Semicolon);

        Ok(Stmt::Package(self.bump.alloc_value_immutable(
            PackageStmt {
                path,
                span: token.span,
            },
        )))
    }

    /// Parse a dot-separated identifier path: `foo`, `foo::bar`, `foo::bar::Baz`
    fn parse_path(&mut self) -> Result<&'bump ir::ast::Path<'a, 'bump>, DiagnosticError<'a>> {
        let mut segments = Vec::new_in(self.bump);

        let (first, span) = self.cursor.expect_ident()?;
        segments.push(first);

        // handle `::ident`
        while self.cursor.peek() == TokenKind::ColonColon
            && matches!(self.cursor.peek_n(1), TokenKind::Ident)
        {
            self.cursor.advance(); // ::
            let (seg, _) = self.cursor.expect_ident()?;
            segments.push(seg);
        }

        let member = if self.cursor.peek() == TokenKind::Dot
            && matches!(self.cursor.peek_n(1), TokenKind::Ident)
        {
            self.cursor.advance(); // .
            let (m, _) = self.cursor.expect_ident()?;
            Some(m)
        } else {
            None
        };

        Ok(self.bump.alloc_value_immutable(ir::ast::Path {
            path: self.bump.alloc_slice_copy(&segments),
            member,
            span,
        }))
    }

    /// Parse `[visibility] module Name { decl* }`
    ///
    /// The `module` keyword must already have been consumed by the caller.
    pub fn parse_module_decl(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let (name, span) = self.cursor.expect_ident()?;
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
            span,
        })))
    }

    pub fn parse_block(&mut self) -> Result<Block<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new_in(self.bump);
        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            match self.parse_stmt(Visibility::Public) {
                Ok(s) => stmts.push(s),
                Err(e) => {
                    self.diag.record(e);
                    let stop = self.diag.synchronize(&mut self.cursor);
                    // If synchronize landed on `}` or EOF, stop looping
                    // DON'T consume the `}` here, let expect() below do it.
                    if stop == TokenKind::RBrace || stop == TokenKind::EOF {
                        break;
                    }
                }
            }
        }

        self.cursor.expect(TokenKind::RBrace)?;

        Ok(Block {
            block: self.bump.alloc_slice_copy(&stmts),
            span: token.span,
        })
    }
}
