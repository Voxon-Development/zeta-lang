use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    ConstStmt, EnumDecl, EnumVariant, Field, ImplDecl, InterfaceDecl, Param, PermitsExpr, Stmt,
    StructDecl, Visibility,
};
use ir::errors::error::DiagnosticError;
use ir::hir::StrId;
use ir::tokens::TokenKind;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    /// Parse a struct declaration
    /// ```rs
    /// pub struct Point<T> {
    ///     x: i32,
    ///     y: i32,
    /// }
    /// ```
    pub fn parse_struct_decl(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Struct)?;

        let (name, _span) = self.cursor.expect_ident()?;

        // Parse optional generics: <T, U: Display>
        let generics = if self.cursor.peek() == TokenKind::Lt {
            self.parse_generics()?
        } else {
            None
        };

        let params = if self.cursor.consume(TokenKind::Semicolon) {
            None
        } else if self.cursor.peek() == TokenKind::LParen {
            // Tuple struct: `struct Foo(i32, i32);`
            let tuple_fields = self.parse_tuple_struct_fields()?;
            self.cursor.expect(TokenKind::Semicolon)?;
            Some(tuple_fields)
        } else {
            self.cursor.expect(TokenKind::LBrace)?;

            let mut fields = Vec::new_in(self.bump);

            while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
                let field_vis = if self.cursor.peek() == TokenKind::Private
                    || self.cursor.peek() == TokenKind::Module
                    || self.cursor.peek() == TokenKind::Package
                {
                    let vis = token_to_visibility(self.cursor.peek());
                    self.cursor.advance();
                    vis
                } else {
                    Visibility::Public
                };

                let (field_name, field_span) = self.cursor.expect_ident()?;
                self.cursor.expect(TokenKind::Colon)?;
                let field_type = self.parse_type()?;

                fields.push(Field {
                    name: field_name,
                    field_type,
                    visibility: field_vis,
                    generics: None,
                    span: field_span,
                });

                self.cursor.consume(TokenKind::Comma);
            }

            self.cursor.expect(TokenKind::RBrace)?;

            let fields_as_params: Vec<Param<'a, 'bump>> = fields
                .into_iter()
                .map(|f| {
                    Param::Normal(self.bump.alloc_value_immutable(ir::ast::NormalParam {
                        is_mut: false,
                        name: f.name,
                        type_annotation: f.field_type,
                        visibility: f.visibility,
                        default_value: None,
                        span: f.span,
                    }))
                })
                .collect();

            if fields_as_params.is_empty() {
                None
            } else {
                Some(self.bump.alloc_slice_copy(&fields_as_params))
            }
        };

        let struct_decl = StructDecl {
            visibility,
            name,
            generics,
            params,
            span: token.span,
        };

        Ok(ir::ast::Stmt::StructDecl(
            self.bump.alloc_value_immutable(struct_decl),
        ))
    }

    /// Parse an impl block
    /// ```rs
    /// impl Point {
    ///     fn new(x: i32, y: i32) -> Point {
    ///         return Point { x, y };
    ///     }
    /// }
    ///
    /// impl Drawable by Point {
    ///     fn draw(&this) { ... }
    /// }
    /// ```
    pub fn parse_impl_decl(
        &mut self,
        _visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Impl)?;

        // Check for generic impl: impl<T> ...
        let generics = if self.cursor.peek() == TokenKind::Lt {
            self.parse_generics()?
        } else {
            None
        };

        let (target, _span) = self.cursor.expect_ident()?;

        let interface = if self.cursor.peek() == TokenKind::By {
            self.cursor.advance();
            let (trait_name, _) = self.cursor.expect_ident()?;
            Some(trait_name)
        } else {
            None
        };

        self.cursor.expect(TokenKind::LBrace)?;

        let mut methods = Vec::new_in(self.bump);
        let mut constants = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            if self.cursor.peek() == TokenKind::Const {
                let const_stmt = self.parse_const_stmt()?;
                constants.push(const_stmt);
                continue;
            }

            let method_vis = if self.cursor.peek() == TokenKind::Private
                || self.cursor.peek() == TokenKind::Module
                || self.cursor.peek() == TokenKind::Package
            {
                let vis = token_to_visibility(self.cursor.peek());
                self.cursor.advance();
                vis
            } else {
                Visibility::Public
            };

            match self.parse_function_with_visibility(method_vis) {
                Ok(func) => {
                    if let ir::ast::Stmt::FuncDecl(func) = func {
                        methods.push(*func);
                    }
                }
                Err(e) => {
                    self.diag.record(e);
                    let stop = self.diag.synchronize(&mut self.cursor);
                    if stop == TokenKind::RBrace || stop == TokenKind::EOF {
                        break;
                    }
                }
            }
        }

        self.cursor.expect(TokenKind::RBrace)?;

        let impl_decl = ImplDecl {
            generics,
            interface,
            target,
            methods: if methods.is_empty() {
                None
            } else {
                Some(self.bump.alloc_slice_copy(&methods))
            },
            constants: if constants.is_empty() {
                None
            } else {
                Some(self.bump.alloc_slice_copy(&constants))
            },
            span: token.span,
        };

        Ok(ir::ast::Stmt::ImplDecl(
            self.bump.alloc_value_immutable(impl_decl),
        ))
    }

    fn parse_tuple_struct_fields(
        &mut self,
    ) -> Result<&'bump [Param<'a, 'bump>], DiagnosticError<'a>> {
        self.cursor.expect(TokenKind::LParen)?;

        let mut fields = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RParen && self.cursor.peek() != TokenKind::EOF {
            let field_vis = if self.cursor.peek() == TokenKind::Private
                || self.cursor.peek() == TokenKind::Module
                || self.cursor.peek() == TokenKind::Package
            {
                let vis = token_to_visibility(self.cursor.peek());
                self.cursor.advance();
                vis
            } else {
                Visibility::Public
            };

            let peeked_token = self.cursor.peek_token();
            let field_type = self.parse_type()?;

            fields.push(Param::Normal(
                self.bump.alloc_value_immutable(ir::ast::NormalParam {
                    is_mut: false,
                    name: StrId(
                        self.string_pool
                            .intern(format!("anonymous_tuple_field_{}", field_type).as_str()),
                    ),
                    type_annotation: field_type,
                    visibility: field_vis,
                    default_value: None,
                    span: peeked_token.span,
                }),
            ));

            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }

        self.cursor.expect(TokenKind::RParen)?;

        Ok(self.bump.alloc_slice_copy(&fields))
    }

    fn parse_const_stmt(&mut self) -> Result<ConstStmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Const)?;
        let (name, _span) = self.cursor.expect_ident()?;

        let type_annotation = if self.cursor.consume(TokenKind::Colon) {
            self.parse_type()?
        } else {
            ir::ast::Type::infer()
        };

        self.cursor.expect(TokenKind::Assign)?;
        let value = self.parse_expr(0)?;
        self.cursor.consume(TokenKind::Semicolon);

        Ok(ConstStmt {
            ident: name,
            type_annotation,
            value: self.bump.alloc_value_immutable(value),
            span: token.span,
        })
    }

    /// Parse an interface declaration (like Rust trait)
    /// ```ignore
    /// interface Drawable {
    ///     fn draw(&this);
    /// }
    ///
    /// sealed interface SealedTrait permits A, B, C {
    ///     fn method(&this);
    /// }
    /// ```
    pub fn parse_interface_decl(
        &mut self,
        visibility: Visibility,
        is_sealed: bool,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Interface)?;

        let (name, _span) = self.cursor.expect_ident()?;

        // Parse optional generics: <T, U: Display>
        let generics = if self.cursor.peek() == TokenKind::Lt {
            self.parse_generics()?
        } else {
            None
        };

        // Parse optional permits clause for sealed interfaces
        let permits = if self.cursor.peek() == TokenKind::Permits {
            Some(self.parse_permits_expr()?)
        } else {
            None
        };

        self.cursor.expect(TokenKind::LBrace)?;

        let mut methods = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            let method_vis = if self.cursor.peek() == TokenKind::Private
                || self.cursor.peek() == TokenKind::Module
                || self.cursor.peek() == TokenKind::Package
            {
                let vis = token_to_visibility(self.cursor.peek());
                self.cursor.advance();
                vis
            } else {
                Visibility::Public
            };

            if self.cursor.peek() == TokenKind::Func {
                let func = self.parse_function_signature(method_vis)?;
                methods.push(func);
            } else {
                // Interface can also have associated types, consts, etc.
                // For now, just skip unknown tokens
                self.cursor.advance();
            }
        }

        self.cursor.expect(TokenKind::RBrace)?;

        let interface_decl = InterfaceDecl {
            name,
            visibility,
            sealed: is_sealed,
            permits,
            methods: if methods.is_empty() {
                None
            } else {
                Some(self.bump.alloc_slice_copy(&methods))
            },
            generics,
            span: token.span,
        };

        Ok(Stmt::InterfaceDecl(
            self.bump.alloc_value_immutable(interface_decl),
        ))
    }

    fn parse_permits_expr(&mut self) -> Result<&'bump PermitsExpr<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Permits)?;

        let mut types = Vec::new_in(self.bump);

        loop {
            let ty = self.parse_type()?;
            types.push(ty);

            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }

        Ok(self.bump.alloc_value_immutable(ir::ast::PermitsExpr {
            types: self.bump.alloc_slice_copy(&types),
            span: token.span,
        }))
    }

    pub fn parse_enum_decl(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::Enum)?;
        let (name, _span) = self.cursor.expect_ident()?;
        let generics = self.parse_generics()?;

        let mut variants = Vec::new();

        self.cursor.expect(TokenKind::LBrace)?;
        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            variants.push(self.parse_variant()?);
            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }
        self.cursor.expect(TokenKind::RBrace)?;
        Ok(Stmt::EnumDecl(self.bump.alloc_value_immutable(EnumDecl {
            name,
            visibility,
            generics,
            variants: self.bump.alloc_slice_copy(&variants),
            span: token.span,
        })))
    }

    /// Allows `VariantName` or `VariantName { first_field: type, second_field: type }`
    fn parse_variant(&mut self) -> Result<EnumVariant<'a, 'bump>, DiagnosticError<'a>> {
        let (name, span) = self.cursor.expect_ident()?;

        if self.cursor.peek() != TokenKind::LBrace {
            Ok(EnumVariant {
                name,
                fields: &[],
                span,
            })
        } else {
            self.cursor.advance();
            let mut fields = Vec::new();
            while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
                let visibility = match self.cursor.peek() {
                    TokenKind::Private => Visibility::Private,
                    TokenKind::Module => Visibility::Module,
                    TokenKind::Internal => Visibility::Internal,
                    _ => Visibility::Public,
                };

                let (field_name, field_span) = self.cursor.expect_ident()?;
                self.cursor.expect(TokenKind::Colon)?;
                let field_type = self.parse_type()?;

                // TODO: generics
                fields.push(Field {
                    name: field_name,
                    field_type,
                    visibility,
                    generics: None,
                    span: field_span,
                });
                if !self.cursor.consume(TokenKind::Comma) {
                    break;
                }
            }

            self.cursor.expect(TokenKind::RBrace)?;
            Ok(EnumVariant {
                name,
                fields: self.bump.alloc_slice_copy(&fields),
                span,
            })
        }
    }
}

fn token_to_visibility(token_kind: TokenKind) -> Visibility {
    match token_kind {
        TokenKind::Private => Visibility::Private,
        TokenKind::Module => Visibility::Module,
        TokenKind::Package => Visibility::Internal,
        _ => Visibility::Public,
    }
}
