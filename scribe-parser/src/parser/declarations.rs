use ir::ast::{
    Block, ConstStmt, Field, FuncDecl, Generic, ImplDecl, InterfaceDecl, Param, StructDecl,
    Visibility,
};
use ir::errors::error::ParserError;
use ir::tokens::TokenKind;
use crate::parser::descent_parser::DescentParser;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    /// Parse a struct declaration
    /// ```ignore
    /// pub struct Point<T> {
    ///     x: i32,
    ///     y: i32,
    /// }
    /// ```
    pub fn parse_struct_decl(&mut self, visibility: Visibility) -> Result<ir::ast::Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Struct);

        let (name, _span) = self.cursor.expect_ident()?;

        // Parse optional generics: <T, U: Display>
        let generics = if self.cursor.peek() == TokenKind::Lt {
            self.parse_generics()?
        } else {
            None
        };

        let (params, body, constants, destructor) = if self.cursor.consume(TokenKind::Semicolon) {
            // Unit struct: `struct Foo;`
            (None, &[], &[], None)
        } else if self.cursor.peek() == TokenKind::LParen {
            // Tuple struct: `struct Foo(i32, i32);`
            let tuple_fields = self.parse_tuple_struct_fields()?;
            self.cursor.expect(TokenKind::Semicolon)?;
            (Some(tuple_fields), &[], &[], None)
        } else {
            // Regular struct with braces
            self.cursor.expect(TokenKind::LBrace)?;

            let mut fields = Vec::new_in(self.bump);
            let mut consts = Vec::new_in(self.bump);
            let mut dtor: Option<&'bump Block<'a, 'bump>> = None;

            while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
                // Check for destructor
                if self.cursor.peek() == TokenKind::BitAnd {
                    self.cursor.advance();
                    self.cursor.expect(TokenKind::This)?;
                    let block = self.parse_block()?;
                    dtor = Some(self.bump.alloc_value_immutable(block));
                    continue;
                }

                // Check for const
                if self.cursor.peek() == TokenKind::Const {
                    let const_stmt = self.parse_const_stmt()?;
                    consts.push(const_stmt);
                    continue;
                }

                // Check for visibility modifiers for fields
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

                // It's a field
                let (field_name, field_span) = self.cursor.expect_ident()?;
                self.cursor.expect(TokenKind::Colon)?;
                let field_type = self.parse_type()?;

                fields.push(Field {
                    name: field_name,
                    field_type,
                    visibility: field_vis,
                    generics: None,
                });

                // Optional trailing comma
                self.cursor.consume(TokenKind::Comma);
            }

            self.cursor.expect(TokenKind::RBrace)?;

            let fields_as_params: Vec<Param<'a, 'bump>> = fields
                .into_iter()
                .map(|f| {
                    Param::Normal(self.bump.alloc_value_immutable(ir::ast::NormalParam {
                        is_mut: false,
                        is_move: false,
                        name: f.name,
                        type_annotation: f.field_type,
                        visibility: f.visibility,
                        default_value: None,
                    }))
                })
                .collect();

            (
                if fields_as_params.is_empty() { None } else { Some(self.bump.alloc_slice_copy(&fields_as_params)) },
                &[],
                self.bump.alloc_slice_copy(&consts),
                dtor,
            )
        };

        let struct_decl = StructDecl {
            visibility,
            name,
            generics,
            params,
            body,
            constants,
            destructor,
        };

        Ok(ir::ast::Stmt::StructDecl(self.bump.alloc_value_immutable(struct_decl)))
    }

    /// Parse an impl block
    /// ```ignore
    /// impl Point {
    ///     fn new(x: i32, y: i32) -> Point {
    ///         return Point { x, y };
    ///     }
    /// }
    ///
    /// impl Drawable for Point {
    ///     fn draw(&this) { ... }
    /// }
    /// ```
    pub fn parse_impl_decl(&mut self, visibility: Visibility) -> Result<ir::ast::Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Impl);

        // Check for generic impl: impl<T> ...
        let generics = if self.cursor.peek() == TokenKind::Lt {
            self.parse_generics()?
        } else {
            None
        };

        // Parse target type name
        let (target, _span) = self.cursor.expect_ident()?;

        // Check for trait implementation: impl Interface for Type
        let interface = if self.cursor.peek() == TokenKind::For {
            self.cursor.advance();
            let (trait_name, _) = self.cursor.expect_ident()?;
            Some(trait_name)
        } else {
            None
        };

        // Parse method bodies
        self.cursor.expect(TokenKind::LBrace)?;

        let mut methods = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            // Parse function with visibility
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

            let func = self.parse_function_with_visibility(method_vis)?;
            if let ir::ast::Stmt::FuncDecl(func) = func {
                methods.push(*func);
            }
        }

        self.cursor.expect(TokenKind::RBrace)?;

        let impl_decl = ImplDecl {
            generics,
            interface,
            target,
            methods: if methods.is_empty() { None } else { Some(self.bump.alloc_slice_copy(&methods)) },
        };

        Ok(ir::ast::Stmt::ImplDecl(self.bump.alloc_value_immutable(impl_decl)))
    }

    fn parse_tuple_struct_fields(&mut self) -> Result<&'bump [Param<'a, 'bump>], ParserError<'a>> {
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

            let field_type = self.parse_type()?;

            fields.push(Param::Normal(self.bump.alloc_value_immutable(ir::ast::NormalParam {
                is_mut: false,
                is_move: false,
                name: ir::hir::StrId::default(), // Anonymous field in tuple struct
                type_annotation: field_type,
                visibility: field_vis,
                default_value: None,
            })));

            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }

        self.cursor.expect(TokenKind::RParen)?;

        Ok(self.bump.alloc_slice_copy(&fields))
    }

    fn parse_const_stmt(&mut self) -> Result<ConstStmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Const);
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
    pub fn parse_interface_decl(&mut self, visibility: Visibility, is_sealed: bool) -> Result<ir::ast::Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Interface);

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

            if self.cursor.peek() == TokenKind::Fn {
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
            methods: if methods.is_empty() { None } else { Some(self.bump.alloc_slice_copy(&methods)) },
            generics,
        };

        Ok(ir::ast::Stmt::InterfaceDecl(self.bump.alloc_value_immutable(interface_decl)))
    }

    fn parse_permits_expr(&mut self) -> Result<&'bump ir::ast::PermitsExpr<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Permits);

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
        }))
    }

    pub fn parse_enum_decl(&mut self, visibility: Visibility) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        self.cursor.consume(TokenKind::Enum);
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
        })))
    }

    /// Allows `VariantName` or `VariantName { first_field: type, second_field: type }`
    fn parse_variant(&mut self) -> Result<EnumVariant<'a, 'bump>, ParserError<'a>> {
        let (name, _span) = self.cursor.expect_ident()?;

        if self.cursor.peek() != TokenKind::LBrace {
            Ok(EnumVariant { name, fields: &[] })
        } else {
            self.cursor.advance();
            let (variant_name, variant_span) = self.cursor.expect_ident()?;
            let mut fields = Vec::new();
            while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
                // visibility
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
                fields.push(Field { name: field_name, field_type, visibility, generics: None });
                if !self.cursor.consume(TokenKind::Comma) {
                    break;
                }
            }

            self.cursor.expect(TokenKind::RBrace)?;
            Ok(EnumVariant { name, fields: self.bump.alloc_slice_copy(&fields) })
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
