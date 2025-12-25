use crate::parser::declaration_parser::DeclarationParser;
use crate::parser::parser_types::parse_type;
use crate::tokenizer::cursor::TokenCursor;
use crate::tokenizer::tokens::TokenKind;
use ir::ast::{
    Block, ConstStmt, EffectDecl, EnumDecl, EnumVariant, Field, FuncDecl, Generic, ImplDecl,
    InterfaceDecl, NormalParam, Param, PermitsExpr, StateMachineDecl, StateRef, Stmt, StructDecl,
    Transition, Type, Visibility,
};
use ir::hir::StrId;
use smallvec::SmallVec;

impl<'a, 'bump> DeclarationParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_enum(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Enum);

        let visibility = Self::fetch_visibility(cursor);

        let name = cursor.consume_ident().expect("Expected enum name");

        let generics = if cursor.peek_kind() == Some(TokenKind::Lt) {
            cursor.advance_kind();
            self.parse_generics(cursor)
        } else {
            None
        };

        cursor.expect_kind(TokenKind::LBrace);

        let mut variants_vec: Vec<EnumVariant<'_, '_>> = Vec::new();
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            let variant_name = match cursor.consume_ident() {
                Some(name) => name,
                None => {
                    eprintln!("Error: Expected variant name");
                    cursor.advance_kind(); // Skip invalid token
                    continue;
                }
            };

            let mut fields_vec: Vec<Field> = Vec::new();

            if cursor.peek_kind() == Some(TokenKind::LParen) {
                cursor.advance_kind(); // consume '('

                while cursor.peek_kind() != Some(TokenKind::RParen) && !cursor.at_end() {
                    let field_type = parse_type(cursor, self.context.clone(), self.bump);

                    let dummy_name = self.context.intern("_");
                    fields_vec.push(Field {
                        name: StrId(dummy_name),
                        field_type,
                        visibility: Visibility::Public,
                        generics: None,
                    });

                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.advance_kind();
                    } else {
                        break;
                    }
                }
                cursor.expect_kind(TokenKind::RParen);
            }

            let fields_slice: &[Field<'a, 'bump>] = self.bump.alloc_slice_copy(&fields_vec);
            variants_vec.push(EnumVariant {
                name: variant_name,
                fields: fields_slice.into(),
            });

            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            }
        }

        cursor.expect_kind(TokenKind::RBrace);

        let variants_slice: &[EnumVariant<'a, 'bump>] = self.bump.alloc_slice_copy(&variants_vec);
        let enum_decl = self.bump.alloc_value(EnumDecl {
            name,
            visibility,
            generics,
            variants: variants_slice.into(),
        });

        Stmt::EnumDecl(enum_decl)
    }

    /// Parse state machine declaration
    pub fn parse_state_machine(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Statem);

        let name = cursor.consume_ident().expect("Expected state machine name");

        cursor.expect_kind(TokenKind::LBrace);

        let mut transitions_vec: Vec<Transition<'a, 'bump>> = Vec::new();
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            // Parse transition: state => state { block }
            let from_state: StateRef = self.parse_state_ref(cursor);

            cursor.expect_kind(TokenKind::FatArrow);

            let to_state: StateRef = self.parse_state_ref(cursor);

            let action: Option<&Block> = if cursor.peek_kind() == Some(TokenKind::LBrace) {
                self.parse_block(cursor)
            } else {
                cursor.expect_kind(TokenKind::Semicolon);
                None
            };

            transitions_vec.push(Transition {
                from_state,
                to_state,
                action,
            });
        }

        cursor.expect_kind(TokenKind::RBrace);

        let transitions_slice: &[Transition<'a, 'bump>] =
            self.bump.alloc_slice_copy(&transitions_vec);
        let state_machine_decl = self.bump.alloc_value(StateMachineDecl {
            name,
            transitions: transitions_slice,
        });

        Stmt::StateMachineDecl(state_machine_decl)
    }

    fn parse_state_ref(&self, cursor: &mut TokenCursor<'a>) -> StateRef {
        match cursor.peek_kind() {
            Some(TokenKind::Mul) => {
                cursor.advance_kind();
                StateRef::Wildcard
            }
            Some(TokenKind::Ident) => {
                let name = cursor.consume_ident().unwrap();
                StateRef::Named(name)
            }
            _ => {
                cursor.advance_kind(); // consume invalid token
                StateRef::Wildcard
            }
        }
    }

    pub fn parse_interface(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Interface);

        let visibility = Self::fetch_visibility(cursor);

        let sealed = if cursor.peek_kind() == Some(TokenKind::Sealed) {
            cursor.advance_kind();
            true
        } else {
            false
        };

        let name = cursor.consume_ident().expect("Expected interface name");

        let permits: Option<&PermitsExpr> = if cursor.peek_kind() == Some(TokenKind::Permits) {
            cursor.advance_kind();
            let mut types_vec = Vec::new();

            loop {
                let permit_type = parse_type(cursor, self.context.clone(), self.bump);
                types_vec.push(permit_type);

                if cursor.peek_kind() == Some(TokenKind::Comma) {
                    cursor.advance_kind();
                } else {
                    break;
                }
            }

            let permits_expr = self.bump.alloc_value_immutable(PermitsExpr {
                types: self.bump.alloc_slice_copy(&types_vec),
            });
            Some(permits_expr)
        } else {
            None
        };

        let generics: Option<&[Generic]> = if cursor.peek_kind() == Some(TokenKind::Lt) {
            cursor.advance_kind();
            self.parse_generics(cursor)
        } else {
            None
        };

        cursor.expect_kind(TokenKind::LBrace);

        let mut methods_vec: Vec<FuncDecl> = Vec::new();
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            let func_decl = self.parse_function_decl(cursor);
            methods_vec.push(func_decl);
        }

        cursor.expect_kind(TokenKind::RBrace);

        let methods_slice: Option<&[FuncDecl]> = if methods_vec.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(&methods_vec))
        };

        let interface_decl = self.bump.alloc_value(InterfaceDecl {
            name,
            visibility,
            sealed,
            permits,
            methods: methods_slice,
            generics,
        });

        Stmt::InterfaceDecl(interface_decl)
    }

    pub fn parse_impl_decl(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Impl);

        let generics: Option<&[Generic]> = if cursor.peek_kind() == Some(TokenKind::Lt) {
            cursor.advance_kind();
            self.parse_generics(cursor)
        } else {
            None
        };

        let interface = cursor.consume_ident().expect("Expected interface name");

        let target = cursor.consume_ident().expect("Expected target type name");

        cursor.expect_kind(TokenKind::LBrace);

        let mut methods_vec: Vec<FuncDecl> = Vec::new();
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            let func_decl = self.parse_function_decl(cursor);
            methods_vec.push(func_decl);
        }

        cursor.expect_kind(TokenKind::RBrace);

        let methods: Option<&[FuncDecl]> = if methods_vec.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(&methods_vec))
        };

        let impl_decl = self.bump.alloc_value(ImplDecl {
            generics,
            interface,
            target,
            methods,
        });

        Stmt::ImplDecl(impl_decl)
    }

    pub fn parse_struct_decl(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Struct);

        let name = cursor.consume_ident().expect("Expected struct name");
        eprintln!(
            "DEBUG struct {} next token = {:?}",
            name,
            cursor.peek_kind()
        );


        let generics = if cursor.peek_kind() == Some(TokenKind::Lt) {
            self.parse_generics(cursor)
        } else {
            None
        };

        cursor.expect_kind(TokenKind::LBrace);
        let params = self.parse_struct_fields(cursor);
        cursor.expect_kind(TokenKind::RBrace);

        cursor.expect_kind(TokenKind::LBrace);

        let mut methods = Vec::new();
        let constants = Vec::new();
        let mut destructor = None;

        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            match cursor.peek_kind() {
                Some(TokenKind::Fn) => {
                    methods.push(self.parse_function_decl(cursor));
                }
                Some(TokenKind::Const) => {
                    cursor.advance_kind();
                    while cursor.peek_kind() != Some(TokenKind::Semicolon) && !cursor.at_end() {
                        cursor.advance_kind();
                    }
                    cursor.expect_kind(TokenKind::Semicolon);
                }
                Some(TokenKind::BitNot) => {
                    cursor.advance_kind();
                    let dtor_name = cursor.consume_ident().expect("Expected destructor name");
                    cursor.expect_kind(TokenKind::LParen);
                    cursor.expect_kind(TokenKind::RParen);

                    if dtor_name != name {
                        eprintln!("Warning: destructor name does not match struct name");
                    }

                    destructor = self.parse_block(cursor);
                }
                _ => {
                    eprintln!(
                        "Warning: Unexpected token {:?} in struct method block. Skipping to next fn/const/~/}}.",
                        cursor.peek_kind()
                    );
                    // Skip to next meaningful token (fn, const, ~, or })
                    while !matches!(
                        cursor.peek_kind(),
                        Some(TokenKind::Fn)
                        | Some(TokenKind::Const)
                        | Some(TokenKind::BitNot)
                        | Some(TokenKind::RBrace)
                        | None
                    ) && !cursor.at_end() {
                        cursor.advance_kind();
                    }
                }
            }
        }

        cursor.expect_kind(TokenKind::RBrace);

        let struct_decl = self.bump.alloc_value(StructDecl {
            visibility: Visibility::Public,
            name,
            generics,
            params,
            body: self.bump.alloc_slice_copy(&methods),
            constants: self.bump.alloc_slice_copy(&constants),
            destructor,
        });

        Stmt::StructDecl(struct_decl)
    }

    pub fn parse_effect(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Effect);

        let visibility = Self::fetch_visibility(cursor);

        let sealed = if cursor.peek_kind() == Some(TokenKind::Sealed) {
            cursor.advance_kind();
            true
        } else {
            false
        };

        let name = cursor.consume_ident().expect("Expected effect name");

        let permits: Option<&[Type]> = if cursor.peek_kind() == Some(TokenKind::Permits) {
            cursor.advance_kind();
            let mut types_vec: Vec<Type> = Vec::with_capacity(3);

            loop {
                let permit_type = parse_type(cursor, self.context.clone(), self.bump);
                types_vec.push(permit_type);

                if cursor.peek_kind() == Some(TokenKind::Comma) {
                    cursor.advance_kind();
                } else {
                    break;
                }
            }

            Some(self.bump.alloc_slice_copy(&types_vec))
        } else {
            None
        };

        let params: Option<&[Param]> = if cursor.peek_kind() == Some(TokenKind::LParen) {
            cursor.advance_kind();
            self.parse_struct_params(cursor)
        } else {
            None
        };

        let body = self.parse_block(cursor);

        let effect_decl = self.bump.alloc_value(EffectDecl {
            name,
            visibility,
            sealed,
            permits,
            params,
            body,
        });

        Stmt::EffectDecl(effect_decl)
    }

    /// Parse struct field declarations: { i32 x, y: String }
    fn parse_struct_fields(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut fields: SmallVec<Param, 8> = SmallVec::new();

        loop {
            if cursor.peek_kind() == Some(TokenKind::RBrace) {
                break;
            }

            // Parse struct field - always use "name: type" format
            if cursor.peek_kind() == Some(TokenKind::Ident) {
                let name = cursor.consume_ident().unwrap_or_else(|| {
                    panic!("Expected field name, found: {:?}", cursor.peek_kind())
                });

                if cursor.expect_kind(TokenKind::Colon) {
                    let param_type = parse_type(cursor, self.context.clone(), self.bump);

                    let normal_param = self.bump.alloc_value(NormalParam {
                        is_mut: false,
                        is_move: false,
                        name,
                        type_annotation: param_type,
                        visibility: Visibility::Public,
                        default_value: None,
                    });

                    fields.push(Param::Normal(normal_param));
                } else {
                    panic!("Expected ':' after field name, found: {:?}", cursor.peek_kind());
                }
            } else {
                panic!("Expected field name (identifier), found: {:?}", cursor.peek_kind());
            }

            // Handle comma separator
            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            } else if cursor.peek_kind() != Some(TokenKind::RBrace) {
                println!(
                    "Debug: Expected comma or RBrace, found: {:?}",
                    cursor.peek_kind()
                );
                break;
            }
        }

        if fields.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(fields.as_slice()))
        }
    }
}
