use ir::ast::{Block, StructDecl, ConstStmt, EffectDecl, EnumDecl, EnumVariant, Field, FuncDecl, Generic, ImplDecl, InterfaceDecl, NormalParam, Param, PermitsExpr, StateMachineDecl, StateRef, Stmt, Transition, Type, Visibility};
use ir::hir::StrId;
use crate::parser::declaration_parser::DeclarationParser;
use crate::parser::parser_types::parse_to_type;
use crate::tokenizer::cursor::TokenCursor;
use crate::tokenizer::tokens::TokenKind;
use smallvec::SmallVec;

impl<'a, 'bump> DeclarationParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_enum(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Enum);

        let visibility = Self::fetch_visibility(cursor);

        let name = cursor.consume_ident()
            .expect("Expected enum name");

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
                    let type_name = match cursor.consume_ident() {
                        Some(name) => name,
                        None => {
                            eprintln!("Error: Expected field type");
                            cursor.advance_kind(); // Skip invalid token
                            continue;
                        }
                    };

                    let type_str = self.context.resolve_string(&type_name);
                    let field_type = parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);

                    let generics = if cursor.peek_kind() == Some(TokenKind::Lt) {
                        cursor.advance_kind(); // consume '<'
                        let mut generic_params = Vec::new();
                        
                        while cursor.peek_kind() != Some(TokenKind::Gt) && !cursor.at_end() {
                            let type_name = match cursor.consume_ident() {
                                Some(name) => name,
                                None => {
                                    eprintln!("Error: Expected type in generic parameters");
                                    cursor.advance_kind();
                                    continue;
                                }
                            };

                            let type_str = self.context.resolve_string(&type_name);
                            let ty = parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);
                            generic_params.push(ty);
                            
                            if cursor.peek_kind() == Some(TokenKind::Comma) {
                                cursor.advance_kind();
                            } else if cursor.peek_kind() != Some(TokenKind::Gt) {
                                eprintln!("Expected ',' or '>' in generic parameters");
                                break;
                            }
                        }
                        
                        if cursor.peek_kind() == Some(TokenKind::Gt) {
                            cursor.advance_kind(); // consume '>'
                            Some(self.bump.alloc_slice(&generic_params))
                        } else {
                            eprintln!("Expected '>' after generic parameters");
                            None
                        }
                    } else {
                        None
                    };

                    let dummy_name = self.context.intern("_");
                    fields_vec.push(Field {
                        name: StrId(dummy_name),
                        field_type,
                        visibility: Visibility::Public,
                        generics,
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

        let name = cursor.consume_ident()
            .expect("Expected state machine name");

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

        let transitions_slice: &[Transition<'a, 'bump>] = self.bump.alloc_slice_copy(&transitions_vec);
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

        let name = cursor.consume_ident()
            .expect("Expected interface name");

        let mut current_kind = cursor.peek_kind();
        let permits: Option<&PermitsExpr> = if current_kind == Some(TokenKind::Permits) {
            cursor.advance_kind();
            current_kind = cursor.peek_kind();
            let mut types_vec = Vec::new();

            loop {
                let type_name = cursor.consume_ident()
                    .expect("Expected type in permits clause");
                let type_str = self.context.resolve_string(&type_name);
                let permit_type = parse_to_type(type_str, current_kind.unwrap(), self.context.clone(), self.bump);
                types_vec.push(permit_type);

                if current_kind == Some(TokenKind::Comma) {
                    cursor.advance_kind();
                    current_kind = cursor.peek_kind();
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

        let interface = cursor.consume_ident()
            .expect("Expected interface name");

        let target = cursor.consume_ident()
            .expect("Expected target type name");

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

        let visibility = Self::fetch_visibility(cursor);

        let name = cursor.consume_ident()
            .expect("Expected class name");

        let generics: Option<&'bump [Generic<'a, 'bump>]> = if cursor.peek_kind() == Some(TokenKind::Lt) {
            cursor.advance_kind();
            self.parse_generics(cursor)
        } else {
            None
        };

        let params: Option<&'bump [Param<'a, 'bump>]> = if cursor.peek_kind() == Some(TokenKind::LParen) {
            println!("Debug: Parsing struct params in parentheses");
            cursor.advance_kind();
            self.parse_struct_params(cursor)
        } else if cursor.peek_kind() == Some(TokenKind::LBrace) {
            cursor.advance_kind();
            let fields = self.parse_struct_fields(cursor);
            cursor.expect_kind(TokenKind::RBrace);
            fields
        } else {
            println!("Debug: No struct params/fields found");
            None
        };

        cursor.expect_kind(TokenKind::LBrace);

        let mut methods_vec: Vec<FuncDecl> = Vec::new();
        let constants_vec: Vec<ConstStmt> = Vec::new();
        let mut destructor: Option<&'bump Block<'a, 'bump>> = None;

        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            if cursor.peek_kind() == Some(TokenKind::BitNot) {
                cursor.advance_kind(); // consume '~'
                let destructor_name = cursor.consume_ident()
                    .expect("Expected destructor name");

                let _ = cursor.expect_kind(TokenKind::LParen);
                let _ = cursor.expect_kind(TokenKind::RParen);

                if destructor_name != name {
                    eprintln!("Warning: Destructor name doesn't match class name");
                }

                destructor = self.parse_block(cursor);
            } else if cursor.peek_kind() == Some(TokenKind::Const) {
                cursor.advance_kind();
                // Skip const parsing for now
                while cursor.peek_kind() != Some(TokenKind::Semicolon) && !cursor.at_end() {
                    cursor.advance_kind();
                }
                cursor.expect_kind(TokenKind::Semicolon);
            } else if cursor.peek_kind() == Some(TokenKind::Fn) {
                let func_decl = self.parse_function_decl(cursor);
                methods_vec.push(func_decl);
            } else {
                // Skip unknown tokens in struct body
                cursor.advance_kind();
            }
        }

        cursor.expect_kind(TokenKind::RBrace);

        let body_slice = self.bump.alloc_slice_copy(&methods_vec);
        let constants_slice = self.bump.alloc_slice_copy(&constants_vec);


        let class_decl = self.bump.alloc_value(StructDecl {
            visibility,
            name,
            generics,
            params,
            body: body_slice,
            constants: constants_slice,
            destructor,
        });

        Stmt::StructDecl(class_decl)
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

        let name = cursor.consume_ident()
            .expect("Expected effect name");

        let mut current_kind = cursor.peek_kind();
        let permits: Option<&[Type]> = if cursor.peek_kind() == Some(TokenKind::Permits) {
            cursor.advance_kind();
            current_kind = cursor.peek_kind();
            let mut types_vec: Vec<Type> = Vec::with_capacity(3);

            loop {
                let type_name = cursor.consume_ident()
                    .expect("Expected type in permits clause");
                let type_str = self.context.resolve_string(&type_name);
                let permit_type = parse_to_type(type_str, current_kind.unwrap(), self.context.clone(), self.bump);
                types_vec.push(permit_type);

                if current_kind == Some(TokenKind::Comma) {
                    cursor.advance_kind();
                    current_kind = cursor.peek_kind();
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
    fn parse_struct_fields(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut fields: SmallVec<Param, 8> = SmallVec::new();
        
        loop {
            match cursor.peek_kind() {
                Some(TokenKind::RBrace) => {
                    // Don't consume RBrace here - let the caller handle it
                    break;
                }
                Some(TokenKind::I32) | Some(TokenKind::I64) | Some(TokenKind::U32) | Some(TokenKind::U64) | 
                Some(TokenKind::F32) | Some(TokenKind::F64) | Some(TokenKind::Boolean) | Some(TokenKind::Str) => {
                    let type_token = cursor.peek_kind().unwrap();
                    cursor.advance_kind(); // consume type token
                    
                    let name = cursor.consume_ident().expect("Expected field name after type");
                    let param_type = match type_token {
                        TokenKind::I32 => Type::i32(),
                        TokenKind::I64 => Type::i64(),
                        TokenKind::U32 => Type::i32(),
                        TokenKind::U64 => Type::i64(),
                        TokenKind::F32 => Type::f32(),
                        TokenKind::F64 => Type::f64(),
                        TokenKind::Boolean => Type::boolean(),
                        TokenKind::Str => Type::string(),
                        _ => Type::void(),
                    };
                    
                    let normal_param = self.bump.alloc_value(NormalParam {
                        is_mut: false,
                        is_move: false,
                        name,
                        type_annotation: param_type,
                        visibility: Visibility::Public,
                        default_value: None,
                    });
                    
                    fields.push(Param::Normal(normal_param));
                }
                Some(TokenKind::Ident) => {
                    // Check if it's "name: type" syntax
                    let name = cursor.consume_ident().expect("Expected field name");
                    
                    if cursor.peek_kind() == Some(TokenKind::Colon) {
                        // Parse: name: type (e.g., "y: String" or "y: i32")
                        cursor.advance_kind(); // consume ':'
                        
                        // Get the next token kind to determine if it's a primitive type
                        let type_kind = cursor.peek_kind().expect("Expected type after colon");
                        let param_type = match type_kind {
                            TokenKind::I8 | TokenKind::I16 | TokenKind::I32 | TokenKind::I64 | TokenKind::I128 |
                            TokenKind::U8 | TokenKind::U16 | TokenKind::U32 | TokenKind::U64 | TokenKind::U128 |
                            TokenKind::F32 | TokenKind::F64 | TokenKind::Boolean | TokenKind::String | TokenKind::Void => {
                                cursor.advance_kind(); // consume the primitive type token
                                match type_kind {
                                    TokenKind::I8 => Type::i8(),
                                    TokenKind::I16 => Type::i16(),
                                    TokenKind::I32 => Type::i32(),
                                    TokenKind::I64 => Type::i64(),
                                    TokenKind::I128 => Type::i128(),
                                    TokenKind::U8 => Type::u8(),
                                    TokenKind::U16 => Type::u16(),
                                    TokenKind::U32 => Type::u32(),
                                    TokenKind::U64 => Type::u64(),
                                    TokenKind::U128 => Type::u128(),
                                    TokenKind::F32 => Type::f32(),
                                    TokenKind::F64 => Type::f64(),
                                    TokenKind::Boolean => Type::boolean(),
                                    TokenKind::String => Type::string(),
                                    TokenKind::Void => Type::void(),
                                    _ => unreachable!(),
                                }
                            },
                            TokenKind::Ident => {
                                let type_name = cursor.consume_ident().expect("Expected type after colon");
                                let type_str = self.context.resolve_string(&type_name);
                                parse_to_type(type_str, type_kind, self.context.clone(), self.bump)
                            },
                            _ => {
                                panic!("Expected type after colon, found: {:?}", type_kind);
                            }
                        };
                        
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
                        let type_str = self.context.resolve_string(&name);
                        let field_name = cursor.consume_ident().expect("Expected field name after type");
                        let param_type = parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);
                        
                        let normal_param = self.bump.alloc_value(NormalParam {
                            is_mut: false,
                            is_move: false,
                            name: field_name,
                            type_annotation: param_type,
                            visibility: Visibility::Public,
                            default_value: None,
                        });
                        
                        fields.push(Param::Normal(normal_param));
                    }
                }
                _ => {
                    println!("Debug: Unexpected token in struct fields: {:?}", cursor.peek_kind());
                    break;
                }
            }
            
            // Handle comma separator
            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            } else if cursor.peek_kind() != Some(TokenKind::RBrace) {
                println!("Debug: Expected comma or RBrace, found: {:?}", cursor.peek_kind());
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