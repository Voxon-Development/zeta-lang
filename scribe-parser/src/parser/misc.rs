use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    Generic, MutabilityState, NormalParam, Param, ParamPassingKind, ThisParam, Type, TypeKind,
    Visibility,
};
use ir::errors::error::{DiagnosticError, ParseErrorKind};
use ir::tokens::TokenKind::LBracket;
use ir::tokens::{Token, TokenKind};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_generics(
        &mut self,
    ) -> Result<Option<&'bump [Generic<'a, 'bump>]>, DiagnosticError<'a>> {
        if self.cursor.peek() != TokenKind::Lt {
            return Ok(None);
        }

        self.cursor.bump(); // consume '<'

        let mut generics = Vec::new_in(self.bump);

        loop {
            let is_const = self.cursor.consume(TokenKind::Const);
            let is_static = if !is_const {
                self.cursor.consume(TokenKind::Static)
            } else {
                false
            };

            let (name, span) = self.cursor.expect_ident()?;

            let constraints = if self.cursor.consume(TokenKind::Colon) {
                let mut types = Vec::new_in(self.bump);

                loop {
                    let ty = self.parse_type()?;
                    types.push(ty);

                    if !self.cursor.consume(TokenKind::Comma)
                        && !self.cursor.consume(TokenKind::Add)
                    {
                        break;
                    }
                }

                self.bump.alloc_slice_copy(&types)
            } else {
                &[]
            };

            generics.push(Generic {
                type_name: name,
                span,
                is_const,
                is_static,
                constraints,
            });

            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }

        self.cursor.expect(TokenKind::Gt)?;

        Ok(Some(self.bump.alloc_slice_copy(&generics)))
    }

    pub fn parse_params(
        &mut self,
    ) -> Result<Option<&'bump [Param<'a, 'bump>]>, DiagnosticError<'a>> {
        let current_kind = self.cursor.peek_token();
        if current_kind.kind != TokenKind::LParen {
            let span = current_kind.span;

            self.diag.record(
                DiagnosticError::unexpected_token(
                    TokenKind::LParen,
                    current_kind.kind,
                    current_kind.span,
                )
                .with_note("Fix: Add a ( to fix it."),
            );
            let stop = self.diag.synchronize(&mut self.cursor);
            if stop == TokenKind::EOF {
                return Err(DiagnosticError::unexpected_eof(
                    Some(TokenKind::LParen),
                    span,
                ));
            }
            return Ok(None);
        }

        self.cursor.advance(); // consume '('

        let mut params: Vec<Param<'a, 'bump>, &GrowableBump> = Vec::new_in(self.bump);

        if self.cursor.peek() == TokenKind::RParen {
            self.cursor.advance();
            return Ok(Some(self.bump.alloc_slice_copy(&params)));
        }

        while self.cursor.peek() != TokenKind::RParen && self.cursor.peek() != TokenKind::EOF {
            let param = if matches!(
                self.cursor.peek(),
                TokenKind::BitAnd | TokenKind::Mul | TokenKind::LBracket
            ) {
                let sigil_token = self.cursor.peek_token();
                let passing_kind = self.parse_param_passing_kind()?;

                // Reject pointer receivers (*mut this, *const this, [*]mut this, etc.)
                if matches!(
                    passing_kind,
                    ParamPassingKind::MutSafePtr
                        | ParamPassingKind::ConstSafePtr
                        | ParamPassingKind::ConstUnsafePtr
                        | ParamPassingKind::MutUnsafePtr
                ) {
                    self.diag.record(DiagnosticError::new(
                        ParseErrorKind::UnexpectedToken {
                            expected: TokenKind::BitAnd,
                            found: sigil_token.kind,
                        },
                        sigil_token.span,
                    ));
                    self.diag.synchronize(&mut self.cursor);
                    continue;
                }

                let this_token = self.cursor.expect(TokenKind::This)?;
                Param::This(self.bump.alloc_value_immutable(ThisParam {
                    passing_kind,
                    span: this_token.span,
                }))
            } else if self.cursor.peek() == TokenKind::This {
                let token = self.cursor.expect(TokenKind::This)?;
                Param::This(self.bump.alloc_value_immutable(ThisParam {
                    passing_kind: ParamPassingKind::Move,
                    span: token.span,
                }))
            } else if self.cursor.peek() == TokenKind::Mut
                && self.cursor.peek_n(1) == TokenKind::This
            {
                // `mut this`: move-by-value receiver with mutable access,
                // e.g. used by destructors (`fn drop(mut this)`).
                self.cursor.advance(); // consume 'mut'
                let token = self.cursor.expect(TokenKind::This)?;
                Param::This(self.bump.alloc_value_immutable(ThisParam {
                    passing_kind: ParamPassingKind::MoveMut,
                    span: token.span,
                }))
            } else {
                let is_mut = self.cursor.consume(TokenKind::Mut);
                let (name, span) = self.cursor.expect_ident()?;

                let param_type: Type<'a, 'bump> = if self.cursor.consume(TokenKind::Colon) {
                    self.parse_type()?
                } else {
                    return Err(DiagnosticError::new(
                        ParseErrorKind::ExpectedTypeAnnotation,
                        self.cursor.peek_token().span,
                    ));
                };

                let default_value = if self.cursor.consume(TokenKind::Eq) {
                    Some(self.parse_expr_inner(0, true)?)
                } else {
                    None
                };

                Param::Normal(self.bump.alloc_value_immutable(NormalParam {
                    is_mut,
                    name,
                    type_annotation: param_type,
                    visibility: Visibility::Public,
                    default_value,
                    span,
                }))
            };

            params.push(param);

            match self.cursor.expect_or(TokenKind::Comma, TokenKind::RParen) {
                Ok(t) if t.kind == TokenKind::RParen => {
                    break;
                }
                Ok(_) => {}
                Err(error) => {
                    self.diag.record(error);
                    let kind = self.diag.synchronize(&mut self.cursor);
                    if kind == TokenKind::LBrace
                        || kind == TokenKind::RBrace
                        || kind == TokenKind::RParen
                        || kind == TokenKind::EOF
                    {
                        return Err(DiagnosticError::new(
                            ParseErrorKind::UnexpectedTokens {
                                expected: vec![TokenKind::RParen, TokenKind::Comma],
                                found: kind,
                            },
                            self.cursor.peek_token().span,
                        ));
                    }
                }
            }
        }

        Ok(Some(self.bump.alloc_slice_copy(&params)))
    }

    fn parse_param_passing_kind(&mut self) -> Result<ParamPassingKind, DiagnosticError<'a>> {
        match self.cursor.peek() {
            TokenKind::BitAnd => {
                self.cursor.advance();

                if self.cursor.consume(TokenKind::Mut) {
                    Ok(ParamPassingKind::RefMut)
                } else {
                    Ok(ParamPassingKind::RefConst)
                }
            }

            TokenKind::Mul => {
                self.cursor.advance();
                match self.cursor.expect_or(TokenKind::Mut, TokenKind::Const) {
                    Ok(token) if token.kind == TokenKind::Mut => {
                        return Ok(ParamPassingKind::MutSafePtr);
                    }
                    Ok(token) if token.kind == TokenKind::Const => {
                        return Ok(ParamPassingKind::ConstSafePtr);
                    }
                    Ok(_) => unreachable!(),
                    Err(_) => todo!(),
                }
            }

            TokenKind::LBracket => {
                self.cursor.advance();

                self.cursor.expect(TokenKind::Mul)?;
                self.cursor.expect(TokenKind::RBracket)?;

                match self.cursor.expect_or(TokenKind::Mut, TokenKind::Const) {
                    Ok(token) if token.kind == TokenKind::Mut => {
                        return Ok(ParamPassingKind::MutSafePtr);
                    }
                    Ok(token) if token.kind == TokenKind::Const => {
                        return Ok(ParamPassingKind::ConstSafePtr);
                    }
                    Ok(_) => unreachable!(),
                    Err(_) => todo!(),
                }
            }

            _ => {
                if self.cursor.consume(TokenKind::Mut) {
                    return Ok(ParamPassingKind::MoveMut);
                }
                Ok(ParamPassingKind::Move)
            }
        }
    }

    pub(crate) fn parse_type(&mut self) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        match self.cursor.peek() {
            TokenKind::BitAnd => {
                self.cursor.advance();

                let token = self.cursor.peek_token();
                if token.kind == LBracket {
                    let kind: TypeKind<'a, 'bump> = self.process_type_kind_after_lbracket(token)?;
                    if let TypeKind::UnsafePointer { .. } = kind {
                        todo!("Handle error when & and [*] are mixed together.")
                    }

                    let nullable = self.cursor.consume(TokenKind::Question);

                    return Ok(Type { kind, nullable });
                } else {
                    let mutability_state = if self.cursor.consume(TokenKind::Mut) {
                        MutabilityState::Mut
                    } else {
                        MutabilityState::Const
                    };

                    let is_dyn = self.cursor.consume(TokenKind::Dyn);

                    if is_dyn {
                        let mut bounds = Vec::new_in(self.bump);

                        bounds.push(self.parse_core_type()?);

                        while self.cursor.consume(TokenKind::Add) {
                            bounds.push(self.parse_core_type()?);
                        }

                        return Ok(Type {
                            kind: TypeKind::Ref {
                                inner: self.bump.alloc_value(Type {
                                    kind: TypeKind::Dyn {
                                        bounds: self.bump.alloc_slice_copy(&bounds),
                                    },
                                    nullable: false,
                                }),
                                mutability_state: MutabilityState::Const,
                            },
                            nullable: false,
                        });
                    }

                    let inner = self.parse_core_type()?;

                    let nullable = self.cursor.consume(TokenKind::Question);

                    return Ok(Type {
                        kind: TypeKind::Ref {
                            inner: self.bump.alloc_value_immutable(inner),
                            mutability_state,
                        },
                        nullable,
                    });
                }
            }
            _ => {}
        }

        let is_dyn = self.cursor.consume(TokenKind::Dyn);

        if is_dyn {
            let mut bounds = Vec::new_in(self.bump);

            bounds.push(self.parse_core_type()?);

            while self.cursor.consume(TokenKind::Add) {
                bounds.push(self.parse_core_type()?);
            }

            return Ok(Type {
                kind: TypeKind::Dyn {
                    bounds: self.bump.alloc_slice_copy(&bounds),
                },
                nullable: false,
            });
        }

        let mut ty = self.parse_core_type()?;

        if self.cursor.consume(TokenKind::Question) {
            ty.nullable = true;
        }

        Ok(ty)
    }

    fn process_type_kind_after_lbracket(
        &mut self,
        token: Token<'a>,
    ) -> Result<TypeKind<'a, 'bump>, DiagnosticError<'a>> {
        self.cursor.advance();
        // can this be an array, slice or unsafe pointer?
        if token.kind == TokenKind::RBracket {
            self.cursor.advance();
            let inner = self.parse_core_type()?;
            let inner_ref = self.bump.alloc_value(inner);
            return Ok(TypeKind::Slice { inner: inner_ref });
        } else if token.kind == TokenKind::Number {
            self.cursor.advance();
            self.cursor.expect(TokenKind::RBracket)?;
            // SAFETY: a token with kind TokenKind::Number always comes with a text to parse.
            // Can this be optimized to be inline-parsed at the lexer and be stored in an ADT instead?
            let number_unparsed = unsafe { token.text.unwrap_unchecked() };
            let length = number_unparsed.as_str().parse::<usize>().map_err(|_| {
                DiagnosticError::new(
                    ParseErrorKind::UnexpectedToken {
                        expected: TokenKind::Number,
                        found: token.kind,
                    },
                    self.cursor.peek_token().span,
                )
            })?;
            let inner = self.parse_core_type()?;
            let inner_ref = self.bump.alloc_value(inner);
            return Ok(TypeKind::Array {
                inner: inner_ref,
                length,
            });
        } else if token.kind == TokenKind::Mul {
            self.cursor.advance(); // consume *
            self.cursor.expect(TokenKind::RBracket)?; // consume ]

            let mutability_token = self.cursor.expect_or(TokenKind::Mut, TokenKind::Const)?;
            let mutability_state = match mutability_token.kind {
                TokenKind::Mut => MutabilityState::Mut,
                // I wish rust knew that only Mut and Const is possible here :(
                _ => MutabilityState::Const,
            };

            let inner = self.parse_core_type()?;
            let inner_ref = self.bump.alloc_value(inner);
            return Ok(TypeKind::UnsafePointer {
                inner: inner_ref,
                mutability_state,
            });
        } else {
            return Err(DiagnosticError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Mul,
                    found: self.cursor.peek_token().kind,
                },
                self.cursor.peek_token().span,
            ));
        }
    }

    fn parse_core_type(&mut self) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        let tok = self.cursor.bump();

        let kind = match tok.kind {
            TokenKind::U8 => return Ok(Type::u8()),
            TokenKind::U16 => return Ok(Type::u16()),
            TokenKind::U32 => return Ok(Type::u32()),
            TokenKind::U64 => return Ok(Type::u64()),
            TokenKind::U128 => return Ok(Type::u128()),
            TokenKind::I8 => return Ok(Type::i8()),
            TokenKind::I16 => return Ok(Type::i16()),
            TokenKind::I32 => return Ok(Type::i32()),
            TokenKind::I64 => return Ok(Type::i64()),
            TokenKind::I128 => return Ok(Type::i128()),
            TokenKind::F32 => return Ok(Type::f32()),
            TokenKind::F64 => return Ok(Type::f64()),
            TokenKind::Boolean => return Ok(Type::boolean()),
            TokenKind::Char => return Ok(Type::char()),
            TokenKind::Str => return Ok(Type::string()),
            TokenKind::Void => return Ok(Type::void()),

            // `this` as a type (for self-referential method return types)
            TokenKind::This => return Ok(Type::this()),

            TokenKind::Underscore => return Ok(Type::infer()),

            TokenKind::Mul => {
                let mutability_token = self.cursor.expect_or(TokenKind::Mut, TokenKind::Const)?;
                let mutability_state = match mutability_token.kind {
                    TokenKind::Mut => MutabilityState::Mut,
                    // I wish rust knew that only Mut and Const is possible here :(
                    _ => MutabilityState::Const,
                };
                // *T is safe pointer (aligned, non-null)
                let inner = self.parse_core_type()?;
                let inner_ref = self.bump.alloc_value(inner);
                TypeKind::SafePointer {
                    inner: inner_ref,
                    mutability_state,
                }
            }

            TokenKind::Fn => {
                self.cursor.expect(TokenKind::LParen)?;
                let mut params: Vec<Type<'a, 'bump>> = Vec::new();
                while self.cursor.peek() != TokenKind::RParen {
                    params.push(self.parse_type()?);
                    if self.cursor.peek() == TokenKind::Comma {
                        self.cursor.advance();
                    }
                }
                self.cursor.expect(TokenKind::RParen)?;

                let return_type = if self.cursor.peek() == TokenKind::Arrow {
                    self.cursor.advance();
                    self.parse_core_type()?
                } else {
                    Type::void()
                };

                let params_bump = self.bump.alloc_slice(&params);
                let ret_ref = self.bump.alloc_value(return_type);
                TypeKind::Lambda {
                    params: params_bump,
                    return_type: ret_ref,
                }
            }

            TokenKind::Ident => {
                let name = tok
                    .text
                    .ok_or_else(|| DiagnosticError::new(ParseErrorKind::EmptyIdent, tok.span))?;

                // Fallback: if the lexer emitted a primitive keyword as a plain Ident
                // (e.g. `void`, `bool`, etc.), resolve it here so we never produce
                // TypeKind::Struct { name: "void", .. }.
                match name.as_str() {
                    "void" => return Ok(Type::void()),
                    "bool" => return Ok(Type::boolean()),
                    "str" => return Ok(Type::string()),
                    "char" => return Ok(Type::char()),
                    "u8" => return Ok(Type::u8()),
                    "u16" => return Ok(Type::u16()),
                    "u32" => return Ok(Type::u32()),
                    "u64" => return Ok(Type::u64()),
                    "u128" => return Ok(Type::u128()),
                    "i8" => return Ok(Type::i8()),
                    "i16" => return Ok(Type::i16()),
                    "i32" => return Ok(Type::i32()),
                    "i64" => return Ok(Type::i64()),
                    "i128" => return Ok(Type::i128()),
                    "f32" => return Ok(Type::f32()),
                    "f64" => return Ok(Type::f64()),
                    "this" => return Ok(Type::this()),
                    _ => {}
                }

                let generics = if self.cursor.peek() == TokenKind::Lt {
                    self.cursor.advance();
                    let mut args: Vec<Type<'a, 'bump>> = Vec::new();
                    loop {
                        args.push(self.parse_type()?);
                        match self.cursor.peek() {
                            TokenKind::Comma => {
                                self.cursor.advance();
                            }
                            TokenKind::Gt => {
                                self.cursor.advance();
                                break;
                            }
                            _ => {
                                let t = self.cursor.peek_token();
                                return Err(DiagnosticError::new(
                                    ParseErrorKind::UnexpectedToken {
                                        expected: TokenKind::Gt,
                                        found: t.kind,
                                    },
                                    t.span,
                                ));
                            }
                        }
                    }
                    self.bump.alloc_slice(&args)
                } else {
                    self.bump.alloc_slice(&[])
                };

                TypeKind::Struct { name, generics }
            }

            _ => {
                return Err(DiagnosticError::new(
                    ParseErrorKind::UnexpectedToken {
                        expected: TokenKind::Ident,
                        found: tok.kind,
                    },
                    tok.span,
                ));
            }
        };

        Ok(Type {
            kind,
            nullable: false,
        })
    }
}
