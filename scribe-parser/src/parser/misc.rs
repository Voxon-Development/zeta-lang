use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    Generic, MutabilityState, NormalParam, Param, ParamPassingKind, ProvenanceAnnotation,
    ProvenancePathSegment, ProvenanceRoot, ThisParam, Type, TypeKind, Visibility,
};
use ir::errors::error::{DiagnosticError, ParseErrorKind};
use ir::tokens::TokenKind::LBracket;
use ir::tokens::{Cursor, TokenKind};
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
        Self::parse_type_impl(&self.bump, &mut self.cursor)
    }

    pub fn parse_type_impl(
        bump: &&'bump GrowableBump,
        cursor: &mut Cursor<'a>,
    ) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        match cursor.peek() {
            TokenKind::LBracket => {
                let kind = Self::parse_bracket_type_kind_impl(bump, cursor)?;
                let nullable = cursor.consume(TokenKind::Question);
                return Ok(Type { kind, nullable });
            }
            TokenKind::BitAnd => {
                cursor.advance();

                let provenance = Self::parse_optional_provenance_impl(bump, cursor)?;

                if cursor.peek() == LBracket {
                    let kind = Self::parse_bracket_type_kind_impl(bump, cursor)?;
                    if let TypeKind::UnsafePointer { .. } = kind {
                        todo!("Handle error when & and [*] are mixed together.")
                    }

                    let nullable = cursor.consume(TokenKind::Question);

                    return Ok(Type { kind, nullable });
                } else {
                    let mutability_state = if cursor.consume(TokenKind::Mut) {
                        MutabilityState::Mut
                    } else {
                        MutabilityState::Const
                    };

                    let is_dyn = cursor.consume(TokenKind::Dyn);

                    if is_dyn {
                        let mut bounds = Vec::new_in(*bump);
                        bounds.push(Self::parse_core_type_impl(bump, cursor)?);
                        while cursor.consume(TokenKind::Add) {
                            bounds.push(Self::parse_core_type_impl(bump, cursor)?);
                        }
                        return Ok(Type {
                            kind: TypeKind::Ref {
                                inner: bump.alloc_value(Type {
                                    kind: TypeKind::Dyn {
                                        bounds: bump.alloc_slice_copy(&bounds),
                                    },
                                    nullable: false,
                                }),
                                mutability_state: MutabilityState::Const,
                                provenance,
                            },
                            nullable: false,
                        });
                    }

                    // parse_core_type now handles `[` directly, so `&mut [4]i64` works.
                    let inner = Self::parse_core_type_impl(bump, cursor)?;
                    let nullable = cursor.consume(TokenKind::Question);

                    return Ok(Type {
                        kind: TypeKind::Ref {
                            inner: bump.alloc_value_immutable(inner),
                            mutability_state,
                            provenance,
                        },
                        nullable,
                    });
                }
            }
            _ => {}
        }

        let is_dyn = cursor.consume(TokenKind::Dyn);

        if is_dyn {
            let mut bounds = Vec::new_in(*bump);

            bounds.push(Self::parse_core_type_impl(bump, cursor)?);

            while cursor.consume(TokenKind::Add) {
                bounds.push(Self::parse_core_type_impl(bump, cursor)?);
            }

            return Ok(Type {
                kind: TypeKind::Dyn {
                    bounds: bump.alloc_slice_copy(&bounds),
                },
                nullable: false,
            });
        }

        let mut ty = Self::parse_core_type_impl(bump, cursor)?;

        if cursor.consume(TokenKind::Question) {
            ty.nullable = true;
        }

        Ok(ty)
    }

    /// Assumes `[` has already been consumed. Parses `]inner`, `N]inner`, or `*]mut/const inner`.
    fn parse_bracket_type_kind_inner_impl(
        bump: &&'bump GrowableBump,
        cursor: &mut Cursor<'a>,
    ) -> Result<TypeKind<'a, 'bump>, DiagnosticError<'a>> {
        let token = cursor.peek_token();

        if token.kind == TokenKind::RBracket {
            cursor.advance();
            let inner = Self::parse_core_type_impl(bump, cursor)?;
            let inner_ref = bump.alloc_value(inner);
            return Ok(TypeKind::Slice { inner: inner_ref });
        } else if token.kind == TokenKind::Number {
            cursor.advance();
            cursor.expect(TokenKind::RBracket)?;

            // SAFETY: a token with kind TokenKind::Number always comes with text.
            let number_unparsed = unsafe { token.text.unwrap_unchecked() };
            let length = number_unparsed.as_str().parse::<usize>().map_err(|_| {
                DiagnosticError::new(
                    ParseErrorKind::UnexpectedToken {
                        expected: TokenKind::Number,
                        found: token.kind,
                    },
                    cursor.peek_token().span,
                )
            })?;

            let inner = Self::parse_core_type_impl(bump, cursor)?;
            let inner_ref = bump.alloc_value(inner);
            return Ok(TypeKind::Array {
                inner: inner_ref,
                length,
            });
        } else if token.kind == TokenKind::Mul {
            cursor.advance(); // consume *
            cursor.expect(TokenKind::RBracket)?; // consume ]

            let mutability_token = cursor.expect_or(TokenKind::Mut, TokenKind::Const)?;
            let mutability_state = match mutability_token.kind {
                TokenKind::Mut => MutabilityState::Mut,
                _ => MutabilityState::Const,
            };

            let inner = Self::parse_core_type_impl(bump, cursor)?;
            let inner_ref = bump.alloc_value(inner);
            return Ok(TypeKind::UnsafePointer {
                inner: inner_ref,
                mutability_state,
            });
        } else {
            return Err(DiagnosticError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Mul,
                    found: cursor.peek_token().kind,
                },
                cursor.peek_token().span,
            ));
        }
    }

    fn parse_optional_provenance_impl(
        bump: &&'bump GrowableBump,
        cursor: &mut Cursor<'a>,
    ) -> Result<Option<ProvenanceAnnotation<'bump>>, DiagnosticError<'a>> {
        // &self Player / &self.world Player
        if cursor.peek() == TokenKind::This {
            let save = cursor.pos();
            cursor.advance();

            if cursor.consume(TokenKind::Dot) {
                let (field, _) = cursor.expect_ident()?;
                if Self::starts_type_impl(cursor) {
                    let path = bump.alloc_slice_copy(&[ProvenancePathSegment::Field(field)]);
                    return Ok(Some(ProvenanceAnnotation {
                        root: ProvenanceRoot::ThisRoot,
                        path,
                    }));
                }
            } else if Self::starts_type_impl(cursor) {
                return Ok(Some(ProvenanceAnnotation {
                    root: ProvenanceRoot::ThisRoot,
                    path: &[],
                }));
            }

            cursor.reset(save); // wasn't provenance, e.g. bare `&this` as a type
            return Ok(None);
        }

        // &world Player
        if cursor.peek() == TokenKind::Ident {
            let save = cursor.pos();
            let (name, _) = cursor.expect_ident()?;
            if Self::starts_type_impl(cursor) {
                return Ok(Some(ProvenanceAnnotation {
                    root: ProvenanceRoot::Var(name),
                    path: &[],
                }));
            }
            cursor.reset(save);
        }

        Ok(None)
    }

    fn starts_type_impl(cursor: &Cursor<'a>) -> bool {
        matches!(
            cursor.peek(),
            TokenKind::Ident | TokenKind::This | TokenKind::LBracket
        ) || cursor.peek().is_primitive_type()
    }

    /// Consumes `[` itself, then delegates. Use this when `[` hasn't been consumed yet.
    fn parse_bracket_type_kind_impl(
        bump: &&'bump GrowableBump,
        cursor: &mut Cursor<'a>,
    ) -> Result<TypeKind<'a, 'bump>, DiagnosticError<'a>> {
        cursor.expect(TokenKind::LBracket)?;
        Self::parse_bracket_type_kind_inner_impl(bump, cursor)
    }

    fn parse_core_type_impl(
        bump: &&'bump GrowableBump,
        cursor: &mut Cursor<'a>,
    ) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        let tok = cursor.bump();

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
            TokenKind::Usize => return Ok(Type::usize()),
            TokenKind::Isize => return Ok(Type::isize()),
            TokenKind::F32 => return Ok(Type::f32()),
            TokenKind::F64 => return Ok(Type::f64()),
            TokenKind::Boolean => return Ok(Type::boolean()),
            TokenKind::Char => return Ok(Type::char()),
            TokenKind::Str => return Ok(Type::string()),
            TokenKind::Void => return Ok(Type::void()),

            // `this` as a type (for self-referential method return types)
            TokenKind::This => return Ok(Type::this()),

            TokenKind::Underscore => return Ok(Type::infer()),

            TokenKind::LBracket => Self::parse_bracket_type_kind_inner_impl(bump, cursor)?,

            TokenKind::Mul => {
                let mutability_token = cursor.expect_or(TokenKind::Mut, TokenKind::Const)?;
                let mutability_state = match mutability_token.kind {
                    TokenKind::Mut => MutabilityState::Mut,
                    // I wish rust knew that only Mut and Const is possible here :(
                    _ => MutabilityState::Const,
                };
                let inner = Self::parse_core_type_impl(bump, cursor)?;
                let inner_ref = bump.alloc_value(inner);
                TypeKind::SafePointer {
                    inner: inner_ref,
                    mutability_state,
                }
            }

            TokenKind::BitXor => {
                let inner = Self::parse_core_type_impl(bump, cursor)?;
                let inner_ref = bump.alloc_value(inner);
                TypeKind::OwnedPointer { inner: inner_ref }
            }

            TokenKind::Func => {
                cursor.expect(TokenKind::LParen)?;
                let mut params: Vec<Type<'a, 'bump>> = Vec::new();
                while cursor.peek() != TokenKind::RParen {
                    params.push(Self::parse_type_impl(bump, cursor)?);
                    if cursor.peek() == TokenKind::Comma {
                        cursor.advance();
                    }
                }
                cursor.expect(TokenKind::RParen)?;

                let return_type = if cursor.peek() == TokenKind::Arrow {
                    cursor.advance();
                    Self::parse_core_type_impl(bump, cursor)?
                } else {
                    Type::void()
                };

                let params_bump = bump.alloc_slice(&params);
                let ret_ref = bump.alloc_value(return_type);
                TypeKind::Lambda {
                    params: params_bump,
                    return_type: ret_ref,
                }
            }

            TokenKind::Ident => {
                let name = tok
                    .text
                    .ok_or_else(|| DiagnosticError::new(ParseErrorKind::EmptyIdent, tok.span))?;

                let mut path = Vec::new();
                let mut name = name;

                while cursor.peek() == TokenKind::ColonColon {
                    cursor.advance(); // ::

                    path.push(name);

                    let tok = cursor.expect(TokenKind::Ident)?;
                    name = tok.text.ok_or_else(|| {
                        DiagnosticError::new(ParseErrorKind::EmptyIdent, tok.span)
                    })?;
                }

                if cursor.peek() == TokenKind::Dot {
                    cursor.advance();

                    let tok = cursor.expect(TokenKind::Ident)?;
                    name = tok.text.ok_or_else(|| {
                        DiagnosticError::new(ParseErrorKind::EmptyIdent, tok.span)
                    })?;
                }

                let path = bump.alloc_slice(&path);

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

                let generics = if cursor.peek() == TokenKind::Lt {
                    cursor.advance();
                    let mut args: Vec<Type<'a, 'bump>> = Vec::new();
                    loop {
                        args.push(Self::parse_type_impl(bump, cursor)?);
                        match cursor.peek() {
                            TokenKind::Comma => {
                                cursor.advance();
                            }
                            TokenKind::Gt => {
                                cursor.advance();
                                break;
                            }
                            _ => {
                                let t = cursor.peek_token();
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
                    bump.alloc_slice(&args)
                } else {
                    bump.alloc_slice(&[])
                };

                TypeKind::Struct {
                    name,
                    path,
                    generics,
                }
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
