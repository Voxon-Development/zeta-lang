use std::sync::Arc;

use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    Block, Generic, MatchArm, MutabilityState, NormalParam, Param, ParamPassingKind,
    ProvenanceAnnotation, ProvenancePathSegment, ProvenanceRoot, ThisParam, Type, TypeKind,
    Visibility,
};
use ir::errors::error::{DiagnosticError, ParseErrorKind};
use ir::tokens::TokenKind::LBracket;
use ir::tokens::{Cursor, TokenKind};
use zetaruntime::arena::GrowableAtomicBump;

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

        let mut generics = Vec::new();

        loop {
            let is_const = self.cursor.consume(TokenKind::Const);
            let is_static = if !is_const {
                self.cursor.consume(TokenKind::Static)
            } else {
                false
            };

            let (name, span) = self.cursor.expect_ident()?;

            let constraints = if self.cursor.consume(TokenKind::Colon) {
                let mut types = Vec::new();

                loop {
                    let ty = self.parse_type()?;
                    types.push(ty);

                    if !self.cursor.consume(TokenKind::Add) {
                        break;
                    }
                }

                self.bump.alloc_slice_immutable(&types)
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

        if !Self::try_consume_close_angle(&mut self.cursor, &mut self.pending_close_angle) {
            let token = self.cursor.peek_token();
            return Err(DiagnosticError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Gt,
                    found: token.kind,
                },
                token.span,
            ));
        }

        Ok(Some(self.bump.alloc_slice_immutable(&generics)))
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

        let mut params: Vec<Param<'a, 'bump>> = Vec::new();

        if self.cursor.peek() == TokenKind::RParen {
            self.cursor.advance();
            return Ok(Some(self.bump.alloc_slice_immutable(&params)));
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

            match self.cursor.peek() {
                TokenKind::Comma => {
                    self.cursor.advance();

                    // Allow trailing comma before ')'
                    if self.cursor.peek() == TokenKind::RParen {
                        self.cursor.advance();
                        break;
                    }
                }

                TokenKind::RParen => {
                    self.cursor.advance();
                    break;
                }

                _ => {
                    let token = self.cursor.peek_token();
                    self.diag.record(DiagnosticError::new(
                        ParseErrorKind::UnexpectedTokens {
                            expected: vec![TokenKind::Comma, TokenKind::RParen],
                            found: token.kind,
                        },
                        token.span,
                    ));

                    let kind = self.diag.synchronize(&mut self.cursor);
                    if matches!(
                        kind,
                        TokenKind::LBrace | TokenKind::RBrace | TokenKind::RParen | TokenKind::EOF
                    ) {
                        return Err(DiagnosticError::new(
                            ParseErrorKind::UnexpectedTokens {
                                expected: vec![TokenKind::Comma, TokenKind::RParen],
                                found: kind,
                            },
                            self.cursor.peek_token().span,
                        ));
                    }
                }
            }
        }

        Ok(Some(self.bump.alloc_slice_immutable(&params)))
    }

    pub fn try_consume_close_angle(cursor: &mut Cursor<'a, 'bump>, pending: &mut u8) -> bool {
        if *pending > 0 {
            *pending -= 1;
            return true;
        }
        match cursor.peek() {
            TokenKind::Gt => {
                cursor.advance();
                true
            }
            TokenKind::Shr => {
                cursor.advance();
                *pending += 1;
                true
            }
            TokenKind::UnsignedShr => {
                cursor.advance();
                *pending += 2;
                true
            }
            _ => false,
        }
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
        Self::parse_type_impl(
            self.bump.clone(),
            &mut self.cursor,
            &mut self.pending_close_angle,
        )
    }

    pub fn parse_type_impl(
        bump: Arc<GrowableAtomicBump<'bump>>,
        cursor: &mut Cursor<'a, 'bump>,
        pending: &mut u8,
    ) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        let nullable = cursor.consume(TokenKind::Question);
        match cursor.peek() {
            TokenKind::LBracket => {
                let kind = Self::parse_bracket_type_kind_impl(bump, cursor, pending)?;
                let nullable = cursor.consume(TokenKind::Question);
                return Ok(Type { kind, nullable });
            }
            TokenKind::BitAnd => {
                cursor.advance();

                let provenance = Self::parse_optional_provenance_impl(bump.clone(), cursor)?;

                if cursor.peek() == LBracket {
                    let kind = Self::parse_bracket_type_kind_impl(bump.clone(), cursor, pending)?;
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
                        let mut bounds = Vec::new();
                        bounds.push(Self::parse_core_type_impl(bump.clone(), cursor, pending)?);
                        while cursor.consume(TokenKind::Add) {
                            bounds.push(Self::parse_core_type_impl(bump.clone(), cursor, pending)?);
                        }
                        return Ok(Type {
                            kind: TypeKind::Ref {
                                inner: bump.alloc_value(Type {
                                    kind: TypeKind::Dyn {
                                        bounds: bump.alloc_slice_immutable(&bounds),
                                    },
                                    nullable: false,
                                }),
                                mutability_state: MutabilityState::Const,
                                provenance,
                            },
                            nullable: false,
                        });
                    }

                    let inner = Self::parse_core_type_impl(bump.clone(), cursor, pending)?;

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
            TokenKind::BitXor => {
                cursor.advance();

                let allocator = Self::parse_optional_provenance_impl(bump.clone(), cursor)?;

                if cursor.peek() == TokenKind::LBracket {
                    let bracket =
                        Self::parse_bracket_type_kind_impl(bump.clone(), cursor, pending)?;

                    let kind = match bracket {
                        TypeKind::Slice { inner } => TypeKind::OwnedPointer {
                            inner: bump.alloc_value_immutable(Type {
                                kind: TypeKind::Slice { inner },
                                nullable: false,
                            }),
                            allocator,
                        },

                        TypeKind::UnsafePointer { .. } => {
                            todo!("Handle error when ^ and [*] are mixed together.")
                        }

                        other => other,
                    };

                    return Ok(Type { kind, nullable });
                }

                let inner = Self::parse_core_type_impl(bump.clone(), cursor, pending)?;

                return Ok(Type {
                    kind: TypeKind::OwnedPointer {
                        inner: bump.alloc_value_immutable(inner),
                        allocator,
                    },
                    nullable,
                });
            }
            _ => {}
        }

        let is_dyn = cursor.consume(TokenKind::Dyn);

        if is_dyn {
            let mut bounds = Vec::new();

            bounds.push(Self::parse_core_type_impl(bump.clone(), cursor, pending)?);

            while cursor.consume(TokenKind::Add) {
                bounds.push(Self::parse_core_type_impl(bump.clone(), cursor, pending)?);
            }

            return Ok(Type {
                kind: TypeKind::Dyn {
                    bounds: bump.alloc_slice_immutable(&bounds),
                },
                nullable: false,
            });
        }

        let mut ty = Self::parse_core_type_impl(bump, cursor, pending)?;

        ty.nullable = nullable;

        Ok(ty)
    }

    /// Assumes `[` has already been consumed. Parses `]inner`, `N]inner`, or `*]mut/const inner`.
    fn parse_bracket_type_kind_inner_impl(
        bump: Arc<GrowableAtomicBump<'bump>>,
        cursor: &mut Cursor<'a, 'bump>,
        pending: &mut u8,
    ) -> Result<TypeKind<'a, 'bump>, DiagnosticError<'a>> {
        let token = cursor.peek_token();

        if token.kind == TokenKind::RBracket {
            cursor.advance();
            let inner = Self::parse_core_type_impl(bump.clone(), cursor, pending)?;
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

            let inner = Self::parse_core_type_impl(bump.clone(), cursor, pending)?;
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

            let inner = Self::parse_core_type_impl(bump.clone(), cursor, pending)?;
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
        bump: Arc<GrowableAtomicBump<'bump>>,
        cursor: &mut Cursor<'a, 'bump>,
    ) -> Result<Option<ProvenanceAnnotation<'bump>>, DiagnosticError<'a>> {
        // &self Player / &self.world Player
        if cursor.peek() == TokenKind::This {
            let save = cursor.pos();
            cursor.advance();

            if cursor.consume(TokenKind::Dot) {
                let (field, _) = cursor.expect_ident()?;
                if Self::starts_type_impl(cursor) {
                    let path = bump.alloc_slice_immutable(&[ProvenancePathSegment::Field(field)]);
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

    fn starts_type_impl(cursor: &Cursor<'a, 'bump>) -> bool {
        matches!(
            cursor.peek(),
            TokenKind::Ident | TokenKind::This | TokenKind::LBracket
        ) || cursor.peek().is_primitive_type()
    }

    /// Consumes `[` itself, then delegates. Use this when `[` hasn't been consumed yet.
    fn parse_bracket_type_kind_impl(
        bump: Arc<GrowableAtomicBump<'bump>>,
        cursor: &mut Cursor<'a, 'bump>,
        pending: &mut u8,
    ) -> Result<TypeKind<'a, 'bump>, DiagnosticError<'a>> {
        cursor.expect(TokenKind::LBracket)?;
        Self::parse_bracket_type_kind_inner_impl(bump, cursor, pending)
    }

    fn parse_core_type_impl(
        bump: Arc<GrowableAtomicBump<'bump>>,
        cursor: &mut Cursor<'a, 'bump>,
        pending: &mut u8,
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
            TokenKind::CharLiteral => return Ok(Type::char()),
            TokenKind::Str => return Ok(Type::string()),
            TokenKind::Void => return Ok(Type::void()),
            TokenKind::Never => return Ok(Type::never()),

            // `this` as a type (for self-referential method return types)
            TokenKind::This => return Ok(Type::this()),

            TokenKind::Underscore => return Ok(Type::infer()),

            TokenKind::LBracket => Self::parse_bracket_type_kind_inner_impl(bump, cursor, pending)?,

            TokenKind::Mul => {
                let mutability_token = cursor.expect_or(TokenKind::Mut, TokenKind::Const)?;
                let mutability_state = match mutability_token.kind {
                    TokenKind::Mut => MutabilityState::Mut,
                    // I wish rust knew that only Mut and Const is possible here :(
                    _ => MutabilityState::Const,
                };
                let inner = Self::parse_type_impl(bump.clone(), cursor, pending)?;
                let inner_ref = bump.alloc_value(inner);
                TypeKind::SafePointer {
                    inner: inner_ref,
                    mutability_state,
                }
            }

            TokenKind::BitXor => {
                let allocator = Self::parse_optional_provenance_impl(bump.clone(), cursor)?;
                let inner = Self::parse_type_impl(bump.clone(), cursor, pending)?;
                let inner_ref = bump.alloc_value(inner);
                TypeKind::OwnedPointer {
                    inner: inner_ref,
                    allocator,
                }
            }

            TokenKind::Func => {
                cursor.expect(TokenKind::LParen)?;
                let mut params: Vec<Type<'a, 'bump>> = Vec::new();
                while cursor.peek() != TokenKind::RParen {
                    params.push(Self::parse_type_impl(bump.clone(), cursor, pending)?);
                    if cursor.peek() == TokenKind::Comma {
                        cursor.advance();
                    }
                }
                cursor.expect(TokenKind::RParen)?;

                let return_type = if cursor.peek() == TokenKind::Colon {
                    cursor.advance();
                    Self::parse_type_impl(bump.clone(), cursor, pending)?
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
                let mut name = tok
                    .text
                    .ok_or_else(|| DiagnosticError::new(ParseErrorKind::EmptyIdent, tok.span))?;

                let mut path = Vec::new();

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
                    "never" => return Ok(Type::never()),
                    _ => {}
                }

                let generics = if cursor.peek() == TokenKind::Lt {
                    cursor.advance();
                    let mut args: Vec<Type<'a, 'bump>> = Vec::new();
                    loop {
                        args.push(Self::parse_type_impl(bump.clone(), cursor, pending)?);

                        if cursor.peek() == TokenKind::Comma {
                            cursor.advance();
                            continue;
                        }
                        if Self::try_consume_close_angle(cursor, pending) {
                            break;
                        }
                        let t = cursor.peek_token();
                        return Err(DiagnosticError::new(
                            ParseErrorKind::UnexpectedToken {
                                expected: TokenKind::Gt,
                                found: t.kind,
                            },
                            t.span,
                        ));
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

    pub fn parse_match_arms(
        &mut self,
    ) -> Result<&'bump [MatchArm<'a, 'bump>], DiagnosticError<'a>> {
        let mut arms = Vec::new();

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            match self.parse_match_arm() {
                Ok(arm) => arms.push(arm),
                Err(e) => {
                    self.diag.record(e);
                    let stop = self.recover_to_arm_boundary();
                    if stop == TokenKind::RBrace || stop == TokenKind::EOF {
                        break;
                    }
                }
            }
        }

        self.cursor.expect(TokenKind::RBrace)?;
        Ok(self.bump.alloc_slice_immutable(&arms))
    }

    pub fn parse_impl_target(&mut self) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        if self.cursor.peek() == TokenKind::LBracket {
            let checkpoint = self.cursor.pos();
            self.cursor.advance(); // consume `[`
            if self.cursor.peek() == TokenKind::RBracket {
                self.cursor.advance(); // consume `]`
                match self.cursor.peek() {
                    TokenKind::By | TokenKind::LBrace => {
                        return Ok(Type {
                            kind: TypeKind::AnySlice,
                            nullable: false,
                        });
                    }
                    _ => {}
                }
            }
            self.cursor.reset(checkpoint);
        }
        self.parse_type()
    }

    /// Parses one `case pattern [if guard] -> body` arm, with targeted
    /// diagnostics for the two mistakes in your example: a bare pattern
    /// with no `case`, and `=>` instead of `->`.
    fn parse_match_arm(&mut self) -> Result<MatchArm<'a, 'bump>, DiagnosticError<'a>> {
        if self.cursor.peek() != TokenKind::Case {
            let span = self.cursor.peek_token().span;
            if self.looks_like_headless_arm() {
                return Err(DiagnosticError::new(
                    ParseErrorKind::MatchArmMissingCase {
                        found: self.cursor.peek(),
                    },
                    span,
                ));
            }
            return Err(DiagnosticError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Case,
                    found: self.cursor.peek(),
                },
                span,
            ));
        }

        let case_token = self.cursor.expect(TokenKind::Case)?;
        let pattern = self.parse_pattern()?;

        let guard = if self.cursor.consume(TokenKind::If) {
            let guard_expr = self.parse_expr(0)?;
            Some(self.bump.alloc_value_immutable(guard_expr))
        } else {
            None
        };

        if self.cursor.peek() == TokenKind::FatArrow {
            let span = self.cursor.peek_token().span;
            return Err(DiagnosticError::new(
                ParseErrorKind::MatchArmWrongArrow,
                span,
            ));
        }
        self.cursor.expect(TokenKind::Arrow)?;

        let block = if self.cursor.peek() == TokenKind::LBrace {
            self.parse_block()?
        } else {
            let span = self.cursor.peek_token().span;
            let stmt = match self.cursor.peek() {
                TokenKind::Return => self.parse_return_stmt()?,
                TokenKind::Break => self.parse_break_stmt()?,
                TokenKind::Continue => self.parse_continue_stmt()?,
                _ => self.parse_expr_stmt()?,
            };
            let stmt_ref = self.bump.alloc_value_immutable(stmt);
            Block {
                block: self.bump.alloc_slice_immutable(&[*stmt_ref]),
                span,
            }
        };

        self.cursor.consume(TokenKind::Comma);

        Ok(MatchArm {
            pattern,
            guard,
            block: self.bump.alloc_value_immutable(block),
            span: case_token.span,
        })
    }

    /// Skips tokens, tracking paren/bracket nesting, until a top-level
    /// comma (consumed) or the match's closing `}` (not consumed).
    fn recover_to_arm_boundary(&mut self) -> TokenKind {
        let mut depth = 0i32;
        loop {
            match self.cursor.peek() {
                TokenKind::EOF => return TokenKind::EOF,
                TokenKind::RBrace if depth == 0 => return TokenKind::RBrace,
                TokenKind::Comma if depth == 0 => {
                    self.cursor.advance();
                    return TokenKind::Comma;
                }
                TokenKind::LParen | TokenKind::LBracket => {
                    depth += 1;
                    self.cursor.advance();
                }
                TokenKind::RParen | TokenKind::RBracket => {
                    depth -= 1;
                    self.cursor.advance();
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }
    }

    fn looks_like_headless_arm(&mut self) -> bool {
        if !matches!(
            self.cursor.peek(),
            TokenKind::Ident
                | TokenKind::Underscore
                | TokenKind::Number
                | TokenKind::String
                | TokenKind::BooleanTrue
                | TokenKind::BooleanFalse
                | TokenKind::LParen
                | TokenKind::LBracket
        ) {
            return false;
        }

        let mut cursor = self.cursor.clone();
        let mut depth = 0i32;
        loop {
            match cursor.peek() {
                TokenKind::EOF => return false,
                TokenKind::LParen | TokenKind::LBracket => {
                    depth += 1;
                    cursor.advance();
                }
                TokenKind::RParen | TokenKind::RBracket => {
                    depth -= 1;
                    cursor.advance();
                }
                TokenKind::Comma if depth == 0 => return false,
                TokenKind::RBrace if depth == 0 => return false,
                TokenKind::Arrow | TokenKind::FatArrow if depth == 0 => return true,
                _ => {
                    cursor.advance();
                }
            }
        }
    }
}
