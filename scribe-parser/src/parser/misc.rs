use crate::parser::descent_parser::DescentParser;
use ir::ast::{Generic, NormalParam, Param, ThisParam, Type, TypeKind, Visibility};
use ir::errors::error::DiagnosticError;
use ir::hir::StrId;
use ir::span::SourceSpan;
use ir::tokens::{Token, TokenKind};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_generics(
        &mut self,
    ) -> Result<Option<&'bump [Generic<'a, 'bump>]>, DiagnosticError<'a>> {
        // No generics present
        if self.cursor.peek() != TokenKind::Lt {
            return Ok(None);
        }

        self.cursor.bump(); // consume '<'

        let mut generics = Vec::new_in(self.bump);

        loop {
            // Optional const/static
            let is_const = self.cursor.consume(TokenKind::Const);
            let is_static = if !is_const {
                self.cursor.consume(TokenKind::Static)
            } else {
                false
            };

            // Identifier
            let (name, span) = self.cursor.expect_ident()?;

            // Optional constraints
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

    pub fn parse_params(&mut self) -> Result<Option<&'bump [Param<'a, 'bump>]>, DiagnosticError<'a>> {
        // No generics present
        if self.cursor.peek() != TokenKind::LParen {
            return Ok(None);
        }

        self.cursor.bump(); // consume '('

        let mut params: Vec<Param<'a, 'bump>, &GrowableBump> = Vec::new_in(self.bump);

        if self.cursor.peek() == TokenKind::RParen {
            self.cursor.advance();
            return Ok(Some(self.bump.alloc_slice_copy(&params)));
        }

        loop {
            let mut is_mut = false;

            // Check for & or &mut at the start (reference parameter like &this or &mut self)
            let mut is_aligned_ptr = false;
            let mut is_ptr = false;
            let mut is_reference = false;
            let mut is_mut_reference = false;

            match self.cursor.peek() {
                TokenKind::BitAnd => {
                    self.cursor.advance();
                    if self.cursor.consume(TokenKind::Mut) {
                        is_mut_reference = true;
                    } else {
                        is_reference = true;
                    }
                }
                TokenKind::Mul => {
                    self.cursor.advance();
                    if self.cursor.peek() != TokenKind::Mul {
                        is_aligned_ptr = true;
                    } else {
                        is_ptr = true;
                    }
                }
                _ => {}
            }

            // Check for mut keyword before parameter name (for by-value mutable)
            if self.cursor.peek() == TokenKind::Mut {
                self.cursor.advance();
                is_mut = true;
            }

            // Identifier (or `this` keyword)
            let (name, span): (StrId, SourceSpan) = match self.cursor.peek() {
                TokenKind::This => {
                    let tok = self.cursor.peek_token();
                    self.cursor.advance();
                    (
                        ir::hir::StrId::new(self.string_pool.intern("this")),
                        tok.span,
                    )
                }
                _ => self.cursor.expect_ident()?,
            };

            match name.as_str() {
                "this" => {
                    params.push(Param::This(self.bump.alloc_value_immutable(ThisParam {
                        is_mut,
                        is_move: !is_reference && !is_mut_reference && !is_ptr && !is_aligned_ptr,
                    })));
                }
                _ => {
                    let param_type: Type<'a, 'bump> = if self.cursor.consume(TokenKind::Colon) {
                        self.parse_type()?
                    } else {
                        panic!("Param type not found.")
                    };

                    let default_value = if self.cursor.consume(TokenKind::Eq) {
                        Some(Self::parse_expr_inner(
                            &mut self.cursor,
                            self.bump,
                            0,
                            true,
                        )?)
                    } else {
                        None
                    };

                    params.push(Param::Normal(self.bump.alloc_value_immutable(
                        NormalParam {
                            is_mut,
                            is_move: !is_reference && !is_ptr && !is_aligned_ptr,
                            name,
                            type_annotation: param_type,
                            visibility: Visibility::Public,
                            default_value,
                        },
                    )));
                }
            }

            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }

        self.cursor.expect(TokenKind::RParen)?;

        Ok(Some(self.bump.alloc_slice_copy(&params)))
    }

    pub(crate) fn parse_type(&mut self) -> Result<Type<'a, 'bump>, DiagnosticError<'a>> {
        let error = match self.cursor.peek() {
            TokenKind::LBrace => {
                self.cursor.advance();
                let mut error_types = Vec::new();
                loop {
                    let (name, _span) = self.cursor.expect_ident()?;
                    error_types.push(name);
                    match self.cursor.peek() {
                        TokenKind::Comma => {
                            self.cursor.advance();
                        }
                        TokenKind::RBrace => {
                            self.cursor.advance();
                            break;
                        }
                        _ => {
                            let tok = self.cursor.peek_token();
                            return Err(ParserError::UnexpectedToken {
                                expected: TokenKind::RBrace,
                                found: tok.kind,
                                span: tok.span,
                            });
                        }
                    }
                }
                // consume the `!`
                let bang = self.cursor.peek_token();
                if self.cursor.peek() != TokenKind::LogicalNot {
                    return Err(ParserError::UnexpectedToken {
                        expected: TokenKind::LogicalNot,
                        found: bang.kind,
                        span: bang.span,
                    });
                }
                self.cursor.advance();
                true
            }

            TokenKind::Ident => {
                if self.cursor.peek_n(1) == TokenKind::LogicalNot {
                    self.cursor.advance(); // consume error type ident
                    self.cursor.advance(); // consume `!`
                    true
                } else {
                    false
                }
            }

            _ => false,
        };

        let mut ty = self.parse_core_type()?;

        let nullable = if self.cursor.peek() == TokenKind::Question {
            self.cursor.advance();
            true
        } else {
            false
        };

        ty.error = error;
        ty.nullable = nullable;
        Ok(ty)
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
                // Check for **T (raw pointer) vs *T (aligned pointer)
                let raw = if self.cursor.peek() == TokenKind::Mul {
                    self.cursor.advance();
                    true
                } else {
                    false
                };
                let inner = self.parse_core_type()?;
                let inner_ref = self.bump.alloc_value(inner);
                TypeKind::Pointer {
                    inner: inner_ref,
                    raw,
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
                    .ok_or_else(|| ParserError::EmptyIdent { location: tok.span })?;

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
                                return Err(ParserError::UnexpectedToken {
                                    expected: TokenKind::Gt,
                                    found: t.kind,
                                    span: t.span,
                                });
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
                return Err(ParserError::UnexpectedToken {
                    expected: TokenKind::Ident,
                    found: tok.kind,
                    span: tok.span,
                });
            }
        };

        Ok(Type {
            kind,
            nullable: false,
            error: false,
        })
    }
}
