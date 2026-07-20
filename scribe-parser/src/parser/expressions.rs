use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    ElseBranch, ErrorHandlerBranch, ErrorHandlerPattern, FieldInit, IfStmt, LambdaModifier,
    LambdaParam, Type,
};
use ir::tokens::TokenKind;
use ir::{
    ast::{Expr, Op},
    errors::error::{DiagnosticError, ParseErrorKind},
};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
    'a: 'bump,
{
    pub fn parse_expr(&mut self, min_bp: u8) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        self.parse_expr_inner(min_bp, true)
    }

    pub fn parse_expr_inner(
        &mut self,
        min_bp: u8,
        allow_struct_init: bool,
    ) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let mut lhs: Expr<'a, 'bump> = self.parse_prefix()?;

        loop {
            loop {
                match self.cursor.peek() {
                    TokenKind::LParen => {
                        self.cursor.advance();

                        let mut args = Vec::new_in(self.bump);

                        if self.cursor.peek() != TokenKind::RParen {
                            loop {
                                let arg = self.parse_expr_inner(0, true)?;
                                args.push(arg);

                                if !self.cursor.consume(TokenKind::Comma) {
                                    break;
                                }
                            }
                        }

                        let span = self.cursor.peek_token().span;
                        self.cursor.expect(TokenKind::RParen)?;

                        let callee = self.bump.alloc_value_immutable(lhs);

                        lhs = Expr::Call {
                            callee,
                            generic_args: &[],
                            arguments: self.bump.alloc_slice_copy(&args),
                            span,
                        };
                    }

                    TokenKind::Dot => {
                        self.cursor.advance();

                        if self.cursor.consume(TokenKind::Mul) {
                            let span = self.cursor.peek_token().span;

                            lhs = Expr::Deref {
                                expr: self.bump.alloc_value_immutable(lhs),
                                span,
                            };
                        } else {
                            let (field_name, span) = self.cursor.expect_ident()?;

                            lhs = if let Expr::ModulePath { segments, .. } = lhs {
                                // First `.` after a module path stops the module
                                // walk and names a type/static-field/function.
                                Expr::ModuleAccess {
                                    segments,
                                    member: field_name,
                                    span,
                                }
                            } else {
                                Expr::FieldAccess {
                                    object: self.bump.alloc_value_immutable(lhs),
                                    field: field_name,
                                    span,
                                }
                            };
                        }
                    }

                    TokenKind::LBracket => {
                        self.cursor.advance();

                        let index = self.parse_expr_inner(0, true)?;

                        let span = self.cursor.peek_token().span;

                        self.cursor.expect(TokenKind::RBracket)?;

                        lhs = Expr::ArrayIndex {
                            expr: self.bump.alloc_value_immutable(lhs),
                            index: self.bump.alloc_value_immutable(index),
                            span,
                        };
                    }

                    TokenKind::Lt if self.is_generic_argument_list() => {
                        self.cursor.advance();

                        let mut generic_args = Vec::new_in(self.bump);

                        if self.cursor.peek() != TokenKind::Gt {
                            loop {
                                let arg = self.parse_type()?;
                                generic_args.push(arg);

                                if !self.cursor.consume(TokenKind::Comma) {
                                    break;
                                }
                            }
                        } else {
                            todo!(
                                "Handle error when trying to call like `hello<>()` because you cannot have an empty generic list"
                            )
                        }

                        self.cursor.expect(TokenKind::Gt)?;

                        let callee = self.bump.alloc_value_immutable(lhs);
                        let type_args = self.bump.alloc_slice_copy(&generic_args);

                        if self.cursor.peek() == TokenKind::LBrace && allow_struct_init {
                            // `Ident<T> { field: value, .. }`
                            lhs = self.parse_struct_init_fields(callee, type_args)?;
                        } else {
                            // `Ident<T>(args...)`
                            self.cursor.expect(TokenKind::LParen)?;

                            let mut args = Vec::new_in(self.bump);

                            if self.cursor.peek() != TokenKind::RParen {
                                loop {
                                    let arg = self.parse_expr_inner(0, true)?;
                                    args.push(arg);

                                    if !self.cursor.consume(TokenKind::Comma) {
                                        break;
                                    }
                                }
                            }

                            let span = self.cursor.peek_token().span;
                            self.cursor.expect(TokenKind::RParen)?;

                            lhs = Expr::Call {
                                callee,
                                generic_args: type_args,
                                arguments: self.bump.alloc_slice_copy(&args),
                                span,
                            };
                        }
                    }

                    TokenKind::LBrace => {
                        if allow_struct_init {
                            let (callee, type_args) = match lhs {
                                Expr::Ident { .. } => {
                                    let generic_args: &'bump [Type<'a, 'bump>] = &[];
                                    (self.bump.alloc_value_immutable(lhs), generic_args)
                                }

                                Expr::Call {
                                    callee,
                                    generic_args,
                                    arguments,
                                    ..
                                } if arguments.is_empty() => (callee, generic_args),

                                _ => break,
                            };

                            lhs = self.parse_struct_init_fields(callee, type_args)?;
                        } else {
                            break;
                        }
                    }

                    _ => break,
                }
            }

            if self.cursor.peek() == TokenKind::As {
                const CAST_BP: u8 = 75;
                if CAST_BP >= min_bp {
                    let span = self.cursor.peek_token().span;
                    self.cursor.advance(); // consume 'as'
                    let target_type = self.parse_type()?;

                    lhs = Expr::Cast {
                        expr: self.bump.alloc_value_immutable(lhs),
                        target_type,
                        span,
                    };
                    continue;
                }
            }

            let op: Op = match self.cursor.peek() {
                TokenKind::Add => Op::Add,
                TokenKind::Sub => Op::Sub,
                TokenKind::Mul => Op::Mul,
                TokenKind::Div => Op::Div,
                TokenKind::Mod => Op::Mod,

                TokenKind::BitAnd => Op::BitAnd,
                TokenKind::BitOr => Op::BitOr,
                TokenKind::BitXor => Op::BitXor,
                TokenKind::Shl => Op::Shl,
                TokenKind::Shr => Op::Shr,

                TokenKind::Eq => Op::Eq,
                TokenKind::Ne => Op::Neq,
                TokenKind::Lt => Op::Lt,
                TokenKind::Le => Op::Lte,
                TokenKind::Gt => Op::Gt,
                TokenKind::Ge => Op::Gte,

                TokenKind::Assign => Op::Assign,
                TokenKind::AddAssign => Op::AddAssign,
                TokenKind::SubAssign => Op::SubAssign,
                TokenKind::MulAssign => Op::MulAssign,
                TokenKind::DivAssign => Op::DivAssign,
                TokenKind::ModAssign => Op::ModAssign,

                TokenKind::ShlAssign => Op::ShlAssign,
                TokenKind::ShrAssign => Op::ShrAssign,

                TokenKind::AndAssign => Op::BitAndAssign,
                TokenKind::OrAssign => Op::BitOrAssign,
                TokenKind::XorAssign => Op::BitXorAssign,

                TokenKind::DotDot => Op::Range,
                TokenKind::DotDotLt => Op::RangeExcl,

                _ => break,
            };

            let (l_bp, r_bp) = op.binding_power();

            if l_bp < min_bp {
                break;
            }

            let span = self.cursor.peek_token().span;

            self.cursor.advance();

            let rhs = self.parse_expr_inner(r_bp, allow_struct_init)?;

            lhs = match op {
                Op::Eq | Op::Neq | Op::Lt | Op::Lte | Op::Gt | Op::Gte => Expr::Comparison {
                    lhs: self.bump.alloc_value_immutable(lhs),
                    op,
                    rhs: self.bump.alloc_value_immutable(rhs),
                    span,
                },

                _ => Expr::Binary {
                    left: self.bump.alloc_value_immutable(lhs),
                    op,
                    right: self.bump.alloc_value_immutable(rhs),
                    span,
                },
            };
        }

        Ok(lhs)
    }

    fn parse_struct_init_fields(
        &mut self,
        callee: &'bump Expr<'a, 'bump>,
        type_args: &'bump [Type<'a, 'bump>],
    ) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        self.cursor.advance(); // consume '{'

        let mut args: Vec<FieldInit<'a, 'bump>, &GrowableBump<'bump>> = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            let (field_name, field_span) = self.cursor.expect_ident()?;

            let value = if self.cursor.consume(TokenKind::Colon) {
                self.parse_expr_inner(0, true)?
            } else {
                Expr::Ident {
                    name: field_name,
                    span: field_span,
                }
            };

            args.push(FieldInit {
                name: field_name,
                name_span: field_span,
                value,
            });

            if !self.cursor.consume(TokenKind::Comma) {
                break;
            }
        }

        let span = self.cursor.peek_token().span;
        self.cursor.expect(TokenKind::RBrace)?;

        Ok(Expr::StructInit {
            callee,
            type_args,
            arguments: self.bump.alloc_slice_copy(args.as_slice()),
            span,
        })
    }

    fn parse_if_expr(&mut self) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let token = self.cursor.expect(TokenKind::If)?;

        self.cursor.expect(TokenKind::LParen)?;
        let condition = self.parse_expr(0)?;
        self.cursor.expect(TokenKind::RParen)?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.cursor.consume(TokenKind::Else) {
            if self.cursor.peek() == TokenKind::If {
                let expr = self.parse_if_expr()?;

                match expr {
                    Expr::If { if_stmt, .. } => {
                        Some(self.bump.alloc_value_immutable(ElseBranch::If(if_stmt)))
                    }

                    _ => unreachable!(),
                }
            } else {
                let block = self.parse_block()?;

                Some(self.bump.alloc_value_immutable(ElseBranch::Else(
                    self.bump.alloc_value_immutable(block),
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

        Ok(Expr::If {
            if_stmt: self.bump.alloc_value_immutable(if_stmt),
            span: token.span,
        })
    }

    fn is_generic_argument_list(&mut self) -> bool {
        let mut cursor = self.cursor.clone();

        if cursor.peek() != TokenKind::Lt {
            return false;
        }

        cursor.advance(); // consume '<'

        if cursor.peek() != TokenKind::Gt {
            loop {
                match Self::parse_type_impl(&self.bump, &mut cursor) {
                    Ok(arg) => arg,
                    Err(_) => return false,
                };

                if !cursor.consume(TokenKind::Comma) {
                    break;
                }
            }
        } else {
            todo!(
                "Handle error when trying to call like `hello<>()` because you cannot have an empty generic list"
            )
        }

        if !cursor.consume(TokenKind::Gt) {
            return false;
        }

        matches!(
            cursor.peek(),
            TokenKind::LParen | TokenKind::LBrace | TokenKind::Dot | TokenKind::ColonColon
        )
    }

    fn parse_prefix(&mut self) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let tok = self.cursor.peek_token();

        match tok.kind {
            TokenKind::If => self.parse_if_expr(),

            TokenKind::Dollar => {
                self.cursor.advance(); // consume '$'
                let (name, _name_span) = self.cursor.expect_ident()?;

                let mut generic_args = Vec::new_in(self.bump);
                if self.cursor.consume(TokenKind::Lt) {
                    if self.cursor.peek() != TokenKind::Gt {
                        loop {
                            let arg = self.parse_type()?;
                            generic_args.push(arg);
                            if !self.cursor.consume(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    self.cursor.expect(TokenKind::Gt)?;
                }

                self.cursor.expect(TokenKind::LParen)?;
                let mut args = Vec::new_in(self.bump);
                if self.cursor.peek() != TokenKind::RParen {
                    loop {
                        let arg = self.parse_expr_inner(0, true)?;
                        args.push(arg);
                        if !self.cursor.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                let end_span = self.cursor.peek_token().span;
                self.cursor.expect(TokenKind::RParen)?;

                Ok(Expr::Intrinsic {
                    name,
                    generic_args: self.bump.alloc_slice_copy(&generic_args),
                    arguments: self.bump.alloc_slice_copy(&args),
                    span: tok.span.merge(end_span),
                })
            }
            TokenKind::Number => {
                self.cursor.advance();
                let text = tok.text.unwrap_or_default();
                let value = text.as_str().parse::<i64>().unwrap_or(0);
                Ok(Expr::Number {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::Undefined => {
                self.cursor.advance();
                Ok(Expr::Undefined { span: tok.span })
            }

            TokenKind::LBracket => {
                self.cursor.advance();

                let mut elements = Vec::new_in(self.bump);

                if self.cursor.peek() != TokenKind::RBracket {
                    loop {
                        let elem = self.parse_expr_inner(0, true)?;
                        elements.push(elem);

                        if !self.cursor.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                }

                let end_span = self.cursor.peek_token().span;
                self.cursor.expect(TokenKind::RBracket)?;

                Ok(Expr::ArrayLiteral {
                    elements: self.bump.alloc_slice_copy(&elements),
                    span: tok.span.merge(end_span),
                })
            }
            TokenKind::Decimal => {
                self.cursor.advance();
                let text = tok.text.unwrap_or_default();
                let value = text.as_str().parse::<f64>().unwrap_or(0.0);
                Ok(Expr::Decimal {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::String => {
                self.cursor.advance();
                let value = tok.text.unwrap_or_default();
                Ok(Expr::String {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::BooleanTrue => {
                self.cursor.advance();
                Ok(Expr::Boolean {
                    value: true,
                    span: tok.span,
                })
            }
            TokenKind::BooleanFalse => {
                self.cursor.advance();
                Ok(Expr::Boolean {
                    value: false,
                    span: tok.span,
                })
            }
            TokenKind::Null => {
                self.cursor.advance();
                Ok(Expr::Null { span: tok.span })
            }
            TokenKind::Char => {
                self.cursor.advance();
                let text = tok.text.unwrap_or_default();
                let value = text.as_str().chars().next().unwrap_or('\0');
                Ok(Expr::Char {
                    value,
                    span: tok.span,
                })
            }

            TokenKind::Ident => {
                self.cursor.advance();
                let name = tok.text.unwrap_or_default();

                if self.cursor.peek() == TokenKind::ColonColon {
                    let mut segments = Vec::new_in(self.bump);
                    segments.push(name);

                    while self.cursor.consume(TokenKind::ColonColon) {
                        let (seg, _seg_span) = self.cursor.expect_ident()?;
                        segments.push(seg);
                    }

                    return Ok(Expr::ModulePath {
                        segments: self.bump.alloc_slice_copy(&segments),
                        span: tok.span,
                    });
                }

                Ok(Expr::Ident {
                    name,
                    span: tok.span,
                })
            }

            TokenKind::This => {
                self.cursor.advance();
                Ok(Expr::This { span: tok.span })
            }

            TokenKind::LParen => {
                self.cursor.advance();
                let expr = self.parse_expr_inner(0, true)?;
                self.cursor.expect(TokenKind::RParen)?;
                Ok(expr)
            }

            TokenKind::Sub => {
                self.cursor.advance();
                let operand = self.parse_expr_inner(80, true)?;
                Ok(Expr::Unary {
                    op: Op::Sub,
                    operand: self.bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }
            TokenKind::LogicalNot => {
                self.cursor.advance();
                let operand = self.parse_expr_inner(80, true)?;
                Ok(Expr::Unary {
                    op: Op::LogicalNot,
                    operand: self.bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }
            TokenKind::BitNot => {
                self.cursor.advance();
                let operand = self.parse_expr_inner(80, true)?;
                Ok(Expr::Unary {
                    op: Op::BitNot,
                    operand: self.bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }

            TokenKind::BitAnd => {
                self.cursor.advance();
                let mutable = self.cursor.consume(TokenKind::Mut);
                let operand = self.parse_expr_inner(80, true)?;
                Ok(Expr::Ref {
                    expr: self.bump.alloc_value_immutable(operand),
                    mutable,
                    span: tok.span,
                })
            }

            TokenKind::Mul => {
                self.cursor.advance();
                let operand = self.parse_expr_inner(80, true)?;
                Ok(Expr::Deref {
                    expr: self.bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }

            TokenKind::Suspend | TokenKind::Nosuspend | TokenKind::Blocking | TokenKind::Func => {
                self.parse_lambda()
            }

            _ => Err(DiagnosticError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Ident,
                    found: tok.kind,
                },
                tok.span,
            )),
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let start_span = self.cursor.peek_token().span;

        let modifiers: Option<LambdaModifier> = match self.cursor.peek() {
            TokenKind::Suspend => {
                self.cursor.advance();
                Some(LambdaModifier::Suspend)
            }
            TokenKind::Nosuspend => {
                self.cursor.advance();
                Some(LambdaModifier::Nosuspend)
            }
            TokenKind::Blocking => {
                self.cursor.advance();
                Some(LambdaModifier::Blocking)
            }
            _ => None,
        };

        let fn_token = self.cursor.expect(TokenKind::Func)?;

        self.cursor.expect(TokenKind::LParen)?;

        let mut params = Vec::new_in(self.bump);
        if self.cursor.peek() != TokenKind::RParen {
            loop {
                let (name, param_span) = self.cursor.expect_ident()?;

                // Types are optional on closures: `fn (a, b)` or `fn (a: i32, b: i32)`.
                let type_annotation = if self.cursor.consume(TokenKind::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                params.push(LambdaParam {
                    name,
                    type_annotation,
                    span: param_span,
                });

                if !self.cursor.consume(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.cursor.expect(TokenKind::RParen)?;

        let return_type = if self.cursor.consume(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let block = self.parse_block()?;
        let body = self.bump.alloc_value_immutable(block);

        let span = start_span.merge(fn_token.span);

        Ok(Expr::Lambda {
            modifiers,
            params: self.bump.alloc_slice_copy(&params),
            return_type,
            body,
            span,
        })
    }

    pub fn parse_catch_pattern(
        &mut self,
    ) -> Result<ErrorHandlerPattern<'a, 'bump>, DiagnosticError<'a>> {
        if self.cursor.peek() == TokenKind::LParen {
            self.parse_catch_single()
        } else {
            self.parse_catch_multiple()
        }
    }

    /// `(Type ident) => { ... }`
    fn parse_catch_single(
        &mut self,
    ) -> Result<ErrorHandlerPattern<'a, 'bump>, DiagnosticError<'a>> {
        self.cursor.expect(TokenKind::LParen)?;

        let error_type = self.parse_type()?;

        let binding = if self.cursor.peek() == TokenKind::Ident {
            let (name, _span) = self.cursor.expect_ident()?;
            Some(name)
        } else {
            None
        };

        self.cursor.expect(TokenKind::RParen)?;
        self.cursor.expect(TokenKind::Arrow)?;

        let body = self.parse_block()?;

        Ok(ErrorHandlerPattern::Single {
            error_type,
            binding,
            body: self.bump.alloc_value_immutable(body),
        })
    }

    /// `{ Type ident => { ... }, Type2 ident2 => { ... } }`
    fn parse_catch_multiple(
        &mut self,
    ) -> Result<ErrorHandlerPattern<'a, 'bump>, DiagnosticError<'a>> {
        self.cursor.expect(TokenKind::LBrace)?;

        let mut branches = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::RBrace && self.cursor.peek() != TokenKind::EOF {
            let branch_span = self.cursor.peek_token().span;

            let error_type = self.parse_type()?;

            let binding = if self.cursor.peek() == TokenKind::Ident {
                let (name, _span) = self.cursor.expect_ident()?;
                Some(name)
            } else {
                None
            };

            self.cursor.expect(TokenKind::FatArrow)?;

            let body = self.parse_block()?;

            branches.push(ErrorHandlerBranch {
                error_type,
                binding,
                body: self.bump.alloc_value_immutable(body),
                span: branch_span,
            });

            self.cursor.consume(TokenKind::Comma);
        }

        self.cursor.expect(TokenKind::RBrace)?;

        Ok(ErrorHandlerPattern::Multiple {
            branches: self.bump.alloc_slice_copy(&branches),
        })
    }
}
