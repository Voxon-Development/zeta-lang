use crate::parser::descent_parser::DescentParser;
use ir::tokens::{Cursor, TokenKind};
use ir::{
    ast::{Expr, Op},
    errors::error::{DiagnosticError, ParseErrorKind},
};
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_expr(&mut self, min_bp: u8) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        Self::parse_expr_inner(&mut self.cursor, self.bump, min_bp, true)
    }

    pub fn parse_expr_inner(
        cursor: &mut Cursor<'a>,
        bump: &'bump GrowableBump<'bump>,
        min_bp: u8,
        allow_struct_init: bool,
    ) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let mut lhs: Expr<'a, 'bump> = Self::parse_prefix(cursor, bump)?;

        loop {
            loop {
                match cursor.peek() {
                    TokenKind::LParen => {
                        cursor.advance();

                        let mut args = Vec::new_in(bump);

                        if cursor.peek() != TokenKind::RParen {
                            loop {
                                let arg = Self::parse_expr_inner(cursor, bump, 0, true)?;
                                args.push(arg);

                                if !cursor.consume(TokenKind::Comma) {
                                    break;
                                }
                            }
                        }

                        let span = cursor.peek_token().span;
                        cursor.expect(TokenKind::RParen)?;

                        let callee = bump.alloc_value_immutable(lhs);

                        lhs = Expr::Call {
                            callee,
                            generic_args: &[],
                            arguments: bump.alloc_slice_copy(&args),
                            span,
                        };
                    }

                    TokenKind::Dot => {
                        cursor.advance();

                        if cursor.consume(TokenKind::Mul) {
                            let span = cursor.peek_token().span;

                            lhs = Expr::Deref {
                                expr: bump.alloc_value_immutable(lhs),
                                span,
                            };
                        } else {
                            let (field_name, span) = cursor.expect_ident()?;

                            lhs = Expr::FieldAccess {
                                object: bump.alloc_value_immutable(lhs),
                                field: field_name,
                                span,
                            };
                        }
                    }

                    TokenKind::LBracket => {
                        cursor.advance();

                        let index = Self::parse_expr_inner(cursor, bump, 0, true)?;

                        let span = cursor.peek_token().span;

                        cursor.expect(TokenKind::RBracket)?;

                        lhs = Expr::ArrayIndex {
                            expr: bump.alloc_value_immutable(lhs),
                            index: bump.alloc_value_immutable(index),
                            span,
                        };
                    }

                    TokenKind::LBrace => {
                        if allow_struct_init {
                            if let Expr::Ident { .. } = lhs {
                                cursor.advance();

                                let mut args = Vec::new_in(bump);

                                while cursor.peek() != TokenKind::RBrace
                                    && cursor.peek() != TokenKind::EOF
                                {
                                    let (field_name, field_span) = cursor.expect_ident()?;

                                    let value = if cursor.consume(TokenKind::Colon) {
                                        Self::parse_expr_inner(cursor, bump, 0, true)?
                                    } else {
                                        Expr::Ident {
                                            name: field_name,
                                            span: field_span,
                                        }
                                    };

                                    args.push(value);

                                    if !cursor.consume(TokenKind::Comma) {
                                        break;
                                    }
                                }

                                let span = cursor.peek_token().span;

                                cursor.expect(TokenKind::RBrace)?;

                                lhs = Expr::StructInit {
                                    callee: bump.alloc_value_immutable(lhs),
                                    arguments: bump.alloc_slice_copy(&args),
                                    span,
                                };
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    _ => break,
                }
            }

            let op: Op = match cursor.peek() {
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

            let span = cursor.peek_token().span;

            cursor.advance();

            let rhs = Self::parse_expr_inner(cursor, bump, r_bp, allow_struct_init)?;

            lhs = match op {
                Op::Eq | Op::Neq | Op::Lt | Op::Lte | Op::Gt | Op::Gte => Expr::Comparison {
                    lhs: bump.alloc_value_immutable(lhs),
                    op,
                    rhs: bump.alloc_value_immutable(rhs),
                    span,
                },

                _ => Expr::Binary {
                    left: bump.alloc_value_immutable(lhs),
                    op,
                    right: bump.alloc_value_immutable(rhs),
                    span,
                },
            };
        }

        Ok(lhs)
    }

    fn parse_prefix(
        cursor: &mut Cursor<'a>,
        bump: &'bump GrowableBump,
    ) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let tok = cursor.peek_token();

        match tok.kind {
            TokenKind::Number => {
                cursor.advance();
                let text = tok.text.unwrap_or_default();
                let value = text.as_str().parse::<i64>().unwrap_or(0);
                Ok(Expr::Number {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::Decimal => {
                cursor.advance();
                let text = tok.text.unwrap_or_default();
                let value = text.as_str().parse::<f64>().unwrap_or(0.0);
                Ok(Expr::Decimal {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::String => {
                cursor.advance();
                let value = tok.text.unwrap_or_default();
                Ok(Expr::String {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::BooleanTrue => {
                cursor.advance();
                Ok(Expr::Boolean {
                    value: true,
                    span: tok.span,
                })
            }
            TokenKind::BooleanFalse => {
                cursor.advance();
                Ok(Expr::Boolean {
                    value: false,
                    span: tok.span,
                })
            }
            TokenKind::Null => {
                cursor.advance();
                Ok(Expr::Null { span: tok.span })
            }
            TokenKind::Char => {
                cursor.advance();
                let text = tok.text.unwrap_or_default();
                let value = text.as_str().chars().next().unwrap_or('\0');
                Ok(Expr::Char {
                    value,
                    span: tok.span,
                })
            }

            TokenKind::Ident => {
                cursor.advance();
                let name = tok.text.unwrap_or_default();
                Ok(Expr::Ident {
                    name,
                    span: tok.span,
                })
            }

            TokenKind::This => {
                cursor.advance();
                Ok(Expr::This { span: tok.span })
            }

            TokenKind::LParen => {
                cursor.advance();
                let expr = Self::parse_expr_inner(cursor, bump, 0, true)?;
                cursor.expect(TokenKind::RParen)?;
                Ok(expr)
            }

            TokenKind::Sub => {
                cursor.advance();
                let operand = Self::parse_expr_inner(cursor, bump, 80, true)?;
                Ok(Expr::Unary {
                    op: Op::Sub,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }
            TokenKind::LogicalNot => {
                cursor.advance();
                let operand = Self::parse_expr_inner(cursor, bump, 80, true)?;
                Ok(Expr::Unary {
                    op: Op::LogicalNot,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }
            TokenKind::BitNot => {
                cursor.advance();
                let operand = Self::parse_expr_inner(cursor, bump, 80, true)?;
                Ok(Expr::Unary {
                    op: Op::BitNot,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }

            TokenKind::BitAnd => {
                cursor.advance();
                let (op, bp) = if cursor.consume(TokenKind::Mut) {
                    (Op::RefMut, 80u8)
                } else {
                    (Op::Ref, 80u8)
                };
                let operand = Self::parse_expr_inner(cursor, bump, bp, true)?;
                Ok(Expr::Unary {
                    op,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }

            TokenKind::Mul => {
                cursor.advance();
                let operand = Self::parse_expr_inner(cursor, bump, 80, true)?;
                Ok(Expr::Unary {
                    op: Op::Deref,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
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
}
