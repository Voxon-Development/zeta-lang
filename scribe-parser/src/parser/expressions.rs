use crate::parser::descent_parser::DescentParser;
use ir::tokens::{Cursor, TokenKind};
use ir::{
    ast::{Expr, Op},
    errors::error::{DiagnosticError, ParseErrorKind},
    span::SourceSpan,
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
            let op: Op = match cursor.peek() {
                // Arithmetic
                TokenKind::Add => Op::Add,
                TokenKind::Sub => Op::Sub,
                TokenKind::Mul => Op::Mul,
                TokenKind::Div => Op::Div,
                TokenKind::Mod => Op::Mod,

                // Bitwise
                TokenKind::BitAnd => Op::BitAnd,
                TokenKind::BitOr => Op::BitOr,
                TokenKind::BitXor => Op::BitXor,
                TokenKind::Shl => Op::Shl,
                TokenKind::Shr => Op::Shr,

                // Comparison
                TokenKind::Eq => Op::Eq,
                TokenKind::Ne => Op::Neq,
                TokenKind::Lt => Op::Lt,
                TokenKind::Le => Op::Lte,
                TokenKind::Gt => Op::Gt,
                TokenKind::Ge => Op::Gte,

                // Assignment
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

                // Range
                TokenKind::DotDot => Op::Range,
                TokenKind::DotDotLt => Op::RangeExcl,

                // Function call, field access, array index handled separately
                _ => break,
            };

            let (l_bp, r_bp): (u8, u8) = op.binding_power();
            if l_bp < min_bp {
                break;
            }

            cursor.advance();
            let rhs: Expr<'a, 'bump> =
                Self::parse_expr_inner(cursor, bump, r_bp, allow_struct_init)?;

            let left = bump.alloc_value_immutable(lhs);
            let right = bump.alloc_value_immutable(rhs);
            let span = cursor.peek_token().span;

            // Comparisons use a different AST node
            lhs = match op {
                Op::Eq | Op::Neq | Op::Lt | Op::Lte | Op::Gt | Op::Gte => Expr::Comparison {
                    lhs: left,
                    op,
                    rhs: right,
                    span,
                },
                _ => Expr::Binary {
                    left,
                    op,
                    right,
                    span,
                },
            };
        }

        // Handle function calls, field access, array indexing (postfix operators)
        loop {
            match cursor.peek() {
                TokenKind::LParen => {
                    // Function call
                    cursor.advance(); // consume '('
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

                    let rparen_span = cursor.peek_token().span;
                    cursor.expect(TokenKind::RParen)?;

                    let callee = bump.alloc_value_immutable(lhs);
                    // TODO: generics
                    lhs = Expr::Call {
                        callee,
                        generic_args: &[],
                        arguments: bump.alloc_slice_copy(&args),
                        span: rparen_span,
                    };
                }
                TokenKind::Dot => {
                    cursor.advance(); // consume '.'

                    // Check for dereference: ptr.*
                    if cursor.consume(TokenKind::Mul) {
                        let span = cursor.peek_token().span;
                        let expr = bump.alloc_value_immutable(lhs);
                        lhs = Expr::Deref { expr, span };
                    } else {
                        // Field access
                        let (field_name, span) = cursor.expect_ident()?;
                        let object = bump.alloc_value_immutable(lhs);
                        lhs = Expr::FieldAccess {
                            object,
                            field: field_name,
                            span,
                        };
                    }
                }
                TokenKind::LBracket => {
                    // Array index
                    cursor.advance(); // consume '['
                    let index = Self::parse_expr_inner(cursor, bump, 0, true)?;
                    let rbracket_span = cursor.peek_token().span;
                    cursor.expect(TokenKind::RBracket)?;

                    let expr = bump.alloc_value_immutable(lhs);
                    lhs = Expr::ArrayIndex {
                        expr,
                        index: bump.alloc_value_immutable(index),
                        span: rbracket_span,
                    };
                }
                TokenKind::LBrace => {
                    // Struct initialization: Point { x, y } or Point { x: 1, y: 2 }
                    // Only valid if lhs is an identifier (the struct name) and struct init is allowed
                    if allow_struct_init {
                        if let Expr::Ident { .. } = lhs {
                            cursor.advance(); // consume '{'
                            let mut args = Vec::new_in(bump);

                            while cursor.peek() != TokenKind::RBrace
                                && cursor.peek() != TokenKind::EOF
                            {
                                let (field_name, field_span) = cursor.expect_ident()?;

                                // Check for explicit value: `field: value` or shorthand `field`
                                let value = if cursor.consume(TokenKind::Colon) {
                                    Self::parse_expr_inner(cursor, bump, 0, true)?
                                } else {
                                    // Shorthand: field name is also the variable name
                                    Expr::Ident {
                                        name: field_name,
                                        span: field_span,
                                    }
                                };

                                args.push(value);

                                if cursor.peek() == TokenKind::Comma {
                                    cursor.advance();
                                }
                            }

                            let rbrace_span = cursor.peek_token().span;
                            cursor.expect(TokenKind::RBrace)?;

                            let callee = bump.alloc_value_immutable(lhs);
                            lhs = Expr::StructInit {
                                callee,
                                arguments: bump.alloc_slice_copy(&args),
                                span: rbrace_span,
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

        Ok(lhs)
    }

    fn parse_prefix(
        cursor: &mut Cursor<'a>,
        bump: &'bump GrowableBump,
    ) -> Result<Expr<'a, 'bump>, DiagnosticError<'a>> {
        let tok = cursor.peek_token();

        match tok.kind {
            // Literals
            TokenKind::Number => {
                cursor.advance();
                let id = tok.text.unwrap_or_default();
                let text = id.as_str();
                let value = text.parse::<i64>().unwrap_or(0);
                Ok(Expr::Number {
                    value,
                    span: tok.span,
                })
            }
            TokenKind::Decimal => {
                cursor.advance();
                let id = tok.text.unwrap_or_default();
                let text = id.as_str();
                let value = text.parse::<f64>().unwrap_or(0.0);
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
                let id = tok.text.unwrap_or_default();
                let text = id.as_str();
                let value = text.chars().next().unwrap_or('\0');
                Ok(Expr::Char {
                    value,
                    span: tok.span,
                })
            }

            // Identifiers
            TokenKind::Ident => {
                cursor.advance();
                let name = tok.text.unwrap_or_default();
                Ok(Expr::Ident {
                    name,
                    span: tok.span,
                })
            }

            // Parenthesized expression
            TokenKind::LParen => {
                cursor.advance(); // consume '('
                let expr = Self::parse_expr_inner(cursor, bump, 0, true)?;
                cursor.expect(TokenKind::RParen)?;
                Ok(expr)
            }

            // Unary operators
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
                let operand = Self::parse_expr_inner(cursor, bump, 80, true)?;
                Ok(Expr::Unary {
                    op: Op::BitAnd,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }
            TokenKind::Mul => {
                cursor.advance();
                let operand = Self::parse_expr_inner(cursor, bump, 80, true)?;
                Ok(Expr::Unary {
                    op: Op::Mul,
                    operand: bump.alloc_value_immutable(operand),
                    span: tok.span,
                })
            }

            _ => Err(DiagnosticError::new(
                ParseErrorKind::UnexpectedToken { 
                    expected: TokenKind::Ident, 
                    found: tok.kind 
                },
                tok.span
            )),
        }
    }
}
