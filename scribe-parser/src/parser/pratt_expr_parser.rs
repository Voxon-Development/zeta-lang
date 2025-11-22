use crate::tokenizer::cursor::TokenCursor;
use crate::tokenizer::tokens::TokenKind;
use ir::ast::*;
use ir::hir::StrId;
use std::sync::Arc;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

/// Pratt parser for expressions with prefix, infix, and postfix operators
pub struct PrattExprParser<'a, 'bump>
where
    'bump: 'a,
{
    context: Arc<StringPool>,
    bump: &'bump GrowableBump<'bump>,
    phantom: std::marker::PhantomData<&'a ()>,
}

/// Binding power for operators (higher = tighter binding)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum BindingPower {
    None = 0,
    Assignment = 10,    // =, +=, -=, etc. (right-associative)
    LogicalOr = 20,     // ||
    LogicalAnd = 30,    // &&
    Equality = 40,      // ==, !=
    Comparison = 50,    // <, >, <=, >=
    BitOr = 60,         // |
    BitXor = 70,        // ^
    BitAnd = 80,        // &
    Shift = 90,         // <<, >>
    Term = 100,         // +, -
    Factor = 110,       // *, /, %
    Unary = 120,        // !, -, ~
    Postfix = 130,      // ., (), []
}

impl BindingPower {
    fn left_associative(self) -> Self {
        match self {
            BindingPower::None => BindingPower::None,
            BindingPower::Assignment => BindingPower::Assignment,
            BindingPower::LogicalOr => BindingPower::LogicalAnd,
            BindingPower::LogicalAnd => BindingPower::Equality,
            BindingPower::Equality => BindingPower::Comparison,
            BindingPower::Comparison => BindingPower::BitOr,
            BindingPower::BitOr => BindingPower::BitXor,
            BindingPower::BitXor => BindingPower::BitAnd,
            BindingPower::BitAnd => BindingPower::Shift,
            BindingPower::Shift => BindingPower::Term,
            BindingPower::Term => BindingPower::Factor,
            BindingPower::Factor => BindingPower::Unary,
            BindingPower::Unary => BindingPower::Postfix,
            BindingPower::Postfix => BindingPower::Postfix,
        }
    }
}

impl<'a, 'bump> PrattExprParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(context: Arc<StringPool>, bump: &'bump GrowableBump) -> Self {
        Self {
            context,
            bump,
            phantom: std::marker::PhantomData,
        }
    }

    /// Parse an expression (main entry point)
    pub fn parse(&self, cursor: &mut TokenCursor<'a>) -> &'bump Expr<'a, 'bump> {
        self.parse_expr(cursor, BindingPower::None)
    }

    /// Parse expression with given minimum binding power (Pratt parsing)
    fn parse_expr(&self, cursor: &mut TokenCursor<'a>, min_bp: BindingPower) -> &'bump Expr<'a, 'bump> {
        // Parse prefix/primary expression
        let mut lhs = self.parse_prefix(cursor);

        // Parse infix and postfix operations
        loop {
            let token = match cursor.peek_kind() {
                Some(t) => t,
                None => break,
            };

            // Check for postfix operators first (highest precedence)
            if let Some(bp) = Self::postfix_binding_power(token) {
                if bp < min_bp {
                    break;
                }
                lhs = self.parse_postfix(cursor, lhs);
                continue;
            }

            // Check for infix operators
            if let Some((l_bp, r_bp)) = Self::infix_binding_power(token) {
                if l_bp < min_bp {
                    break;
                }

                cursor.advance_kind(); // consume operator
                let rhs = self.parse_expr(cursor, r_bp);

                let op = Self::token_to_op(token).expect("Valid operator");
                let span = cursor.current_span();
                
                lhs = if Self::is_assignment_op(op) {
                    self.bump.alloc_value(Expr::Assignment {
                        lhs,
                        op,
                        rhs,
                        span,
                    })
                } else if Self::is_comparison_op(op) {
                    self.bump.alloc_value(Expr::Comparison {
                        lhs,
                        op,
                        rhs,
                        span,
                    })
                } else {
                    self.bump.alloc_value(Expr::Binary {
                        left: lhs,
                        op,
                        right: rhs,
                        span,
                    })
                };
                continue;
            }

            break;
        }

        lhs
    }

    /// Parse prefix operators and primary expressions
    fn parse_prefix(&self, cursor: &mut TokenCursor<'a>) -> &'bump Expr<'a, 'bump> {
        match cursor.peek_kind() {
            // Prefix operators: !, -, ~
            Some(TokenKind::BitNot) | Some(TokenKind::Sub) | Some(TokenKind::LogicalNot) => {
                let op_token = cursor.peek_kind().unwrap();
                let span = cursor.current_span();
                cursor.advance_kind();
                
                let operand = self.parse_expr(cursor, BindingPower::Unary);
                let op = match op_token {
                    TokenKind::BitNot => Op::BitNot,
                    TokenKind::Sub => Op::Sub,
                    TokenKind::LogicalNot => Op::LogicalNot,
                    _ => unreachable!(),
                };
                
                self.bump.alloc_value(Expr::Unary {
                    op,
                    operand,
                    span,
                })
            }
            
            // Literals
            Some(TokenKind::Number) => {
                let span = cursor.current_span();
                let text_id = cursor.consume_number().unwrap();
                let text = self.context.resolve_string(&text_id);
                let value = text.parse::<i64>().unwrap_or(0);
                self.bump.alloc_value(Expr::Number { value, span })
            }
            
            Some(TokenKind::Decimal) => {
                let span = cursor.current_span();
                let text_id = cursor.consume_decimal().unwrap();
                let text = self.context.resolve_string(&text_id);
                let value = text.parse::<f64>().unwrap_or(0.0);
                self.bump.alloc_value(Expr::Decimal { value, span })
            }
            
            Some(TokenKind::String) => {
                let span = cursor.current_span();
                let value = cursor.consume_string().unwrap();
                self.bump.alloc_value(Expr::String { value, span })
            }
            
            Some(TokenKind::BooleanTrue) => {
                let span = cursor.current_span();
                cursor.advance_kind();
                self.bump.alloc_value(Expr::Boolean { value: true, span })
            }
            
            Some(TokenKind::BooleanFalse) => {
                let span = cursor.current_span();
                cursor.advance_kind();
                self.bump.alloc_value(Expr::Boolean { value: false, span })
            }
            
            Some(TokenKind::This) => {
                let span = cursor.current_span();
                cursor.advance_kind();
                let this_id = self.context.intern("this");
                self.bump.alloc_value(Expr::Ident { name: StrId(this_id), span })
            }
            
            Some(TokenKind::Ident) => {
                let span = cursor.current_span();
                let name = cursor.consume_ident().unwrap();
                self.bump.alloc_value(Expr::Ident { name, span })
            }

            // Parenthesized expression
            Some(TokenKind::LParen) => {
                cursor.advance_kind(); // consume '('
                let expr = self.parse_expr(cursor, BindingPower::None);
                cursor.expect_kind(TokenKind::RParen);
                expr
            }

            _ => {
                // Error: unexpected token, return placeholder
                let span = cursor.current_span();
                cursor.advance_kind();
                self.bump.alloc_value(Expr::Number { value: 0, span })
            }
        }
    }

    /// Parse postfix operators (function calls, field access, array indexing)
    fn parse_postfix(&self, cursor: &mut TokenCursor<'a>, expr: &'bump Expr<'a, 'bump>) -> &'bump Expr<'a, 'bump> {
        match cursor.peek_kind() {
            Some(TokenKind::LParen) => {
                if let Expr::Ident { name: _, span } = expr {
                    cursor.advance_kind(); // consume '('
                    let args = self.parse_call_args(cursor);
                    let args_slice = self.bump.alloc_slice_copy(&args);
                    
                    self.bump.alloc_value(Expr::ClassInit {
                        callee: expr,
                        arguments: args_slice,
                        positional: true,
                        span: *span,
                    })
                } else {
                    let span = cursor.current_span();
                    cursor.advance_kind();
                    let args = self.parse_call_args(cursor);
                    let args_slice = self.bump.alloc_slice_copy(&args);
                    self.bump.alloc_value(Expr::Call {
                        callee: expr,
                        arguments: args_slice,
                        span,
                    })
                }
            }

            Some(TokenKind::LBrace) => {
                // Class initialization: MyStruct { field: value, ... } or MyStruct { value1, value2 }
                let span = cursor.current_span();
                cursor.advance_kind(); // consume '{'
                let (args, positional) = self.parse_class_init_args(cursor);
                let args_slice = self.bump.alloc_slice_copy(&args);
                self.bump.alloc_value(Expr::ClassInit {
                    callee: expr,
                    arguments: args_slice,
                    positional,
                    span,
                })
            }

            Some(TokenKind::Dot) => {
                // Field access
                let span = cursor.current_span();
                cursor.advance_kind(); // consume '.'
                let field = cursor.consume_ident().unwrap();
                self.bump.alloc_value(Expr::Get {
                    object: expr,
                    field,
                    span,
                })
            }
            
            Some(TokenKind::LBracket) => {
                let span = cursor.current_span();
                cursor.advance_kind(); // consume '['
                let index = self.parse_expr(cursor, BindingPower::None);
                cursor.expect_kind(TokenKind::RBracket);
                self.bump.alloc_value_immutable(Expr::ArrayIndex { expr, index, span })
            }
            
            _ => expr,
        }
    }

    /// Parse function call arguments
    fn parse_call_args(&self, cursor: &mut TokenCursor<'a>) -> Vec<Expr<'a, 'bump>> {
        let mut args = Vec::new();

        while cursor.peek_kind() != Some(TokenKind::RParen) && !cursor.at_end() {
            let arg = self.parse_expr(cursor, BindingPower::None);
            args.push(*arg);

            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            } else {
                break;
            }
        }

        cursor.expect_kind(TokenKind::RParen);
        args
    }

    fn parse_class_init_args(&self, cursor: &mut TokenCursor<'a>) -> (Vec<Expr<'a, 'bump>>, bool) {
        let mut args = Vec::new();
        let mut positional = true;

        // Check if empty
        if cursor.peek_kind() == Some(TokenKind::RBrace) {
            cursor.advance_kind();
            return (args, true);
        }

        // Look ahead to determine if this is named or positional
        // If we see `ident :`, it's named
        if cursor.peek_kind() == Some(TokenKind::Ident) {
            let checkpoint = cursor.checkpoint();
            cursor.advance_kind(); // skip ident
            if cursor.peek_kind() == Some(TokenKind::Colon) {
                positional = false;
            }
            cursor.restore(checkpoint);
        }

        // Parse arguments based on format
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            if positional {
                // Positional: just parse expression
                let arg = self.parse_expr(cursor, BindingPower::None);
                args.push(*arg);
            } else {
                // Named: parse `ident : expr`
                let _field_name = cursor.consume_ident().unwrap();
                cursor.expect_kind(TokenKind::Colon);
                let arg = self.parse_expr(cursor, BindingPower::None);
                args.push(*arg);
            }

            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            } else {
                break;
            }
        }

        cursor.expect_kind(TokenKind::RBrace);
        (args, positional)
    }


    /// Get binding power for postfix operators
    fn postfix_binding_power(token: TokenKind) -> Option<BindingPower> {
        match token {
            TokenKind::LParen | TokenKind::Dot | TokenKind::LBracket => Some(BindingPower::Postfix),
            _ => None,
        }
    }

    /// Get binding power for infix operators (left_bp, right_bp)
    /// Right-associative operators have right_bp = left_bp
    /// Left-associative operators have right_bp = left_bp + 1
    fn infix_binding_power(token: TokenKind) -> Option<(BindingPower, BindingPower)> {
        let bp = match token {
            // Assignment (right-associative)
            TokenKind::Assign | TokenKind::AddAssign | TokenKind::SubAssign |
            TokenKind::MulAssign | TokenKind::DivAssign | TokenKind::ModAssign |
            TokenKind::ShlAssign | TokenKind::ShrAssign | TokenKind::AndAssign |
            TokenKind::OrAssign | TokenKind::XorAssign => {
                let bp = BindingPower::Assignment;
                (bp, bp) // right-associative
            }

            // Logical OR
            TokenKind::OrOr => {
                let bp = BindingPower::LogicalOr;
                (bp, bp.left_associative())
            }

            // Logical AND
            TokenKind::AndAnd => {
                let bp = BindingPower::LogicalAnd;
                (bp, bp.left_associative())
            }

            // Equality
            TokenKind::Eq | TokenKind::Ne => {
                let bp = BindingPower::Equality;
                (bp, bp.left_associative())
            }

            // Comparison
            TokenKind::Lt | TokenKind::Gt | TokenKind::Le | TokenKind::Ge => {
                let bp = BindingPower::Comparison;
                (bp, bp.left_associative())
            }

            // Bitwise OR
            TokenKind::BitOr => {
                let bp = BindingPower::BitOr;
                (bp, bp.left_associative())
            }

            // Bitwise XOR
            TokenKind::BitXor => {
                let bp = BindingPower::BitXor;
                (bp, bp.left_associative())
            }

            // Bitwise AND
            TokenKind::BitAnd => {
                let bp = BindingPower::BitAnd;
                (bp, bp.left_associative())
            }

            // Shift
            TokenKind::Shl | TokenKind::Shr | TokenKind::UnsignedShr => {
                let bp = BindingPower::Shift;
                (bp, bp.left_associative())
            }

            // Addition, Subtraction
            TokenKind::Add | TokenKind::Sub => {
                let bp = BindingPower::Term;
                (bp, bp.left_associative())
            }

            // Multiplication, Division, Modulo
            TokenKind::Mul | TokenKind::Div | TokenKind::Mod => {
                let bp = BindingPower::Factor;
                (bp, bp.left_associative())
            }

            _ => return None,
        };

        Some(bp)
    }

    /// Convert token to operator
    fn token_to_op(token: TokenKind) -> Option<Op> {
        match token {
            TokenKind::Assign => Some(Op::Assign),
            TokenKind::AddAssign => Some(Op::AddAssign),
            TokenKind::SubAssign => Some(Op::SubAssign),
            TokenKind::MulAssign => Some(Op::MulAssign),
            TokenKind::DivAssign => Some(Op::DivAssign),
            TokenKind::ModAssign => Some(Op::ModAssign),
            TokenKind::ShlAssign => Some(Op::ShlAssign),
            TokenKind::ShrAssign => Some(Op::ShrAssign),
            TokenKind::AndAssign => Some(Op::BitAndAssign),
            TokenKind::OrAssign => Some(Op::BitOrAssign),
            TokenKind::XorAssign => Some(Op::BitXorAssign),
            
            TokenKind::Eq => Some(Op::Eq),
            TokenKind::Ne => Some(Op::Neq),
            TokenKind::Lt => Some(Op::Lt),
            TokenKind::Gt => Some(Op::Gt),
            TokenKind::Le => Some(Op::Lte),
            TokenKind::Ge => Some(Op::Gte),
            
            TokenKind::Add => Some(Op::Add),
            TokenKind::Sub => Some(Op::Sub),
            TokenKind::Mul => Some(Op::Mul),
            TokenKind::Div => Some(Op::Div),
            TokenKind::Mod => Some(Op::Mod),
            
            TokenKind::BitOr | TokenKind::OrOr => Some(Op::BitOr),
            TokenKind::BitXor => Some(Op::BitXor),
            TokenKind::BitAnd | TokenKind::AndAnd => Some(Op::BitAnd),
            
            TokenKind::Shl => Some(Op::Shl),
            TokenKind::Shr => Some(Op::Shr),
            
            _ => None,
        }
    }

    fn is_assignment_op(op: Op) -> bool {
        matches!(op, 
            Op::Assign | Op::AddAssign | Op::SubAssign | Op::MulAssign | 
            Op::DivAssign | Op::ModAssign | Op::ShlAssign | Op::ShrAssign |
            Op::BitAndAssign | Op::BitOrAssign | Op::BitXorAssign
        )
    }

    fn is_comparison_op(op: Op) -> bool {
        matches!(op, Op::Eq | Op::Neq | Op::Lt | Op::Gt | Op::Lte | Op::Gte)
    }
}
