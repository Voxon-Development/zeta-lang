use ir::ast::{Expr, Type, Op};
use crate::rules::rule::TypeRule;
use crate::type_checker::TypeChecker;

pub struct MathTypeRule;

impl TypeRule for MathTypeRule {
    fn check_expr(&self, ctx: &mut TypeChecker, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Binary { left, op, right } => {
                let left_ty = ctx.check_expr(left).ok()?;
                let right_ty = ctx.check_expr(right).ok()?;

                match op {
                    Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod => {
                        if left_ty.is_numeric() && right_ty.is_numeric() {
                            // Numeric promotion rules
                            if left_ty == Type::F64 || right_ty == Type::F64 {
                                Some(Type::F64)
                            } else if left_ty == Type::I64 || right_ty == Type::I64 {
                                Some(Type::I64)
                            } else if left_ty == Type::I32 || right_ty == Type::I32 {
                                Some(Type::I32)
                            } else {
                                Some(Type::I32) // Default to i32 for integer literals
                            }
                        } else if op == &Op::Add {
                            // String concatenation
                            if left_ty == Type::String && right_ty == Type::String {
                                Some(Type::String)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    },
                    _ => None,
                }
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ir::ast::*;

    fn test_math_op(op: Op, left: Expr, right: Expr, expected: Type) {
        let rule = MathTypeRule;
        let mut ctx = TypeChecker::new();
        let expr = Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        };
        assert_eq!(rule.check_expr(&mut ctx, &expr), Some(expected));
    }

    #[test]
    fn test_numeric_promotion() {
        test_math_op(
            Op::Add,
            Expr::Number(42),
            Expr::Number(10),
            Type::I32,
        );

        test_math_op(
            Op::Add,
            Expr::Number(42),
            Expr::Decimal(10.88),
            Type::F64,
        );
    }

    #[test]
    fn test_string_concatenation() {
        test_math_op(
            Op::Add,
            Expr::String("hello".to_string()),
            Expr::String(" world".to_string()),
            Type::String,
        );
    }
}
