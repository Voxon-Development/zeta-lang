use ir::ast::{Expr, Type};
use crate::rules::rule::TypeRule;
use crate::type_checker::TypeChecker;

pub struct MathematicalTypeRule;

impl TypeRule for MathematicalTypeRule {
    fn check_expr(&self, ctx: &mut TypeChecker, expr: &Expr) -> Option<Type> {
        // Example: check binary math ops
        match expr {
            Expr::Binary { op, left, right } if op.is_math_op() => {
                let ltype = ctx.get_expr_type(left);
                let rtype = ctx.get_expr_type(right);
                if ltype == rtype && ltype.is_numeric() {
                    Some(ltype)
                } else {
                    panic!("Type error in math operation")
                }
            }
            _ => None,
        }
    }
}