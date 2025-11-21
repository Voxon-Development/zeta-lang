use std::marker::PhantomData;
use ir::ast::{Expr, Type};
use crate::rules::rule::TypeRule;
use crate::type_checker::TypeChecker;

pub struct MathematicalTypeRule<'a, 'bump> {
    phantom_1: PhantomData<&'a ()>,
    phantom_2: PhantomData<&'bump ()>
}

impl<'a, 'bump> MathematicalTypeRule<'a, 'bump> {
    pub fn new() -> Self {
        MathematicalTypeRule {
            phantom_1: PhantomData,
            phantom_2: PhantomData
        }
    }
}

/*impl<'a, 'bump> TypeRule for MathematicalTypeRule<'a, 'bump> {
    fn check_expr(&self, ctx: &mut TypeChecker<'a, 'bump>, expr: &Expr<'a, 'bump>) -> Option<Type<'a, 'bump>> {
        // Example: check binary math ops
        match expr {
            Expr::Binary { op, left, right, span } if op.is_math_op() => {
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
}*/