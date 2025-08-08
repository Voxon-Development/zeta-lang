use crate::midend::ir::hir::{HirExpr, HirStmt, HirType};
use crate::midend::type_checker::errors::error::{SourceSpan, TypeError};
use crate::midend::type_checker::rules::rule::TypeRule;
use crate::midend::type_checker::type_checker::TypeCheckerCtx;

pub struct VarInferenceRule;

impl TypeRule for VarInferenceRule {
    fn check_stmt(&self, ctx: &mut TypeCheckerCtx, stmt: &HirStmt) {
        match stmt {
            HirStmt::Let { name, ty, value, .. }=> {
                let value_ty = self.check_expr(ctx, value);
                if value_ty.is_none() {
                    ctx.variables.insert(name.clone(), value_ty.unwrap());
                    return;
                }
                let value_ty = value_ty.unwrap();
                Self::infer_type(ctx, ty, &value_ty);
                ctx.variables.insert(name.clone(), value_ty);
            }
            _ => {}
        }
    }

    fn check_expr(&self, ctx: &mut TypeCheckerCtx, expr: &HirExpr) -> Option<HirType> {
        match expr {
            HirExpr::Number(_) => Some(HirType::Primitive("i64".into())),
            HirExpr::String(_) => Some(HirType::Primitive("string".into())),
            HirExpr::Boolean(_) => Some(HirType::Primitive("bool".into())),
            HirExpr::Ident(name) => ctx.variables.get(name).cloned(),
            _ => None,
        }
    }
}

impl VarInferenceRule {
    fn infer_type(ctx: &mut TypeCheckerCtx, ty: &Option<HirType>, value_ty: &HirType) {
        if let Some(expected) = ty {
            let expected = expected.clone();
            let value_ty = value_ty.clone();
            if expected != value_ty {
                ctx.error_reporter.add_error(TypeError::Mismatch {
                    expected,
                    found: value_ty,
                    location: SourceSpan { file: String::new(), line: 0, column: 0 }
                });
            }
        }
    }
}
