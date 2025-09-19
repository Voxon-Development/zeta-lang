use std::ops::Deref;
use ir::ast::{FuncDecl, Stmt, Type};
use ir::errors::error::{SourceSpan, TypeError};
use crate::rules::rule::TypeRule;
use crate::type_checker::TypeChecker;

pub struct NoVoidReturnRule;

impl TypeRule for NoVoidReturnRule {
    fn check_func(&self, ctx: &mut TypeChecker, func: &FuncDecl) {
        let Some(Type::Void) = func.return_type else { return; };
        let Some(ref body) = func.body else { return; };

        for stmt in &body.block {
            let Stmt::Return(return_stmt) = stmt else { continue };
            let Some(ref return_value) = return_stmt.value else { continue };
            ctx.ctx.error_reporter.add_error(TypeError::VoidFunctionWithNonVoidReturn {
                function_name: ctx.context.borrow_mut().string_pool.resolve_string(&*func.name.clone()).to_string(),
                return_value: return_value.deref().clone(),
                location: SourceSpan::default()
            })
        }
    }
}