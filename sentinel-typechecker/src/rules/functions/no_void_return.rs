use std::ops::Deref;
use ir::ast::{FuncDecl, Stmt, Type};
use ir::errors::error::{TypeError};
use ir::span::SourceSpan;
use crate::rules::rule::TypeRule;
use crate::type_checker::TypeChecker;

pub struct NoVoidReturnRule<'a, 'bump>
where 'bump: 'a {
    phantom: std::marker::PhantomData<&'a ()>,
    phantom_2: std::marker::PhantomData<&'bump ()>,
}

impl<'a, 'bump> NoVoidReturnRule<'a, 'bump>
where 'bump: 'a {
    pub fn new() -> Self {
        Self {
            phantom: std::marker::PhantomData,
            phantom_2: std::marker::PhantomData,
        }
    }
}

/*impl<'a, 'bump> TypeRule for NoVoidReturnRule<'a, 'bump> {
    fn check_func(&self, ctx: &mut TypeChecker<'a, 'bump>, func: &FuncDecl) {
        let Some(Type::Void) = func.return_type else { return; };
        let Some(body) = func.body else { return; };

        for stmt in body.block {
            let Stmt::Return(return_stmt) = stmt else { continue };
            let Some(ref return_value) = return_stmt.value else { continue };
            ctx.ctx.error_reporter.add_error(TypeError::VoidFunctionWithNonVoidReturn {
                function_name: ctx.context.resolve_string(&*func.name.clone()).to_string(),
                return_value: return_value.deref().clone(),
                location: SourceSpan::default()
            })
        }
    }
}*/