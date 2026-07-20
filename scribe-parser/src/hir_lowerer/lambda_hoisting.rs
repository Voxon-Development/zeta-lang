use ir::ast::{ExternModifier, FuncSafety, InlineModifier, Visibility};
use ir::hir::{
    FuncModifiers, Hir, HirExpr, HirFieldInit, HirFunc, HirModule, HirParam, HirStmt, StrId,
};
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

pub struct LambdaHoister<'a, 'bump> {
    bump: Arc<GrowableAtomicBump<'bump>>,
    context: Arc<StringPool>,
    counter: usize,
    module_name: StrId,
    hoisted: Vec<Hir<'a, 'bump>>,
}

impl<'a, 'bump> LambdaHoister<'a, 'bump> {
    pub fn new(
        bump: Arc<GrowableAtomicBump<'bump>>,
        context: Arc<StringPool>,
        module_name: StrId,
    ) -> Self {
        Self {
            bump,
            context,
            counter: 0,
            module_name,
            hoisted: Vec::new(),
        }
    }

    pub fn run(mut self, module: HirModule<'a, 'bump>) -> HirModule<'a, 'bump> {
        let mut new_items: Vec<Hir<'a, 'bump>> = Vec::with_capacity(module.items.len());

        for item in module.items {
            let rewritten = self.rewrite_item(*item);
            new_items.push(rewritten);
        }

        new_items.extend(self.hoisted.drain(..));

        HirModule {
            name: module.name,
            imports: module.imports,
            items: self.bump.alloc_slice(&new_items),
        }
    }

    fn rewrite_item(&mut self, item: Hir<'a, 'bump>) -> Hir<'a, 'bump> {
        match item {
            Hir::Func(f) => {
                let rewritten = self.rewrite_func(*f);
                Hir::Func(self.bump.alloc_value(rewritten))
            }
            Hir::Impl(i) => {
                if let Some(methods) = i.methods {
                    let rewritten_methods: Vec<HirFunc<'a, 'bump>> =
                        methods.iter().map(|m| self.rewrite_func(*m)).collect();
                    let methods_slice = self.bump.alloc_slice_immutable(&rewritten_methods);
                    let mut new_impl = *i;
                    new_impl.methods = Some(methods_slice);
                    Hir::Impl(self.bump.alloc_value(new_impl))
                } else {
                    item
                }
            }
            other => other,
        }
    }

    fn rewrite_func(&mut self, func: HirFunc<'a, 'bump>) -> HirFunc<'a, 'bump> {
        let Some(body) = func.body else {
            return func;
        };
        let new_body = self.rewrite_stmt(body);
        HirFunc {
            body: Some(new_body),
            ..func
        }
    }

    fn rewrite_stmt(&mut self, stmt: HirStmt<'a, 'bump>) -> HirStmt<'a, 'bump> {
        match stmt {
            HirStmt::Let {
                name,
                ty,
                value,
                is_static,
                mutable,
                catch_pattern,
                else_block,
                span,
            } => HirStmt::Let {
                name,
                ty,
                value: self.rewrite_expr(value),
                mutable,
                is_static,
                catch_pattern,
                else_block,
                span,
            },
            HirStmt::Return(Some(expr)) => {
                let new_expr = self.rewrite_expr(*expr);
                HirStmt::Return(Some(self.bump.alloc_value_immutable(new_expr)))
            }
            HirStmt::Expr(expr) => {
                let new_expr = self.rewrite_expr(*expr);
                HirStmt::Expr(self.bump.alloc_value_immutable(new_expr))
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let new_cond = self.rewrite_expr(cond);
                let new_then: Vec<HirStmt<'a, 'bump>> =
                    then_block.iter().map(|s| self.rewrite_stmt(*s)).collect();
                let new_then_slice = self.bump.alloc_slice(&new_then);
                let new_else = else_block.map(|e| {
                    let rewritten = self.rewrite_stmt(*e);
                    self.bump.alloc_value_immutable(rewritten)
                });
                HirStmt::If {
                    cond: new_cond,
                    then_block: new_then_slice,
                    else_block: new_else,
                }
            }
            HirStmt::While { cond, body } => {
                let new_cond = self.rewrite_expr(*cond);
                let new_body = self.rewrite_stmt(*body);
                HirStmt::While {
                    cond: self.bump.alloc_value_immutable(new_cond),
                    body: self.bump.alloc_value_immutable(new_body),
                }
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                let new_init = init.map(|i| {
                    let r = self.rewrite_stmt(*i);
                    self.bump.alloc_value_immutable(r)
                });
                let new_cond = condition.map(|c| {
                    let r = self.rewrite_expr(*c);
                    self.bump.alloc_value_immutable(r)
                });
                let new_inc = increment.map(|i| {
                    let r = self.rewrite_expr(*i);
                    self.bump.alloc_value_immutable(r)
                });
                let new_body = self.rewrite_stmt(*body);
                HirStmt::For {
                    init: new_init,
                    condition: new_cond,
                    increment: new_inc,
                    body: self.bump.alloc_value_immutable(new_body),
                }
            }
            HirStmt::Match { expr, arms } => {
                let new_expr = self.rewrite_expr(*expr);
                let new_arms: Vec<ir::hir::HirMatchArm<'a, 'bump>> = arms
                    .iter()
                    .map(|arm| {
                        let new_guard = arm.guard.map(|g| {
                            let r = self.rewrite_expr(*g);
                            self.bump.alloc_value_immutable(r)
                        });
                        let new_body = self.rewrite_stmt(*arm.body);
                        ir::hir::HirMatchArm {
                            pattern: arm.pattern,
                            guard: new_guard,
                            body: self.bump.alloc_value_immutable(new_body),
                        }
                    })
                    .collect();
                HirStmt::Match {
                    expr: self.bump.alloc_value_immutable(new_expr),
                    arms: self.bump.alloc_slice(&new_arms),
                }
            }
            HirStmt::UnsafeBlock { body } => {
                let new_body = self.rewrite_stmt(*body);
                HirStmt::UnsafeBlock {
                    body: self.bump.alloc_value_immutable(new_body),
                }
            }
            HirStmt::Block { body } => {
                let new_body: Vec<HirStmt<'a, 'bump>> =
                    body.iter().map(|s| self.rewrite_stmt(*s)).collect();
                HirStmt::Block {
                    body: self.bump.alloc_slice(&new_body),
                }
            }
            HirStmt::Defer(inner) => {
                let new_inner = self.rewrite_stmt(*inner);
                HirStmt::Defer(self.bump.alloc_value_immutable(new_inner))
            }
            HirStmt::Break(Some(expr), span) => {
                let new_expr = self.rewrite_expr(*expr);
                HirStmt::Break(Some(self.bump.alloc_value_immutable(new_expr)), span)
            }
            other => other,
        }
    }

    fn rewrite_expr(&mut self, expr: HirExpr<'a, 'bump>) -> HirExpr<'a, 'bump> {
        match expr {
            HirExpr::Lambda {
                modifier: _,
                params,
                return_type,
                body,
                span,
            } => {
                let inner_rewritten_body = self.rewrite_stmt(*body);

                let synthetic_name = self.fresh_lambda_name();

                let hir_params: Vec<HirParam<'a, 'bump>> = params
                    .iter()
                    .map(|p| HirParam::Normal {
                        name: p.name,
                        param_type: p.param_type.unwrap_or_else(|| {
                            panic!(
                                "lambda parameter {:?} has no resolved type at hoisting time \
                                 hoisting must run after type inference",
                                p.name
                            )
                        }),
                        span: p.span,
                    })
                    .collect();
                let params_slice = self.bump.alloc_slice_immutable(&hir_params);

                let synthetic_func = HirFunc {
                    name: synthetic_name,
                    function_metadata: FuncModifiers {
                        visibility: Visibility::Private,
                        extern_modifier: ExternModifier::None,
                        inline_modifier: InlineModifier::None,
                        func_safety: FuncSafety::Safe,
                    },
                    generics: None,
                    params: Some(params_slice),
                    return_type: Some(*return_type),
                    body: Some(inner_rewritten_body),
                    unmangled_name: synthetic_name, // Not a real function so it just gets any name xD
                    declaring_module_idx: 0,        // Only important for type checking.
                    impl_target: None,
                };

                self.hoisted
                    .push(Hir::Func(self.bump.alloc_value(synthetic_func)));

                HirExpr::Ident(synthetic_name, span)
            }

            HirExpr::Binary {
                left,
                op,
                right,
                span,
            } => {
                let l = self.rewrite_expr(*left);
                let r = self.rewrite_expr(*right);
                HirExpr::Binary {
                    left: self.bump.alloc_value(l),
                    op,
                    right: self.bump.alloc_value(r),
                    span,
                }
            }
            HirExpr::Comparison {
                left,
                op,
                right,
                span,
            } => {
                let l = self.rewrite_expr(*left);
                let r = self.rewrite_expr(*right);
                HirExpr::Comparison {
                    left: self.bump.alloc_value(l),
                    op,
                    right: self.bump.alloc_value(r),
                    span,
                }
            }
            HirExpr::Call {
                callee,
                args,
                span,
                type_args,
            } => {
                let new_callee = self.rewrite_expr(*callee);
                let new_args: Vec<HirExpr<'a, 'bump>> =
                    args.iter().map(|a| self.rewrite_expr(*a)).collect();
                HirExpr::Call {
                    callee: self.bump.alloc_value(new_callee),
                    args: self.bump.alloc_slice(&new_args),
                    span,
                    type_args,
                }
            }
            HirExpr::InterfaceCall {
                callee,
                args,
                interface,
                span,
            } => {
                let new_callee = self.rewrite_expr(*callee);
                let new_args: Vec<HirExpr<'a, 'bump>> =
                    args.iter().map(|a| self.rewrite_expr(*a)).collect();
                HirExpr::InterfaceCall {
                    callee: self.bump.alloc_value(new_callee),
                    args: self.bump.alloc_slice(&new_args),
                    interface,
                    span,
                }
            }
            HirExpr::FieldAccess {
                object,
                field,
                span,
            } => {
                let new_obj = self.rewrite_expr(*object);
                HirExpr::FieldAccess {
                    object: self.bump.alloc_value(new_obj),
                    field,
                    span,
                }
            }
            HirExpr::Get {
                object,
                field,
                span,
            } => {
                let new_obj = self.rewrite_expr(*object);
                HirExpr::Get {
                    object: self.bump.alloc_value(new_obj),
                    field,
                    span,
                }
            }
            HirExpr::Assignment {
                target,
                op,
                value,
                span,
            } => {
                let new_target = self.rewrite_expr(*target);
                let new_value = self.rewrite_expr(*value);
                HirExpr::Assignment {
                    target: self.bump.alloc_value(new_target),
                    op,
                    value: self.bump.alloc_value(new_value),
                    span,
                }
            }
            HirExpr::StructInit {
                name,
                args,
                span,
                type_args,
            } => {
                let new_name = self.rewrite_expr(*name);
                let new_args: Vec<HirFieldInit<'a, 'bump>> = args
                    .iter()
                    .map(|a| HirFieldInit {
                        name: a.name,
                        name_span: a.name_span,
                        value: self.rewrite_expr(*&a.value),
                    })
                    .collect();
                HirExpr::StructInit {
                    name: self.bump.alloc_value(new_name),
                    args: self.bump.alloc_slice(&new_args),
                    span,
                    type_args,
                }
            }
            HirExpr::ExprList { list, span } => {
                let new_list: Vec<HirExpr<'a, 'bump>> =
                    list.iter().map(|e| self.rewrite_expr(*e)).collect();
                HirExpr::ExprList {
                    list: self.bump.alloc_slice(&new_list),
                    span,
                }
            }
            HirExpr::Ref {
                expr,
                mutable,
                span,
            } => {
                let new_inner = self.rewrite_expr(*expr);
                HirExpr::Ref {
                    expr: self.bump.alloc_value(new_inner),
                    mutable,
                    span,
                }
            }
            HirExpr::Deref { expr, span } => {
                let new_inner = self.rewrite_expr(*expr);
                HirExpr::Deref {
                    expr: self.bump.alloc_value(new_inner),
                    span,
                }
            }
            other => other,
        }
    }

    fn fresh_lambda_name(&mut self) -> StrId {
        let module_str = self.context.resolve_string(&self.module_name);
        let name = format!("__lambda_{}_{}", module_str, self.counter);
        self.counter += 1;
        StrId(self.context.intern(&name))
    }
}
