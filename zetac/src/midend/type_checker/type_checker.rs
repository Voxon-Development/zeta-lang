use crate::frontend::hir_lowerer::{LoweringCtx, SeaHashBuilder};
use crate::midend::ir::hir::{Hir, HirClass, HirExpr, HirFunc, HirModule, HirStmt, HirType};
use crate::midend::type_checker::errors::reporter::ErrorReporter;
use crate::midend::type_checker::rules::rule::TypeRule;
use std::collections::HashMap;

pub struct TypeCheckerCtx<'a> {
    pub lowerer_ctx: &'a LoweringCtx,
    pub variables: HashMap<String, HirType, SeaHashBuilder>,
    pub error_reporter: ErrorReporter,
}

impl<'a> TypeCheckerCtx<'a> {
    pub fn new(lowerer_ctx: &'a LoweringCtx) -> Self {
        Self {
            lowerer_ctx,
            variables: HashMap::with_hasher(SeaHashBuilder::new()),
            error_reporter: ErrorReporter::new(),
        }
    }
}

// The entry point for type-checking the program.
pub struct TypeChecker<'a> {
    pub ctx: TypeCheckerCtx<'a>,
    pub rules: Vec<Box<dyn TypeRule>>
}

impl<'a> TypeChecker<'a> {
    pub fn new(lowerer_ctx: &'a LoweringCtx) -> Self {
        Self {
            ctx: TypeCheckerCtx::new(lowerer_ctx),
            rules: Vec::new(),
        }
    }

    pub fn add_rule<R: TypeRule + 'static>(&mut self, rule: R) {
        self.rules.push(Box::new(rule));
    }

    pub fn check_module(&mut self, module: &HirModule) {
        for item in &module.items {
            match item {
                Hir::Func(f) => self.check_func(f),
                Hir::Class(c) => self.check_class(c),
                Hir::Interface(_) | Hir::Impl(_) | Hir::Enum(_) | Hir::Stmt(_) => {},
                Hir::Module(_) | Hir::Expr(_) => todo!()
            }
        }
    }

    fn check_func(&mut self, func: &HirFunc) {
        for rule in &self.rules {
            rule.check_func(&mut self.ctx, func);
        }
        if let Some(body) = &func.body {
            match body {
                HirStmt::Block { body } => {
                    for stmt in body {
                        self.check_stmt(stmt);
                    }
                }
                _ => { self.check_stmt(body); }
            }
        }
    }

    fn check_class(&mut self, class: &HirClass) {
        for rule in &self.rules {
            rule.check_class(&mut self.ctx, class);
        }
    }

    fn check_stmt(&mut self, stmt: &HirStmt) {
        for rule in &self.rules {
            rule.check_stmt(&mut self.ctx, stmt);
        }
        match stmt {
            HirStmt::Expr(e) | HirStmt::Return(Some(e)) => {
                self.check_expr(e);
            }
            HirStmt::Block { body } => {
                for s in body {
                    self.check_stmt(s);
                }
            }
            _ => {}
        }
    }

    fn check_expr(&mut self, expr: &HirExpr) -> HirType {
        for rule in &self.rules {
            if let Some(ty) = rule.check_expr(&mut self.ctx, expr) {
                return ty;
            }
        }
        HirType::Void // fallback
    }

}
