use crate::ast::*;

pub struct IRStmtCompiler;

impl IRStmtCompiler {
    pub fn new() -> Self {
        IRStmtCompiler
    }
    
    pub fn compile_stmts(&self, stmts: Vec<Stmt>) {
        for stmt in stmts {
            self.compile_stmt(&stmt);
        }
    }
    
    fn compile_stmt(&self, stmt: &Stmt) {
        match stmt {
            Stmt::ExprStmt(InternalExprStmt { expr }) => {
                self.compile_expr(expr);
            }
            _ => todo!()
        }
    }
    
    fn compile_expr(&self, expr: &Expr) {
        match expr {
            _ => todo!()
        }
    }
}