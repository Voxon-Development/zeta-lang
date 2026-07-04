#[cfg(test)]
mod tests {
    use ir::ast::{Expr, Stmt};
    use ir::hir::StrId;
    use scribe_parser::parser::parse_program;
    use std::sync::Arc;
    use zetaruntime::arena::GrowableAtomicBump;
    use zetaruntime::bump::GrowableBump;
    use zetaruntime::string_pool::StringPool;

    fn parse<'a, 'bump>(
        src: &'bump str,
        bump: Arc<GrowableAtomicBump<'bump>>,
    ) -> Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>> {
        let ctx = Arc::new(StringPool::new().unwrap());
        let ctx_for_parse = Arc::clone(&ctx);
        parse_program(
            StrId(ctx_for_parse.intern(src)),
            "<test>",
            ctx_for_parse,
            bump,
        )
        .statements
    }

    macro_rules! first_stmt {
        ($stmts:expr) => {
            $stmts.into_iter().next().expect("no statements")
        };
    }

    #[test]
    fn test_simple_dereference() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { ptr.*; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::ExprStmt(expr_stmt) => match expr_stmt.expr {
                        Expr::Deref { .. } => {}
                        _ => panic!("Expected Deref expression, got {:?}", expr_stmt.expr),
                    },
                    _ => panic!("Expected ExprStmt"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_deref_then_field_access() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { ptr.*.field; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::ExprStmt(expr_stmt) => match expr_stmt.expr {
                        Expr::FieldAccess { object, .. } => match object {
                            Expr::Deref { .. } => {}
                            _ => panic!("Expected Deref inside FieldAccess, got {:?}", object),
                        },
                        _ => panic!("Expected FieldAccess expression, got {:?}", expr_stmt.expr),
                    },
                    _ => panic!("Expected ExprStmt"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_nested_deref() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { ptr.*.*; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::ExprStmt(expr_stmt) => match expr_stmt.expr {
                        Expr::Deref { expr: inner, .. } => match inner {
                            Expr::Deref { .. } => {}
                            _ => panic!("Expected nested Deref, got {:?}", inner),
                        },
                        _ => panic!("Expected Deref expression, got {:?}", expr_stmt.expr),
                    },
                    _ => panic!("Expected ExprStmt"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }
}
