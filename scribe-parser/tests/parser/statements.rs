#[cfg(test)]
mod tests {
    use ir::ast::{ForKind, Stmt, TypeKind};
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
        std::mem::forget(ctx);
        parse_program(src, "<test>", ctx_for_parse, bump).statements
    }

    // Helper to get first statement
    macro_rules! first_stmt {
        ($stmts:expr) => {{ $stmts.into_iter().next().expect("no statements") }};
    }

    #[test]
    fn test_let_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x = 42; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                let stmt = body.block[0];
                match stmt {
                    Stmt::Let(let_stmt) => {
                        assert_eq!(let_stmt.ident.as_str(), "x");
                        assert!(!let_stmt.mutable);
                        assert!(matches!(let_stmt.type_annotation.kind, TypeKind::Infer));
                    }
                    _ => panic!("Expected Let, got {:?}", stmt),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_let_mut() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let mut y = 10; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        assert_eq!(let_stmt.ident.as_str(), "y");
                        assert!(let_stmt.mutable);
                    }
                    _ => panic!("Expected Let"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_let_with_type() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x: i32 = 5; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        assert_eq!(let_stmt.ident.as_str(), "x");
                        assert!(matches!(let_stmt.type_annotation.kind, TypeKind::I32));
                    }
                    _ => panic!("Expected Let"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_shorthand_let() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { hello_world := \"Hello, World\"; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        assert_eq!(let_stmt.ident.as_str(), "hello_world");
                        assert!(!let_stmt.mutable);
                        assert!(matches!(let_stmt.type_annotation.kind, TypeKind::Infer));
                    }
                    _ => panic!("Expected Let for shorthand"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_if_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { if (x > 0) { let y = 1; } }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::If(if_stmt) => {
                        assert!(if_stmt.else_branch.is_none());
                        assert_eq!(if_stmt.then_branch.block.len(), 1);
                    }
                    _ => panic!("Expected If"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_if_else() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { if (x > 0) { let a = 1; } else { let b = 2; } }",
            bump,
        );
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::If(if_stmt) => {
                        assert!(if_stmt.else_branch.is_some());
                        assert_eq!(if_stmt.then_branch.block.len(), 1);
                    }
                    _ => panic!("Expected If"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_if_else_if() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { if (x > 0) { let a = 1; } else if (x < 0) { let b = 2; } else { let c = 3; } }",
            bump,
        );
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::If(if_stmt) => {
                        assert!(if_stmt.else_branch.is_some());
                        assert_eq!(if_stmt.then_branch.block.len(), 1);
                    }
                    _ => panic!("Expected If"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_while_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { while (i < 10) { i += 1; } }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::While(while_stmt) => {
                        assert_eq!(while_stmt.block.block.len(), 1);
                    }
                    _ => panic!("Expected While"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_while_empty_body() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { while (true) {} }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::While(while_stmt) => {
                        assert_eq!(while_stmt.block.block.len(), 0);
                    }
                    _ => panic!("Expected While"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_for_c_style_full() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { for (let i = 0; i < 10; i += 1) { let x = i; } }",
            bump,
        );
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::For(for_stmt) => {
                        match for_stmt.kind {
                            ForKind::CStyle {
                                let_stmt,
                                condition,
                                increment,
                            } => {
                                assert!(let_stmt.is_some());
                                assert!(condition.is_some());
                                assert!(increment.is_some());
                                let ls = let_stmt.unwrap();
                                assert_eq!(ls.ident.as_str(), "i");
                            }
                            _ => panic!("Expected CStyle for loop"),
                        }
                        assert_eq!(for_stmt.block.block.len(), 1);
                    }
                    _ => panic!("Expected For"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_for_c_style_no_init() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { for (; i < 10; i += 1) {} }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::For(for_stmt) => match for_stmt.kind {
                        ForKind::CStyle {
                            let_stmt,
                            condition,
                            increment,
                        } => {
                            assert!(let_stmt.is_none());
                            assert!(condition.is_some());
                            assert!(increment.is_some());
                        }
                        _ => panic!("Expected CStyle for loop"),
                    },
                    _ => panic!("Expected For"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_for_c_style_no_condition() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { for (let i = 0;; i += 1) { break; } }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::For(for_stmt) => match for_stmt.kind {
                        ForKind::CStyle {
                            let_stmt,
                            condition,
                            increment,
                        } => {
                            assert!(let_stmt.is_some());
                            assert!(condition.is_none());
                            assert!(increment.is_some());
                        }
                        _ => panic!("Expected CStyle for loop"),
                    },
                    _ => panic!("Expected For"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_for_c_style_no_increment() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { for (let i = 0; i < 10;) {} }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::For(for_stmt) => match for_stmt.kind {
                        ForKind::CStyle {
                            let_stmt,
                            condition,
                            increment,
                        } => {
                            assert!(let_stmt.is_some());
                            assert!(condition.is_some());
                            assert!(increment.is_none());
                        }
                        _ => panic!("Expected CStyle for loop"),
                    },
                    _ => panic!("Expected For"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_for_c_style_mut_variable() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { for (let mut i = 0; i < 10; i += 1) {} }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::For(for_stmt) => match for_stmt.kind {
                        ForKind::CStyle { let_stmt, .. } => {
                            let ls = let_stmt.expect("expected let stmt");
                            assert!(ls.mutable);
                            assert_eq!(ls.ident.as_str(), "i");
                        }
                        _ => panic!("Expected CStyle for loop"),
                    },
                    _ => panic!("Expected For"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }
}
