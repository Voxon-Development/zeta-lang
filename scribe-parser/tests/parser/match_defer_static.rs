#[cfg(test)]
mod tests {
    use ir::ast::{DeferAction, Pattern, Stmt};
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
        let ctx_clone = Arc::clone(&ctx);
        std::mem::forget(ctx);
        parse_program(src, "<test>", ctx_clone, bump).statements
    }

    /// Drill into `fn main() { <stmt at index> }` and return that inner statement.
    macro_rules! body_stmt {
        ($stmts:expr, $idx:expr) => {{
            match $stmts.into_iter().next().expect("no stmts") {
                Stmt::FuncDecl(f) => f.body.expect("no body").block[$idx],
                other => panic!("expected FuncDecl, got {:?}", other),
            }
        }};
    }

    // ═══════════════════════════════════════════════════════════════════════
    // match statement tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_match_wildcard_arm() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { match x { case _ -> {} } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 1);
                assert!(matches!(m.arms[0].pattern, Pattern::Wildcard));
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_number_literal_arm() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { match code { case 200 -> {} case 404 -> {} } }",
            bump,
        );
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 2);
                assert!(matches!(m.arms[0].pattern, Pattern::Number(200)));
                assert!(matches!(m.arms[1].pattern, Pattern::Number(404)));
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_string_literal_arm() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(r#"fn main() { match s { case "hello" -> {} } }"#, bump);
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 1);
                assert!(matches!(m.arms[0].pattern, Pattern::String(_)));
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_boolean_arms() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { match flag { case true -> {} case false -> {} } }",
            bump,
        );
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 2);
                assert!(matches!(m.arms[0].pattern, Pattern::Boolean(true)));
                assert!(matches!(m.arms[1].pattern, Pattern::Boolean(false)));
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_identifier_binding() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { match val { case x -> {} } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 1);
                match m.arms[0].pattern {
                    Pattern::Ident(name) => assert_eq!(name.as_str(), "x"),
                    other => panic!("expected Ident pattern, got {:?}", other),
                }
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_enum_variant_single_binding() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { match opt { case Some(v) -> {} case None -> {} } }",
            bump,
        );
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 2);
                // First arm: Some(v)
                match m.arms[0].pattern {
                    Pattern::EnumVariant { name, bindings } => {
                        assert_eq!(name.as_str(), "Some");
                        assert_eq!(bindings.len(), 1);
                        assert!(matches!(bindings[0], Pattern::Ident(_)));
                    }
                    other => panic!("expected EnumVariant, got {:?}", other),
                }
                // Second arm: None (no bindings → Ident pattern)
                assert!(matches!(
                    m.arms[1].pattern,
                    Pattern::Ident(_) | Pattern::EnumVariant { .. }
                ));
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_enum_variant_multiple_bindings() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { match pair { case Pair(a, b) -> {} } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 1);
                match m.arms[0].pattern {
                    Pattern::EnumVariant { name, bindings } => {
                        assert_eq!(name.as_str(), "Pair");
                        assert_eq!(bindings.len(), 2);
                    }
                    other => panic!("expected EnumVariant, got {:?}", other),
                }
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_with_guard() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { match n { case x if x > 0 -> {} case _ -> {} } }",
            bump,
        );
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 2);
                // First arm has a guard
                assert!(m.arms[0].guard.is_some(), "expected guard on first arm");
                // Second arm (wildcard) has no guard
                assert!(m.arms[1].guard.is_none());
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_inline_arm_no_braces() {
        // Inline arms: `case pat -> expr;` without a brace block.
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "fn main() { match x { case 1 -> foo(), case _ -> bar() } }",
            bump,
        );
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 2);
                // Each arm body is a single-statement block (the inline expr was
                // wrapped in a synthetic block by the parser).
                assert_eq!(m.arms[0].block.block.len(), 1);
                assert_eq!(m.arms[1].block.block.len(), 1);
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_tuple_pattern() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { match p { case (a, b) -> {} } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 1);
                match m.arms[0].pattern {
                    Pattern::Tuple(inner) => assert_eq!(inner.len(), 2),
                    other => panic!("expected Tuple pattern, got {:?}", other),
                }
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_multiple_arms_subject_is_not_struct_init() {
        // Regression: `match my_var { ... }` must NOT be mis-parsed as struct init.
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { match result { case _ -> {} } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Match(_) => {} // success
            other => panic!("expected Match, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // defer statement tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_defer_block_form() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { defer { cleanup(); } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Defer(d) => {
                assert!(
                    matches!(d.action, DeferAction::Block(_)),
                    "expected DeferAction::Block"
                );
            }
            other => panic!("expected Defer, got {:?}", other),
        }
    }

    #[test]
    fn test_defer_inline_form() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { defer cleanup(); }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Defer(d) => {
                assert!(
                    matches!(d.action, DeferAction::Stmt(_)),
                    "expected DeferAction::Stmt"
                );
            }
            other => panic!("expected Defer, got {:?}", other),
        }
    }

    #[test]
    fn test_defer_block_with_multiple_stmts() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { defer { close(f); log(\"done\"); } }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Defer(d) => match &d.action {
                DeferAction::Block(block) => {
                    assert_eq!(block.block.len(), 2, "expected 2 statements in defer block");
                }
                _ => panic!("expected DeferAction::Block"),
            },
            other => panic!("expected Defer, got {:?}", other),
        }
    }

    #[test]
    fn test_defer_before_other_stmts() {
        // defer must not consume subsequent statements
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { defer cleanup(); let x = 1; }", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::FuncDecl(f) => {
                let body = f.body.expect("no body");
                assert_eq!(body.block.len(), 2);
                assert!(matches!(body.block[0], Stmt::Defer(_)));
                assert!(matches!(body.block[1], Stmt::Let(_)));
            }
            _ => panic!("expected FuncDecl"),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // static let tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_static_let_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("static let X: i32 = 5;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Let(l) => {
                assert_eq!(l.ident.as_str(), "X");
                assert!(l.is_static, "expected is_static = true");
                assert!(!l.mutable, "static let should be immutable by default");
            }
            other => panic!("expected Let, got {:?}", other),
        }
    }

    #[test]
    fn test_static_let_mut() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("static let mut COUNTER: i32 = 0;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Let(l) => {
                assert_eq!(l.ident.as_str(), "COUNTER");
                assert!(l.is_static, "expected is_static = true");
                assert!(l.mutable, "expected mutable");
            }
            other => panic!("expected Let, got {:?}", other),
        }
    }

    #[test]
    fn test_static_let_type_inferred() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("static let VERSION = 1;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Let(l) => {
                assert_eq!(l.ident.as_str(), "VERSION");
                assert!(l.is_static);
            }
            other => panic!("expected Let, got {:?}", other),
        }
    }

    #[test]
    fn test_plain_let_is_not_static() {
        // Regression: ordinary `let` must never set is_static.
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x = 1; }", bump);
        match body_stmt!(stmts, 0) {
            Stmt::Let(l) => {
                assert!(!l.is_static, "plain let must not be static");
            }
            other => panic!("expected Let, got {:?}", other),
        }
    }
}
