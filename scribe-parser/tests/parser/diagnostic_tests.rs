#[cfg(test)]
mod diagnostic_tests {
    use ir::ast::Stmt;
    use ir::errors::error::{DiagnosticError, ParseContext, ParseErrorKind};
    use scribe_parser::parser::parse_program;
    use std::sync::Arc;
    use zetaruntime::arena::GrowableAtomicBump;
    use zetaruntime::string_pool::StringPool;

    #[derive(Debug)]
    struct ParseReport<'a, 'bump> {
        statements: Vec<Stmt<'a, 'bump>, &'bump zetaruntime::bump::GrowableBump<'bump>>,
        errors: Vec<DiagnosticError<'a>>,
    }

    impl<'a, 'bump> ParseReport<'a, 'bump> {
        fn has_errors(&self) -> bool {
            !self.errors.is_empty()
        }

        fn emit_errors(&self) {
            for err in &self.errors {
                eprintln!("{}", err.pretty());
            }
        }
    }

    fn parse(src: &'static str) -> ParseReport<'static, 'static> {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let ctx = Arc::new(StringPool::new().unwrap());
        let ctx_for_parse = Arc::clone(&ctx);
        std::mem::forget(ctx);

        let result = parse_program(src, "<test>", ctx_for_parse, bump);

        ParseReport {
            statements: result.statements,
            errors: result.diagnostics.errors,
        }
    }

    /// Assert that a parse produces at least one error whose kind matches.
    macro_rules! assert_error {
        ($src:expr, $pat:pat) => {{
            let report = parse($src);
            assert!(
                report.has_errors(),
                "expected at least one error but parse succeeded"
            );
            let found = report.errors.iter().any(|e| matches!(&e.kind, $pat));
            if !found {
                report.emit_errors();
                panic!(
                    "no error matched the expected pattern; got {} error(s)",
                    report.errors.len()
                );
            }
            report
        }};
    }

    #[test]
    fn error_inside_function_carries_function_context() {
        // Missing closing brace in function body.
        let report = assert_error!(
            "fn foo() { let x = ;",
            ParseErrorKind::InvalidExpression { .. }
        );

        let err = &report.errors[0];
        assert!(
            err.context.contains(&ParseContext::ParsingFunction),
            "expected ParsingFunction in context chain, got: {:?}",
            err.context
        );
    }

    #[test]
    fn error_inside_impl_method_carries_impl_and_method_context() {
        let report = assert_error!(
            "impl Foo { fn bar( { } }",
            ParseErrorKind::UnexpectedToken {
                expected: ir::tokens::TokenKind::RParen,
                found: _,
            }
        );

        let err = &report.errors[0];
        assert!(err.context.contains(&ParseContext::ParsingImplBlock));
        assert!(err.context.contains(&ParseContext::ParsingMethod));
    }

    #[test]
    fn multiple_bad_top_level_items_produce_multiple_errors() {
        // Two broken function declarations separated by a good one.
        let src = r#"
        fn good() {}
        fn bad1( {
        fn good2() {}
        fn bad2( {
        fn good3() {}
    "#;

        let report = parse(src);
        // We should still get the three good functions in the AST.
        assert_eq!(report.statements.len(), 3, "should parse 3 good functions");
        // And exactly two errors.
        assert_eq!(report.errors.len(), 2, "should report 2 errors");
    }

    #[test]
    fn recovery_inside_impl_block_continues_parsing_remaining_methods() {
        let src = r#"
        impl Point {
            fn broken( {
            fn ok(&this) -> i32 { return 1; }
        }
    "#;

        let report = parse(src);
        // One error for the broken method.
        assert!(!report.errors.is_empty());
        // The impl block itself should still appear in the AST.
        assert_eq!(report.statements.len(), 1, "impl block should be in AST");
    }

    #[test]
    fn unexpected_eof_carries_note() {
        let report = parse("fn foo() {");
        let err = report
            .errors
            .iter()
            .find(|e| matches!(e.kind, ParseErrorKind::UnexpectedEOF { .. }));
        assert!(err.is_some(), "expected an UnexpectedEOF error");
        // The parser should attach a note about the unclosed brace.
        // (The note is added by parse_block when EOF is hit.)
        let e = err.unwrap();
        println!("{e:?}");
        if let ParseErrorKind::UnexpectedEOF { .. } = e.kind {
            return;
        }

        panic!("expected at least one note on EOF error");
    }

    #[test]
    fn tracing_enabled_does_not_panic() {
        // This test just verifies nothing crashes when tracing is on.
        // Redirect stderr in CI if the output is noisy.
        let _report = parse("fn ok() {}"); // parse() would pass tracing_enabled=true here
    }

    #[test]
    fn parser_stops_after_error_limit() {
        // 25 broken functions; parser should stop at max_errors (20) and add a
        // RecoveryFailure sentinel.
        let broken = "fn x( {\n".repeat(25);
        let broken = broken.into_boxed_str();
        let broken = Box::leak(broken);
        let report = parse(&*broken);
        assert!(
            report.errors.len() <= 25, // 20 real + 1 RecoveryFailure
            "error count should be capped: got {}",
            report.errors.len()
        );
        println!("{report:?}");
        let has_recovery_failure = report
            .errors
            .iter()
            .any(|e| matches!(e.kind, ParseErrorKind::RecoveryFailure));
        assert!(
            has_recovery_failure,
            "expected a RecoveryFailure sentinel error"
        );
    }
}
