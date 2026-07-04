#[cfg(test)]
mod tests {
    use ir::ast::{
        ExternModifier, FuncSafety, InlineModifier, Param, Stmt, Type, TypeKind, Visibility,
    };
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
        parse_program(StrId(ctx.intern(src)), "<test>", ctx_for_parse, bump).statements
    }

    macro_rules! func {
        ($stmts:expr) => {{
            match $stmts.into_iter().next().expect("no statements") {
                Stmt::FuncDecl(f) => f,
                other => panic!("Expected FuncDecl, got {:?}", other),
            }
        }};
    }

    #[test]
    fn test_simple_name() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert_eq!(f.name.as_str(), "foo");
    }

    #[test]
    fn test_name_with_underscores() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn my_cool_func() {}", bump));
        assert_eq!(f.name.as_str(), "my_cool_func");
    }

    #[test]
    fn test_name_starts_with_underscore() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn _internal() {}", bump));
        assert_eq!(f.name.as_str(), "_internal");
    }

    #[test]
    #[should_panic]
    fn test_invalid_name_number() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("fn 123() {}", bump);
    }

    #[test]
    #[should_panic]
    fn test_missing_name() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("fn () {}", bump);
    }

    #[test]
    fn test_visibility_public_by_default() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert_eq!(f.function_metadata.visibility, Visibility::Public);
    }

    /*#[test]
    fn test_visibility_private() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("private fn foo() {}", bump));
        assert_eq!(f.function_metadata.visibility, Visibility::Private);
    }

    #[test]
    fn test_visibility_module() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("module fn foo() {}", bump));
        assert_eq!(f.function_metadata.visibility, Visibility::Module);
    }

    #[test]
    fn test_visibility_internal() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("internal fn foo() {}", bump));
        assert_eq!(f.function_metadata.visibility, Visibility::Internal);
    }*/

    #[test]
    fn test_safe_by_default() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert_eq!(f.function_metadata.func_safety, FuncSafety::Safe);
    }

    #[test]
    fn test_unsafe_modifier() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("unsafe fn foo() {}", bump));
        assert_eq!(f.function_metadata.func_safety, FuncSafety::Unsafe);
    }

    #[test]
    fn test_inline_modifier() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("inline fn foo() {}", bump));
        assert_eq!(f.function_metadata.inline_modifier, InlineModifier::Inline);
    }

    #[test]
    fn test_noinline_modifier() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("noinline fn foo() {}", bump));
        assert_eq!(
            f.function_metadata.inline_modifier,
            InlineModifier::Noinline
        );
    }

    #[test]
    fn test_no_inline_modifier_defaults_to_none() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert_eq!(f.function_metadata.inline_modifier, InlineModifier::None);
    }

    #[test]
    #[should_panic]
    fn test_inline_then_noinline_panics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("inline noinline fn foo() {}", bump);
    }

    #[test]
    #[should_panic]
    fn test_noinline_then_inline_panics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("noinline inline fn foo() {}", bump);
    }

    #[test]
    #[should_panic]
    fn test_duplicate_inline_panics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("inline inline fn foo() {}", bump);
    }

    #[test]
    #[should_panic]
    fn test_duplicate_noinline_panics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("noinline noinline fn foo() {}", bump);
    }

    #[test]
    fn test_extern_c_abi() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse(r#"extern "C" fn foo() {}"#, bump));
        match f.function_metadata.extern_modifier {
            ExternModifier::Abi(name) => assert_eq!(name.as_str(), "C"),
            ExternModifier::None => panic!("Expected Abi, got None"),
        }
    }

    #[test]
    fn test_no_extern_defaults_to_none() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert_eq!(f.function_metadata.extern_modifier, ExternModifier::None);
    }

    #[test]
    fn test_no_generics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert!(f.generics.map_or(true, |g| g.is_empty()));
    }

    #[test]
    fn test_single_generic() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo<T>() {}", bump));
        let g = f.generics.unwrap();
        assert_eq!(g.len(), 1);
        assert_eq!(g[0].type_name.as_str(), "T");
    }

    #[test]
    fn test_multiple_generics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo<T, U, V>() {}", bump));
        assert_eq!(f.generics.unwrap().len(), 3);
    }

    #[test]
    fn test_generic_with_single_constraint() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo<T: Serialize>() {}", bump));
        assert_eq!(f.generics.unwrap()[0].constraints.len(), 1);
    }

    #[test]
    fn test_generic_with_multiple_constraints() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo<T: Serialize + Clone>() {}", bump));
        assert_eq!(f.generics.unwrap()[0].constraints.len(), 2);
    }

    #[test]
    #[should_panic]
    fn test_unclosed_generic_panics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("fn foo<T() {}", bump);
    }

    #[test]
    fn test_no_params() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert!(f.params.map_or(true, |p| p.is_empty()));
    }

    #[test]
    fn test_single_param() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo(x: i32) {}", bump));
        assert_eq!(f.params.unwrap().len(), 1);
    }

    #[test]
    fn test_multiple_params() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo(x: i32, y: f64, z: boolean) {}", bump));
        assert_eq!(f.params.unwrap().len(), 3);
    }

    #[test]
    fn test_this_param() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo(this) {}", bump));
        assert!(
            f.params
                .unwrap()
                .iter()
                .any(|p| matches!(p, Param::This(_)))
        );
    }

    #[test]
    fn test_this_with_normal_param() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo(this, x: i32) {}", bump));
        let params = f.params.unwrap();
        assert_eq!(params.len(), 2);
        assert!(matches!(params[0], Param::This(_)));
    }

    #[test]
    fn test_mut_param() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo(mut x: i32) {}", bump));
        match f.params.unwrap()[0] {
            Param::Normal(n) => assert!(n.is_mut),
            _ => panic!("Expected normal param"),
        }
    }

    #[test]
    fn test_no_return_type() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() {}", bump));
        assert!(f.return_type.is_none());
    }

    #[test]
    fn test_primitive_return_i32() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> i32 {}", bump));
        assert_eq!(f.return_type.unwrap(), Type::i32());
    }

    #[test]
    fn test_primitive_return_bool() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> boolean {}", bump));
        assert_eq!(f.return_type.unwrap(), Type::boolean());
    }

    #[test]
    fn test_named_return_type() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> Order {}", bump));
        assert!(matches!(
            f.return_type.unwrap().kind,
            TypeKind::Struct { .. }
        ));
    }

    #[test]
    fn test_generic_return_type() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> List<Order> {}", bump));
        match f.return_type.unwrap().kind {
            TypeKind::Struct { generics, .. } => assert_eq!(generics.len(), 1),
            _ => panic!("Expected Struct type"),
        }
    }

    #[test]
    fn test_nullable_return_type() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> Order? {}", bump));
        assert!(f.return_type.unwrap().nullable);
    }

    /*
    #[test]
    fn test_error_return_type_single() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> ParseError!Order {}", bump));
        assert!(f.return_type.unwrap().error);
    }

    #[test]
    fn test_error_return_type_multi() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> {ParseError, IoError}!Order {}", bump));
        assert!(f.return_type.unwrap().error);
    }

    #[test]
    fn test_error_and_nullable_return_type() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let f = func!(parse("fn foo() -> {ParseError, IoError}!Order? {}", bump));
        let ret = f.return_type.unwrap();
        assert!(ret.error);
        assert!(ret.nullable);
    }
     */

    #[test]
    #[should_panic]
    fn test_missing_type_after_arrow_panics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        parse("fn foo() -> {}", bump);
    }
}
