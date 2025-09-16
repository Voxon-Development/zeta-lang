//\! Integration tests for the Zeta DSL fixture from the PR diff (Vec3f/Vec9f).
//\!
//\! Testing library/framework: Rust built-in test harness (#[test], assert\!, assert_eq\!).
//\!
//\! These tests focus on:
//\! - Parsing the diff program successfully using scribe_parser::parser::parse_program.
//\! - Type-checking via sentinel_typechecker::TypeChecker (+ MathTypeRule).
//\! - Lowering to HIR via scribe_parser::hir_lowerer::HirLowerer and sanity-checking identifiers.
//\! - Failure case: malformed source should yield a parse error.
//\! - Arena parser smoke test: scribe_parser::parse_program_to_ast returns a non-empty AST.
//\!
//\! Note: Execution/runtime evaluation is out of scope for this repository stage.
//\! We verify structural correctness (parse/type/lower) rather than runtime behavior.

fn fixture_basic() -> &'static str {
    r#"
type Vec3f(i32 x, i32 y, i32 z) {
    multiply(Vec3f by) {
        this.x *= by.x();
        this.y *= by.y();
        this.z *= by.z();
    }

    x() -> i32 = this.x;

    y() -> i32 = this.y;

    z() -> i32 = this.z;

    sum() -> i32 = this.x + this.y + this.z;
}

type Vec9f(Vec3f x, Vec3f y, Vec3f z) {
    multiply(Vec3f by) {
        this.x.multiply(by);
        this.y.multiply(by);
        this.z.multiply(by);
    }

    x() -> Vec3f = this.x;

    y() -> Vec3f = this.y;

    z() -> Vec3f = this.z;

    sum() -> i32 = this.x.sum() + this.y.sum() + this.z.sum();
}

main() {
    Vec3f firstVec3f = Vec3f { 1, 1, 1 };
    Vec3f secondVec3f = Vec3f { 1, 1, 1 };
    Vec3f thirdVec3f = Vec3f { 1, 1, 1 };
    Vec9f vec9f = Vec9f {
        firstVec3f,
        secondVec3f,
        thirdVec3f
    };
    i32 secondSum = vec9f.sum();
    println_int(secondSum);
}
"#
}

fn fixture_negative_zero() -> &'static str {
    r#"
type Vec3f(i32 x, i32 y, i32 z) {
    x() -> i32 = this.x;
    y() -> i32 = this.y;
    z() -> i32 = this.z;
    sum() -> i32 = this.x + this.y + this.z;
}
type Vec9f(Vec3f x, Vec3f y, Vec3f z) {
    sum() -> i32 = this.x.sum() + this.y.sum() + this.z.sum();
}
main() {
    Vec3f a = Vec3f { -1, 0, 1 };   // sum 0
    Vec3f b = Vec3f { -2, 2, 0 };   // sum 0
    Vec3f c = Vec3f { -3, -3, -3 }; // sum -9
    Vec9f v = Vec9f { a, b, c };
    println_int(v.sum()); // -9
}
"#
}

fn fixture_multiply() -> &'static str {
    r#"
type Vec3f(i32 x, i32 y, i32 z) {
    multiply(Vec3f by) {
        this.x *= by.x();
        this.y *= by.y();
        this.z *= by.z();
    }
    x() -> i32 = this.x;
    y() -> i32 = this.y;
    z() -> i32 = this.z;
    sum() -> i32 = this.x + this.y + this.z;
}
type Vec9f(Vec3f x, Vec3f y, Vec3f z) {
    multiply(Vec3f by) {
        this.x.multiply(by);
        this.y.multiply(by);
        this.z.multiply(by);
    }
    sum() -> i32 = this.x.sum() + this.y.sum() + this.z.sum();
}
main() {
    Vec3f a = Vec3f { 1, 2, 3 };   // -> *2 => 2,4,6 sum 12
    Vec3f b = Vec3f { 2, 3, 4 };   // -> *2 => 4,6,8 sum 18
    Vec3f c = Vec3f { -1, 0, 5 };  // -> *2 => -2,0,10 sum 8
    Vec9f v = Vec9f { a, b, c };
    Vec3f factor = Vec3f { 2, 2, 2 };
    v.multiply(factor);
    println_int(v.sum()); // 38
}
"#
}

fn fixture_zeroes() -> &'static str {
    r#"
type Vec3f(i32 x, i32 y, i32 z) {
    x() -> i32 = this.x;
    y() -> i32 = this.y;
    z() -> i32 = this.z;
    sum() -> i32 = this.x + this.y + this.z;
}
type Vec9f(Vec3f x, Vec3f y, Vec3f z) {
    sum() -> i32 = this.x.sum() + this.y.sum() + this.z.sum();
}
main() {
    Vec3f a = Vec3f { 0, 0, 0 };
    Vec3f b = Vec3f { 0, 0, 0 };
    Vec3f c = Vec3f { 0, 0, 0 };
    Vec9f v = Vec9f { a, b, c };
    println_int(v.sum()); // 0
}
"#
}

fn fixture_malformed() -> &'static str {
    r#"
type Vec3f(i32 x, i32 y, i32 z) {
    x() -> i32 = this.x;
    y() -> i32 = this.y;
    z() -> i32 = this.z;
    sum() -> i32 = this.x + this.y + this.z;
}

type Vec9f(Vec3f x, Vec3f y, Vec3f z) {
    sum() -> i32 = this.x.sum() + this.y.sum() + this.z.sum();
}

main() {
    Vec3f a = Vec3f { 1, 2, 3 };
    Vec3f b = Vec3f { 4, 5, 6 };
    Vec3f c = Vec3f { 7, 8, 9 };
    Vec9f v = Vec9f { a, b, c ; // <-- missing closing brace causes parse error
    println_int(v.sum());
}
"#
}

fn fixture_large_literals() -> &'static str {
    r#"
type Vec3f(i32 x, i32 y, i32 z) {
    x() -> i32 = this.x;
    y() -> i32 = this.y;
    z() -> i32 = this.z;
    sum() -> i32 = this.x + this.y + this.z;
}
type Vec9f(Vec3f x, Vec3f y, Vec3f z) {
    sum() -> i32 = this.x.sum() + this.y.sum() + this.z.sum();
}
main() {
    Vec3f a = Vec3f { 2147483647, 0, 0 };
    Vec3f b = Vec3f { 2147483647, 0, 0 };
    Vec3f c = Vec3f { 2147483647, 0, 0 };
    Vec9f v = Vec9f { a, b, c };
    println_int(v.sum());
}
"#
}

fn parse_then_typecheck(src: &str) -> Result<Vec<ir::ast::Stmt>, String> {
    let stmts = scribe_parser::parser::parse_program(src)
        .map_err(|e| format\!("parse error: {e}"))?;
    let mut tc = sentinel_typechecker::type_checker::TypeChecker::new();
    tc.add_rule(sentinel_typechecker::rules::MathTypeRule);
    tc.check_program(stmts.as_slice())
        .map_err(|errs| format\!("type errors: {errs:?}"))?;
    Ok(stmts)
}

fn lower_to_hir(stmts: Vec<ir::ast::Stmt>) -> ir::hir::HirModule {
    let mut lowerer = scribe_parser::hir_lowerer::HirLowerer::new();
    lowerer.lower_module(stmts)
}

#[test]
fn parses_and_typechecks_basic_fixture() {
    let src = fixture_basic();
    let parsed = scribe_parser::parser::parse_program(src);
    assert\!(parsed.is_ok(), "Expected basic fixture to parse, got: {parsed:?}");
    let stmts = parsed.unwrap();

    let mut tc = sentinel_typechecker::type_checker::TypeChecker::new();
    tc.add_rule(sentinel_typechecker::rules::MathTypeRule);
    let tc_res = tc.check_program(stmts.as_slice());
    assert\!(tc_res.is_ok(), "Type checking failed: {tc_res:?}");
}

#[test]
fn lowers_basic_fixture_and_contains_key_identifiers() {
    let stmts = parse_then_typecheck(fixture_basic()).expect("parse+typecheck should succeed");
    let module = lower_to_hir(stmts);
    let dbg = format\!("{:#?}", module);

    // Sanity checks for presence of important identifiers from the diff
    for needle in ["Vec3f", "Vec9f", "sum", "multiply", "main"] {
        assert\!(
            dbg.contains(needle),
            "HIR debug output missing identifier '{needle}'. HIR was:\n{dbg}"
        );
    }
}

#[test]
fn negative_and_zero_fixture_typechecks_and_lowers() {
    let stmts = parse_then_typecheck(fixture_negative_zero()).expect("parse+typecheck should succeed");
    let module = lower_to_hir(stmts);
    // Presence of types and main function references
    let dbg = format\!("{module:#?}");
    assert\!(dbg.contains("Vec3f") && dbg.contains("Vec9f") && dbg.contains("main"));
}

#[test]
fn multiply_fixture_typechecks_and_lowers() {
    let stmts = parse_then_typecheck(fixture_multiply()).expect("parse+typecheck should succeed");
    let module = lower_to_hir(stmts);
    let dbg = format\!("{module:#?}");
    assert\!(dbg.contains("multiply"), "Expected method 'multiply' present in HIR");
}

#[test]
fn zeroes_fixture_typechecks_and_lowers() {
    let stmts = parse_then_typecheck(fixture_zeroes()).expect("parse+typecheck should succeed");
    let module = lower_to_hir(stmts);
    let dbg = format\!("{module:#?}");
    assert\!(dbg.contains("sum"), "Expected 'sum' present in lowered HIR");
}

#[test]
fn malformed_fixture_fails_to_parse() {
    let res = scribe_parser::parser::parse_program(fixture_malformed());
    assert\!(res.is_err(), "Malformed input should fail to parse");
}

#[test]
fn arena_parser_smoke_test_non_empty_ast() {
    // Ensure the new arena parser can also parse the program without panicking.
    let ast = scribe_parser::parse_program_to_ast(fixture_basic());
    assert\!(
        \!ast.stmts.is_empty(),
        "Arena parser returned an empty AST for the basic fixture"
    );
}

#[test]
fn large_literals_parse_and_typecheck() {
    let res = parse_then_typecheck(fixture_large_literals());
    assert\!(res.is_ok(), "Large literal handling should parse+typecheck: {res:?}");
}