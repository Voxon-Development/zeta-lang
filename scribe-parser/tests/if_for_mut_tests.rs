use ir::hir::StrId;
use scribe_parser::parser::parse_program;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

#[test]
fn test_if_expression_parsing() {
    let code = r#"
fn test_if(): i32 {
    let x: i32 = if (5 > 3) { 10 } else { 20 };
    x
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_if_expression_with_return() {
    let code = r#"
fn abs(x: i32): i32 {
    if (x < 0) { return -x } else { return x }
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_for_in_range_loop() {
    let code = r#"
fn sum_range(): i32 {
    mut sum: i32 = 0;
    for i in 0..10 {
        sum = sum + i;
    }
    sum
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_for_in_exclusive_range() {
    let code = r#"
fn sum_exclusive(): i32 {
    mut sum: i32 = 0;
    for i in 0..< 10 {
        sum = sum + i;
    }
    sum
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_mutable_let_binding() {
    let code = r#"
fn test_mut(): i32 {
    mut x: i32 = 5;
    x = x + 10;
    x
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_immutable_let_binding() {
    let code = r#"
fn test_immut(): i32 {
    let x: i32 = 5;
    x
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_nested_if_expressions() {
    let code = r#"
fn nested_if(x: i32): i32 {
    if (x > 0) {
        if (x > 10) { 100 } else { 50 }
    } else {
        if (x < -10) { -100 } else { -50 }
    }
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_for_loop_with_mut_variable() {
    let code = r#"
fn loop_with_mut(): i32 {
    mut total: i32 = 0;
    for i in 1..5 {
        mut temp: i32 = i * 2;
        total = total + temp;
    }
    total
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_range_in_expression() {
    let code = r#"
fn test_range(): i32 {
    let r: i32 = 0..10;
    r
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    // Should parse without errors (even though range semantics aren't fully implemented)
    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}

#[test]
fn test_complex_if_with_arithmetic() {
    let code = r#"
fn complex_if(x: i32, y: i32): i32 {
    let result: i32 = if (x + y > 10) {
        x * y
    } else if (x - y < 0) {
        x + y
    } else {
        x / y
    };
    result
}
"#;

    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(
        StrId(context.clone().intern(code)),
        "test.zeta",
        context.clone(),
        atomic_bump.clone(),
    );

    assert!(
        !result.diagnostics.has_errors(),
        "Parser should not have errors"
    );
    assert!(
        !result.statements.is_empty(),
        "Should have parsed statements"
    );
}
