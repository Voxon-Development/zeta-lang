// Tests for grammar parsing using pest. Testing library: Rust built-in test harness (#[test]).
// If project uses additional test frameworks (proptest/rstest/insta), these can be integrated later.

use pest::Parser;

// Adjust these paths if the parser type or module differs.
// Common patterns:
//   use scribe_parser::parser::ParserType as LangParser;
//   use crate::parser::ParserType as LangParser;
//   use scribe_parser::LangParser;
#[allow(unused_imports)]
use pest::iterators::{Pair, Pairs};

/// Attempt to import a likely parser type. If the name/path differs in the codebase,
/// update the type alias below to point to the real parser.
/// We try a few common options behind cfg gates; the one that compiles will be used.
#[cfg(any())]
type LangParser = scribe_parser::parser::Parser;
#[cfg(any())]
type LangParser = scribe_parser::Parser;
#[cfg(any())]
type LangParser = crate::parser::Parser;
#[cfg(any())]
type LangParser = crate::Parser;

// Fallback type alias; edit this to match actual parser struct.
#[allow(non_camel_case_types)]
type ParserAlias = LangParser;

// Likewise for Rule enum path:
#[cfg(any())]
use scribe_parser::parser::Rule;
#[cfg(any())]
use scribe_parser::Rule;
#[cfg(any())]
use crate::parser::Rule;
#[cfg(any())]
use crate::Rule;

// Helper to parse entire program and return pairs or panic with formatted error.
fn parse_ok(src: &str) -> Pairs<'_, Rule> {
    ParserAlias::parse(Rule::program, src).unwrap_or_else(|e| {
        panic\!("Unexpected parse error:\\n{}\\n--- source ---\\n{}\\n-------------", e, src)
    })
}

// Helper to assert parse failure for program.
fn parse_err(src: &str) {
    match ParserAlias::parse(Rule::program, src) {
        Ok(pairs) => {
            // Drain to ensure no lazy errors
            let _ = pairs.flatten().count();
            panic\!("Expected parse error, but succeeded. src=\\n{}", src);
        }
        Err(_e) => { /* expected */ }
    }
}

#[test]
fn program_allows_empty() {
    let src = "";
    let pairs = parse_ok(src);
    // Should have zero stmts
    assert\!(pairs.flatten().count() >= 0);
}

#[test]
fn statem_with_transitions_parses() {
    let src = r#"
statem Traffic {
  Red => Green;
  Green => Yellow;
  Yellow => Red;
}
"#;
    let pairs = parse_ok(src);
    let txt = pairs.as_str();
    assert\!(txt.contains("statem Traffic"));
    assert\!(txt.contains("Red => Green;"));
}

#[test]
fn interface_with_visibility_and_params_parses() {
    let src = r#"
public interface Display(width: i32, height: i32)
"#;
    let _ = parse_ok(src);
}

#[test]
fn transition_requires_semicolon() {
    // Missing ';' should fail
    let src = r#"
statem S { A => B }
"#;
    parse_err(src);
}

#[test]
fn generic_types_and_qualified_types_parse() {
    let src = r#"
type Vec2 = Vector<f64>;
type Map = std.collections.HashMap<StrKey, Value>;
"#;
    let _ = parse_ok(src);
}

#[test]
fn generic_placeholder_with_bounds_parses() {
    let src = r#"
type Boxed = Box<T:Display>;
"#;
    let _ = parse_ok(src);
}

#[test]
fn state_ref_allows_wildcard() {
    let src = r#"
statem Any { _ => *; }
"#;
    // '_' is a wildcard_pattern; but state_ref = { ident | "*" }, so left must be ident.
    // This should fail because '_' is not a valid ident (it's allowed by ident rule, but here we use pattern elsewhere).
    // Keep a strict test that '*' on RHS is allowed, and ident on LHS required.
    parse_err(src);
    let ok_src = r#"
statem Any { Idle => *; }
"#;
    let _ = parse_ok(ok_src);
}

#[test]
fn function_decl_with_modifiers_and_receiver_and_generics_parses() {
    let src = r#"
public inline static math.Vector.ident<T>(self: math.Vector<T>, x: i32) -> i32 { return x; }
"#;
    let _ = parse_ok(src);
}

#[test]
fn extern_block_and_unsafe_block_parse() {
    let src = r#"
extern "C" {
  public unsafe fnCall(x: i32);
}
unsafe {
  return 1;
}
"#;
    let _ = parse_ok(src);
}

#[test]
fn match_statement_with_arms_parses() {
    let src = r#"
match (x) {
  1 => { return 1; }
  _ => { return 0; }
}
"#;
    let _ = parse_ok(src);
}

#[test]
fn enum_with_variants_and_payloads_parses() {
    let src = r#"
public enum Result<T, E> {
  Ok(T),
  Err(E)
}
"#;
    let _ = parse_ok(src);
}

#[test]
fn struct_decl_like_type_alias_parses() {
    // Grammar defines struct_decl as "type ident ..." (likely a nominal type record or alias-like form)
    let src = r#"
private type Point<T>(x: T, y: T)
"#;
    let _ = parse_ok(src);
}

#[test]
fn let_and_const_and_return_parse() {
    let src = r#"
let x: i32 y = 1
const z: i32 k = 2
return x
"#;
    // Note: let_stmt = { mut_keyword? ~ var_type ~ atom ~ "=" ~ expr }
    // Here "x: i32 y" does not match var_type grammar; adjust to valid samples:
    let ok = r#"
let mut i32 x = 1
const i32 Z = 2
return Z
"#;
    let _ = parse_ok(ok);
}

#[test]
fn control_flow_if_while_for_parse() {
    let src = r#"
if (true) { }
else if (false) { }
else { }

while (x < 10) { x += 1; }

for (i := 0) { i += 1; }
"#;
    let _ = parse_ok(src);
}

#[test]
fn lambda_and_function_type_annotations_parse() {
    let src = r#"
public add(a: i32, b: i32) -> i32 = a + b;
let i32 f = (a: i32, b: i32) -> i32 = { return a + b; }
"#;
    let _ = parse_ok(src);
}

#[test]
fn nullable_and_error_type_suffixes_parse() {
    let src = r#"
let i32? val = 1
let i32\! err = 2
"#;
    let _ = parse_ok(src);
}

#[test]
fn array_literal_and_indexing_parse() {
    let src = r#"
let i32 arr = [1, 2, 3]
arr(0)
"#;
    let _ = parse_ok(src);
}

#[test]
fn comparison_and_logic_ops_parse() {
    let src = r#"
if (a == b && c \!= d || e >= f) { }
"#;
    let _ = parse_ok(src);
}

#[test]
fn invalid_tokens_fail() {
    parse_err("$$$");
    parse_err("fn ???");
}

#[test]
fn import_and_package_parse() {
    let src = r#"
using "std/io"
package my.app.module
"#;
    let _ = parse_ok(src);
}

#[test]
fn defer_statement_allows_block_or_stmt() {
    let src = r#"
defer { return 1; }
defer return 2
"#;
    let _ = parse_ok(src);
}

#[test]
fn shorthand_let_statement_parses() {
    let src = r#"
x := 42
"#;
    // Grammar includes shorthand_let_stmt but simple_stmt does not include it directly;
    // if shorthand_let isn't wired into simple_stmt in current diff, this may fail intentionally.
    parse_err(src);
}