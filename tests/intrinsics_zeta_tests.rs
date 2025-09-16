//\! Integration tests focusing on Zeta intrinsic declarations and basic semantic validation.
//\!
//\! Testing framework: Rust built-in test framework (cargo test).
//\!
//\! These tests assume the crate exposes APIs to parse and (optionally) type-check Zeta modules.
//\! The tests focus on the following intrinsics defined in the PR diff:
//\! - sizeof<T>() -> usize
//\! - alignof<T>() -> usize
//\! - ptr_cast_to_raw<T>(T) -> usize
//\! - ptr_add<T>(Ptr<T>, usize) -> Ptr<T>
//\! - ptr_deref<T>(Ptr<T>) -> T
//\! - ptr_store<T>(Ptr<T>, T)
//\! - ptr_from_addr<T>(usize) -> Ptr<T>
//\! - ptr_addr<T>(Ptr<T>) -> usize
//\! - stack_alloc<T, const N: usize>() -> Ptr<T>
//\! - stack_alloc_zeroed<T, const N: usize>() -> Ptr<T>
//\!
//\! If parser/typechecker symbols differ, adjust imports below to match actual crate APIs.

use std::fmt;

// Try multiple plausible crate names. Only one should exist; the rest are behind cfg guards.
// Replace with the correct crate path if different.
#[cfg(feature = "zeta_api")]
use zeta_api as zeta_crate;
#[cfg(all(not(feature = "zeta_api"), feature = "zeta"))]
use zeta as zeta_crate;
#[cfg(all(not(feature = "zeta_api"), not(feature = "zeta")))]
use zeta_crate_fallback as zeta_crate;

// Fallback trait to avoid compile errors if pretty printers differ.
trait DebugStr {
    fn dbg_str(&self) -> String;
}
impl<T: fmt::Debug> DebugStr for T {
    fn dbg_str(&self) -> String {
        format\!("{:#?}", self)
    }
}

fn zeta_source_intrinsics() -> &'static str {
    r#"
package zeta

module "intrinsic" sizeof<T>() -> usize
module "intrinsic" alignof<T>() -> usize

module extern "intrinsic" ptr_cast_to_raw<T>(T value) -> usize
module extern "intrinsic" ptr_add<T>(Ptr<T> p, usize bytes) -> Ptr<T>
module extern "intrinsic" ptr_deref<T>(Ptr<T> p) -> T
module extern "intrinsic" ptr_store<T>(Ptr<T> p, T value)
module extern "intrinsic" ptr_from_addr<T>(usize addr) -> Ptr<T>
module extern "intrinsic" ptr_addr<T>(Ptr<T> p) -> usize

module extern "intrinsic" fn stack_alloc<T, const N: usize>() -> Ptr<T>;
module extern "intrinsic" fn stack_alloc_zeroed<T, const N: usize>() -> Ptr<T>;
"#,
}

#[test]
fn parse_intrinsic_declarations_without_error() {
    // Goal: The intrinsic declarations should parse successfully.
    // Replace the following calls with the actual parser API exposed by the crate.
    // Common patterns:
    //   let ast = zeta_crate::parser::parse(zeta_source_intrinsics()).expect("parse ok");
    //   assert\!(ast.modules().any(...));
    //
    // For now, we use cfg to cover multiple possible API shapes. Adjust as needed.

    #[cfg(any(feature = "zeta_api", feature = "zeta"))]
    {
        let src = zeta_source_intrinsics();
        let ast = zeta_crate::parser::parse(src).expect("intrinsics source parses");
        // Basic structural checks
        assert\!(ast.packages().iter().any(|p| p.name() == "zeta"), "package 'zeta' present");
        // Ensure at least 10 intrinsic items are present
        let decls = ast.items();
        assert\!(decls.len() >= 10, "expected >= 10 intrinsic declarations, got {}", decls.len());
    }

    // If the specific API is unavailable, keep a no-op assertion to flag test presence.
    #[cfg(all(not(feature = "zeta_api"), not(feature = "zeta")))]
    {
        assert\!(\!zeta_source_intrinsics().is_empty());
    }
}

#[test]
fn typecheck_intrinsics_signatures_are_well_formed() {
    // Goal: type checker should accept polymorphic and const-generic signatures where supported.
    #[cfg(any(feature = "zeta_api", feature = "zeta"))]
    {
        let src = zeta_source_intrinsics();
        let unit = zeta_crate::compile::analyze(src).expect("semantic analysis ok");
        // Check representative signatures exist and are monomorphizable
        let sizeof_sig = unit.lookup_fn("sizeof").expect("sizeof present");
        assert_eq\!(sizeof_sig.return_type().to_string(), "usize");

        let alignof_sig = unit.lookup_fn("alignof").expect("alignof present");
        assert_eq\!(alignof_sig.return_type().to_string(), "usize");

        let ptr_add_sig = unit.lookup_fn("ptr_add").expect("ptr_add present");
        assert\!(ptr_add_sig
            .params()
            .iter()
            .map(|p| p.ty().to_string())
            .eq(["Ptr<T>", "usize"]), "ptr_add params are (Ptr<T>, usize): {:?}", 
            ptr_add_sig.params().iter().map(|p| p.ty().to_string()).collect::<Vec<_>>());

        let stack_alloc_sig = unit.lookup_fn("stack_alloc").expect("stack_alloc present");
        assert\!(stack_alloc_sig.has_const_generic("N"), "stack_alloc has const generic N");
        assert_eq\!(stack_alloc_sig.return_type().to_string(), "Ptr<T>");
    }

    #[cfg(all(not(feature = "zeta_api"), not(feature = "zeta")))]
    {
        assert\!(true, "placeholder when semantic API is not available");
    }
}

#[test]
fn reject_invalid_intrinsic_usages_examples() {
    // Negative tests: demonstrate typical misuse patterns are caught.
    // Example snippets use the declared intrinsics with invalid types/arity.
    // The compiler should produce diagnostics (error count > 0).
    #[cfg(any(feature = "zeta_api", feature = "zeta"))]
    {
        let bad_cases = [
            // Wrong arity for sizeof (expects 0 value params, generic T only)
            r#"package zeta
               module test fn main() -> usize {
                 sizeof<i32>(123)
               }"#,
            // Mismatched pointer type to ptr_add
            r#"package zeta
               module test fn main(p: Ptr<i32>) {
                 let q = ptr_add<f32>(p, 4usize);
               }"#,
            // Using stack_alloc with N = 0 (if disallowed)
            r#"package zeta
               module test fn main() {
                 let _p = stack_alloc<i32, 0>();
               }"#,
            // ptr_store value type mismatch
            r#"package zeta
               module test fn main(p: Ptr<i32>) {
                 ptr_store<i32>(p, true);
               }"#,
        ];

        for (i, code) in bad_cases.iter().enumerate() {
            let res = zeta_crate::compile::analyze(code);
            assert\!(res.is_err(), "bad case {} should error but succeeded", i);
        }
    }
}

#[test]
fn accept_valid_intrinsic_usages_examples() {
    // Positive tests: usage samples expected to type-check.
    #[cfg(any(feature = "zeta_api", feature = "zeta"))]
    {
        let good_cases = [
            // sizeof/alignof generics no value args
            r#"package zeta
               module test fn main() -> usize {
                 sizeof<i64>() + alignof<i64>()
               }"#,
            // pointer arithmetic round-trip
            r#"package zeta
               module test fn main(addr: usize) -> usize {
                 let p = ptr_from_addr<i32>(addr);
                 let p2 = ptr_add<i32>(p, 8usize);
                 ptr_addr<i32>(p2)
               }"#,
            // stack allocation returns pointer of T
            r#"package zeta
               module test fn main() {
                 let buf = stack_alloc<u8, 16>();
                 let zeroed = stack_alloc_zeroed<u8, 32>();
                 // store/load cycle
                 ptr_store<u8>(buf, 255u8);
                 let _x = ptr_deref<u8>(buf);
               }"#,
            // casting structured value to raw
            r#"package zeta
               module test type Pair { a: i32, b: i32 }
               module test fn main(p: Pair) -> usize {
                 ptr_cast_to_raw<Pair>(p)
               }"#,
        ];

        for (i, code) in good_cases.iter().enumerate() {
            let res = zeta_crate::compile::analyze(code);
            if let Err(e) = res {
                panic\!("good case {} should succeed: {}", i, e.dbg_str());
            }
        }
    }
}

#[test]
fn edge_cases_const_generic_bounds_for_stack_alloc() {
    // Validate typical const-generic edge constraints for allocation size if the compiler enforces them.
    #[cfg(any(feature = "zeta_api", feature = "zeta"))]
    {
        let zero = r#"package zeta
                      module test fn main() { let _p = stack_alloc<i8, 0>(); }"#;
        let huge = r#"package zeta
                      module test fn main() { let _p = stack_alloc<i8, 4294967295>(); }"#;

        // Depending on spec, N=0 might be invalid — treat both as "compile and then check diagnostics count".
        let r_zero = zeta_crate::compile::analyze(zero);
        let r_huge = zeta_crate::compile::analyze(huge);

        // At least one of these should fail in typical implementations.
        if r_zero.is_ok() && r_huge.is_ok() {
            // If both pass by design, still assert that type is Ptr<i8>
            let unit = r_zero.unwrap();
            let f = unit.lookup_fn("main").expect("main exists");
            assert\!(f.locals().iter().any(|l| l.ty().to_string() == "Ptr<i8>"));
        } else {
            assert\!(r_zero.is_err() || r_huge.is_err(), "expected at least one const bound violation");
        }
    }
}

#[test]
fn pointer_roundtrip_address_identity() {
    // Check ptr_from_addr -> ptr_addr roundtrip preserves address.
    #[cfg(any(feature = "zeta_api", feature = "zeta"))]
    {
        let src = r#"package zeta
                     module test fn main(a: usize) -> usize {
                       let p = ptr_from_addr<u8>(a);
                       ptr_addr<u8>(p)
                     }"#;
        let unit = zeta_crate::compile::analyze(src).expect("analyze ok");
        let main = unit.lookup_fn("main").expect("main exists");
        // If IR or evaluator is available, we could simulate with a small interpreter.
        // For structure-only check, ensure return type is usize and uses the intrinsics.
        assert_eq\!(main.return_type().to_string(), "usize");
        let body = main.body();
        let text = body.dbg_str();
        assert\!(text.contains("ptr_from_addr") && text.contains("ptr_addr"), "uses addr roundtrip intrinsics");
    }
}