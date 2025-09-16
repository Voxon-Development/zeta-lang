// Tests for zetaruntime::fiber
// Testing library/framework: Rust built-in #[test] (no external macros).
// Rationale:
// - The PR did not include a consumable API diff snippet for fiber; we therefore validate the public module export
//   and statically analyze the source to assert presence of key public items and safety annotations.
// - If spawn/join/cancel/yield APIs are present, these tests will detect them in source. When the exact API
//   is confirmed, replace static assertions with direct runtime interaction tests.
//
// To convert these into behavioral tests later:
// - Replace source scanning with concrete calls (e.g., fiber::spawn(...).join()) once the API is known.
// - Add deterministic scheduling helpers if the crate provides them, avoiding sleeps for flake-free tests.

use std::fs;
use std::path::PathBuf;
use std::time::{Duration, Instant};

#[allow(unused_imports)]
use zetaruntime::fiber; // Ensures the module is publicly exported

fn crate_root() -> PathBuf {
    // For integration tests in `zetaruntime/tests`, this resolves to the crate root (`zetaruntime/`)
    PathBuf::from(env\!("CARGO_MANIFEST_DIR"))
}

fn load_fiber_source() -> (PathBuf, String) {
    let base = crate_root();
    let candidates = [
        base.join("src").join("fiber.rs"),
        base.join("src").join("fiber").join("mod.rs"),
    ];

    for p in candidates {
        if p.exists() {
            let content = fs::read_to_string(&p)
                .unwrap_or_else(|e| panic\!("Failed reading {}: {e}", p.display()));
            return (p, content);
        }
    }
    panic\!("Could not locate fiber source file at src/fiber.rs or src/fiber/mod.rs");
}

fn contains_any(hay: &str, needles: &[&str]) -> bool {
    needles.iter().any(|n| hay.contains(n))
}

fn count_occurrences(hay: &str, needle: &str) -> usize {
    hay.match_indices(needle).count()
}

fn normalize_ws(s: &str) -> String {
    // Compress whitespace to make simple contains() checks more resilient
    let mut out = String::with_capacity(s.len());
    let mut prev_space = false;
    for ch in s.chars() {
        let is_space = ch.is_whitespace();
        if is_space {
            if \!prev_space {
                out.push(' ');
            }
        } else {
            out.push(ch);
        }
        prev_space = is_space;
    }
    out
}

fn wait_until(timeout: Duration, mut cond: impl FnMut() -> bool) -> bool {
    let start = Instant::now();
    while Instant::now().duration_since(start) < timeout {
        if cond() { return true; }
        std::thread::yield_now();
    }
    false
}

#[test]
fn fiber_module_is_publicly_exported() {
    // The `use` at top-level ensures name resolution. Here we just assert a trivial condition to keep the test active.
    let _module_visible = true;
    assert\!(_module_visible, "zetaruntime::fiber must be publicly exported from lib.rs");
}

#[test]
fn fiber_source_exists_and_is_non_empty() {
    let (path, content) = load_fiber_source();
    assert\!(\!content.trim().is_empty(), "fiber source at {} is empty", path.display());
    assert\!(content.lines().count() >= 5, "fiber source seems too short to be meaningful");
}

#[test]
fn fiber_declares_public_items() {
    let (_, content) = load_fiber_source();
    let norm = normalize_ws(&content);

    let has_pub_struct = norm.contains("pub struct ");
    let has_pub_enum   = norm.contains("pub enum ");
    let has_pub_fn     = norm.contains("pub fn ");

    assert\!(has_pub_struct || has_pub_enum || has_pub_fn,
        "Expected fiber module to declare at least one public struct/enum/function");
}

#[test]
fn fiber_exposes_core_concurrency_primitives_if_present() {
    let (_, content) = load_fiber_source();
    let norm = normalize_ws(&content);

    // We do not fail if a specific primitive is absent, but we verify and document presence if available.
    let spawn_like   = contains_any(&norm, &["pub fn spawn", "pub fn spawn_fn", "spawn_with", "JoinHandle", "FiberHandle"]);
    let join_like    = contains_any(&norm, &["fn join(", "impl Join"]); // rough join indicator
    let cancel_like  = contains_any(&norm, &["fn cancel(", "fn abort(", "fn stop("]);
    let yield_like   = contains_any(&norm, &["fn yield_now(", "fn park(", "fn unpark("]);

    // These asserts are intentionally permissive: at least one of the typical primitives should exist.
    assert\!(spawn_like || join_like || cancel_like || yield_like,
        "Expected fiber to expose at least one typical primitive (spawn/join/cancel/yield/park/unpark)");
}

#[test]
fn fiber_unsafe_blocks_are_documented_with_safety_comment() {
    let (_, content) = load_fiber_source();
    let unsafe_blocks = count_occurrences(&content, "unsafe {")
        + count_occurrences(&content, "unsafe{");
    let safety_notes  = count_occurrences(&content, "SAFETY:")
        + count_occurrences(&content, "Safety:")
        + count_occurrences(&content, "safety:");

    if unsafe_blocks > 0 {
        assert\!(safety_notes >= unsafe_blocks,
            "Found {unsafe_blocks} unsafe block(s) but only {safety_notes} safety note(s). \
             Please annotate each unsafe block with a 'SAFETY:' comment explaining invariants.");
    } else {
        assert_eq\!(unsafe_blocks, 0, "No unsafe blocks expected");
    }
}

#[test]
fn fiber_is_re_exported_from_lib_rs() {
    // Sanity-check lib.rs export so external crates can access fiber.
    let lib_path = crate_root().join("src").join("lib.rs");
    let lib = fs::read_to_string(&lib_path)
        .unwrap_or_else(|e| panic\!("Failed reading {}: {e}", lib_path.display()));
    assert\!(lib.contains("pub mod fiber"),
        "lib.rs should contain `pub mod fiber;` to re-export the module");
}

// Placeholder behavioral scaffold:
// Once the exact Fiber API is known, convert the following into executable tests.
// They are written as compile-time comments to show intended coverage without breaking current build.
//
// #[test]
// fn fiber_spawn_and_join_happy_path() {
//     let h = zetaruntime::fiber::spawn(|| 42usize);
//     assert_eq\!(h.join().unwrap(), 42);
// }
//
// #[test]
// fn fiber_double_join_returns_same_or_error() {
//     let h = zetaruntime::fiber::spawn(|| 7i32);
//     let _ = h.join();
//     let second = h.join();
//     let dbg = format\!("{second:?}");
//     assert\!(dbg.contains("joined") || dbg.contains("completed") || dbg.contains("Ok(7)"));
// }
//
// #[test]
// fn fiber_panic_propagates() {
//     let h = zetaruntime::fiber::spawn(|| -> usize { panic\!("boom"); });
//     let r = h.join();
//     let d = format\!("{r:?}");
//     assert\!(d.contains("panic") || d.contains("Err"));
// }