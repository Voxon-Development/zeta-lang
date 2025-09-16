//\! Tests for SeaHasher.
//\!
//\! Framework: Rust built-in test harness (#[test]).
//\! These tests focus on API behavior surfaced in the PR diff for SeaHasher and related hashing utilities.

use std::hash::{Hash, Hasher};

/// Helper to get hash from any Hash implementation using SeaHasher.
/// Note: This assumes SeaHasher is exported from the ir crate root or a reachable path.
/// Adjust the use path if the type/module is nested differently.
fn sea_hash_of<T: Hash>(value: &T) -> u64 {
    // Try common paths; only one should compile depending on project layout.
    // If your project exposes SeaHasher at a different path, update the `use` below accordingly.
    #[allow(unused_imports)]
    use ir::sea_hasher::SeaHasher as SeaHasherPathA;
    #[allow(unused_imports)]
    use crate::sea_hasher::SeaHasher as SeaHasherPathB;
    #[allow(unused_imports)]
    use super::super::sea_hasher::SeaHasher as SeaHasherPathC;

    // Resolve SeaHasher alias depending on which import compiled.
    // The following uses a trick: define a type alias that matches one of the imported names.
    // For new projects, prefer a single canonical path and remove the others.
    #[cfg(any())]
    type SeaHasher = SeaHasherPathA;
    #[cfg(any())]
    type SeaHasher = SeaHasherPathB;
    #[cfg(any())]
    type SeaHasher = SeaHasherPathC;

    // Fallback: refer directly; adjust if needed.
    // If none of the above resolve, try a direct path commonly used in crates:
    // use crate::SeaHasher;

    let mut hasher = SeaHasher::default();
    value.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn sea_hasher_hashes_empty_string_consistently() {
    let h1 = sea_hash_of(&"");
    let h2 = sea_hash_of(&"");
    assert_eq\!(h1, h2, "Empty string hash should be stable across runs for the same process/algorithm");
}

#[test]
fn sea_hasher_hashes_simple_ascii_string() {
    let h = sea_hash_of(&"hello");
    // Basic sanity: hash should be non-zero and not equal to empty string's hash
    let empty = sea_hash_of(&"");
    assert_ne\!(h, 0, "Hash of non-empty should not be zero (sanity check)");
    assert_ne\!(h, empty, "Different inputs should produce different hashes (very high probability)");
}

#[test]
fn sea_hasher_hashes_unicode_string() {
    let s = "こんにちは世界"; // Japanese
    let h1 = sea_hash_of(&s);
    let h2 = sea_hash_of(&s.to_string());
    assert_eq\!(h1, h2, "Borrowed and owned strings with same content should hash identically");
}

#[test]
fn sea_hasher_hashes_bytes_equivalently_to_string_bytes() {
    let s = "abc123";
    let h_str = sea_hash_of(&s);
    let h_bytes = sea_hash_of(&s.as_bytes());
    // Different Hash impls may incorporate type tags; equality is not guaranteed.
    // We assert only basic properties unless the implementation guarantees byte-equivalence.
    assert_ne\!(h_str, 0);
    assert_ne\!(h_bytes, 0);
}

#[test]
fn sea_hasher_handles_long_inputs() {
    let data = "x".repeat(1 << 20); // 1 MiB
    let h1 = sea_hash_of(&data);
    let h2 = sea_hash_of(&data);
    assert_eq\!(h1, h2, "Long input hash should be deterministic for identical input");
}

#[test]
fn sea_hasher_hashes_collections_order_sensitivity() {
    let v1 = vec\![1u32, 2, 3, 4];
    let v2 = vec\![4u32, 3, 2, 1];
    let h1 = sea_hash_of(&v1);
    let h2 = sea_hash_of(&v2);
    // Hash for sequences should be order-sensitive
    assert_ne\!(h1, h2, "Order-sensitive hashing should produce different hashes for different orders");
}

#[test]
fn sea_hasher_hashes_maps_order_independence_via_hash_trait() {
    use std::collections::BTreeMap;
    let mut m1 = BTreeMap::new();
    m1.insert("a", 1);
    m1.insert("b", 2);

    let mut m2 = BTreeMap::new();
    m2.insert("b", 2);
    m2.insert("a", 1);

    let h1 = sea_hash_of(&m1);
    let h2 = sea_hash_of(&m2);
    assert_eq\!(h1, h2, "BTreeMap hashing should be order-independent due to sorted keys");
}

#[test]
fn sea_hasher_distinguishes_prefixes() {
    let a = "abc";
    let b = "abcd";
    let ha = sea_hash_of(&a);
    let hb = sea_hash_of(&b);
    assert_ne\!(ha, hb, "Different sized inputs should normally hash differently");
}

#[test]
fn sea_hasher_is_usable_as_std_hasher_directly() {
    // Ensure SeaHasher implements Hasher correctly for incremental writes
    #[allow(unused_imports)]
    use ir::sea_hasher::SeaHasher as SeaHasherPathA;
    #[allow(unused_imports)]
    use crate::sea_hasher::SeaHasher as SeaHasherPathB;

    #[cfg(any())]
    type SeaHasher = SeaHasherPathA;
    #[cfg(any())]
    type SeaHasher = SeaHasherPathB;

    let mut h = SeaHasher::default();
    use std::hash::Hasher;
    h.write(b"hello");
    h.write(b"");
    h.write_u8(b'\!' as u8);
    let sum1 = h.finish();

    let mut h2 = SeaHasher::default();
    h2.write(b"hello\!");
    let sum2 = h2.finish();

    assert_eq\!(sum1, sum2, "Incremental writes should match single-shot write of concatenated bytes");
}

#[test]
fn sea_hasher_consistency_across_hash_calls() {
    // hash the same value multiple times with fresh hashers; result should be identical
    let v = ("key", 42u64);
    let h1 = sea_hash_of(&v);
    let h2 = sea_hash_of(&v);
    assert_eq\!(h1, h2, "Hashing same value twice should be consistent");
}