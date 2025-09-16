// Added by test generation on 2025-09-16
// Test framework: Rust's built-in test harness (#[test]) — no external test libs.
// Focus: Verify chunked vs one-shot hashing equivalence, specialized write_* correctness,
// determinism on empty input, idempotent finish(), and basic non-collision sanity checks.
// These scenarios commonly regress when refactoring hashers or altering internal buffering.

use std::hash::Hasher;
// If the crate name differs from "ir" or the module path is different, adjust the import below.
use ir::sea_hasher::SeaHasher;

#[inline]
fn sea_hash_one_shot(data: &[u8]) -> u64 {
    let mut h: SeaHasher = Default::default();
    h.write(data);
    h.finish()
}

#[inline]
fn sea_hash_chunked(data: &[u8], chunk_sizes: &[usize]) -> u64 {
    let mut h: SeaHasher = Default::default();
    let mut idx = 0usize;
    for &sz in chunk_sizes {
        if idx >= data.len() { break; }
        let end = std::cmp::min(idx + sz, data.len());
        h.write(&data[idx..end]);
        idx = end;
    }
    if idx < data.len() {
        h.write(&data[idx..]);
    }
    h.finish()
}

#[inline]
fn patterned_bytes(len: usize) -> Vec<u8> {
    // Lightweight, deterministic pseudo-random-ish generator (no external deps).
    let mut out = Vec::with_capacity(len);
    let mut x: u64 = 0x9E37_79B9_7F4A_7C15;
    for i in 0..len {
        x = x.wrapping_add((i as u64).wrapping_mul(0xBF58_476D_1CE4_E5B9));
        x ^= x >> 30;
        x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        x ^= x >> 27;
        x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
        x ^= x >> 31;
        out.push((x & 0xFF) as u8);
    }
    out
}

#[test]
fn sea_hasher_empty_is_deterministic_and_idempotent() {
    let v1 = sea_hash_one_shot(&[]);
    let v2 = sea_hash_one_shot(&[]);
    assert_eq\!(v1, v2, "hash of empty slice should be stable and deterministic");

    // finish() must be idempotent on the same state
    let mut h: SeaHasher = Default::default();
    let f1 = h.finish();
    let f2 = h.finish();
    assert_eq\!(f1, f2, "finish() should be repeatable without changing state");

    // A fresh hasher with no writes should equal previous empty-state finish
    let mut fresh: SeaHasher = Default::default();
    assert_eq\!(f1, fresh.finish(), "two fresh empty hashers should agree");
}

#[test]
fn sea_hasher_chunking_equivalence_various_splits_small_inputs() {
    let samples: &[&[u8]] = &[
        b"",
        b"a",
        b"ab",
        b"abc",
        b"abcd",
        b"abcde",
        b"hello",
        b"The quick brown fox jumps over the lazy dog",
        &[0, 0, 0, 0, 0],
        &[255, 254, 253, 252, 251, 250, 0, 1, 2, 3],
    ];
    let chunk_patterns: &[&[usize]] = &[
        &[],                 // treated as single write
        &[1],
        &[2],
        &[3],
        &[1, 1, 1, 1, 1, 1], // many tiny writes
        &[2, 3, 5, 7, 11],   // mixed
        &[64],               // larger than input
    ];
    for data in samples {
        let expected = sea_hash_one_shot(data);
        for chunks in chunk_patterns {
            let got = sea_hash_chunked(data, chunks);
            assert_eq\!(got, expected, "chunking {:?} must equal one-shot for input {:?}", chunks, String::from_utf8_lossy(data));
        }
    }
}

#[test]
fn sea_hasher_chunking_equivalence_across_lengths_up_to_4k() {
    // Exercise edge lengths around 8-byte boundaries and powers of two
    let lens = [0usize, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257, 511, 512, 1023, 1024, 2047, 2048, 4095, 4096];
    for &len in &lens {
        let data = patterned_bytes(len);
        let expected = sea_hash_one_shot(&data);
        // Representative chunk patterns including misaligned sizes
        let patterns: &[&[usize]] = &[
            &[1; 64],           // worst-case: byte-by-byte writes
            &[3; 21],           // non-power-of-two chunks
            &[7, 13, 29, 4],    // mixed sizes
            &[1024],            // large chunk
            &[5, 5, 5, 5, 5, 5],// repeated
        ];
        for p in patterns {
            let got = sea_hash_chunked(&data, p);
            assert_eq\!(got, expected, "length {} with pattern {:?} should match one-shot", len, p);
        }
    }
}

#[test]
fn sea_hasher_write_uXX_equivalence() {
    // Specialized numeric writers should match writing the native-endian bytes.
    let u64_vals: [u64; 8] = [
        0,
        1,
        u64::MAX,
        u64::from_le_bytes([0,1,2,3,4,5,6,7]),
        0x0123_4567_89AB_CDEF,
        0xFFFF_0000_AAAA_5555,
        0xDEAD_BEEF_CAFE_BABE,
        0x8000_0000_0000_0001,
    ];
    for &v in &u64_vals {
        let mut h1: SeaHasher = Default::default();
        h1.write_u64(v);
        let d1 = h1.finish();

        let mut h2: SeaHasher = Default::default();
        h2.write(&v.to_ne_bytes());
        let d2 = h2.finish();

        assert_eq\!(d1, d2, "write_u64({:#x}) should equal write(to_ne_bytes())", v);
    }

    let u32_vals: [u32; 6] = [0, 1, u32::MAX, 0x0123_4567, 0x89AB_CDEF, 0x8000_0001];
    for &v in &u32_vals {
        let mut h1: SeaHasher = Default::default();
        h1.write_u32(v);
        let d1 = h1.finish();

        let mut h2: SeaHasher = Default::default();
        h2.write(&v.to_ne_bytes());
        let d2 = h2.finish();

        assert_eq\!(d1, d2, "write_u32({:#x}) should equal write(to_ne_bytes())", v);
    }

    let u8_vals: [u8; 5] = [0, 1, 127, 128, 255];
    for &v in &u8_vals {
        let mut h1: SeaHasher = Default::default();
        h1.write_u8(v);
        let d1 = h1.finish();

        let mut h2: SeaHasher = Default::default();
        h2.write(&[v]);
        let d2 = h2.finish();

        assert_eq\!(d1, d2, "write_u8({:#x}) should equal write(&[v])", v);
    }
}

#[test]
fn sea_hasher_distinguishes_different_inputs_basic_smoke() {
    // Not a formal collision test — just sanity for a handful of distinct inputs.
    let inputs: &[&[u8]] = &[
        b"a",
        b"b",
        b"ab",
        b"ba",
        b"abc",
        b"abcd",
        b"abcde",
        &[0],
        &[1],
        &[0, 0, 0, 0, 0, 0, 0, 0],
        &[1, 0, 0, 0, 0, 0, 0, 0],
    ];
    let mut hashes = std::collections::HashSet::new();
    for &data in inputs {
        let h = sea_hash_one_shot(data);
        assert\!(hashes.insert(h), "unexpected collision for input {:?}", data);
    }
}

#[test]
fn sea_hasher_is_unaffected_by_after_finish_reads() {
    // Ensure calling finish() doesn't mutate state (finish is &self), and a fresh hasher
    // hashing the same prefix yields the same result.
    let data1 = b"prefix bytes";
    let data2 = b" and suffix";

    let mut h1: SeaHasher = Default::default();
    h1.write(data1);
    let before = h1.finish();
    let before_again = h1.finish();
    assert_eq\!(before, before_again, "finish should be stable across multiple calls on the same state");

    // Combined write on a fresh hasher should match one-shot of concatenated bytes.
    let mut h2: SeaHasher = Default::default();
    h2.write(data1);
    h2.write(data2);
    let combined = h2.finish();

    let expected = sea_hash_one_shot(&[data1.as_ref(), data2.as_ref()].concat());
    assert_eq\!(combined, expected, "concatenated writes must equal one-shot on the concatenated slice");
}