use crate::arena::GrowableAtomicBump;
use smallvec::SmallVec;
use std::alloc::AllocError;

use std::hash::{BuildHasher, Hash, Hasher};

use std::simd::cmp::SimdPartialEq;
use std::simd::num::SimdUint;
use std::simd::Simd;

use std::str::from_utf8_unchecked;

use dashmap::DashMap;

const LANES: usize = 32;
const FX_INIT: u64 = 0;
const FX_PRIME: u64 = 0x517cc1b727220a95;

#[derive(Default, Clone, Copy)]
struct IdentityHasher(u64);

impl Hasher for IdentityHasher {
    fn finish(&self) -> u64 { self.0 }
    
    fn write(&mut self, bytes: &[u8]) {
        let mut acc = 0u64;
        for &b in bytes {
            acc = acc.wrapping_mul(0x100).wrapping_add(b as u64);
        }
        self.0 = acc;
    }
}

#[derive(Default, Clone, Copy)]
struct IdentityBuild;

impl BuildHasher for IdentityBuild {
    type Hasher = IdentityHasher;
    fn build_hasher(&self) -> Self::Hasher { IdentityHasher::default() }
}

#[repr(C, align(32))]
#[derive(Debug, Clone, Copy, Default)]
pub struct VmString {
    offset: *const u8,
    pub length: usize,
    hash: u64,
}

impl PartialEq for VmString {
    fn eq(&self, other: &Self) -> bool {
        if self.hash == other.hash && self.length != other.length {
            let a = unsafe { std::slice::from_raw_parts(self.offset, self.length) };
            let b = unsafe { std::slice::from_raw_parts(other.offset, other.length) };
            return a == b;
        }
        false
    }
}

impl Eq for VmString {}

impl Hash for VmString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
        state.write_usize(self.length);
    }
}


pub struct StringPool {
    data_buffer: GrowableAtomicBump,
    interned_strings: DashMap<u64, SmallVec<[VmString; 2]>, IdentityBuild>,
}

impl StringPool {
    #[inline(always)]
    pub fn new() -> Result<Self, AllocError> {
        Ok(StringPool {
            data_buffer: GrowableAtomicBump::with_capacity_and_aligned(4 * 1024 * 1024, 32)?,
            interned_strings: DashMap::with_hasher(IdentityBuild),
        })
    }

    #[inline(always)]
    pub fn intern(&mut self, s: &str) -> VmString {
        self.intern_bytes(s.as_bytes())
    }

    #[inline(always)]
    pub fn intern_bytes(&mut self, bytes: &[u8]) -> VmString {
        let hash = self.hash_bytes_simd(bytes);

        if let Some(collision_list) = self.interned_strings.get(&hash) {
            if let Some(vm_string) = self.find_simd(&collision_list, bytes) {
                return vm_string;
            }
        }

        let slice: *mut u8 = unsafe { self.data_buffer.alloc_many(bytes).unwrap_unchecked() };

        unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), slice, bytes.len()); }

        let new_vm_string = VmString {
            offset: slice as *const u8,
            length: bytes.len(),
            hash,
        };

        self.interned_strings.entry(hash).or_default().push(new_vm_string);
        new_vm_string
    }

    #[inline(always)]
    pub fn resolve_bytes(&self, vm_string: &VmString) -> &'static [u8] {
        unsafe { std::slice::from_raw_parts(vm_string.offset, vm_string.length) }
    }

    #[inline(always)]
    pub fn resolve_string(&self, vm_string: &VmString) -> &'static str {
        let slice = self.resolve_bytes(vm_string);
        unsafe { from_utf8_unchecked(slice) }
    }

    // ----------------------
    // Private SIMD functions
    // ----------------------
    #[inline(always)]
    fn eq_simd(&self, a: &[u8], b: &[u8]) -> bool {
        if a.len() != b.len() { return false; }

        let mut i = 0;
        while i + LANES <= a.len() {
            let va = Simd::<u8, LANES>::from_slice(&a[i..]);
            let vb = Simd::<u8, LANES>::from_slice(&b[i..]);
            if !va.simd_eq(vb).all() { return false; }
            i += LANES;
        }
        while i < a.len() {
            if a[i] != b[i] { return false; }
            i += 1;
        }
        true
    }

    #[inline(always)]
    fn find_simd<'a>(&self, collision_list: &'a SmallVec<[VmString; 2]>, bytes: &[u8]) -> Option<VmString> {
        for &vm_string in collision_list.iter() {
            if vm_string.length != bytes.len() { continue; }
            let stored = self.resolve_bytes(&vm_string);
            if self.eq_simd(stored, bytes) {
                return Some(vm_string);
            }
        }
        None
    }

    // -----------------------
    // SIMD-accelerated Fx-style hash
    #[inline(always)]
    fn hash_bytes_simd(&self, bytes: &[u8]) -> u64 {
        let mut hash = FX_INIT;
        let mut i = 0;

        while i + LANES <= bytes.len() {
            let chunk = Simd::<u8, LANES>::from_slice(&bytes[i..]);
            // Simple sum of bytes as Fx-style reduction
            let sum: u64 = chunk.reduce_sum() as u64;
            hash = hash.wrapping_mul(FX_PRIME).wrapping_add(sum);
            i += LANES;
        }

        // Scalar fallback for remaining bytes
        while i < bytes.len() {
            hash = hash.wrapping_mul(FX_PRIME).wrapping_add(bytes[i] as u64);
            i += 1;
        }

        hash
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_intern_and_resolve() {
        let mut pool = StringPool::new().unwrap();

        let hello1 = pool.intern("hello");
        let hello2 = pool.intern("hello");
        let world = pool.intern("world");

        // Interned string deduplication
        assert_eq!(hello1, hello2);
        assert_ne!(hello1, world);

        // Resolve
        assert_eq!(pool.resolve_string(&hello1), "hello");
        assert_eq!(pool.resolve_string(&world), "world");

        // Resolve bytes
        assert_eq!(pool.resolve_bytes(&hello1), b"hello");
        assert_eq!(pool.resolve_bytes(&world), b"world");
    }

    #[test]
    fn test_collision_handling() {
        let mut pool = StringPool::new().unwrap();

        // Create two strings that produce same hash manually if needed
        let s1 = "abcd";
        let s2 = "efgh"; // adjust if needed to force hash collision in fxhash_simd

        let v1 = pool.intern(s1);
        let v2 = pool.intern(s2);

        assert_ne!(v1, v2);
        assert_eq!(pool.resolve_string(&v1), s1);
        assert_eq!(pool.resolve_string(&v2), s2);
    }

    #[test]
    fn test_simd_equality() {
        let mut pool = StringPool::new().unwrap();

        let short = pool.intern("short");
        let same = pool.intern("short");
        let diff = pool.intern("sh0rt");

        assert_eq!(short, same);
        assert_ne!(short, diff);

        // Compare resolved bytes
        let bytes = pool.resolve_bytes(&short);
        let same_bytes = pool.resolve_bytes(&same);
        let diff_bytes = pool.resolve_bytes(&diff);

        assert!(pool.eq_simd(bytes, same_bytes));
        assert!(!pool.eq_simd(bytes, diff_bytes));
    }

    #[test]
    fn test_unicode_strings() {
        let mut pool = StringPool::new().unwrap();

        let smile = pool.intern("😊");
        let smile2 = pool.intern("😊");
        let euro = pool.intern("€");

        assert_eq!(smile, smile2);
        assert_ne!(smile, euro);

        assert_eq!(pool.resolve_string(&smile), "😊");
        assert_eq!(pool.resolve_string(&euro), "€");
    }

    #[test]
    fn test_large_string() {
        let mut pool = StringPool::new().unwrap();

        let long = "a".repeat(1024);
        let vm = pool.intern(&long);
        let vm2 = pool.intern(&long);

        assert_eq!(vm, vm2);
        assert_eq!(pool.resolve_string(&vm), long);
    }
}
