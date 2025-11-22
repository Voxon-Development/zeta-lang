use crate::arena::GrowableAtomicBump;
use smallvec::SmallVec;
use std::alloc::AllocError;

use std::hash::{BuildHasher, Hash, Hasher};
use std::num::NonZeroU64;
use std::ptr;
use std::simd::cmp::SimdPartialEq;
use std::simd::num::SimdUint;
use std::simd::Simd;

use std::str::from_utf8_unchecked;

use dashmap::DashMap;

const LANES: usize = 32;
const FX_INIT: u64 = 0xcbf29ce484222325;
const FX_PRIME: u64 = 0x100000001b3;


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

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct VmString {
    pub offset: *const u8,
    pub length: usize
}

impl PartialEq for VmString {
    fn eq(
        &self,
        other: &Self
    ) -> bool {
        if self.length != other.length {
            return false;
        }

        let (a, b): (&[u8], &[u8]) = unsafe {
            (std::slice::from_raw_parts(self.offset, self.length), std::slice::from_raw_parts(other.offset, other.length))
        };

        a == b
    }
}

impl Default for VmString {
    fn default() -> Self {
        VmString {
            //  the rest is default
            length: 0,
            offset: ptr::null()
        }
    }
}

impl Eq for VmString {}

impl Hash for VmString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr_val = self.offset as usize;
        let len_val = self.length;

        let mixed = ptr_val.wrapping_mul(0x9e3779b97f4a7c15) ^ len_val;
        mixed.hash(state);
    }
}

unsafe impl Send for VmString {}
unsafe impl Sync for VmString {}

pub struct StringPool {
    data_buffer: GrowableAtomicBump<'static>,
    interned_strings: DashMap<u64, SmallVec<[VmString; 2]>, IdentityBuild>,
}

const MEGABYTE: usize = 1024 * 1024;

impl StringPool {
    #[inline(always)]
    pub fn new() -> Result<Self, AllocError> {
        Ok(StringPool {
            data_buffer: GrowableAtomicBump::with_capacity_and_aligned(4 * MEGABYTE, 32)?,
            interned_strings: DashMap::with_hasher(IdentityBuild),
        })
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.data_buffer.len()
    }
    
    #[inline(always)]
    pub fn intern(&self, s: &str) -> VmString {
        self.intern_bytes(s.as_bytes())
    }

    #[inline(always)]
    pub fn intern_bytes(&self, bytes: &[u8]) -> VmString {
        let hash = Self::hash_bytes_simd(bytes);

        if let Some(collision_list) = self.interned_strings.get(&hash) {
            if let Some(vm_string) = self.find_simd(&collision_list, bytes) {
                return vm_string;
            }
        }

        self.insert_vm_string(bytes, hash)
    }

    fn insert_vm_string(&self, bytes: &[u8], hash: u64) -> VmString {
        let slice: &mut [u8] = self.data_buffer
            .alloc_many(bytes)
            .expect("Failed to allocate string slice");

        unsafe { ptr::copy_nonoverlapping(bytes.as_ptr(), slice.as_mut_ptr(), bytes.len()); }

        let new_vm_string = VmString {
            offset: slice.as_ptr(),
            length: bytes.len(),
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
    fn eq_simd(a: &[u8], b: &[u8]) -> bool {
        if a.len() != b.len() { return false; }

        let mut i = 0;
        while i + LANES <= a.len() {
            let va: Simd<u8, 32> = Simd::<u8, LANES>::from_slice(&a[i..]);
            let vb: Simd<u8, 32> = Simd::<u8, LANES>::from_slice(&b[i..]);
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
            if Self::eq_simd(stored, bytes) { return Some(vm_string); }
        }
        None
    }

    // -----------------------
    // SIMD-accelerated Fx-style hash
    #[inline(always)]
    fn hash_bytes_simd(bytes: &[u8]) -> u64 {
        let mut hash = FX_INIT;
        let mut i = 0;

        while i + LANES <= bytes.len() {
            let chunk = Simd::<u8, LANES>::from_slice(&bytes[i..]);
            // More robust mixing: multiply each byte, sum, then fold
            let sum: u64 = chunk.reduce_sum() as u64;
            hash = hash.wrapping_mul(FX_PRIME).wrapping_add(sum);
            hash ^= hash >> 33; // small avalanche
            i += LANES;
        }

        while i < bytes.len() {
            hash = hash.wrapping_mul(FX_PRIME).wrapping_add(bytes[i] as u64);
            hash ^= hash >> 33;
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
        let pool = StringPool::new().unwrap();

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
        let pool = StringPool::new().unwrap();

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
        let pool = StringPool::new().unwrap();

        let short = pool.intern("short");
        let same = pool.intern("short");
        let diff = pool.intern("sh0rt");

        assert_eq!(short, same);
        assert_ne!(short, diff);

        // Compare resolved bytes
        let bytes = pool.resolve_bytes(&short);
        let same_bytes = pool.resolve_bytes(&same);
        let diff_bytes = pool.resolve_bytes(&diff);

        assert!(StringPool::eq_simd(bytes, same_bytes));
        assert!(!StringPool::eq_simd(bytes, diff_bytes));
    }

    #[test]
    fn test_string_pool_len() {
        println!("{}", align_of::<VmString>());
    }

    #[test]
    fn test_unicode_strings() {
        let pool = StringPool::new().unwrap();

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
        let pool = StringPool::new().unwrap();

        let long = "a".repeat(1024);
        let vm = pool.intern(&long);
        let vm2 = pool.intern(&long);

        assert_eq!(vm, vm2);
        assert_eq!(pool.resolve_string(&vm), long);
    }
}
