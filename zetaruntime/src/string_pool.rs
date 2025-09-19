use crate::arena::GrowableAtomicBump;
use smallvec::SmallVec;
use std::alloc::AllocError;

use std::hash::{BuildHasher, Hasher};

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
        self.0 = u64::from_ne_bytes(bytes.try_into().unwrap());
    }
}

#[derive(Default, Clone, Copy)]
struct IdentityBuild;

impl BuildHasher for IdentityBuild {
    type Hasher = IdentityHasher;
    fn build_hasher(&self) -> Self::Hasher { IdentityHasher::default() }
}

#[repr(C, align(32))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct VmString {
    offset: usize,
    pub length: usize,
    hash: u64,
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

    /// Fully SIMD-optimized intern_bytes
    #[inline(always)]
    pub fn intern_bytes(&mut self, bytes: &[u8]) -> VmString {
        // Compute SIMD hash (Fx-like)
        let hash = self.hash_bytes_simd(bytes);

        // Check collision list
        if let Some(collision_list) = self.interned_strings.get(&hash) {
            if let Some(vm_string) = self.find_simd(&collision_list, bytes) {
                return vm_string;
            }
        }

        // Allocate new string in bump buffer
        let offset = self.data_buffer.len();
        unsafe { self.data_buffer.alloc_many(bytes).unwrap_unchecked(); }

        let new_vm_string = VmString {
            offset,
            length: bytes.len(),
            hash,
        };

        self.interned_strings.entry(hash).or_default().push(new_vm_string);
        new_vm_string
    }

    #[inline(always)]
    pub fn resolve_bytes(&self, vm_string: &VmString) -> &'static [u8] {
        unsafe { self.data_buffer.get_slice_unchecked(vm_string.offset, vm_string.length) }
    }

    #[inline(always)]
    pub fn resolve_string(&self, vm_string: &VmString) -> &'static str {
        let slice = self.resolve_bytes(vm_string);
        unsafe { from_utf8_unchecked(slice) }
    }

    // -----------------------
    // SIMD equality (portable)
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