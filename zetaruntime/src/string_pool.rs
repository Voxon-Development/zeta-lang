use std::alloc::AllocError;
use dashmap::DashMap;
use crate::arena::GrowableAtomicBump;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct VmString {
    offset: usize,
    length: usize,
    hash: u64,
}

#[derive(Debug)]
pub struct StringPool {
    data_buffer: GrowableAtomicBump,
    interned_strings: DashMap<u64, Vec<VmString>>,
}

impl StringPool {
    #[inline(always)]
    pub fn new() -> Result<Self, AllocError> {
        Ok(StringPool {
            data_buffer: GrowableAtomicBump::with_capacity_and_aligned(4 * 1024 * 1024, 32)?,
            interned_strings: DashMap::new(), // Default hasher for u64 is fine
        })
    }

    #[inline(always)]
    pub fn intern(&mut self, s: &str) -> Option<VmString> {
        let s_hash = seahash::hash(s.as_bytes());

        if let Some(collision_list) = self.interned_strings.get(&s_hash) {
            // Hash collision: iterate through the list of VmStrings
            // that share this hash to find an exact content match.
            let vm_string = collision_list.iter().find(|vm_string| {
                self.resolve_string(vm_string) == s
            });
            if let Some(vm_string) = vm_string {
                return Some(*vm_string);
            }
        }

        let offset = self.data_buffer.len();
        self.data_buffer.alloc_many(s.as_bytes())?;

        let new_vm_string = VmString {
            offset,
            length: s.len(),
            hash: s_hash,
        };

        self.interned_strings.entry(s_hash).or_default().push(new_vm_string);
        Some(new_vm_string)
    }

    #[inline(always)]
    pub fn resolve_string(&self, vm_string: &VmString) -> &str {
        let start = vm_string.offset;
        let end = start + vm_string.length;

        unsafe { std::str::from_utf8_unchecked(&self.data_buffer.get_slice(start, end)) }
    }
}