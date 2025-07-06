use std::sync::atomic::{AtomicUsize, Ordering};
use dashmap::DashMap;
use lazy_static::lazy_static;
use ir::VmString;
use ir::bump::AtomicBump;

pub struct AppendBuffer {
    data: AtomicBump,
    index: AtomicUsize,
    capacity: usize,
}

unsafe impl Sync for AppendBuffer {}
unsafe impl Send for AppendBuffer {}

impl AppendBuffer {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: AtomicBump::with_capacity(cap),
            index: AtomicUsize::new(0),
            capacity: cap,
        }
    }

    pub fn append(&self, bytes: &[u8]) -> Option<usize> {
        let len = bytes.len();
        let start = self.index.fetch_add(len, Ordering::Relaxed);
        if start + len > self.capacity {
            return None; // Trigger resize
        }

        for (i, &b) in bytes.iter().enumerate() {
            unsafe {
                *self.data[start + i].get() = b;
            }
        }

        Some(start)
    }

    pub fn get_slice(&self, offset: usize, length: usize) -> &[u8] {
        self.data.get_slice(offset, length)
    }
}

pub struct StringPool {
    data_buffer: AppendBuffer,
    interned_strings: DashMap<u64, Vec<VmString>>,
}

lazy_static! {
    static ref HASHER: ahash::RandomState = ahash::RandomState::new();
}

impl StringPool {
    #[inline(always)]
    pub fn new() -> Self {
        StringPool {
            data_buffer: AppendBuffer::with_capacity(64),
            interned_strings: DashMap::new(),
        }
    }

    pub fn intern(&self, s: &str) -> VmString {
        let hash = HASHER.hash_one(s);

        if let Some(vec) = self.interned_strings.get(&hash) {
            if let Some(existing) = vec.iter().find(|v| self.resolve_string(v) == s) {
                return *existing;
            }
        }

        // Slow path: insert new string
        let bytes = s.as_bytes();
        let offset = match self.data_buffer.append(bytes) {
            Some(offset) => offset,
            None => panic!("Out of memory")
        };

        let vm_string = VmString {
            offset,
            length: bytes.len(),
            hash,
        };

        self.interned_strings.entry(hash).or_default().push(vm_string);
        vm_string
    }

    #[inline(always)]
    pub fn resolve_string(&self, vm_string: &VmString) -> String {
        let slice = self.data_buffer.get_slice(vm_string.offset, vm_string.length);
        unsafe { String::from_utf8_unchecked(slice.to_vec()) }
    }
}