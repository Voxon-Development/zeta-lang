// TODO: Allow for custom allocators to be used as well!
use std::alloc::*;

pub struct HeapMemory {
    pub memory: Vec<u8>,
}

impl HeapMemory {
    pub fn new() -> Self {
        Self { memory: Vec::new() }
    }

    #[inline]
    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        unsafe {
            let layout = Layout::from_size_align(size, 8).unwrap();
            let ptr = alloc(layout);
            self.memory.extend_from_slice(std::slice::from_raw_parts(ptr, size));
            ptr
        }
    }

    #[inline]
    pub fn free(&mut self, ptr: *mut u8) {
        unsafe {
            dealloc(ptr, Layout::from_size_align(0, 8).unwrap());
        }
    }

    pub fn len(&self) -> usize {
        self.memory.len()
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.memory
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.memory
    }

    pub fn free_all(&mut self) {
        unsafe {
            dealloc(self.memory.as_mut_ptr(), Layout::from_size_align(self.memory.len(), 8).unwrap());
        }
    }
}