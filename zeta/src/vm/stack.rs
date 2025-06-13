use std::mem::{align_of, size_of};
use ir::VmString;
use crate::vm::frames::StackFrame;

#[derive(Debug, Clone, Default)]
pub struct BumpStack {
    buffer: Vec<u8>,
    offset: usize,
    frames: Vec<StackFrame>
}

impl BumpStack {
    pub fn new(size: usize) -> Self {
        Self { 
            buffer: Vec::with_capacity(size), 
            offset: 0, 
            frames: Vec::new() 
        }
    }
    
    pub fn pop(&mut self) -> Option<u8> {
        if self.offset > 0 {
            self.offset -= 1;
            Some(self.buffer[self.offset])
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<u8> {
        if self.offset > 0 {
            Some(self.buffer[self.offset - 1])
        } else {
            None
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn peek_n(&self, n: usize) -> Option<u8> {
        if self.offset > n {
            Some(self.buffer[n])
        } else {
            None
        }
    }

    pub fn push_string(&mut self, vm_str: VmString) {
        self.push_bytes(&vm_str.offset.to_ne_bytes());
        self.push_bytes(&vm_str.length.to_ne_bytes());
        self.push_bytes(&vm_str.hash.to_ne_bytes());
    }
    
    pub fn pop_bool(&mut self) -> bool {
        let value = self.buffer[self.offset - 1];
        self.offset -= 1;
        value != 0
    }

    pub fn pop_string(&mut self) -> VmString {
        let hash_bytes = self.pop_bytes(size_of::<u64>()).expect("Stack underflow");
        let length_bytes = self.pop_bytes(size_of::<usize>()).expect("Stack underflow");
        let offset_bytes = self.pop_bytes(size_of::<usize>()).expect("Stack underflow");

        VmString {
            offset: usize::from_ne_bytes(offset_bytes.try_into().unwrap()),
            length: usize::from_ne_bytes(length_bytes.try_into().unwrap()),
            hash: u64::from_ne_bytes(hash_bytes.try_into().unwrap()),
        }
    }

    pub fn push_ptr(&mut self, ptr: *mut u8) {
        self.buffer.push(ptr as u8);
        self.offset += 1;
    }
    
    pub fn push_f64(&mut self, value: f64) {
        self.buffer.extend_from_slice(&value.to_ne_bytes());
        self.offset += size_of::<f64>();
    }

    pub fn pop_int(&mut self) -> i32 {
        let vector: Vec<u8> = self.buffer.drain(self.offset - size_of::<i32>()..self.offset).collect();

        assert_eq!(vector.len(), size_of::<i32>());
        let value = i32::from_ne_bytes(vector.as_slice().try_into().unwrap());
        self.offset -= size_of::<i32>();
        value
    }

    pub fn push_bool(&mut self, value: bool) {
        self.buffer.push(value as u8);
        self.offset += 1;
    }

    pub fn push_int(&mut self, value: i32) {
        self.buffer.extend_from_slice(&value.to_ne_bytes());
        self.offset += size_of::<i32>();
    }

    pub fn push_i64(&mut self, value: i64) {
        self.buffer.extend_from_slice(&value.to_ne_bytes());
        self.offset += size_of::<i64>();
    }

    pub fn pop_ptr(&mut self) -> Option<*mut u8> {
        if self.offset > 0 {
            let ptr = self.buffer[self.offset - 1] as *mut u8;
            self.offset -= 1;
            Some(ptr)
        } else {
            None
        }
    }
    
    pub fn pop_bytes(&mut self, size: usize) -> Option<Vec<u8>> {
        if self.offset > size {
            let bytes = self.buffer.drain(self.offset - size..self.offset).collect();
            self.offset -= size;
            Some(bytes)
        } else {
            None
        }
    }
    
    pub fn get_frame(&self) -> Option<&StackFrame> {
        self.frames.last()
    }
    
    pub fn get_frame_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }
    
    pub fn get_offset(&self) -> usize {
        self.offset
    }
    
    pub fn push(&mut self, value: u8) {
        self.buffer.push(value);
        self.offset += 1;
    }
    
    pub fn push_bytes(&mut self, bytes: &[u8]) {
        self.buffer.extend_from_slice(bytes);
        self.offset += bytes.len();
    }
    
    pub fn get_memory(&self, offset: usize) -> Option<&u8> {
        if offset < self.offset {
            Some(&self.buffer[offset])
        } else {
            None
        }
    }
    

    pub fn allocate<T>(&mut self) -> Option<&'static mut T> {
        let align = align_of::<T>();
        let size = size_of::<T>();
        let align_mask = align - 1;
        let aligned_offset = (self.offset + align_mask) & !align_mask;

        let allocated_memory = aligned_offset + size;

        if allocated_memory > self.buffer.len() {
            return None // Out of memory
        }

        let ptr = self.buffer.as_mut_ptr().wrapping_add(aligned_offset) as *mut T;
        self.offset = allocated_memory;

        // there is no overlap
        // the lifetimes are valid within the frame scope (I hope?)
        //
        // let's play russian roulette I guess lmao
        Some(unsafe { &mut *ptr })
    }

    pub fn reset(&mut self) {
        self.offset = 0;
    }
}