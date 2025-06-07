use std::mem::{align_of, size_of};
use crate::vm::frames;
use crate::vm::frames::{CallFrame, StackFrame};

pub struct BumpStack {
    buffer: Vec<u8>,
    offset: usize,
    frames: Vec<frames::StackFrame>,
    function_frames: Vec<frames::CallFrame>
}

impl BumpStack {
    pub fn new(size: usize) -> Self {
        Self { buffer: vec![0u8; size], offset: 0, frames: vec![], function_frames: vec![] }
    }

    pub fn push_frame(&mut self) -> &StackFrame {
        let stack_frame = StackFrame { offset: self.offset };
        self.frames.push(stack_frame);
        self.frames.last().unwrap()
    }
    
    pub fn pop_frame(&mut self) {
        if let Some(frame) = self.frames.pop() {
            self.offset = frame.offset;
        }
    }

    pub fn allocate<T>(&mut self) -> Option<&'static mut T> {
        let align = align_of::<T>();
        let size = size_of::<T>();
        let align_mask = align - 1;
        let aligned_offset = (self.offset + align_mask) & !align_mask;

        let allocated_memory = aligned_offset + size;

        if allocated_memory > self.buffer.len() {
            return None //out of memory
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