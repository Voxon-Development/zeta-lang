use std::mem::{align_of, size_of};
use ir::{VMValue, VmString};
use crate::vm::memory::frames::StackFrame;

#[derive(Debug, Clone, Default)]
pub struct BumpStack {
    buffer: Vec<VMValue>,
    frames: Vec<StackFrame>,
}

impl BumpStack {
    pub fn new(size: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(size),
            frames: Vec::new(),
        }
    }

    pub fn push_vm(&mut self, value: VMValue) {
        self.buffer.push(value);
    }

    pub fn pop_vm(&mut self) -> Option<VMValue> {
        self.buffer.pop()
    }

    pub fn peek_vm(&self) -> Option<&VMValue> {
        self.buffer.last()
    }

    pub fn reset(&mut self) {
        self.buffer.clear();
    }

    pub fn offset(&self) -> usize {
        self.buffer.len()
    }

    pub fn get_frame(&self) -> Option<&StackFrame> {
        self.frames.last()
    }

    pub fn get_frame_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }
}