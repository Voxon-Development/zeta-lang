//! Instruction handlers for the Zeta VM.
//! 
//! This module contains the implementation of all VM instructions, split into
//! logical groups for better organization and maintainability.

pub mod arithmetic;
pub mod control_flow;
pub mod memory;
pub mod stack_ops;
pub mod type_ops;
pub mod array_ops;
pub mod pointer_ops;
pub(crate) mod class_ops;

use crate::vm::memory::frames::StackFrame;
use ir::VMValue;

use std::collections::HashMap;
use crate::vm::virtualmachine::VirtualMachine;

/// Common result type for instruction handlers.
pub type InstructionResult = Result<(), String>;

/// Trait for instruction handlers.
pub trait InstructionHandler: Send + Sync + CloneInstructionHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult;
}

pub trait CloneInstructionHandler {
    fn clone_box(&self) -> Box<dyn InstructionHandler>;
}

impl<T> CloneInstructionHandler for T
where
    T: 'static + InstructionHandler + Clone,
{
    fn clone_box(&self) -> Box<dyn InstructionHandler> {
        println!("Cloning handler of type: {}", std::any::type_name::<T>());
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn InstructionHandler> {
    fn clone(&self) -> Box<dyn InstructionHandler> {
        self.clone_box()
    }
}

impl InstructionHandler for Box<dyn InstructionHandler> {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        (**self).execute(frame, function)
    }
}

pub struct InstructionDispatcher {
    handlers: [Option<Box<dyn InstructionHandler>>; 256],
}

impl InstructionDispatcher {
    pub fn new() -> Self {
        Self {
            handlers: std::array::from_fn(|_| None),
        }
    }

    pub fn register<T>(&mut self, opcode: u8, handler: T)
    where T: InstructionHandler + CloneInstructionHandler + Clone + 'static {
        self.handlers[opcode as usize] = Some(Box::new(handler));
    }

    pub fn register_many_opcodes<T>(&mut self, opcodes: &[u8], handler: T) 
    where T: InstructionHandler + CloneInstructionHandler + Clone + 'static {
        for &opcode in opcodes {
            self.handlers[opcode as usize] = Some(Box::new(handler.clone()));
        }
    }

    pub fn execute(
        &mut self,
        instr: u8,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        match self.handlers[instr as usize].as_mut() {
            Some(handler) => handler.execute(frame, function),
            None => Err(format!("Unknown instruction: {:#X}", instr)),
        }
    }
}

/// Helper function to fetch a value from the stack.
pub fn pop_value(frame: &mut StackFrame) -> Result<VMValue, String> {
    frame.stack.pop_vm().ok_or_else(|| "Stack underflow".to_string())
}

/// Helper function to push a value to the stack.
pub fn push_value(frame: &mut StackFrame, value: VMValue) {
    frame.stack.push_vm(value);
}
/// Helper function to push a class initialization to the stack.
pub fn push_class(frame: &mut StackFrame, value: ir::Class) {
    frame.stack.push_class(value);
}