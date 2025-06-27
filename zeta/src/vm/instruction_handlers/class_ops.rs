//! Class operation instruction handlers for the Zeta VM.
//! 
//! This module contains handlers for class-related operations like class initialization
//! and method calls.

use crate::vm::instruction_handlers::{pop_value, push_class, InstructionHandler, InstructionResult};
use crate::vm::interpreter::fetch_u64;
use crate::vm::memory::frames::StackFrame;
use ir::{Class, VMValue};
use zetac::codegen::ir::ir_compiler::CompressedClassTable;

/// Handles class initialization and method calls in the VM.
/// 
/// This handler is responsible for creating new class instances and managing
/// their fields and method calls. It works with the compressed class table
/// to efficiently store and access class metadata.
#[derive(Clone, Debug, Default)]
pub struct ClassInitHandler {
    pub(crate) class_table: CompressedClassTable
}

impl InstructionHandler for ClassInitHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        // 1. Read the class ID from the bytecode
        let class_id = fetch_u64(&function.to_vec(), &mut frame.program_counter) as usize;
        
        // 2. Get the class layout from the VM's compressed class table
        let class_layout = self.class_table
            .layouts
            .get(class_id)
            .ok_or_else(|| format!("Class with ID {} not found in class table", class_id))?;
        
        // 3. Create a new hashmap to store field values
        let mut fields: Vec<VMValue> = Vec::with_capacity(class_layout.field_order.len());
        
        // 4. For each field in the class (in reverse order since we're popping from the stack)
        for field_name in class_layout.field_order.iter() {
            // Pop the value from the stack and store it in the field
            let value = pop_value(frame)
                .map_err(|e| format!("Failed to get value for field '{}': {}", field_name, e))?;
            
            // Store the field value in the map
            fields.push(value);
        }
        
        // 5. Create the object with the initialized fields
        let obj = Class { fields };
        
        // 6. Push the new object onto the stack
        push_class(frame, obj);
        
        Ok(())
    }
}
