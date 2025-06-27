//! Array operation instruction handlers for the Zeta VM.

use super::*;
use crate::vm::interpreter::*;

/// Handles all array operations.
#[derive(Clone)]
pub struct ArrayOpsHandler;

impl InstructionHandler for ArrayOpsHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        _function: &[u8],
    ) -> InstructionResult {
        match frame.current_opcode {
            op if op == ARRAY_GET => {
                let index = match pop_value(frame)? {
                    VMValue::I32(i) if i >= 0 => i as usize,
                    _ => return Err("Expected non-negative integer index for ARRAY_GET".to_string()),
                };
                
                match pop_value(frame)? {
                    VMValue::Array(len, data) => {
                        // SAFETY: We trust the VM to pass valid pointers
                        let data = unsafe { std::slice::from_raw_parts(data, len) };
                        
                        if index >= len {
                            return Err(format!("Array index out of bounds: {} >= {}", index, len));
                        }
                        match data.get(index) {
                            Some(value) => push_value(frame, value.clone()),
                            None => return Err(format!("Invalid array access at index {}", index)),
                        }
                    }
                    _ => return Err("Expected array for ARRAY_GET".to_string()),
                }
            }
            
            op if op == ARRAY_SET => {
                let value = pop_value(frame)?;
                let index = match pop_value(frame)? {
                    VMValue::I32(i) if i >= 0 => i as usize,
                    _ => return Err("Expected non-negative integer index for ARRAY_SET".to_string()),
                };
                 
                match pop_value(frame)? {
                    VMValue::Array(len, ref mut data) => {
                        // SAFETY: We trust the VM to pass valid pointers
                        let data = unsafe { std::slice::from_raw_parts_mut(data, len) };
                        
                        if index >= len {
                            return Err(format!("Array index out of bounds: {} >= {}", index, len));
                        }
                        match data.get_mut(index) {
                            Some(elem) => *elem = &value,
                            None => return Err(format!("Invalid array access at index {}", index)),
                        }
                    }
                    _ => return Err("Expected array for ARRAY_SET".to_string()),
                }
            }
            
            op if op == ARRAY_LEN => {
                match pop_value(frame)? {
                    VMValue::Array(len, _) => {
                        push_value(frame, VMValue::U32(len as u32));
                    }
                    _ => return Err("Expected array for ARRAY_LEN".to_string()),
                }
            }
            
            op if op == ARRAY_ALLOC => {
                let len = match pop_value(frame)? {
                    VMValue::I32(len) if len >= 0 => len as usize,
                    _ => return Err("Expected non-negative length for ARRAY_ALLOC".to_string()),
                };
                
                let default_value = pop_value(frame)?;
                // In a real implementation, we would allocate a new array with 'len' elements
                // all initialized to 'default_value'
            }
            
            op if op == GET_ARRAY_MUT => {
                // In a real implementation, this would get a mutable reference to an array
                // For now, we'll just push a dummy array
            }
            
            op if op == SET_ARRAY_MUT => {
                // In a real implementation, this would set an array through a mutable reference
                // For now, we'll just pop the values
                let _value = pop_value(frame)?;
                let _array_ref = pop_value(frame)?;
            }
            
            _ => return Err("Not an array operation".to_string()),
        }
        
        Ok(())
    }
}