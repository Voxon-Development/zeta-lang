//! Memory operation instruction handlers for the Zeta VM.

use ahash::AHashMap;
use crate::vm::interpreter;
use super::*;
use crate::vm::interpreter::*;

/// Handles all memory operations.
#[derive(Clone)]
pub struct MemoryHandler {
    globals: AHashMap<String, VMValue>
}

impl MemoryHandler {
    pub fn new() -> MemoryHandler {
        MemoryHandler {
            globals: AHashMap::new()
        }
    }
}

impl InstructionHandler for MemoryHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        match frame.current_opcode {
            // Load/Store operations
            op if op == LOAD_VAR => {
                let offset = fetch_u16(&function.to_vec(), &mut frame.program_counter);
                let name = interpreter::fetch_string(offset, &function.to_vec(), &mut frame.program_counter);

                match frame.locals.get(&name) {
                    Some(value) => push_value(frame, value.clone()),
                    None => return Err(format!("Undefined local variable '{}'", name)),
                }
            }

            op if op == STORE_VAR => {
                let offset = fetch_u16(&function.to_vec(), &mut frame.program_counter);
                let name = fetch_string(offset, &function.to_vec(), &mut frame.program_counter);

                let value = pop_value(frame)?;
                frame.locals.insert(name, value);
            }

            op if op == LOAD_GLOBAL => {
                let offset = fetch_u16(&function.to_vec(), &mut frame.program_counter);
                let name = interpreter::fetch_string(offset, &function.to_vec(), &mut frame.program_counter);
                
                match self.globals.get(&name) {
                    Some(value) => push_value(frame, value.clone()),
                    None => return Err(format!("Undefined global variable '{}'", name)),
                }
            }

            op if op == STORE_GLOBAL => {
                let offset = fetch_u16(&function.to_vec(), &mut frame.program_counter);
                let name = fetch_string(offset, &function.to_vec(), &mut frame.program_counter);
                
                let value = pop_value(frame)?;
                self.globals.insert(name, value);
            }

            op if op == LOAD_LOCAL => {
                let index = fetch_u16(&function.to_vec(), &mut frame.program_counter) as usize;
                // In a real implementation, this would load from a local variable at 'index'
                push_value(frame, VMValue::I32(0));
            }

            op if op == STORE_LOCAL => {
                let _index = fetch_u16(&function.to_vec(), &mut frame.program_counter) as usize;
                let _value = pop_value(frame)?;
                // In a real implementation, this would store to a local variable at 'index'
            }


            // Memory operations
            op if op == LOAD => {
                let _addr = match pop_value(frame)? {
                    VMValue::Ptr(addr) => addr,
                    _ => return Err("Expected pointer for LOAD".to_string()),
                };
                // In a real implementation, this would load from memory at 'addr'
                push_value(frame, VMValue::I32(0));
            }

            op if op == STORE => {
                let value = pop_value(frame)?;
                let _addr = match pop_value(frame)? {
                    VMValue::Ptr(addr) => addr,
                    _ => return Err("Expected pointer for STORE".to_string()),
                };
                // In a real implementation, this would store 'value' at memory 'addr'
            }

            _ => return Err("Not a memory operation".to_string()),
        }
        
        Ok(())
    } 
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_load_store_var() {
        let mut frame = StackFrame::new(0, 0, 0);
        
        // Test STORE_VAR
        let mut code = vec![];
        // Store string offset (0)
        code.extend_from_slice(&0x0000u16.to_be_bytes());
        // String "x"
        code.push(1); // Length
        code.push(b'x');
        
        // Push a value to store
        frame.stack.push_vm(VMValue::I32(42));
        
        let mut memory_handler = MemoryHandler::new();
        memory_handler.execute(&mut frame, &code).unwrap();
        
        // Test LOAD_VAR
        let mut load_code = vec![];
        // Load string offset (0)
        load_code.extend_from_slice(&0x0000u16.to_be_bytes());
        // String "x"
        load_code.push(1); // Length
        load_code.push(b'x');

        memory_handler.execute(&mut frame, &load_code).unwrap();
        
        assert_eq!(frame.stack.pop_vm(), Some(VMValue::I32(42)));
    }
    
    // More tests for other memory operations...
}
