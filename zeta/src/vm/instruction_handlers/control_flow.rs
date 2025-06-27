//! Control flow instruction handlers for the Zeta VM.

use super::*;
use crate::vm::interpreter::*;

/// Handles all control flow operations.
#[derive(Clone)]
pub struct ControlFlowHandler;

// In control_flow.rs
macro_rules! handle_jump {
    ($frame:expr, $function:expr, $condition:expr) => {{
        let offset = fetch_i16(&$function.to_vec(), &mut $frame.program_counter);
        if $condition {
            $frame.program_counter = $frame.program_counter.wrapping_add(offset as usize);
        }
        Ok(())
    }};
}

impl InstructionHandler for ControlFlowHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        match frame.current_opcode {
            // Unconditional jump
            op if op == JUMP => {
                handle_jump!(frame, function, true)
            }
            
            // Conditional jumps
            op if op == JUMP_IF_TRUE => {
                // use handle_jump macro
                frame.program_counter += 1;
                handle_jump!(frame, function, {
                    match pop_value(frame)? {
                        VMValue::Bool(condition) => condition,
                        _ => false,
                    }
                })
            }
            
            op if op == JUMP_IF_FALSE => {
                frame.program_counter += 1;
                handle_jump!(frame, function, {
                    match pop_value(frame)? {
                        VMValue::Bool(condition) => !condition,
                        _ => true,
                    }
                })
            }
            
            // Function return
            op if op == RETURN => {
                Err("Return from function".to_string())
            }
            
            _ => Err("Not a control flow operation".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use zetac::codegen::ir::module::ZetaModule;
    use super::*;
    use crate::vm::memory::stack::BumpStack;
    
    #[test]
    fn test_jump() {
        let mut frame = StackFrame::new(0, 0, 0);
        
        // Set up test
        let mut code = vec![0; 10];
        code[0] = JUMP;
        code[1] = 0x00;  // offset high byte
        code[2] = 0x04;  // offset low byte (jump 4 bytes)
        
        // Initial PC is 0, after JUMP it should be 6 (2 for opcode + 2 for offset + 2 for jump)
        let mut handler = ControlFlowHandler;
        handler.execute(&mut frame, &code).unwrap();
        
        assert_eq!(frame.program_counter, 6);
    }
    
    // More tests for other control flow operations...
}
