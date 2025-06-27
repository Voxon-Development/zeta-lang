//! Stack operation instruction handlers for the Zeta VM.

use std::sync::Arc;
use parking_lot::Mutex;
use trc::SharedTrc;
use super::*;
use crate::vm::interpreter::*;
use crate::vm::memory::string_pool::StringPool;

/// Handles all stack operations.
#[derive(Clone)]
pub struct StackOpsHandler {
    string_pool: SharedTrc<Mutex<StringPool>>
}

impl StackOpsHandler {
    #[inline(always)]
    pub fn new(string_pool: SharedTrc<Mutex<StringPool>>) -> StackOpsHandler {
        StackOpsHandler { string_pool }
    }
}

impl InstructionHandler for StackOpsHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        match frame.current_opcode {
            // Push operations
            op if op == PUSH_INT => {
                let v = fetch_i64(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::I64(v));
            }
            
            op if op == PUSH_U8 => {
                let v = fetch_u8(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::U8(v));
            }
            
            op if op == PUSH_I8 => {
                let v = fetch_i8(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::I8(v));
            }
            
            op if op == PUSH_U16 => {
                let v = fetch_u16(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::U16(v));
            }
            
            op if op == PUSH_I16 => {
                let v = fetch_i16(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::I16(v));
            }
            
            op if op == PUSH_U32 => {
                let v = fetch_u32(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::U32(v));
            }
            
            op if op == PUSH_I32 => {
                let v = fetch_i32(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::I32(v));
            }
            
            op if op == PUSH_F32 => {
                let v = fetch_f32(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::F32(v));
            }
            
            op if op == PUSH_F64 => {
                let v = fetch_f64(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::F64(v));
            }
            
            op if op == PUSH_U64 => {
                let v = fetch_u64(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::U64(v));
            }
            
            op if op == PUSH_I64 => {
                let v = fetch_i64(&function.to_vec(), &mut frame.program_counter);
                push_value(frame, VMValue::I64(v));
            }
            
            op if op == PUSH_BOOL => {
                let b = fetch_u8(&function.to_vec(), &mut frame.program_counter) != 0;
                push_value(frame, VMValue::Bool(b));
            }
            
            op if op == PUSH_STR => {
                let offset = fetch_u16(&function.to_vec(), &mut frame.program_counter);
                let s = fetch_string(offset, &function.to_vec(), &mut frame.program_counter);
                let id = self.string_pool.lock().intern(&s);
                push_value(frame, VMValue::Str(id));
            }
            
            // Stack manipulation
            op if op == POP => {
                pop_value(frame)?;
            }
            
            op if op == DUP => {
                let val = frame.stack.peek_vm()
                    .ok_or_else(|| "Stack underflow in DUP")?
                    .clone();
                push_value(frame, val);
            }
            
            op if op == SWAP => {
                let v1 = pop_value(frame)?;
                let v2 = pop_value(frame)?;
                push_value(frame, v1);
                push_value(frame, v2);
            }
            
            _ => return Err("Not a stack operation".to_string()),
        }
        
        Ok(())
    }
}

mod tests {
    #[test]
    fn test() {}
}