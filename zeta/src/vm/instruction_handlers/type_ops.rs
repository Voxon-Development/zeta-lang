//! Type operation instruction handlers for the Zeta VM.

use super::*;
use ir::BytecodeType;
use crate::vm::interpreter::*;
use crate::vm::virtualmachine::VirtualMachine;

/// Handles all type operations.
#[derive(Clone)]
pub struct TypeOpsHandler;

impl InstructionHandler for TypeOpsHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        function: &[u8],
    ) -> InstructionResult {
        match frame.current_opcode {
            op if op == CAST => {
                use ir::VMValue::*;
                use std::convert::TryInto;
                
                let target_type = BytecodeType::from(fetch_u8(&function.to_vec(), &mut frame.program_counter));
                let value = pop_value(frame)?;
                
                let result = match (value, target_type) {
                    // Integer to integer casts
                    (I8(v), BytecodeType::I8) => I8(v),
                    (I8(v), BytecodeType::I16) => I16(v.try_into().unwrap()),
                    (I8(v), BytecodeType::I32) => I32(v.try_into().unwrap()),
                    (I8(v), BytecodeType::I64) => I64(v.try_into().unwrap()),
                    (I8(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (I8(v), BytecodeType::U16) => U16(v.try_into().unwrap()),
                    (I8(v), BytecodeType::U32) => U32(v.try_into().unwrap()),
                    (I8(v), BytecodeType::U64) => U64(v.try_into().unwrap()),
                    
                    (I16(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (I16(v), BytecodeType::I16) => I16(v),
                    (I16(v), BytecodeType::I32) => I32(v.try_into().unwrap()),
                    (I16(v), BytecodeType::I64) => I64(v.try_into().unwrap()),
                    (I16(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (I16(v), BytecodeType::U16) => U16(v as u16),
                    (I16(v), BytecodeType::U32) => U32(v.try_into().unwrap()),
                    (I16(v), BytecodeType::U64) => U64(v.try_into().unwrap()),
                    
                    (I32(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (I32(v), BytecodeType::I16) => I16(v.try_into().unwrap()),
                    (I32(v), BytecodeType::I32) => I32(v),
                    (I32(v), BytecodeType::I64) => I64(v.try_into().unwrap()),
                    (I32(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (I32(v), BytecodeType::U16) => U16(v.try_into().unwrap()),
                    (I32(v), BytecodeType::U32) => U32(v as u32),
                    (I32(v), BytecodeType::U64) => U64(v.try_into().unwrap()),
                    
                    (I64(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (I64(v), BytecodeType::I16) => I16(v.try_into().unwrap()),
                    (I64(v), BytecodeType::I32) => I32(v.try_into().unwrap()),
                    (I64(v), BytecodeType::I64) => I64(v),
                    (I64(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (I64(v), BytecodeType::U16) => U16(v.try_into().unwrap()),
                    (I64(v), BytecodeType::U32) => U32(v.try_into().unwrap()),
                    (I64(v), BytecodeType::U64) => U64(v as u64),
                    
                    (U8(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (U8(v), BytecodeType::I16) => I16(v.try_into().unwrap()),
                    (U8(v), BytecodeType::I32) => I32(v.try_into().unwrap()),
                    (U8(v), BytecodeType::I64) => I64(v.try_into().unwrap()),
                    (U8(v), BytecodeType::U8) => U8(v),
                    (U8(v), BytecodeType::U16) => U16(v.try_into().unwrap()),
                    (U8(v), BytecodeType::U32) => U32(v.try_into().unwrap()),
                    (U8(v), BytecodeType::U64) => U64(v.try_into().unwrap()),
                    
                    (U16(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (U16(v), BytecodeType::I16) => I16(v as i16),
                    (U16(v), BytecodeType::I32) => I32(v.try_into().unwrap()),
                    (U16(v), BytecodeType::I64) => I64(v.try_into().unwrap()),
                    (U16(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (U16(v), BytecodeType::U16) => U16(v),
                    (U16(v), BytecodeType::U32) => U32(v.try_into().unwrap()),
                    (U16(v), BytecodeType::U64) => U64(v.try_into().unwrap()),
                    
                    (U32(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (U32(v), BytecodeType::I16) => I16(v.try_into().unwrap()),
                    (U32(v), BytecodeType::I32) => I32(v as i32),
                    (U32(v), BytecodeType::I64) => I64(v.try_into().unwrap()),
                    (U32(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (U32(v), BytecodeType::U16) => U16(v.try_into().unwrap()),
                    (U32(v), BytecodeType::U32) => U32(v),
                    (U32(v), BytecodeType::U64) => U64(v.try_into().unwrap()),
                    
                    (U64(v), BytecodeType::I8) => I8(v.try_into().unwrap()),
                    (U64(v), BytecodeType::I16) => I16(v.try_into().unwrap()),
                    (U64(v), BytecodeType::I32) => I32(v.try_into().unwrap()),
                    (U64(v), BytecodeType::I64) => I64(v as i64),
                    (U64(v), BytecodeType::U8) => U8(v.try_into().unwrap()),
                    (U64(v), BytecodeType::U16) => U16(v.try_into().unwrap()),
                    (U64(v), BytecodeType::U32) => U32(v.try_into().unwrap()),
                    (U64(v), BytecodeType::U64) => U64(v),
                    
                    // Float conversions
                    (F32(v), BytecodeType::F32) => F32(v),
                    (F32(v), BytecodeType::F64) => F64(v as f64),
                    (F32(v), BytecodeType::I8) => I8(v as i8),
                    (F32(v), BytecodeType::I16) => I16(v as i16),
                    (F32(v), BytecodeType::I32) => I32(v as i32),
                    (F32(v), BytecodeType::I64) => I64(v as i64),
                    (F32(v), BytecodeType::U8) => U8(v as u8),
                    (F32(v), BytecodeType::U16) => U16(v as u16),
                    (F32(v), BytecodeType::U32) => U32(v as u32),
                    (F32(v), BytecodeType::U64) => U64(v as u64),
                    
                    (F64(v), BytecodeType::F32) => F32(v as f32),
                    (F64(v), BytecodeType::F64) => F64(v),
                    (F64(v), BytecodeType::I8) => I8(v as i8),
                    (F64(v), BytecodeType::I16) => I16(v as i16),
                    (F64(v), BytecodeType::I32) => I32(v as i32),
                    (F64(v), BytecodeType::I64) => I64(v as i64),
                    (F64(v), BytecodeType::U8) => U8(v as u8),
                    (F64(v), BytecodeType::U16) => U16(v as u16),
                    (F64(v), BytecodeType::U32) => U32(v as u32),
                    (F64(v), BytecodeType::U64) => U64(v as u64),
                    
                    // Bool conversions
                    (Bool(v), BytecodeType::I8) => I8(v as i8),
                    (Bool(v), BytecodeType::I16) => I16(v as i16),
                    (Bool(v), BytecodeType::I32) => I32(v as i32),
                    (Bool(v), BytecodeType::I64) => I64(v as i64),
                    (Bool(v), BytecodeType::U8) => U8(v as u8),
                    (Bool(v), BytecodeType::U16) => U16(v as u16),
                    (Bool(v), BytecodeType::U32) => U32(v as u32),
                    (Bool(v), BytecodeType::U64) => U64(v as u64),
                    (Bool(v), BytecodeType::Boolean) => Bool(v),
                    
                    // Unsupported cast
                    (v, t) => return Err(format!("Unsupported cast from {:?} to {:?}", v, t)),
                };
                
                push_value(frame, result);
            }
            
            _ => return Err("Not a type operation".to_string()),
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use zetac::codegen::ir::module::ZetaModule;
    use super::*;
    use crate::vm::memory::stack::BumpStack;
    
    #[test]
    fn test_i32_to_f64_cast() {
        let mut frame = StackFrame::new(0, 0, 0);
        
        // Push a value to cast
        frame.stack.push_vm(VMValue::I32(42));
        
        // Set up CAST instruction with target type F64
        let code = vec![CAST, BytecodeType::F64 as u8];
        
        let mut handler = TypeOpsHandler;
        handler.execute(&mut frame, &code).expect("Failed to execute CAST instruction");
        
        match frame.stack.pop_vm() {
            Some(VMValue::F64(v)) => assert_eq!(v, 42.0),
            other => panic!("Expected F64(42.0), got {:?}", other),
        }
    }
}
