//! Arithmetic instruction handlers for the Zeta VM.

use super::*;
use crate::binary_op_int;
use crate::binary_op_cmp;
use crate::vm::interpreter::*;
use crate::vm::virtualmachine::VirtualMachine;

/// Handles all arithmetic operations.
#[derive(Clone)]
pub struct ArithmeticHandler;

impl InstructionHandler for ArithmeticHandler {
    fn execute(
        &mut self,
        frame: &mut StackFrame,
        _function: &[u8],
    ) -> InstructionResult {
        use ir::VMValue::*;
        
        match frame.current_opcode {
            // Binary operations
            op if op == ADD => binary_op_int!(frame.stack, +),
            op if op == SUB => binary_op_int!(frame.stack, -),
            op if op == MUL => binary_op_int!(frame.stack, *),
            op if op == DIV => binary_op_int!(frame.stack, /),
            op if op == MOD => binary_op_int!(frame.stack, %),
            op if op == AND => binary_op_int!(frame.stack, &),
            op if op == OR => binary_op_int!(frame.stack, |),
            op if op == XOR => binary_op_int!(frame.stack, ^),
            op if op == SHL => binary_op_int!(frame.stack, <<),
            op if op == SHR => binary_op_int!(frame.stack, >>),
            
            // Comparison operations
            op if op == EQ => binary_op_cmp!(frame.stack, ==),
            op if op == NEQ => binary_op_cmp!(frame.stack, !=),
            op if op == LT => binary_op_cmp!(frame.stack, <),
            op if op == LTE => binary_op_cmp!(frame.stack, <=),
            op if op == GT => binary_op_cmp!(frame.stack, >),
            op if op == GTE => binary_op_cmp!(frame.stack, >=),

            op if op == ASSIGN => {
                let value = frame.stack.pop_vm().expect("Stack underflow on Assign");
                frame.stack.push_vm(value);
            }

            op if op == ADD_ASSIGN => binary_op_int!(frame.stack, +),
            op if op == SUB_ASSIGN => binary_op_int!(frame.stack, -),
            op if op == MUL_ASSIGN => binary_op_int!(frame.stack, *),
            op if op == DIV_ASSIGN => binary_op_int!(frame.stack, /),
            op if op == MOD_ASSIGN => binary_op_int!(frame.stack, %),
            op if op == SHL_ASSIGN => binary_op_int!(frame.stack, <<),
            op if op == SHR_ASSIGN => binary_op_int!(frame.stack, >>),
            op if op == BIT_AND_ASSIGN => binary_op_int!(frame.stack, &),
            op if op == BIT_OR_ASSIGN => binary_op_int!(frame.stack, |),
            op if op == BIT_XOR_ASSIGN => binary_op_int!(frame.stack, ^),
            op if op == POW_ASSIGN => {
                let rhs_option = frame.stack.pop_vm();
                let lhs_option = frame.stack.pop_vm();

                let rhs = match rhs_option {
                    Some(vm_value) => vm_value,
                    None => panic!("RHS is None")
                };

                let lhs = match lhs_option {
                    Some(vm_value) => vm_value,
                    None => panic!("RHS is None")
                };

                match (lhs.clone(), rhs.clone()) {
                    (I32(lhs), I32(rhs)) => {
                        frame.stack.push_vm(I32(lhs.pow(rhs as u32)));
                    }
                    (I64(lhs), I64(rhs)) => {
                        frame.stack.push_vm(I64(lhs.pow(rhs as u32)));
                    }
                    (U32(lhs), U32(rhs)) => {
                        frame.stack.push_vm(U32(lhs.pow(rhs)));
                    }
                    (U64(lhs), U64(rhs)) => {
                        frame.stack.push_vm(U64(lhs.pow(rhs as u32)));
                    }
                    (F32(lhs), F32(rhs)) => {
                        frame.stack.push_vm(F32(lhs.powf(rhs)));
                    }
                    (F64(lhs), F64(rhs)) => {
                        frame.stack.push_vm(F64(lhs.powf(rhs)));
                    }
                    (I8(lhs), I8(rhs)) => {
                        frame.stack.push_vm(I8(lhs.pow(rhs as u32)));
                    }
                    (I16(lhs), I16(rhs)) => {
                        frame.stack.push_vm(I16(lhs.pow(rhs as u32)));
                    }
                    (U8(lhs), U8(rhs)) => {
                        frame.stack.push_vm(U8(lhs.pow(rhs as u32)));
                    }
                    (U16(lhs), U16(rhs)) => {
                        frame.stack.push_vm(U16(lhs.pow(rhs as u32)));
                    }
                    (Ptr(lhs), Ptr(rhs)) => {
                        frame.stack.push_vm(Ptr(lhs.pow(rhs as u32)));
                    }

                    _ => panic!("Invalid types for binary operation: {:?} {:?}", lhs, rhs)
                }
            }
            op if op == NOT => {
                let val = pop_value(frame)?;
                match val {
                    Bool(val) => push_value(frame, Bool(!val)),
                    _ => return Err("Invalid type for NOT".to_string()),
                }
            }
            
            _ => return Err("Not an arithmetic operation".to_string()),
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use zetac::codegen::ir::module::ZetaModule;
    use super::*;
    
    #[test]
    fn test_add() {
        let mut frame = StackFrame::new(0, 0, 0);
        
        // Set up test
        frame.stack.push_vm(VMValue::I32(5));
        frame.stack.push_vm(VMValue::I32(3));
        
        // Execute ADD
        let mut handler = ArithmeticHandler;
        handler.execute(&mut frame, &[ADD]).unwrap();
        
        // Check result
        assert_eq!(frame.stack.pop_vm(), Some(VMValue::I32(8)));
    }
    
    // More tests for other arithmetic operations...
}
