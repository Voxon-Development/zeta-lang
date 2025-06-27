use cranelift::frontend::*;
use cranelift::prelude::*;
use cranelift::prelude::Value;
use ir::Bytecode;

/// Translate VM bytecode to Cranelift IR
pub fn translate_bytecode(
    builder: &mut FunctionBuilder,
    bytecode: &[u8],
    vm_ptr: Value,
    args_ptr: Value,
) -> anyhow::Result<()> {
    use ir::Bytecode::*;

    let mut pc = 0;
    let len = bytecode.len();
    let int32_ty = types::I32;
    let int64_ty = types::I64;
    let float32_ty = types::F32;
    let float64_ty = types::F64;

    // Stack for storing SSA values
    let mut stack: Vec<Value> = Vec::new();

    // Helper to pop a value from the stack
    let pop = |stack: &mut Vec<Value>| -> Value {
        stack.pop().expect("Stack underflow")
    };

    while pc < len {
        let op = bytecode[pc];
        pc += 1;

        match Bytecode::try_from(op).map_err(|_| anyhow::anyhow!("Invalid opcode: {}", op))? {
            // Constants
            PushI32 => {
                let value = i32::from_le_bytes([bytecode[pc], bytecode[pc+1], bytecode[pc+2], bytecode[pc+3]]);
                pc += 4;
                let val = builder.ins().iconst(int32_ty, i64::from(value));
                stack.push(val);
            }
            PushI64 => {
                let value = i64::from_le_bytes([
                    bytecode[pc], bytecode[pc+1], bytecode[pc+2], bytecode[pc+3],
                    bytecode[pc+4], bytecode[pc+5], bytecode[pc+6], bytecode[pc+7],
                ]);
                pc += 8;
                let val = builder.ins().iconst(int64_ty, value);
                stack.push(val);
            }
            PushF32 => {
                let bits = u32::from_le_bytes([bytecode[pc], bytecode[pc+1], bytecode[pc+2], bytecode[pc+3]]);
                pc += 4;
                let val = builder.ins().f32const(Ieee32::with_bits(bits));
                stack.push(val);
            }
            PushF64 => {
                let bits = u64::from_le_bytes([
                    bytecode[pc], bytecode[pc+1], bytecode[pc+2], bytecode[pc+3],
                    bytecode[pc+4], bytecode[pc+5], bytecode[pc+6], bytecode[pc+7],
                ]);
                pc += 8;
                let val = builder.ins().f64const(Ieee64::with_bits(bits));
                stack.push(val);
            }
            PushBool => {
                let value = bytecode[pc] as i64;
                pc += 1;
                let val = builder.ins().iconst(types::I8, value);
                stack.push(val);
            }

            // Arithmetic operations
            Add => {
                let b = pop(&mut stack);
                let a = pop(&mut stack);
                let a_type = builder.func.dfg.value_type(a);
                let b_type = builder.func.dfg.value_type(b);
                let val = match (a, b) {
                    (a, b) if a_type == int32_ty && b_type == int32_ty => {
                        builder.ins().iadd(a, b)
                    }
                    (a, b) if a_type == int64_ty && b_type == int64_ty => {
                        builder.ins().iadd(a, b)
                    }
                    (a, b) if a_type == float32_ty && b_type == float32_ty => {
                        builder.ins().fadd(a, b)
                    }
                    (a, b) if a_type == float64_ty && b_type == float64_ty => {
                        builder.ins().fadd(a, b)
                    }
                    _ => return Err(anyhow::anyhow!("Type mismatch in addition")),
                };
                stack.push(val);
            }

            Sub => {
                let b = pop(&mut stack);
                let a = pop(&mut stack);
                let a_type = builder.func.dfg.value_type(a);
                let b_type = builder.func.dfg.value_type(b);
                let val = match (a, b) {
                    (a, b) if a_type == int32_ty && b_type == int32_ty => {
                        builder.ins().isub(a, b)
                    }
                    (a, b) if a_type == int64_ty && b_type == int64_ty => {
                        builder.ins().isub(a, b)
                    }
                    (a, b) if a_type == float32_ty && b_type == float32_ty => {
                        builder.ins().fsub(a, b)
                    }
                    (a, b) if a_type == float64_ty && b_type == float64_ty => {
                        builder.ins().fsub(a, b)
                    }
                    _ => return Err(anyhow::anyhow!("Type mismatch in subtraction")),
                };
                stack.push(val);
            }

            Mul => {
                let b = pop(&mut stack);
                let a = pop(&mut stack);
                let a_type = builder.func.dfg.value_type(a);
                let b_type = builder.func.dfg.value_type(b);
                let val = match (a, b) {
                    (a, b) if a_type == int32_ty && b_type == int32_ty => {
                        builder.ins().imul(a, b)
                    }
                    (a, b) if a_type == int64_ty && b_type == int64_ty => {
                        builder.ins().imul(a, b)
                    }
                    (a, b) if a_type == float32_ty && b_type == float32_ty => {
                        builder.ins().fmul(a, b)
                    }
                    (a, b) if a_type == float64_ty && b_type == float64_ty => {
                        builder.ins().fmul(a, b)
                    }
                    _ => return Err(anyhow::anyhow!("Type mismatch in multiplication")),
                };
                stack.push(val);
            }

            Div => {
                let b = pop(&mut stack);
                let a = pop(&mut stack);
                let a_type = builder.func.dfg.value_type(a);
                let b_type = builder.func.dfg.value_type(b);
                let val = match (a, b) {
                    (a, b) if a_type == int32_ty && b_type == int32_ty => {
                        builder.ins().udiv(a, b) // or sdiv for signed division
                    }
                    (a, b) if a_type == int64_ty && b_type == int64_ty => {
                        builder.ins().udiv(a, b) // or sdiv for signed division
                    }
                    (a, b) if a_type == float32_ty && b_type == float32_ty => {
                        builder.ins().fdiv(a, b)
                    }
                    (a, b) if a_type == float64_ty && b_type == float64_ty => {
                        builder.ins().fdiv(a, b)
                    }
                    _ => return Err(anyhow::anyhow!("Type mismatch in division")),
                };
                stack.push(val);
            }

            // Comparison operations
            Eq => {
                let b = pop(&mut stack);
                let a = pop(&mut stack);
                let a_type = builder.func.dfg.value_type(a);
                let b_type = builder.func.dfg.value_type(b);
                let val = match (a, b) {
                    (a, b) if a_type == int32_ty && b_type == int32_ty => {
                        builder.ins().icmp(IntCC::Equal, a, b)
                    }
                    (a, b) if a_type == int64_ty && b_type == int64_ty => {
                        builder.ins().icmp(IntCC::Equal, a, b)
                    }
                    (a, b) if a_type == float32_ty && b_type == float32_ty => {
                        builder.ins().fcmp(FloatCC::Equal, a, b)
                    }
                    (a, b) if a_type == float64_ty && b_type == float64_ty => {
                        builder.ins().fcmp(FloatCC::Equal, a, b)
                    }
                    _ => return Err(anyhow::anyhow!("Type mismatch in equality comparison")),
                };
                stack.push(val);
            }

            // Control flow
            Jump => {
                let offset = i16::from_le_bytes([bytecode[pc], bytecode[pc+1]]) as i64;
                pc += 2;
                // In a real implementation, we'd need to handle jumps
                // This is simplified and would need proper basic block handling
                return Err(anyhow::anyhow!("Jump not yet implemented"));
            }

            JumpIfTrue => {
                let offset = i16::from_le_bytes([bytecode[pc], bytecode[pc+1]]) as i64;
                pc += 2;
                let cond = pop(&mut stack);
                // In a real implementation, we'd need to handle conditional jumps
                return Err(anyhow::anyhow!("JumpIfTrue not yet implemented"));
            }

            JumpIfFalse => {
                let offset = i16::from_le_bytes([bytecode[pc], bytecode[pc+1]]) as i64;
                pc += 2;
                let cond = pop(&mut stack);
                // In a real implementation, we'd need to handle conditional jumps
                return Err(anyhow::anyhow!("JumpIfFalse not yet implemented"));
            }

            // Function calls
            Call => {
                let func_idx = bytecode[pc] as u16 | ((bytecode[pc+1] as u16) << 8);
                pc += 2;
                let arg_count = bytecode[pc] as usize;
                pc += 1;

                // Collect arguments from the stack
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(pop(&mut stack));
                }
                args.reverse(); // Arguments are pushed right-to-left

                // In a real implementation, we'd look up the function and call it
                return Err(anyhow::anyhow!("Function calls not yet implemented"));
            }

            // Return
            Return => {
                if !stack.is_empty() {
                    let ret_val = pop(&mut stack);
                    // In a real implementation, we'd need to convert the value to a VMValue
                    builder.ins().return_(&[ret_val]);
                } else {
                    // Return void
                    builder.ins().return_(&[]);
                }
            }

            // Stack operations
            Pop => {
                let _ = pop(&mut stack);
            }

            Dup => {
                let val = *stack.last().ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                stack.push(val);
            }

            // More bytecodes would be implemented here...
            _ => return Err(anyhow::anyhow!("Unimplemented opcode: {:?}", op)),
        }
    }

    // If we fell through the end of the function without a return, add one
    if !builder.is_unreachable() {
        builder.ins().return_(&[]);
    }

    Ok(())
}