use enum_map::Enum;
use std::collections::HashMap;
use ir::Bytecode;

use crate::codegen::ir::module::ZetaModule;

use std::sync::{Arc, Mutex};

pub trait Pass: Send + Sync + 'static {
    fn optimize(&self, bytecode: &mut Vec<u8>, module: &ZetaModule) -> anyhow::Result<()>;
    fn priority(&self) -> OptimizationPassPriority;
    
    /// Create a new boxed instance of the pass (for thread-safe cloning)
    #[inline]
    fn boxed(self) -> Box<dyn Pass> 
    where 
        Self: Sized + 'static 
    {
        Box::new(self)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug, Enum)]
pub enum OptimizationPassPriority {
    Max,
    High,
    Medium,
    Low,
    Min,
}

impl OptimizationPassPriority {
    pub fn to_string(&self) -> String {
        match self {
            OptimizationPassPriority::Max => "Max".to_string(),
            OptimizationPassPriority::High => "High".to_string(),
            OptimizationPassPriority::Medium => "Normal".to_string(),
            OptimizationPassPriority::Low => "Low".to_string(),
            OptimizationPassPriority::Min => "Min".to_string(),
        }
    }
}

impl From<u8> for OptimizationPassPriority {
    fn from(value: u8) -> Self {
        match value {
            0 => OptimizationPassPriority::Max,
            1 => OptimizationPassPriority::High,
            2 => OptimizationPassPriority::Medium,
            3 => OptimizationPassPriority::Low,
            _ => OptimizationPassPriority::Min,
        }
    }
}

impl Default for OptimizationPassPriority {
    fn default() -> Self {
        OptimizationPassPriority::Medium
    }
}

pub struct ConstantFoldingPass;

impl Pass for ConstantFoldingPass {
    fn optimize(&self, bytecode: &mut Vec<u8>, _module: &ZetaModule) -> anyhow::Result<()> {
        let mut i = 0;
        while i < bytecode.len() {
            // Check if we have a binary operation with two constants before it
            if i >= 2 && Self::is_binary_op(bytecode[i]) {
                let op = bytecode[i];
                // Check if the previous two instructions are constants
                if i >= 2 && Self::is_constant_load(&bytecode[i-2..i]) {
                    // Get the two constant values
                    let (val1, val1_size) = Self::get_constant_value(&bytecode[..i-1]);
                    let (val2, val2_size) = Self::get_constant_value(&bytecode[i-1..i+1]);
                    
                    if let Some(result) = Self::evaluate_binary_op(op, val1, val2) {
                        let result_bytes = Self::get_constant_bytes(result);
                        let replace_start = i - val1_size - val2_size;
                        let replace_end = i + 1;

                        let len = result_bytes.len();
                        bytecode.splice(replace_start..replace_end, result_bytes);
                        i = replace_start + len;
                        continue;
                    }
                }
            }
            i += 1;
        }
        Ok(())
    }

    
    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Medium
    }
}

impl ConstantFoldingPass {
    fn is_binary_op(op: u8) -> bool {
        matches!(Bytecode::try_from(op).unwrap(),
            Bytecode::Add | Bytecode::Sub | 
            Bytecode::Mul | Bytecode::Div | 
            Bytecode::Mod | Bytecode::BitOr | 
            Bytecode::BitAnd | Bytecode::BitXor |
            Bytecode::Shl | Bytecode::Shr
        )
    }
    
    fn is_constant_load(ops: &[u8]) -> bool {
        let result = Bytecode::try_from(ops[0]);
        if result.is_err() {
            return false;
        }
        matches!(result.unwrap(),
            Bytecode::PushI32 | Bytecode::PushI64 |
            Bytecode::PushU8 | Bytecode::PushU16 |
            Bytecode::PushU32 | Bytecode::PushU64
        )
    }
    
    fn get_constant_value(ops: &[u8]) -> (i64, usize) {
        match Bytecode::try_from(ops[0]).unwrap() {
            Bytecode::PushI32 => (i32::from_le_bytes([ops[1], ops[2], ops[3], ops[4]]) as i64, 5),
            Bytecode::PushI64 => 
                (i64::from_le_bytes([ops[1], ops[2], ops[3], ops[4], ops[5], ops[6], ops[7], ops[8]]), 9),
            Bytecode::PushU8 => (ops[1] as i64, 2),
            Bytecode::PushU16 => (u16::from_le_bytes([ops[1], ops[2]]) as i64, 3),
            Bytecode::PushU32 => (u32::from_le_bytes([ops[1], ops[2], ops[3], ops[4]]) as i64, 5),
            Bytecode::PushU64 =>
                (u64::from_le_bytes([ops[1], ops[2], ops[3], ops[4], ops[5], ops[6], ops[7], ops[8]]) as i64, 9),
            _ => (0, 0),
        }
    }
    
    fn evaluate_binary_op(op: u8, left: i64, right: i64) -> Option<i64> {
        match Bytecode::try_from(op).unwrap() {
            Bytecode::Add => left.checked_add(right),
            Bytecode::Sub => left.checked_sub(right),
            Bytecode::Mul => left.checked_mul(right),
            Bytecode::Div => left.checked_div(right),
            Bytecode::Mod => left.checked_rem(right),
            Bytecode::BitOr => Some(left | right),
            Bytecode::BitAnd => Some(left & right),
            Bytecode::BitXor => Some(left ^ right),
            Bytecode::Shl => left.checked_shl(right as u32),
            Bytecode::Shr => left.checked_shr(right as u32),
            _ => None,
        }
    }
    
    fn get_constant_bytes(value: i64) -> Vec<u8> {
        if value >= i8::MIN as i64 && value <= i8::MAX as i64 {
            vec![Bytecode::PushI8 as u8, value as u8]
        } else if value >= i16::MIN as i64 && value <= i16::MAX as i64 {
            let bytes = (value as i16).to_le_bytes();
            vec![Bytecode::PushI16 as u8, bytes[0], bytes[1]]
        } else if value >= i32::MIN as i64 && value <= i32::MAX as i64 {
            let bytes = (value as i32).to_le_bytes();
            vec![Bytecode::PushI32 as u8, bytes[0], bytes[1], bytes[2], bytes[3]]
        } else {
            let bytes = value.to_le_bytes();
            let mut result = vec![Bytecode::PushI64 as u8];
            result.extend_from_slice(&bytes);
            result
        }
    }
}

pub struct DeadCodeEliminationPass;

impl Pass for DeadCodeEliminationPass {
    fn optimize(&self, bytecode: &mut Vec<u8>, _module: &ZetaModule) -> anyhow::Result<()> {
        let mut i = 0;
        while i < bytecode.len() {
            match Bytecode::try_from(bytecode[i]).unwrap() {
                // Remove unreachable code after return/break/continue/throw
                Bytecode::Return | Bytecode::Halt => {
                    // Remove all instructions until the next label or end of function
                    let mut j = i + 1;
                    while j < bytecode.len() {
                        if Self::is_control_flow(&bytecode[j..]) {
                            break;
                        }
                        j += 1;
                    }
                    if j > i + 1 {
                        bytecode.drain(i+1..j);
                    }
                }
                // Remove dead stores (store followed by another store to same location)
                Bytecode::StoreLocal | Bytecode::StoreGlobal | Bytecode::StoreVar => {
                    if let Some(loc) = Self::get_store_location(&bytecode[i..]) {
                        let mut j = i + loc.len();
                        while j < bytecode.len() {
                            if Self::is_control_flow(&bytecode[j..]) {
                                break;
                            }
                            
                            // Check if this is a store to the same location
                            if let Some(other_loc) = Self::get_store_location(&bytecode[j..]) {
                                if other_loc == loc {
                                    // Found a store to the same location, remove the first one
                                    bytecode.drain(i..i+loc.len());
                                    i -= 1; // Adjust index since we removed items
                                    break;
                                }
                                j += other_loc.len();
                            } else {
                                j += 1;
                            }
                        }
                    }
                }
                _ => {}
            }
            i += 1;
        }
        Ok(())
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Medium
    }
}

impl DeadCodeEliminationPass {
    fn is_control_flow(bytecode: &[u8]) -> bool {
        matches!(
            Bytecode::try_from(0).unwrap(),
            Bytecode::Jump
                | Bytecode::JumpIfTrue
                | Bytecode::JumpIfFalse
                | Bytecode::Return
                | Bytecode::Halt
                | Bytecode::Call
                | Bytecode::TailCall
                | Bytecode::CallNative
        )
    }
    
    fn get_store_location(bytecode: &[u8]) -> Option<Vec<u8>> {
        match Bytecode::try_from(bytecode[0]).unwrap() {
            Bytecode::StoreLocal | Bytecode::LoadLocal => {
                if bytecode.len() > 1 {
                    Some(bytecode[0..2].to_vec())
                } else {
                    None
                }
            }
            Bytecode::StoreGlobal | Bytecode::LoadGlobal => {
                if bytecode.len() > 1 {
                    Some(bytecode[0..2].to_vec())
                } else {
                    None
                }
            }
            Bytecode::StoreVar | Bytecode::LoadVar => {
                if bytecode.len() > 1 {
                    // For StoreVar, the name length is the next byte
                    let name_len = bytecode[1] as usize;
                    if bytecode.len() > 1 + name_len {
                        Some(bytecode[0..2 + name_len].to_vec())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}


pub struct InliningPass {
    // Maximum size of function to inline (in bytes)
    max_inline_size: usize,
}

impl Default for InliningPass {
    fn default() -> Self {
        Self {
            max_inline_size: 120, // Reasonable default for small functions
        }
    }
}

impl Pass for InliningPass {
    fn optimize(&self, bytecode: &mut Vec<u8>, module: &ZetaModule) -> anyhow::Result<()> {
        let mut i = 0;
        while i < bytecode.len() {
            if i + 1 < bytecode.len() && Bytecode::try_from(bytecode[i]).unwrap() == Bytecode::Call {
                // Get the function name
                if let Some((name, name_len)) = self.get_function_name(&bytecode[i+1..]) {
                    // Look up the function in the module
                    if let Some((_, func)) = module.functions.iter().find(|(_, f)| f.name == name) {
                        // Check if the function is small enough to inline
                        if func.code.len() <= self.max_inline_size {
                            // Replace the call with the function body
                            let call_size = 1 + 1 + name_len; // CALL op + name length + name
                            let mut new_code = func.code.clone();
                            
                            // Replace return with jump to the end of inlined code
                            if let Some(return_pos) = new_code.iter().position(|&b| Bytecode::try_from(b).unwrap() == Bytecode::Return) {
                                new_code.truncate(return_pos);
                            }

                            let len = new_code.len();
                            bytecode.splice(i..i+call_size, new_code);
                            i += len;
                            continue;
                        }
                    }
                }
            }
            i += 1;
        }
        Ok(())
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Max
    }
}

impl InliningPass {
    fn get_function_name(&self, bytecode: &[u8]) -> Option<(String, usize)> {
        if bytecode.is_empty() {
            return None;
        }
        
        let name_len = bytecode[0] as usize;
        if bytecode.len() > name_len {
            let name = String::from_utf8_lossy(&bytecode[1..=name_len]).into_owned();
            Some((name, name_len + 1)) // +1 for the length byte
        } else {
            None
        }
    }
}