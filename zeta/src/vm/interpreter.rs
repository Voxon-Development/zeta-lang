use std::cell::RefCell;
use std::sync::LazyLock;
use lazy_static::lazy_static;
use ir::VMValue;
use ir::Bytecode;
use crate::vm::memory::frames::StackFrame;
use crate::vm::memory::regions::RegionAllocator;

pub const ADD: u8 = Bytecode::Add as u8;
pub const SUB: u8 = Bytecode::Sub as u8;
pub const MUL: u8 = Bytecode::Mul as u8;
pub const DIV: u8 = Bytecode::Div as u8;
pub const MOD: u8 = Bytecode::Mod as u8;
pub const AND: u8 = Bytecode::And as u8;
pub const OR: u8 = Bytecode::Or as u8;
pub const XOR: u8 = Bytecode::Xor as u8;
pub const NOT: u8 = Bytecode::Not as u8;
pub const SHL: u8 = Bytecode::Shl as u8;
pub const SHR: u8 = Bytecode::Shr as u8;
pub const EQ: u8 = Bytecode::Eq as u8;
pub const NEQ: u8 = Bytecode::Ne as u8;
pub const LT: u8 = Bytecode::Lt as u8;
pub const LTE: u8 = Bytecode::Le as u8;
pub const GT: u8 = Bytecode::Gt as u8;
pub const GTE: u8 = Bytecode::Ge as u8;
pub const JUMP: u8 = Bytecode::Jump as u8;
pub const JUMP_IF_TRUE: u8 = Bytecode::JumpIfTrue as u8;
pub const JUMP_IF_FALSE: u8 = Bytecode::JumpIfFalse as u8;

// Regions

pub const NEW_REGION: u8 = Bytecode::NewRegion as u8;


// Assignments

pub const ASSIGN: u8 = Bytecode::Assign as u8;
pub const ADD_ASSIGN: u8 = Bytecode::AddAssign as u8;
pub const SUB_ASSIGN: u8 = Bytecode::SubAssign as u8;
pub const MUL_ASSIGN: u8 = Bytecode::MulAssign as u8;
pub const DIV_ASSIGN: u8 = Bytecode::DivAssign as u8;
pub const MOD_ASSIGN: u8 = Bytecode::ModAssign as u8;
pub const BIT_AND_ASSIGN: u8 = Bytecode::BitAndAssign as u8;
pub const BIT_OR_ASSIGN: u8 = Bytecode::BitOrAssign as u8;
pub const BIT_XOR_ASSIGN: u8 = Bytecode::BitXorAssign as u8;
pub const SHL_ASSIGN: u8 = Bytecode::ShlAssign as u8;
pub const SHR_ASSIGN: u8 = Bytecode::ShrAssign as u8;
pub const POW_ASSIGN: u8 = Bytecode::PowAssign as u8;

pub const BRANCH: u8 = Bytecode::Branch as u8;
pub const RETURN: u8 = Bytecode::Return as u8;
pub const HALT: u8 = Bytecode::Halt as u8;
pub const CALL: u8 = Bytecode::Call as u8;
pub const TAIL_CALL: u8 = Bytecode::TailCall as u8;
pub const ARRAY_GET: u8 = Bytecode::ArrayGet as u8;
pub const ARRAY_SET: u8 = Bytecode::ArraySet as u8;
pub const GET_ARRAY_MUT: u8 = Bytecode::GetArrayMut as u8;
pub const SET_ARRAY_MUT: u8 = Bytecode::SetArrayMut as u8;
pub const GET_FIELD: u8 = Bytecode::GetField as u8;
pub const ARRAY_ALLOC: u8 = Bytecode::ArrayAlloc as u8;
pub const ARRAY_LEN: u8 = Bytecode::ArrayLen as u8;

pub const PUSH_I64: u8 = Bytecode::PushI64 as u8;
pub const PUSH_F64: u8 = Bytecode::PushF64 as u8;
pub const PUSH_U8: u8 = Bytecode::PushU8 as u8;
pub const PUSH_U16: u8 = Bytecode::PushU16 as u8;
pub const PUSH_U32: u8 = Bytecode::PushU32 as u8;
pub const PUSH_U64: u8 = Bytecode::PushU64 as u8;
pub const PUSH_I8: u8 = Bytecode::PushI8 as u8;
pub const PUSH_I16: u8 = Bytecode::PushI16 as u8;
pub const PUSH_I32: u8 = Bytecode::PushI32 as u8;

pub const PUSH_F32: u8 = Bytecode::PushF32 as u8;
pub const PUSH_STR: u8 = Bytecode::PushStr as u8;
pub const PUSH_BOOL: u8 = Bytecode::PushBool as u8;

pub const POP: u8 = Bytecode::Pop as u8;
pub const DUP: u8 = Bytecode::Dup as u8;
pub const SWAP: u8 = Bytecode::Swap as u8;

pub const LOAD: u8 = Bytecode::Load as u8;
pub const STORE: u8 = Bytecode::Store as u8;
pub const LOAD_VAR: u8 = Bytecode::LoadVar as u8;
pub const STORE_VAR: u8 = Bytecode::StoreVar as u8;
pub const LOAD_GLOBAL: u8 = Bytecode::LoadGlobal as u8;
pub const STORE_GLOBAL: u8 = Bytecode::StoreGlobal as u8;
pub const LOAD_LOCAL: u8 = Bytecode::LoadLocal as u8;
pub const STORE_LOCAL: u8 = Bytecode::StoreLocal as u8;

pub const GET_PTR: u8 = Bytecode::GetPtr as u8;
pub const SET_PTR: u8 = Bytecode::SetPtr as u8;
pub const GET_PTR_MUT: u8 = Bytecode::GetPtrMut as u8;
pub const SET_PTR_MUT: u8 = Bytecode::SetPtrMut as u8;
pub const ADDRESS_OF: u8 = Bytecode::AddressOf as u8;
pub const DEREF: u8 = Bytecode::Deref as u8;
pub const DEREF_MUT: u8 = Bytecode::DerefMut as u8;
pub const CAST: u8 = Bytecode::Cast as u8;

#[macro_export] 
macro_rules! binary_op_int {
    ($stack:expr, $op:tt) => {{
        let rhs_option = $stack.pop_vm();
        let lhs_option = $stack.pop_vm();

        let rhs = match rhs_option {
            Some(vm_value) => vm_value,
            None => panic!("RHS is None")
        };

        let lhs = match lhs_option {
            Some(vm_value) => vm_value,
            None => panic!("RHS is None")
        };


        match (lhs.clone(), rhs.clone()) {
            (VMValue::I32(lhs), VMValue::I32(rhs)) => {
                $stack.push_vm(VMValue::I32(lhs $op rhs));
            }
            (VMValue::I64(lhs), VMValue::I64(rhs)) => {
                $stack.push_vm(VMValue::I64(lhs $op rhs));
            }
            (VMValue::U32(lhs), VMValue::U32(rhs)) => {
                $stack.push_vm(VMValue::U32(lhs $op rhs));
            }
            (VMValue::U64(lhs), VMValue::U64(rhs)) => {
                $stack.push_vm(VMValue::U64(lhs $op rhs));
            }
            (VMValue::I8(lhs), VMValue::I8(rhs)) => {
                $stack.push_vm(VMValue::I8(lhs $op rhs));
            }
            (VMValue::I16(lhs), VMValue::I16(rhs)) => {
                $stack.push_vm(VMValue::I16(lhs $op rhs));
            }
            (VMValue::U8(lhs), VMValue::U8(rhs)) => {
                $stack.push_vm(VMValue::U8(lhs $op rhs));
            }
            (VMValue::U16(lhs), VMValue::U16(rhs)) => {
                $stack.push_vm(VMValue::U16(lhs $op rhs));
            }
            (VMValue::Ptr(lhs), VMValue::Ptr(rhs)) => {
                $stack.push_vm(VMValue::Ptr(lhs $op rhs));
            }
            _ => panic!("Invalid types for binary operation: {:?} {:?}", lhs, rhs)
        }
    }};
}

#[macro_export]
macro_rules! binary_op_cmp {
    ($stack:expr, $op:tt) => {{
        let rhs_option = $stack.pop_vm();
        let lhs_option = $stack.pop_vm();

        let rhs = match rhs_option {
            Some(vm_value) => vm_value,
            None => panic!("RHS is None")
        };

        let lhs = match lhs_option {
            Some(vm_value) => vm_value,
            None => panic!("RHS is None")
        };
        match (lhs.clone(), rhs.clone()) {
            (VMValue::Bool(lhs), VMValue::Bool(rhs)) => {
                $stack.push_vm(VMValue::Bool(lhs $op rhs));
            }
            _ => {
                panic!("Invalid types for binary operation: {:?} {:?}", lhs, rhs)
            }
        }
    }};
}

pub fn fetch_u8(code: &Vec<u8>, instruction_counter: &mut usize) -> u8 {
    let byte = code[*instruction_counter];
    *instruction_counter += 1;
    byte
}

pub fn fetch_u16(function: &Vec<u8>, instruction_counter: &mut usize) -> u16 {
    if *instruction_counter + 1 >= function.len() {
        panic!("fetch_u16 out of bounds");
    }

    let low = function[*instruction_counter];
    let high = function[*instruction_counter + 1];
    *instruction_counter += 2;

    u16::from_ne_bytes([low, high])
}

pub fn fetch_u32(code: &Vec<u8>, instruction_counter: &mut usize) -> u32 {
    let bytes = &code[*instruction_counter..*instruction_counter + 3];
    *instruction_counter += 4;
    u32::from_ne_bytes(bytes.try_into().unwrap())
}

pub fn fetch_u64(code: &Vec<u8>, instruction_counter: &mut usize) -> u64 {
    let bytes = &code[*instruction_counter..*instruction_counter + 7];
    *instruction_counter += 8;
    u64::from_ne_bytes(bytes.try_into().unwrap())
}

pub fn fetch_i8(code: &Vec<u8>, instruction_counter: &mut usize) -> i8 {
    fetch_u8(code, instruction_counter) as i8
}

pub fn fetch_i16(code: &Vec<u8>, instruction_counter: &mut usize) -> i16 {
    fetch_u16(code, instruction_counter) as i16
}

pub fn fetch_i32(code: &Vec<u8>, instruction_counter: &mut usize) -> i32 {
    fetch_u32(code, instruction_counter) as i32
}

pub fn fetch_i64(code: &Vec<u8>, instruction_counter: &mut usize) -> i64 {
    fetch_u64(code, instruction_counter) as i64
}

pub fn fetch_f32(code: &Vec<u8>, instruction_counter: &mut usize) -> f32 {
    f32::from_bits(fetch_u32(code, instruction_counter))
}

pub fn fetch_f64(code: &Vec<u8>, instruction_counter: &mut usize) -> f64 {
    f64::from_bits(fetch_u64(code, instruction_counter))
}

pub fn fetch_string_u16_len(function: &Vec<u8>, instruction_counter: &mut usize) -> String {
    let len = fetch_u16(function, instruction_counter);
    if *instruction_counter + len as usize > function.len() {
        panic!("Out of bounds string fetch");
    }

    fetch_string(len, function, instruction_counter)
}

/// Assumes string is encoded as an u16 length followed by that many bytes
pub fn fetch_string(len: u16, function: &Vec<u8>, instruction_counter: &mut usize) -> String {
    if *instruction_counter + (len as usize) > function.len() {
        panic!(
            "fetch_string: trying to read past end of buffer! ip={}, len={}, buffer_len={}",
            *instruction_counter, len, function.len()
        );
    }

    let result = &function[*instruction_counter..*instruction_counter + len as usize];
    *instruction_counter += len as usize;
    String::from_utf8_lossy(result).to_string()
}