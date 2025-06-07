use std::fmt;

pub enum Bytecode {
    // Arithmetic & Bitwise
    Add, Sub, Mul, Div, Mod,
    BitOr, BitXor, BitAnd, Shl, Shr,

    // Assignment
    Assign,
    AddAssign, SubAssign, MulAssign, DivAssign, ModAssign,
    ShlAssign, ShrAssign, BitOrAssign, BitXorAssign, BitAndAssign,

    // Comparison
    Eq, Ne, Gt, Ge, Lt, Le,

    // Logic
    And, Or, Not, Xor,

    // Stack/Memory
    // Numbers
    PushInt(usize),
    PushU8(u8),
    PushU16(u16),
    PushU32(u32),
    PushU64(u64),
    PushI8(i8),
    PushI16(i16),
    PushI32(i32),
    PushI64(i64),
    PushF32(f32),
    PushUF32(f32),
    PushUF64(f64),
    PushF64(f64),
    PushU128(u128),
    PushI128(i128),
    
    PushChar(char),
    PushPtr(usize),
    
    // Booleans
    PushBool(bool),
    
    // Strings
    PushStr(*const u8),

    // Stack
    Pop, Dup, Swap,
    Load, Store,
    LoadGlobal, StoreGlobal,
    LoadLocal, StoreLocal,
    StoreVar { name: String },
    LoadVar { name: String },

    // Control Flow
    Jump(usize),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Branch(usize), // generic conditional jump
    Return,
    Halt,

    // Call
    Call,
    TailCall(usize),
    CallNative(String),

    // Heap / Object / Arrays
    AllocArray {
        array_type: BytecodeType,
        num_of_elements: u32
    },
    ArrayGet,
    ArraySet,
    AllocStruct,
    StructGet(String),
    StructSet(String),

    // Debug
    Nop,
    Trace,
}

impl Bytecode {
    pub fn is_jump(&self) -> bool {
        matches!(
            self,
            Bytecode::Jump(_)
                | Bytecode::JumpIfTrue(_)
                | Bytecode::JumpIfFalse(_)
                | Bytecode::Branch(_)
        )
    }

    pub fn is_push(&self) -> bool {
        matches!(self, Bytecode::PushInt(_) | Bytecode::PushBool(_) | Bytecode::PushStr(_))
    }

    pub fn mnemonic(&self) -> String {
        match self {
            Bytecode::Add => "add".to_string(),
            Bytecode::Sub => "sub".to_string(),
            Bytecode::Mul => "mul".to_string(),
            Bytecode::Div => "div".to_string(),
            Bytecode::PushInt(_) => "push_int".to_string(),
            Bytecode::Call => "call".to_string(),
            Bytecode::Mod => "mod".to_string(),
            Bytecode::BitOr => "bit_or".to_string(),
            Bytecode::BitXor => "bit_xor".to_string(),
            Bytecode::BitAnd => "bit_and".to_string(),
            Bytecode::Shl => "shl".to_string(),
            Bytecode::Shr => "shr".to_string(),
            Bytecode::Assign => "assign".to_string(),
            Bytecode::AddAssign => "add_assign".to_string(),
            Bytecode::SubAssign => "sub_assign".to_string(),
            Bytecode::MulAssign => "mul_assign".to_string(),
            Bytecode::DivAssign => "div_assign".to_string(),
            Bytecode::ModAssign => "mod_assign".to_string(),
            Bytecode::ShlAssign => "shl_assign".to_string(),
            Bytecode::ShrAssign => "shr_assign".to_string(),
            Bytecode::BitOrAssign => "bit_or_assign".to_string(),
            Bytecode::BitXorAssign => "bit_xor_assign".to_string(),
            Bytecode::BitAndAssign => "bit_and_assign".to_string(),
            Bytecode::Eq => "eq".to_string(),
            Bytecode::Ne => "ne".to_string(),
            Bytecode::Gt => "gt".to_string(),
            Bytecode::Ge => "ge".to_string(),
            Bytecode::Lt => "lt".to_string(),
            Bytecode::Le => "le".to_string(),
            Bytecode::And => "and".to_string(),
            Bytecode::Or => "or".to_string(),
            Bytecode::Not => "not".to_string(),
            Bytecode::Xor => "xor".to_string(),
            Bytecode::Pop => "pop".to_string(),
            Bytecode::Dup => "dup".to_string(),
            Bytecode::Swap => "swap".to_string(),
            Bytecode::Load => "load".to_string(),
            Bytecode::Store => "store".to_string(),
            Bytecode::LoadGlobal => "load_global".to_string(),
            Bytecode::StoreGlobal => "store_global".to_string(),
            Bytecode::LoadLocal => "load_local".to_string(),
            Bytecode::StoreLocal => "store_local".to_string(),
            Bytecode::Jump(jmp) => format!("jump {}", jmp),
            Bytecode::JumpIfTrue(jmp) => format!("jump_if_true {}", jmp),
            Bytecode::JumpIfFalse(jmp) => format!("jump_if_false {}", jmp),
            Bytecode::Branch(jmp) => format!("branch {}", jmp),
            Bytecode::Return => "return".to_string(),
            Bytecode::Halt => "halt".to_string(),
            Bytecode::TailCall(call) => format!("tail_call {}", call),
            Bytecode::CallNative(name) => format!("call_native {}", name),
            Bytecode::AllocArray { array_type: _, num_of_elements: _ } => "alloc_array".to_string(),
            Bytecode::ArrayGet => "array_get".to_string(),
            Bytecode::ArraySet => "array_set".to_string(),
            Bytecode::AllocStruct => "alloc_struct".to_string(),
            Bytecode::StructGet(name) => format!("struct_get {}", name),
            Bytecode::StructSet(name) => format!("struct_set {}", name),
            Bytecode::Nop => "nop".to_string(),
            Bytecode::Trace => "trace".to_string(),
            Bytecode::PushBool(b) => format!("push_bool {}", b),
            Bytecode::PushStr(string) => format!("push_str {:?}", string),
            Bytecode::PushU8(num) => format!("push_u8 {}", num),
            Bytecode::PushU16(num) => format!("push_u16 {}", num),
            Bytecode::PushU32(num) => format!("push_u32 {}", num),
            Bytecode::PushU64(num) => format!("push_u64 {}", num),
            Bytecode::PushU128(num) => format!("push_u128 {}", num),
            Bytecode::PushI8(num) => format!("push_i8 {}", num),
            Bytecode::PushI16(num) => format!("push_i16 {}", num),
            Bytecode::PushI32(num) => format!("push_i32 {}", num),
            Bytecode::PushI64(num) => format!("push_i64 {}", num),
            Bytecode::PushI128(num) => format!("push_i128 {}", num),
            Bytecode::PushF32(num) => format!("push_f32 {}", num),
            Bytecode::PushF64(num) => format!("push_f64 {}", num),
            Bytecode::PushChar(c) => format!("push_char {}", c),
            Bytecode::PushPtr(ptr) => format!("push_ptr {}", ptr),
            Bytecode::LoadVar { .. } => "load_var".to_string(),
            Bytecode::StoreVar { .. } => "store_var".to_string(),
            Bytecode::PushUF32(num) => format!("push_uf32 {}", num),
            Bytecode::PushUF64(num) => format!("push_uf64 {}", num),
        }
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.mnemonic())
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum BytecodeType {
    U8,
    I8,
    U16,
    I16,
    I32,
    F32,
    F64,
    I64,
    String,
    Boolean,
    UF32,
    U32,
    U64,
    I128,
    U128,
    UF64,
    Void,
    Array(Box<BytecodeType>, Option<u64>),
    Class(String),
}