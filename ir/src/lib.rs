use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VmString {
    pub offset: usize,
    pub length: usize,
    pub hash: u64, // Storing hash here is crucial for collision resolution
}

impl fmt::Display for VmString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "offset: {}, length: {}, hash: {}", self.offset, self.length, self.hash)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
#[repr(u8)]
pub enum Bytecode {
    // Math
    Add = 0x01,
    Sub = 0x02,
    Mul = 0x03,
    Div = 0x04,
    Mod = 0x05,
    // Bitwise
    BitOr = 0x06,
    BitXor = 0x07,
    BitAnd = 0x08,
    Shl = 0x09,
    Shr = 0x0A,
    // Assignment
    Assign = 0x0B,
    AddAssign = 0x0C,
    SubAssign = 0x0D,
    MulAssign = 0x0E,
    DivAssign = 0x0F,
    ModAssign = 0x10,
    ShlAssign = 0x11,
    ShrAssign = 0x12,
    BitOrAssign = 0x13,
    BitXorAssign = 0x14,
    BitAndAssign = 0x15,
    // Comparison
    Eq = 0x16,
    Ne = 0x17,
    Gt = 0x18,
    Ge = 0x19,
    Lt = 0x1A,
    Le = 0x1B,
    // Logic
    And = 0x1C,
    Or = 0x1D,
    Not = 0x1E,
    Xor = 0x1F,
    // Stack/Memory
    PushInt = 0x20,
    PushU8 = 0x21,
    PushU16 = 0x22,
    PushU32 = 0x23,
    PushU64 = 0x24,
    PushI8 = 0x25,
    PushI16 = 0x26,
    PushI32 = 0x27,
    PushI64 = 0x28,
    PushF32 = 0x29,
    PushUF32 = 0x2A,
    PushUF64 = 0x2B,
    PushF64 = 0x2C,
    PushU128 = 0x2D,
    PushI128 = 0x2E,
    PushChar = 0x2F,
    PushPtr = 0x30,
    PushBool = 0x31,
    PushStr = 0x32,
    Pop = 0x33,
    Dup = 0x34,
    Swap = 0x35,
    Load = 0x36,
    Store = 0x37,
    LoadGlobal = 0x38,
    StoreGlobal = 0x39,
    LoadLocal = 0x3A,
    StoreLocal = 0x3B,
    StoreVar = 0x3C,
    LoadVar = 0x3D,
    GetPtr = 0x3E,
    SetPtr = 0x3F,
    GetPtrMut = 0x40,
    SetPtrMut = 0x41,
    AddressOf = 0x42,
    Deref = 0x43,
    DerefMut = 0x44,
    Cast = 0x45,
    
    // Control
    Jump = 0x46,
    JumpIfTrue = 0x47,
    JumpIfFalse = 0x48,
    Branch = 0x49,
    Return = 0x4A,
    Halt = 0x4B,
    Call = 0x4C,
    TailCall = 0x4D,
    CallNative = 0x4E,
    ArrayGet = 0x4F,
    ArraySet = 0x50,
    GetArrayMut = 0x51,
    SetArrayMut = 0x52,
    GetField = 0x53,
    ArrayAlloc = 0x54,
    ArrayLen = 0x55,
    PowAssign = 0x56,
    NewRegion = 0x57,
    StoreRegion = 0x58,
}

impl TryFrom<u8> for Bytecode {
    type Error = ();

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        if byte < 1 || byte > 0x58 {
            Err(())
        } else {
            Ok(unsafe { std::mem::transmute(byte) })
        }
    }
}

impl From<Bytecode> for u8 {
    fn from(byte: Bytecode) -> Self {
        byte as u8
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VMValue {
    Int(usize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    Ptr(usize),
    Str(VmString),
    Array(usize, Box<VMValue>),
    Region(usize),
    Void
}

impl Eq for VMValue {}


#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BytecodeType {
    U8 = 0x60,
    I8 = 0x61,
    U16 = 0x62,
    I16 = 0x63,
    I32 = 0x64,
    F32 = 0x65,
    F64 = 0x66,
    I64 = 0x67,
    String = 0x68, 
    Boolean = 0x69,
    UF32 = 0x6A,
    U32 = 0x6B,
    U64 = 0x6C,
    I128 = 0x6D,
    U128 = 0x6E,
    UF64 = 0x6F,
    Void = 0x70,
    Array = 0x71,
    Class = 0x72,
    Region = 0x73,
}

impl fmt::Display for BytecodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BytecodeType::U8 => write!(f, "u8"),
            BytecodeType::I8 => write!(f, "i8"),
            BytecodeType::U16 => write!(f, "u16"),
            BytecodeType::I16 => write!(f, "i16"),
            BytecodeType::I32 => write!(f, "i32"),
            BytecodeType::F32 => write!(f, "f32"),
            BytecodeType::F64 => write!(f, "f64"),
            BytecodeType::I64 => write!(f, "i64"),
            BytecodeType::String => write!(f, "string"),
            BytecodeType::Boolean => write!(f, "bool"),
            BytecodeType::UF32 => write!(f, "uf32"),
            BytecodeType::U32 => write!(f, "u32"),
            BytecodeType::U64 => write!(f, "u64"),
            BytecodeType::I128 => write!(f, "i128"),
            BytecodeType::UF64 => write!(f, "uf64"),
            BytecodeType::U128 => write!(f, "u128"),
            BytecodeType::Void => write!(f, "void"),
            BytecodeType::Array => write!(f, "array"),
            BytecodeType::Class => write!(f, "class"),
            BytecodeType::Region => write!(f, "region"),
        }
    }
}