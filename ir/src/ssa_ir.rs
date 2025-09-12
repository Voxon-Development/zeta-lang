// =====================================
// SSA IR (Low-level)
// =====================================

use std::collections::HashMap;
use crate::hir::{HirClass, HirEnum, HirInterface, StrId};
use crate::ir_hasher::FxHashBuilder;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operand {
    Value(Value),
    ConstInt(i64),
    ConstBool(bool),
    ConstString(StrId),
    FunctionRef(StrId),
    GlobalRef(StrId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SsaType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    F32,
    F64,
    ISize,
    USize,

    Bool,
    String,
    Void,
    User(StrId, Vec<SsaType>), // class/interface/enum type
    Enum(Vec<SsaType>),
    Tuple(Vec<SsaType>),
    Dyn,
    Slice,
}

use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Binary operation: dest = left OP right
    Binary {
        dest: Value,
        op: BinOp,
        left: Operand,
        right: Operand,
    },

    /// Unary operation: dest = OP operand
    Unary {
        dest: Value,
        op: UnOp,
        operand: Operand,
    },

    /// Phi node for SSA
    Phi {
        dest: Value,
        incomings: SmallVec<[(BlockId, Value); 4]>, // usually <=4 predecessors
    },

    /// Function call
    Call {
        dest: Option<Value>,
        func: Operand,
        args: SmallVec<[Operand; 8]>, // most calls <8 args
    },

    /// Direct class method call (resolved at compile time)
    ClassCall {
        dest: Option<Value>,
        object: Value,          // `this` pointer
        method_id: usize,       // method index
        args: SmallVec<[Operand; 8]>,
    },

    /// Virtual/interface call through vtable
    InterfaceDispatch {
        dest: Option<Value>,
        object: Value,          // interface reference
        method_slot: usize,     // vtable slot
        args: SmallVec<[Operand; 8]>,
    },

    /// Cast class -> interface
    UpcastToInterface {
        dest: Value,
        object: Value,
        interface_id: usize,
    },

    /// Stack allocation (SSA-local)
    Alloc {
        dest: Value,
        ty: SsaType,
        count: usize, // number of elements if array
    },

    StoreField { base: Operand, offset: usize, value: Operand },
    LoadField { dest: Value, base: Operand, offset: usize },

    /// Interpolation (string concatenation)
    Interpolate {
        dest: Value,
        parts: SmallVec<[InterpolationOperand; 4]>, // usually <=4 parts
    },

    EnumConstruct {
        dest: Value,
        enum_name: StrId,
        variant: StrId,
        args: SmallVec<[Operand; 8]>, // usually <=8 fields
    },

    MatchEnum {
        value: Value,
        arms: SmallVec<[(StrId, BlockId); 8]>, // usually <=8 arms
    },

    Jump { target: BlockId },

    Branch {
        cond: Operand,
        then_bb: BlockId,
        else_bb: BlockId,
    },

    Ret { value: Option<Operand> },
    Const { dest: Value, ty: SsaType, value: Operand },
}


#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationOperand {
    Literal(StrId),
    Value(Value),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: StrId,
    pub params: SmallVec<[(Value, SsaType); 8]>,
    pub ret_type: SsaType,
    pub blocks: SmallVec<[BasicBlock; 12]>,
    pub value_types: HashMap<Value, SsaType, FxHashBuilder>,
    pub entry: BlockId,
}

#[derive(Debug, Clone, Default)]
pub struct MethodInfo {
    pub name: StrId,
    pub slot: usize, // index in vtable
    pub is_virtual: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ClassLayout {
    pub vtable: SmallVec<[MethodInfo; 12]>,
}

#[derive(Debug, Clone, Default)]
pub struct InterfaceLayout {
    pub methods: SmallVec<[StrId; 12]>, // names in declaration order
}

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub funcs: HashMap<StrId, Function, FxHashBuilder>,
    pub classes: HashMap<StrId, HirClass, FxHashBuilder>,
    pub interfaces: HashMap<StrId, HirInterface, FxHashBuilder>,
    pub enums: HashMap<StrId, HirEnum, FxHashBuilder>,
    pub types: HashMap<StrId, SsaType, FxHashBuilder>,

    pub class_layouts: HashMap<StrId, ClassLayout, FxHashBuilder>,
    pub interface_layouts: HashMap<StrId, InterfaceLayout, FxHashBuilder>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    Bool,
    Str,
    Enum(StrId),
    // possibly more like Float, Struct, etc.
}


impl Module {
    pub fn new() -> Module {
        Module::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Eq, Ne, Lt, Le, Gt, Ge,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}