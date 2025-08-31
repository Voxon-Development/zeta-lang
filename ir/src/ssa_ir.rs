// =====================================
// SSA IR (Low-level)
// =====================================

use std::collections::HashMap;
use crate::hir::{HirClass, HirEnum, HirInterface};
use crate::sea_hasher::SeaHashBuilder;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Value(Value),
    ConstInt(i64),
    ConstBool(bool),
    ConstString(String),
    FunctionRef(String),
    GlobalRef(String),
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

    Bool,
    String,
    Void,
    User(String), // class/interface/enum type
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Binary { dest: Value, op: BinOp, left: Operand, right: Operand },
    Unary { dest: Value, op: UnOp, operand: Operand },
    Phi { dest: Value, incomings: Vec<(BlockId, Value)> },
    Call { dest: Option<Value>, func: Operand, args: Vec<Operand> },
    Interpolate { dest: Value, parts: Vec<InterpolationOperand> },
    EnumConstruct { dest: Value, enum_name: String, variant: String, args: Vec<Operand> },
    MatchEnum { value: Value, arms: Vec<(String, BlockId)> },
    Jump { target: BlockId },
    Branch { cond: Operand, then_bb: BlockId, else_bb: BlockId },
    Ret { value: Option<Operand> },
    Const { dest: Value, ty: SsaType, value: Operand },
    /// Direct method call on a concrete class (resolved at compile time)
    ClassCall {
        dest: Option<Value>,
        object: Value,          // `this` pointer/value
        method_id: usize,       // method index in the class vtable or direct offset
        args: Vec<Operand>,     // already-lowered arguments
    },

    /// Virtual call through interface vtable
    InterfaceDispatch {
        dest: Option<Value>,
        object: Value,          // interface-typed object reference
        method_slot: usize,     // index in interface method table
        args: Vec<Operand>,
    },

    /// Convert a concrete class reference to an interface reference
    UpcastToInterface {
        dest: Value,
        object: Value,          // class-typed object
        interface_id: usize,    // which interface
    },

    Alloc { dest: Value, ty: SsaType },
    StoreField { base: Operand, offset: usize, value: Operand },
    LoadField { dest: Value, base: Operand, offset: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationOperand {
    Literal(String),
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
    pub name: String,
    pub params: Vec<(Value, SsaType)>,
    pub ret_type: SsaType,
    pub blocks: Vec<BasicBlock>,
    pub value_types: HashMap<Value, SsaType, SeaHashBuilder>,
    pub entry: BlockId,
}

#[derive(Debug, Clone, Default)]
pub struct MethodInfo {
    pub name: String,
    pub slot: usize, // index in vtable
    pub is_virtual: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ClassLayout {
    pub vtable: Vec<MethodInfo>,
}

#[derive(Debug, Clone, Default)]
pub struct InterfaceLayout {
    pub methods: Vec<String>, // names in declaration order
}

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub funcs: HashMap<String, Function, SeaHashBuilder>,
    pub classes: HashMap<String, HirClass, SeaHashBuilder>,
    pub interfaces: HashMap<String, HirInterface, SeaHashBuilder>,
    pub enums: HashMap<String, HirEnum, SeaHashBuilder>,
    pub types: HashMap<String, SsaType, SeaHashBuilder>,

    pub class_layouts: HashMap<String, ClassLayout, SeaHashBuilder>,
    pub interface_layouts: HashMap<String, InterfaceLayout, SeaHashBuilder>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    Bool,
    Str,
    Enum(String),
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