// =====================================
// SSA IR (Low-level)
// =====================================

use std::collections::HashMap;
use crate::midend::ir::hir::{HirClass, HirEnum, HirInterface};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Value(Value),
    ConstInt(i64),
    ConstBool(bool),
    ConstString(String),
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
    Ptr(Box<SsaType>),
    String,
    Void,
    User(String), // class/interface/enum type
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Binary { dest: Value, op: BinOp, left: Operand, right: Operand },
    Unary { dest: Value, op: UnOp, operand: Operand },
    Phi { dest: Value, incomings: Vec<(BlockId, Value)> },
    Call { dest: Option<Value>, func: String, args: Vec<Operand> },
    Interpolate { dest: Value, parts: Vec<InterpolationOperand> },
    EnumConstruct { dest: Value, enum_name: String, variant: String, args: Vec<Operand> },
    MatchEnum { value: Value, arms: Vec<(String, BlockId)> },
    Jump { target: BlockId },
    Branch { cond: Operand, then_bb: BlockId, else_bb: BlockId },
    Ret { value: Option<Operand> },
    Const { dest: Value, ty: SsaType, value: Operand }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    pub value_types: HashMap<Value, SsaType>,
    pub entry: BlockId,
}

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub funcs: Vec<Function>,
    pub classes: Vec<HirClass>,
    pub interfaces: Vec<HirInterface>,
    pub enums: Vec<HirEnum>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    Bool,
    Str,
    Enum(String),
    Ptr(Box<Type>),
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
}