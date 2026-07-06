use crate::hir::{HirEnum, HirInterface, HirStruct, StrId};
use crate::ir_hasher::FxHashBuilder;
use std::collections::HashMap;

impl SsaType {
    pub fn ptr_to(inner: SsaType) -> Self {
        SsaType::Pointer(Box::new(inner))
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, SsaType::Pointer(_))
    }

    pub fn as_pointer(&self) -> Option<&SsaType> {
        if let SsaType::Pointer(inner) = self {
            Some(inner)
        } else {
            None
        }
    }

    /// True if this nullable can be represented with zero-cost null-pointer
    /// encoding (no tag byte needed), only applies when the inner type is
    /// itself a pointer.
    pub fn nullable_is_pointer_optimized(&self) -> bool {
        matches!(self, SsaType::Nullable(inner) if inner.is_pointer())
    }

    /// If this is `Nullable(Pointer(_))`, returns the underlying pointer
    /// type directly, the nullable collapses to the pointer with 0 = null.
    pub fn nullable_pointer_repr(&self) -> Option<&SsaType> {
        match self {
            SsaType::Nullable(inner) if inner.is_pointer() => Some(inner),
            _ => None,
        }
    }

    /// True if this is a tagged-union nullable (inner type is NOT a pointer,
    /// so it needs an explicit discriminant byte).
    pub fn is_tagged_nullable(&self) -> bool {
        matches!(self, SsaType::Nullable(inner) if !inner.is_pointer())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operand {
    Value(Value),
    ConstInt(i64),
    ConstFloat(f64),
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
    U128,
    F32,
    F64,
    ISize, // Signed pointer-sized integer
    USize, // Unsigned pointer-sized integer
    Null,

    // Special types
    Bool,
    String, // Fat pointer to string data
    Void,   // Unit type, zero size

    // Composite types
    User(StrId, Vec<SsaType>), // User-defined type with fields
    Interface(StrId),
    Enum(Vec<SsaType>),  // Tagged union of types
    Tuple(Vec<SsaType>), // Fixed-size collection of heterogeneous types

    // Pointer types
    Pointer(Box<SsaType>), // Pointer to another type

    // Dynamically sized types
    Dyn,   // Trait object (fat pointer)
    Slice, // Slice (fat pointer)
    Char,

    Nullable(Box<SsaType>),
}

use crate::ast::FuncModifiers;
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
        incoming: SmallVec<(BlockId, Value), 4>, // usually <=4 predecessors
    },

    /// Function call
    Call {
        dest: Option<Value>,
        func: Operand,
        args: SmallVec<Operand, 8>, // most calls <8 args
    },

    /// Virtual/interface call through vtable
    InterfaceDispatch {
        dest: Option<Value>,
        object: Value,      // interface reference
        method_slot: usize, // vtable slot
        args: SmallVec<Operand, 8>,
    },

    /// Cast class -> interface
    UpcastToInterface {
        dest: Value,
        object: Value,
        interface_id: usize,
    },

    /// Stack allocation (SSA-local)
    StackAlloc {
        dest: Value,
        ty: SsaType,
        count: usize, // number of elements if array
    },

    StoreField {
        base: Operand,
        offset: usize,
        value: Operand,
    },
    LoadField {
        dest: Value,
        base: Operand,
        offset: usize,
    },

    /// Interpolation (string concatenation)
    Interpolate {
        dest: Value,
        parts: SmallVec<InterpolationOperand, 4>, // usually <=4 parts
    },

    EnumConstruct {
        dest: Value,
        enum_name: StrId,
        variant: StrId,
        args: SmallVec<Operand, 8>, // usually <=8 fields
    },

    MatchEnum {
        value: Value,
        arms: SmallVec<(StrId, BlockId), 8>, // usually <=8 arms
    },

    Jump {
        target: BlockId,
    },

    Branch {
        cond: Operand,
        then_bb: BlockId,
        else_bb: BlockId,
    },

    Ret {
        value: Option<Operand>,
    },
    Const {
        dest: Value,
        ty: SsaType,
        value: Operand,
    },
    AddressOf {
        dest: Value,
        source: Value,
    },
    Load {
        dest: Value,
        ptr: Operand,
    },
    Store {
        ptr: Operand,
        value: Operand,
    },
    FieldAddr {
        dest: Value,
        base: Operand,
        offset: usize,
    },
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
    pub params: SmallVec<(Value, SsaType), 8>,
    pub ret_type: SsaType,
    pub blocks: SmallVec<BasicBlock, 3>,
    pub value_types: HashMap<Value, SsaType, FxHashBuilder>,
    pub entry: BlockId,
    pub function_metadata: FuncModifiers,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: StrId,
    pub slot: usize, // index in vtable
    pub is_virtual: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ClassLayout {
    pub vtable: SmallVec<MethodInfo, 12>,
}

#[derive(Debug, Clone, Default)]
pub struct InterfaceLayout {
    pub methods: SmallVec<StrId, 12>, // names in declaration order
}

#[derive(Debug, Clone, Default)]
pub struct Module<'a, 'bump>
where
    'bump: 'a,
{
    pub functions: HashMap<StrId, Function, FxHashBuilder>,
    pub classes: HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
    pub interfaces: HashMap<StrId, HirInterface<'a, 'bump>, FxHashBuilder>,
    pub enums: HashMap<StrId, HirEnum<'a, 'bump>, FxHashBuilder>,
    pub types: HashMap<StrId, SsaType, FxHashBuilder>,

    pub class_layouts: HashMap<StrId, ClassLayout, FxHashBuilder>,
    pub interface_layouts: HashMap<StrId, InterfaceLayout, FxHashBuilder>,
    pub class_interface_vtables: HashMap<(StrId, StrId), VTableInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    Bool,
    Str,
    Enum(StrId),
}

impl<'a, 'bump> Module<'a, 'bump> {
    pub fn new() -> Module<'a, 'bump> {
        Module::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}

pub fn inst_is_terminator(inst: &Instruction) -> bool {
    matches!(
        inst,
        Instruction::Jump { .. }
            | Instruction::Branch { .. }
            | Instruction::Ret { .. }
            | Instruction::MatchEnum { .. } // lower this to br_table (terminator)
    )
}

#[derive(Debug, Clone, Default)]
pub struct VTable {
    pub class: StrId,
    pub interface: StrId,
    pub methods: Vec<StrId>,
}

#[derive(Debug, Clone, Default)]
pub struct VTableInfo {
    pub interface: StrId,
    pub methods: Vec<StrId>,
}
