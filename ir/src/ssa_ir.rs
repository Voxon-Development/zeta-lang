use crate::hir::{
    HirEnum, HirFunc, HirInterface, HirParam, HirStruct, HirType, StrId, ThisPassingKind,
};
use crate::ir_conversion::lower_type_hir;
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

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            SsaType::I8
                | SsaType::U8
                | SsaType::I16
                | SsaType::U16
                | SsaType::I32
                | SsaType::U32
                | SsaType::I64
                | SsaType::U64
                | SsaType::I128
                | SsaType::U128
                | SsaType::Isize
                | SsaType::Usize
        )
    }

    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            SsaType::I8
                | SsaType::I16
                | SsaType::I32
                | SsaType::I64
                | SsaType::I128
                | SsaType::Isize
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, SsaType::F32 | SsaType::F64)
    }

    pub fn bit_width(&self) -> Option<u16> {
        Some(match self {
            SsaType::Bool => 1,

            SsaType::I8 | SsaType::U8 => 8,
            SsaType::I16 | SsaType::U16 => 16,
            SsaType::I32 | SsaType::U32 | SsaType::F32 => 32,
            SsaType::I64 | SsaType::U64 | SsaType::F64 => 64,
            SsaType::I128 | SsaType::U128 => 128,

            SsaType::Isize | SsaType::Usize => 64, // target for now

            _ => return None,
        })
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicOp {
    SizeOf,
    AlignOf,
    AssertAlign,
    TypeName,
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
    Isize, // Signed pointer-sized integer
    Usize, // Unsigned pointer-sized integer
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
    Dyn,                        // Trait object (fat pointer)
    Slice(Box<SsaType>),        // Slice (fat pointer)
    Array(Box<SsaType>, usize), // Array
    Char,

    Nullable(Box<SsaType>),
}

use crate::ast::FuncModifiers;
use smallvec::SmallVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastKind {
    // integer
    Truncate,
    SignExtend,
    ZeroExtend,

    // integer <-> float
    SignedIntToFloat,
    UnsignedIntToFloat,
    FloatToSignedInt,
    FloatToUnsignedInt,

    // float
    FloatExtend,
    FloatTruncate,

    // bit reinterpretation
    Bitcast,

    // pointers
    PtrToInt,
    IntToPtr,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Panic {
        message: Operand, // string value: pointer to [len:8][bytes...] blob
    },

    Intrinsic {
        dest: Option<Value>,
        op: IntrinsicOp,
        /// The type being queried, for SizeOf/AlignOf/TypeName. None for AssertAlign.
        query_ty: Option<SsaType>,
        /// Runtime args: [ptr, align] for AssertAlign, empty otherwise.
        args: SmallVec<Operand, 2>,
    },

    /// Binary operation: dest = left OP right
    Binary {
        dest: Value,
        op: BinOp,
        left: Operand,
        right: Operand,
    },

    Cast {
        dest: Value,
        value: Operand,
        kind: CastKind,
    },

    Undef {
        dest: Value,
        ty: SsaType,
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

impl Function {
    /// Builds a Function's signature
    pub fn from_signature(hir_fn: &HirFunc) -> Function {
        let mut params: SmallVec<(Value, SsaType), 8> = SmallVec::new();
        let mut value_types: HashMap<Value, SsaType, FxHashBuilder> =
            HashMap::with_hasher(FxHashBuilder);
        let mut next_value = 0usize;

        if let Some(hir_params) = hir_fn.params {
            for p in hir_params {
                let v = Value(next_value);
                next_value += 1;

                let ty = match p {
                    HirParam::This { kind, .. } => {
                        let inner = match hir_fn.impl_target {
                            Some(class_name) => SsaType::User(class_name, vec![]),
                            None => unreachable!(),
                        };
                        match kind {
                            ThisPassingKind::Move | ThisPassingKind::MoveMut => inner,
                            _ => SsaType::Pointer(Box::new(inner)),
                        }
                    }
                    HirParam::Normal { param_type, .. } => lower_type_hir(param_type),
                };

                value_types.insert(v, ty.clone());
                params.push((v, ty));
            }
        }

        let ret_type = lower_type_hir(hir_fn.return_type.as_ref().unwrap_or(&HirType::Void));

        let function = Function {
            name: hir_fn.name,
            params,
            ret_type,
            blocks: SmallVec::new(),
            value_types,
            entry: BlockId(0),
            function_metadata: hir_fn.function_metadata,
        };

        function
    }
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
    pub class_interface_vtables: HashMap<(StrId, StrId), VTableInfo, FxHashBuilder>,
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
            | Instruction::Panic { .. }
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

pub fn cast_kind(src: &SsaType, dst: &SsaType) -> CastKind {
    use CastKind::*;

    if src == dst {
        return Bitcast;
    }

    if src.is_integer() && dst.is_integer() {
        let sb = src.bit_width().unwrap();
        let db = dst.bit_width().unwrap();

        return if db > sb {
            if src.is_signed_integer() {
                SignExtend
            } else {
                ZeroExtend
            }
        } else {
            Truncate
        };
    }

    if src.is_integer() && dst.is_float() {
        return if src.is_signed_integer() {
            SignedIntToFloat
        } else {
            UnsignedIntToFloat
        };
    }

    if src.is_float() && dst.is_integer() {
        return if dst.is_signed_integer() {
            FloatToSignedInt
        } else {
            FloatToUnsignedInt
        };
    }

    if src.is_float() && dst.is_float() {
        let sb = src.bit_width().unwrap();
        let db = dst.bit_width().unwrap();

        return if db > sb { FloatExtend } else { FloatTruncate };
    }

    if src.is_pointer() && dst.is_pointer() {
        return Bitcast;
    }

    if src.is_pointer() && dst.is_integer() {
        return PtrToInt;
    }

    if src.is_integer() && dst.is_pointer() {
        return IntToPtr;
    }

    panic!("unsupported cast {:?} -> {:?}", src, dst);
}
