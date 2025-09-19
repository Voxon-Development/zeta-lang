use std::fmt::{Display, Formatter};
use std::ops::Deref;
use zetaruntime::string_pool::VmString;

// =====================================
// HIR (High-Level IR)
// =====================================

/// A reference to an interned string in the global string pool
#[derive(Debug, Clone, Copy, PartialEq, Default, Eq, Hash)]
#[repr(align(32))]
pub struct StrId(pub VmString);

impl Deref for StrId {
    type Target = VmString;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl StrId {
    /// Create a new string ID from a VmString
    pub fn new(vm_string: VmString) -> Self {
        StrId(vm_string)
    }

    /// Get the underlying VmString
    pub fn into_inner(self) -> VmString {
        self.0
    }
    
    /// Get the hash of the string
    pub fn hash(&self) -> u64 {
        // Use the VmString's hash method if available, or a fallback
        // This is a simplified version - in practice, you'd want to expose the hash from VmString
        // or use a different approach to get a unique identifier
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        std::hash::Hash::hash(&self.0, &mut hasher);
        std::hash::Hasher::finish(&hasher)
    }
    
    /// Get the length of the string in bytes
    pub fn len(&self) -> usize {
        self.0.length
    }
    
    /// Check if the string is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl From<VmString> for StrId {
    fn from(vm_string: VmString) -> Self {
        StrId(vm_string)
    }
}

impl Display for StrId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // This will be resolved properly when we have access to the context
        write!(f, "StrId({:?})", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Hir {
    Module(HirModule),
    Func(HirFunc),
    Class(HirClass),
    Interface(HirInterface),
    Impl(HirImpl),
    Enum(HirEnum),
    Stmt(Box<HirStmt>),
    Expr(HirExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub name: StrId,
    pub imports: Vec<StrId>,
    pub items: Vec<Hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFunc {
    pub name: StrId,
    pub visibility: Visibility,
    pub is_unsafe: bool,
    pub generics: Vec<HirGeneric>,
    pub params: Vec<HirParam>,
    pub return_type: Option<HirType>,
    pub body: Option<HirStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirClass {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Vec<HirGeneric>,
    pub fields: Vec<HirField>,
    pub interfaces: Vec<StrId>, // implemented interfaces
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirImpl {
    pub generics: Vec<HirGeneric>,
    pub interface: StrId,
    pub target: StrId,
    pub methods: Vec<HirFunc>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirInterface {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Vec<HirGeneric>,
    pub methods: Vec<HirFunc>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnum {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Vec<HirGeneric>,
    pub variants: Vec<HirEnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnumVariant {
    pub name: StrId,
    pub fields: Vec<HirField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirField {
    pub name: StrId,
    pub field_type: HirType,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub name: StrId,
    pub param_type: HirType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirGeneric {
    pub name: StrId,
    pub constraints: Vec<StrId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    I128,
    U128,
    Boolean,
    String,
    Class(StrId, Vec<HirType>),
    Interface(StrId, Vec<HirType>),
    Enum(StrId, Vec<HirType>),
    Lambda {
        params: Vec<HirType>,
        return_type: Box<HirType>,
        concurrent: bool,
    },
    Generic(StrId),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmt {
    Let { name: StrId, ty: HirType, value: HirExpr, mutable: bool },
    Return(Option<HirExpr>),
    Expr(HirExpr),
    If { cond: HirExpr, then_block: Box<HirStmt>, else_block: Option<Box<HirStmt>> },
    While { cond: HirExpr, body: Box<HirStmt> },
    For { init: Option<Box<HirStmt>>, condition: Option<HirExpr>, increment: Option<HirExpr>, body: Box<HirStmt> },
    Match { expr: HirExpr, arms: Vec<HirMatchArm> },
    UnsafeBlock { body: Box<HirStmt> },
    Block { body: Vec<HirStmt> },
    Break,
    Continue,
}

impl FromIterator<HirStmt> for HirStmt {
    fn from_iter<T: IntoIterator<Item = HirStmt>>(iter: T) -> Self {
        HirStmt::Block { body: iter.into_iter().collect() }
    }
}

impl FromIterator<Box<HirStmt>> for HirStmt {
    fn from_iter<T: IntoIterator<Item = Box<HirStmt>>>(iter: T) -> Self {
        HirStmt::Block { body: iter.into_iter().map(|s| *s).collect() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirMatchArm {
    pub pattern: HirPattern,
    pub guard: Option<HirExpr>,
    pub body: HirStmt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirPattern {
    Ident(StrId),
    Number(i64),
    String(StrId),
    Tuple(Vec<HirPattern>),
    EnumVariant { enum_name: StrId, variant: StrId, bindings: Vec<StrId> },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExpr {
    Number(i64),
    String(StrId),
    Boolean(bool),
    Ident(StrId),
    Tuple(Vec<HirExpr>),
    Decimal(f64),
    Binary { left: Box<HirExpr>, op: Operator, right: Box<HirExpr> },
    Call { callee: Box<HirExpr>, args: Vec<HirExpr> },
    InterfaceCall {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
        interface: StrId
    },
    FieldAccess { object: Box<HirExpr>, field: StrId },
    Assignment { target: Box<HirExpr>, op: AssignmentOperator, value: Box<HirExpr> },
    InterpolatedString(Vec<InterpolationPart>),
    EnumInit { enum_name: StrId, variant: StrId, args: Vec<HirExpr> },
    ExprList(Vec<HirExpr>),
    Get { object: Box<HirExpr>, field: StrId },
    ClassInit { name: Box<HirExpr>, args: Vec<HirExpr> },
    Comparison { left: Box<HirExpr>, op: Operator, right: Box<HirExpr> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationPart {
    String(StrId),
    Expr(Box<HirExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Module,
    Package
}

impl Display for HirType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HirType::I8 => write!(f, "i8"),
            HirType::I16 => write!(f, "i16"),
            HirType::I32 => write!(f, "i32"),
            HirType::I64 => write!(f, "i64"),
            HirType::U8 => write!(f, "u8"),
            HirType::U16 => write!(f, "u16"),
            HirType::U32 => write!(f, "u32"),
            HirType::U64 => write!(f, "u64"),
            HirType::I128 => write!(f, "i128"),
            HirType::U128 => write!(f, "u128"),
            HirType::F32 => write!(f, "f32"),
            HirType::F64 => write!(f, "f64"),
            HirType::Boolean => write!(f, "bool"),
            HirType::String => write!(f, "string"),
            HirType::Class(name, args) => Self::write_args(f, name, args),
            HirType::Interface(name, args) => Self::write_args(f, name, args),
            HirType::Enum(name, args) => Self::write_args(f, name, args),
            HirType::Lambda { params, return_type, concurrent } => {
                let params_str: Vec<_> = params.iter().map(|p| p.to_string()).collect();
                if *concurrent {
                    write!(f, "concurrent fn({}) -> {}", params_str.join(", "), return_type)
                } else {
                    write!(f, "fn({}) -> {}", params_str.join(", "), return_type)
                }
            }
            HirType::Generic(name) => write!(f, "{}", name),
            HirType::Void => write!(f, "void"),
        }
    }
}

impl HirType {
    fn write_args(f: &mut Formatter, name: &StrId, args: &Vec<HirType>) -> std::fmt::Result {
        if args.is_empty() {
            write!(f, "{}", name)
        } else {
            let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
            write!(f, "{}<{}>", name, args_str.join(", "))
        }
    }
}