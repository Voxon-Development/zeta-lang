use std::fmt::Display;
// =====================================
// HIR (High-Level IR)
// =====================================

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
    pub name: String,
    pub imports: Vec<String>,
    pub items: Vec<Hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFunc {
    pub name: String,
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_unsafe: bool,
    pub generics: Vec<HirGeneric>,
    pub params: Vec<HirParam>,
    pub return_type: Option<HirType>,
    pub body: Option<HirStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirClass {
    pub name: String,
    pub visibility: Visibility,
    pub generics: Vec<HirGeneric>,
    pub fields: Vec<HirField>,
    pub methods: Vec<HirFunc>,
    pub interfaces: Vec<String>, // implemented interfaces
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirImpl {
    pub generics: Vec<HirGeneric>,
    pub interface: String,
    pub target: String,
    pub methods: Vec<HirFunc>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirInterface {
    pub name: String,
    pub visibility: Visibility,
    pub methods: Vec<HirFunc>,
    pub generics: Vec<HirGeneric>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnum {
    pub name: String,
    pub visibility: Visibility,
    pub generics: Vec<HirGeneric>,
    pub variants: Vec<HirEnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnumVariant {
    pub name: String,
    pub fields: Vec<HirField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirField {
    pub name: String,
    pub field_type: HirType,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub name: String,
    pub ty: HirType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirGeneric {
    pub name: String,
    pub constraints: Vec<String>,
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
    Class(String, Vec<HirType>),
    Interface(String, Vec<HirType>),
    Enum(String, Vec<HirType>),
    Lambda {
        params: Vec<HirType>,
        return_type: Box<HirType>,
        concurrent: bool,
    },
    Array(Box<HirType>),
    Generic(String),
    Void,
}

impl Display for HirType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmt {
    Let { name: String, ty: HirType, value: HirExpr, mutable: bool },
    Return(Option<HirExpr>),
    Expr(HirExpr),
    If { cond: HirExpr, then_block: Box<HirStmt>, else_block: Option<Box<HirStmt>> },
    While { cond: HirExpr, body: Box<HirStmt> },
    For { init: Option<Box<HirStmt>>, condition: Option<HirExpr>, increment: Option<HirExpr>, body: Box<HirStmt>, },
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
    pub body: Box<HirStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirPattern {
    Ident(String),
    Number(i64),
    String(String),
    Tuple(Vec<HirPattern>),
    EnumVariant { enum_name: String, variant: String, bindings: Vec<String> },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExpr {
    Number(i64),
    String(String),
    Boolean(bool),
    Ident(String),
    Tuple(Vec<HirExpr>),
    Decimal(f64),
    Binary { left: Box<HirExpr>, op: Operator, right: Box<HirExpr> },
    Call { callee: Box<HirExpr>, args: Vec<HirExpr> },
    InterfaceCall {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
        interface: String
    },
    FieldAccess { object: Box<HirExpr>, field: String },
    Assignment { target: Box<HirExpr>, op: AssignmentOperator, value: Box<HirExpr> },
    InterpolatedString(Vec<InterpolationPart>),
    EnumInit { enum_name: String, variant: String, args: Vec<HirExpr> },
    ExprList(Vec<HirExpr>),
    Get { object: Box<HirExpr>, field: String },
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
    Literal(String),
    Expr(Box<HirExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Internal,
}