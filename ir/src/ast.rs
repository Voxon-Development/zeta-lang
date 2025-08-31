use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Import(ImportStmt),
    Let(LetStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Match(MatchStmt),
    UnsafeBlock(UnsafeBlock),
    FuncDecl(FuncDecl),
    ClassDecl(ClassDecl),
    InterfaceDecl(InterfaceDecl),
    ImplDecl(ImplDecl),
    EnumDecl(EnumDecl),
    ExprStmt(InternalExprStmt),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub path: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub mutability: bool,
    pub ident: String,
    pub type_annotation: Type,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    pub generics: Option<Vec<Generic>>,
    pub interface: String,
    pub target: String,
    pub methods: Option<Vec<FuncDecl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    pub name: String,
    pub visibility: Visibility,
    pub methods: Option<Vec<FuncDecl>>,
    pub generics: Option<Vec<Generic>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: String,
    pub visibility: Visibility,
    pub generics: Option<Vec<Generic>>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: Type,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Block,
    pub else_branch: Option<Box<ElseBranch>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseBranch {
    If(Box<IfStmt>),
    Else(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub let_stmt: Option<LetStmt>,
    pub condition: Option<Box<Expr>>,
    pub increment: Option<Box<Expr>>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add, Sub, Mul, Div, Mod,
    Shl, Shr, BitOr, BitXor, BitAnd,
    Assign,
    AddAssign, SubAssign, MulAssign, DivAssign,
    ShlAssign, ShrAssign, BitOrAssign, BitXorAssign, BitAndAssign,
    Eq, Neq, Lt, Lte, Gt, Gte,
    // etc.
    ModAssign,
}

impl Op {
    pub fn is_math_op(&self) -> bool {
        match self {
            Op::Assign => false,
            _ => true
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual
}


#[derive(Debug, Clone, PartialEq)]
pub struct MatchStmt {
    pub expr: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(String),
    Number(i64),
    String(String),
    Tuple(Vec<Pattern>),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnsafeBlock {
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_unsafe: bool,
    pub is_extern: bool,
    pub name: String,
    pub generics: Option<Vec<Generic>>,
    pub regions: Option<Vec<RegionParam>>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub visibility: Visibility,
    pub name: String,
    pub generics: Option<Vec<Generic>>,
    pub regions: Option<Vec<RegionParam>>,
    pub params: Option<Vec<Param>>,
    pub body: Option<Block>
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegionParam {
    pub name: String
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Internal
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub type_annotation: Type,
    pub visibility: Visibility
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Block {
    pub block: Vec<Stmt>,
}

impl IntoIterator for Block {
    type Item = Stmt;
    type IntoIter = std::vec::IntoIter<Stmt>;

    fn into_iter(self) -> Self::IntoIter {
        self.block.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generic {
    pub const_generic: bool,
    pub type_name: String,
    pub constraints: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalExprStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Array {
        elements: Box<Expr>
    },
    ArrayIndex {
        array: Box<Expr>,
        index: Box<Expr>
    },
    ArrayInit {
        array_type: Type,
        num_of_elements: u32
    },
    Number(i64),
    Decimal(f64),
    String(String),
    Ident(String),
    Boolean(bool),
    Comparison {
        lhs: Box<Expr>,
        op: ComparisonOp,
        rhs: Box<Expr>,
    },
    ClassInit {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String
    },
    Binary {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        field: String
    },
    Assignment {
        lhs: Box<Expr>, 
        op: String, 
        rhs: Box<Expr>
    },
    ExprList {
        exprs: Vec<Expr>,
    },
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
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
    Lambda {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Class(String),
    Char,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Class(_) | Type::Lambda { params: _, return_type: _ } => false,
            _ => true
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U8 => f.write_str("u8"),
            Type::I8 => f.write_str("i8"),
            Type::U16 => f.write_str("u16"),
            Type::I16 => f.write_str("i16"),
            Type::I32 => f.write_str("i32"),
            Type::F32 => f.write_str("f32"),
            Type::F64 => f.write_str("f64"),
            Type::I64 => f.write_str("i64"),
            Type::String => f.write_str("string"),
            Type::Boolean => f.write_str("bool"),
            Type::UF32 => f.write_str("uf32"),
            Type::U32 => f.write_str("u32"),
            Type::U64 => f.write_str("u64"),
            Type::I128 => f.write_str("i128"),
            Type::U128 => f.write_str("u128"),
            Type::UF64 => f.write_str("uf64"),
            Type::Void => f.write_str("void"),
            Type::Lambda { params, return_type } => f.write_str(format!("({:?}) -> {}", params, return_type).as_str()),
            Type::Class(name) => write!(f, "{}", name),
            Type::Char => f.write_str("char"),
        }
    }
}