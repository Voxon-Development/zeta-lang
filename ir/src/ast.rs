use crate::hir::StrId;
use crate::span::SourceSpan;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Import(ImportStmt),
    Package(PackageStmt),
    Let(LetStmt),
    Const(ConstStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Match(MatchStmt),
    UnsafeBlock(UnsafeBlock),
    Block(Block),
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
    pub path: StrId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PackageStmt {
    pub path: StrId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub mutability: bool,
    pub ident: StrId,
    pub type_annotation: Type,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstStmt {
    pub ident: StrId,
    pub type_annotation: Type,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    pub generics: Option<Vec<Generic>>,
    pub interface: StrId,
    pub target: StrId,
    pub methods: Option<Vec<FuncDecl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    pub name: StrId,
    pub visibility: Visibility,
    pub methods: Option<Vec<FuncDecl>>,
    pub generics: Option<Vec<Generic>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<Vec<Generic>>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: StrId,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: StrId,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BitOr,
    BitXor,
    BitAnd,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ShlAssign,
    ShrAssign,
    BitOrAssign,
    BitXorAssign,
    BitAndAssign,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

impl Into<Op> for &str {
    fn into(self) -> Op {
        match self {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "%" => Op::Mod,
            "<<" => Op::Shl,
            ">>" => Op::Shr,
            "|" => Op::BitOr,
            "^" => Op::BitXor,
            "&" => Op::BitAnd,

            "=" => Op::Assign,
            "+=" => Op::AddAssign,
            "-=" => Op::SubAssign,
            "*=" => Op::MulAssign,
            "/=" => Op::DivAssign,
            "%=" => Op::ModAssign,
            "<<=" => Op::ShlAssign,
            ">>=" => Op::ShrAssign,
            "|=" => Op::BitOrAssign,
            "^=" => Op::BitXorAssign,
            "&=" => Op::BitAndAssign,

            "==" => Op::Eq,
            "!=" => Op::Neq,
            "<" => Op::Lt,
            "<=" => Op::Lte,
            ">" => Op::Gt,
            ">=" => Op::Gte,

            other => panic!("Unknown operator string: {}", other),
        }
    }
}

impl Op {
    pub fn is_math_op(&self) -> bool {
        match self {
            Op::Assign => false,
            _ => true,
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
    GreaterThanOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchStmt {
    pub expr: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(StrId),
    Number(i64),
    String(StrId),
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
    pub is_unsafe: bool,
    pub is_extern: bool,
    pub extern_string: Option<StrId>,
    pub name: StrId,
    pub generics: Option<Vec<Generic>>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub visibility: Visibility,
    pub name: StrId,
    pub generics: Option<Vec<Generic>>,
    pub params: Option<Vec<Param>>,
    pub body: Vec<FuncDecl>,
    pub constants: Vec<ConstStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Module,
    Package,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Param {
    Normal(NormalParam),
    This(ThisParam),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NormalParam {
    pub is_mut: bool,
    pub is_move: bool,
    pub name: StrId,
    pub type_annotation: Type,
    pub visibility: Visibility,
    pub default_value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThisParam {
    pub is_mut: bool,
    pub is_move: bool,
    pub type_annotation: Option<Type>, // e.g. Ptr<this>, this?, etc
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
    pub type_name: StrId,
    pub constraints: Vec<StrId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalExprStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number { value: i64, span: SourceSpan },
    Decimal { value: f64, span: SourceSpan },
    String { value: StrId, span: SourceSpan },
    Ident { name: StrId, span: SourceSpan },
    Boolean { value: bool, span: SourceSpan },
    Comparison {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
        span: SourceSpan,
    },
    ClassInit {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        positional: bool,
        span: SourceSpan,
    },
    FieldAccess {
        object: Box<Expr>,
        field: StrId,
        span: SourceSpan,
    },
    Binary {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
        span: SourceSpan,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        span: SourceSpan,
    },
    Get {
        object: Box<Expr>,
        field: StrId,
        span: SourceSpan,
    },
    If { if_stmt: Box<IfStmt>, span: SourceSpan },
    Match { match_stmt: Box<MatchStmt>, span: SourceSpan },
    Assignment {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
        span: SourceSpan,
    },
    ExprList {
        expressions: Vec<Expr>,
        span: SourceSpan,
    },

    Char { value: char, span: SourceSpan },
    FieldInit { ident: StrId, expr: Box<Expr>, span: SourceSpan },
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
    Infer,
    Lambda {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Class {
        name: StrId,
        generics: Vec<Type>,
    },
    Nullable(Option<Box<Type>>),
    PossibleError(PossibleErrorType),
    This,
    Char,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PossibleErrorType {
    Some(Box<Type>),
    Error(Box<Type>),
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Class {
                name: _,
                generics: _,
            }
            | Type::Lambda {
                params: _,
                return_type: _,
            }
            | Type::Nullable(_)
            | Type::PossibleError(_)
            | Type::This
            | Type::String => false,
            _ => true,
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
            Type::String => f.write_str("StrId"),
            Type::Boolean => f.write_str("bool"),
            Type::UF32 => f.write_str("uf32"),
            Type::U32 => f.write_str("u32"),
            Type::U64 => f.write_str("u64"),
            Type::I128 => f.write_str("i128"),
            Type::U128 => f.write_str("u128"),
            Type::UF64 => f.write_str("uf64"),
            Type::Void => f.write_str("void"),
            Type::Lambda {
                params,
                return_type,
            } => f.write_str(format!("({:?}) -> {}", params, return_type).as_str()),
            Type::Class { name, generics } => {
                write!(f, "{}", name)?;
                if !generics.is_empty() {
                    write!(f, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Char => f.write_str("char"),
            Type::Nullable(ty) => {
                if let Some(ty) = ty {
                    std::fmt::Display::fmt(&ty, f)
                } else {
                    f.write_str("null")
                }
            }
            Type::PossibleError(err_type) => match err_type {
                PossibleErrorType::Some(ty) => std::fmt::Display::fmt(&ty, f),
                PossibleErrorType::Error(err) => std::fmt::Display::fmt(&err, f),
            },
            Type::This => f.write_str("this"),
            Type::Infer => todo!(),
        }
    }
}

