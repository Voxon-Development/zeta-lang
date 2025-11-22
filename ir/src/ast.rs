use crate::hir::StrId;
use crate::span::SourceSpan;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt<'a, 'bump> where 'bump: 'a {
    Import(&'bump ImportStmt),
    Package(&'bump PackageStmt),
    Let(&'bump LetStmt<'a, 'bump>),
    Const(&'bump ConstStmt<'a, 'bump>),
    Return(&'bump ReturnStmt<'a, 'bump>),
    If(&'bump IfStmt<'a, 'bump>),
    While(&'bump WhileStmt<'a, 'bump>),
    For(&'bump ForStmt<'a, 'bump>),
    Match(&'bump MatchStmt<'a, 'bump>),
    UnsafeBlock(&'bump UnsafeBlock<'a, 'bump>),
    Block(&'bump Block<'a, 'bump>),
    FuncDecl(&'bump FuncDecl<'a, 'bump>),
    ClassDecl(&'bump ClassDecl<'a, 'bump>),
    InterfaceDecl(&'bump InterfaceDecl<'a, 'bump>),
    ImplDecl(&'bump ImplDecl<'a, 'bump>),
    EnumDecl(&'bump EnumDecl<'a, 'bump>),
    StateMachineDecl(&'bump StateMachineDecl<'a, 'bump>),
    EffectDecl(&'bump EffectDecl<'a, 'bump>),
    Defer(&'bump DeferStmt<'a, 'bump>),
    ExprStmt(&'bump InternalExprStmt<'a, 'bump>),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImportStmt {
    pub path: StrId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PackageStmt {
    pub path: StrId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LetStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub mutability: bool,
    pub ident: StrId,
    pub type_annotation: Type<'a, 'bump>,
    pub value: &'bump Expr<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub ident: StrId,
    pub type_annotation: Type<'a, 'bump>,
    pub value: &'bump Expr<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImplDecl<'a, 'bump> 
where
    'bump: 'a,
{
    pub generics: Option<&'bump [Generic<'a>]>,
    pub interface: StrId,
    pub target: StrId,
    pub methods: Option<&'bump [FuncDecl<'a, 'bump>]>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InterfaceDecl<'a, 'bump> 
where
    'bump: 'a,
{
    pub name: StrId,
    pub visibility: Visibility,
    pub sealed: bool,
    pub permits: Option<&'bump PermitsExpr<'a, 'bump>>,
    pub methods: Option<&'bump [FuncDecl<'a, 'bump>]>,
    pub generics: Option<&'bump [Generic<'a>]>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EnumDecl<'a, 'bump> {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<&'bump [Generic<'a>]>,
    pub variants: &'bump [EnumVariant<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EnumVariant<'a, 'bump> {
    pub name: StrId,
    pub fields: &'bump [Field<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Field<'a, 'bump> 
where
    'bump: 'a,
{
    pub name: StrId,
    pub field_type: Type<'a, 'bump>,
    pub visibility: Visibility,
    /// Generic type parameters for this field
    pub generics: Option<&'bump [Type<'a, 'bump>]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub value: Option<&'bump Expr<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IfStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub condition: &'bump Expr<'a, 'bump>,
    pub then_branch: &'bump Block<'a, 'bump>,
    pub else_branch: Option<&'bump ElseBranch<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ElseBranch<'a, 'bump> 
where
    'bump: 'a,
{
    If(&'bump IfStmt<'a, 'bump>),
    Else(&'bump Block<'a, 'bump>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub condition: &'bump Expr<'a, 'bump>,
    pub block: &'bump Block<'a, 'bump>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub let_stmt: Option<&'bump LetStmt<'a, 'bump>>,
    pub condition: Option<&'bump Expr<'a, 'bump>>,
    pub increment: Option<&'bump Expr<'a, 'bump>>,
    pub block: &'bump Block<'a, 'bump>,
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
    BitNot,
    LogicalNot,
}

impl TryFrom<&str> for Op {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Ok(match s {
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

            _ => return Err("Unknown operator string"),
        })
    }
}

impl Op {
    pub fn is_math_op(&self) -> bool {
        match self {
            Op::Assign | Op::Eq | Op::Neq | Op::Lt | Op::Lte | Op::Gt | Op::Gte => false,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MatchStmt<'a, 'bump> where 'bump: 'a {
    pub expr: &'bump Expr<'a, 'bump>,
    pub arms: &'bump [MatchArm<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MatchArm<'a, 'bump> 
where
    'bump: 'a,
{
    pub pattern: Pattern<'bump>,
    pub guard: Option<&'bump Expr<'a, 'bump>>,
    pub block: &'bump Block<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Pattern<'bump> {
    Ident(StrId),
    Number(i64),
    String(StrId),
    Boolean(bool),
    Tuple(&'bump [Pattern<'bump>]),
    Array(&'bump [Pattern<'bump>]),
    Struct { name: StrId, fields: &'bump [(StrId, Pattern<'bump>)] },
    Or(&'bump [Pattern<'bump>]),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnsafeBlock<'a, 'bump> 
where
    'bump: 'a,
{
    pub block: &'bump Block<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncDecl<'a, 'bump> 
where
    'bump: 'a,
{
    pub visibility: Visibility,
    pub is_unsafe: bool,
    pub is_extern: bool,
    pub inline: bool,
    pub noinline: bool,
    pub extern_string: Option<StrId>,
    pub name: StrId,
    pub generics: Option<&'bump [Generic<'a>]>,
    pub params: Option<&'bump [Param<'a, 'bump>]>,
    pub return_type: Option<Type<'a, 'bump>>,
    pub body: Option<&'bump Block<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClassDecl<'a, 'bump> 
where
    'bump: 'a,
{
    pub visibility: Visibility,
    pub name: StrId,
    pub generics: Option<&'bump [Generic<'a>]>,
    pub params: Option<&'bump [Param<'a, 'bump>]>,
    pub body: &'bump [FuncDecl<'a, 'bump>],
    pub constants: &'bump [ConstStmt<'a, 'bump>],
    pub destructor: Option<&'bump Block<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Module,
    Package,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Param<'a, 'bump> 
where
    'bump: 'a,
{
    Normal(&'bump NormalParam<'a, 'bump>),
    This(&'bump ThisParam<'a, 'bump>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NormalParam<'a, 'bump> 
where
    'bump: 'a,
{
    pub is_mut: bool,
    pub is_move: bool,
    pub name: StrId,
    pub type_annotation: Type<'a, 'bump>,
    pub visibility: Visibility,
    pub default_value: Option<&'bump Expr<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ThisParam<'a, 'bump> {
    pub is_mut: bool,
    pub is_move: bool,
    pub type_annotation: Option<Type<'a, 'bump>>, // e.g. Ptr<this>, this?, etc
}

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub struct Block<'a, 'bump> 
where
    'bump: 'a,
{
    pub block: &'bump [Stmt<'a, 'bump>],
}

impl<'a, 'bump> IntoIterator for &'bump Block<'a, 'bump> 
where
    'bump: 'a,
{
    type Item = &'bump Stmt<'a, 'bump>;
    type IntoIter = std::slice::Iter<'bump, Stmt<'a, 'bump>>;

    fn into_iter(self) -> Self::IntoIter {
        self.block.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Generic<'bump> {
    pub const_generic: bool,
    pub type_name: StrId,
    pub constraints: &'bump [StrId],
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalExprStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub expr: &'bump Expr<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expr<'a, 'bump> 
where
    'bump: 'a,
{
    Number { value: i64, span: SourceSpan<'a> },
    Decimal { value: f64, span: SourceSpan<'a> },
    String { value: StrId, span: SourceSpan<'a> },
    Ident { name: StrId, span: SourceSpan<'a> },
    Boolean { value: bool, span: SourceSpan<'a> },
    Comparison {
        lhs: &'bump Expr<'a, 'bump>,
        op: Op,
        rhs: &'bump Expr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    ClassInit {
        callee: &'bump Expr<'a, 'bump>,
        arguments: &'bump [Expr<'a, 'bump>],
        positional: bool,
        span: SourceSpan<'a>,
    },
    FieldAccess {
        object: &'bump Expr<'a, 'bump>,
        field: StrId,
        span: SourceSpan<'a>,
    },
    Binary {
        left: &'bump Expr<'a, 'bump>,
        op: Op,
        right: &'bump Expr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    Call {
        callee: &'bump Expr<'a, 'bump>,
        arguments: &'bump [Expr<'a, 'bump>],
        span: SourceSpan<'a>,
    },
    Get {
        object: &'bump Expr<'a, 'bump>,
        field: StrId,
        span: SourceSpan<'a>,
    },
    If { if_stmt: &'bump IfStmt<'a, 'bump>, span: SourceSpan<'a> },
    Match { match_stmt: &'bump MatchStmt<'a, 'bump>, span: SourceSpan<'a> },
    Assignment {
        lhs: &'bump Expr<'a, 'bump>,
        op: Op,
        rhs: &'bump Expr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    ExprList {
        expressions: &'bump [Expr<'a, 'bump>],
        span: SourceSpan<'a>,
    },

    Char { value: char, span: SourceSpan<'a> },
    FieldInit { ident: StrId, expr: &'bump Expr<'a, 'bump>, span: SourceSpan<'a> },
    Unary { op: Op, operand: &'bump Expr<'a, 'bump>, span: SourceSpan<'a> },
    ArrayIndex { expr: &'bump Expr<'a, 'bump>, index: &'bump Expr<'a, 'bump>, span: SourceSpan<'a> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type<'a, 'bump> where 'bump: 'a {
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
        params: &'bump [Type<'a, 'bump>],
        return_type: &'a Type<'a, 'bump>,
    },
    Class {
        name: StrId,
        generics: &'bump [Type<'a, 'bump>],
    },
    Nullable(Option<&'a Type<'a, 'bump>>),
    PossibleError(PossibleErrorType<'a, 'bump>),
    This,
    Char,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PossibleErrorType<'a, 'bump> {
    Ok(&'a Type<'a, 'bump>),
    Err(&'a Type<'a, 'bump>),
}

impl<'a, 'bump> Type<'a, 'bump> {
    pub fn is_numeric(&self) -> bool {
        matches!(self,
            Type::U8 | Type::I8 | Type::U16 | Type::I16 | Type::I32 |
            Type::I64 | Type::I128 | Type::U32 | Type::U64 | Type::U128 |
            Type::F32 | Type::F64 | Type::UF32 | Type::UF64
        )
    }
}

impl<'a, 'bump> Display for Type<'a, 'bump> {
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
            } => write!(f, "({:?}) -> {}", params, return_type),
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
            Type::Nullable(opt) => {
                if let Some(ty) = opt {
                    write!(f, "{}?", ty)
                } else {
                    f.write_str("null")
                }
            }

            Type::PossibleError(err_type) => match err_type {
                PossibleErrorType::Ok(ty) => std::fmt::Display::fmt(&ty, f),
                PossibleErrorType::Err(err) => std::fmt::Display::fmt(&err, f),
            },
            Type::This => f.write_str("this"),
            Type::Infer => todo!(),
        }
    }
}

// New AST types for grammar features

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StateMachineDecl<'a, 'bump> 
where
    'bump: 'a,
{
    pub name: StrId,
    pub transitions: &'bump [Transition<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Transition<'a, 'bump> 
where
    'bump: 'a,
{
    pub from_state: StateRef,
    pub to_state: StateRef,
    pub action: Option<&'bump Block<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StateRef {
    Named(StrId),
    Wildcard, // *
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EffectDecl<'a, 'bump> 
where
    'bump: 'a,
{
    pub name: StrId,
    pub visibility: Visibility,
    pub sealed: bool,
    pub permits: Option<&'bump [Type<'a, 'bump>]>,
    pub params: Option<&'bump [Param<'a, 'bump>]>,
    pub body: Option<&'bump Block<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DeferStmt<'a, 'bump> 
where
    'bump: 'a,
{
    pub action: DeferAction<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeferAction<'a, 'bump> 
where
    'bump: 'a,
{
    Block(&'bump Block<'a, 'bump>),
    Stmt(&'bump Stmt<'a, 'bump>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PermitsExpr<'a, 'bump> 
where
    'bump: 'a,
{
    pub types: &'bump [Type<'a, 'bump>],
}