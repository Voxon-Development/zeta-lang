#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    pub mutability: Option<MutKeyword>,
    pub ident: String,
    pub type_annotation: Option<Type>,
    pub value: Box<Expr>,
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

#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    pub visibility: Option<String>,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub visibility: Option<String>,
    pub name: String,
    pub params: Option<Vec<Param>>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
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
pub struct InternalExprStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MutKeyword {
    Mut,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
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
    Class(String),
}