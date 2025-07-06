use ir::bump::AtomicBump;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Import(ImportStmt),
    Let(LetStmt),
    Const(ConstStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Match(MatchStmt),
    UnsafeBlock(UnsafeBlock),
    FuncDecl(FuncDecl),
    ClassDecl(ClassDecl),
    ExprStmt(InternalExprStmt),
    EffectDecl(EffectDecl),
    InterfaceDecl(InterfaceDecl),
    ImplementInterfaceDecl(ImplementInterfaceDecl),
    EnumDecl(EnumDecl),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    pub visibility: Option<Visibility>,
    pub name: String,
    pub methods: Vec<FuncDecl>,
    pub const_stmts: Vec<ConstStmt>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplementInterfaceDecl {
    pub name: String,
    pub methods: Vec<FuncDecl>
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub visibility: Option<Visibility>,
    pub name: String,
    pub variants: Vec<EnumVariant>
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectDecl {
    pub name: String,
    pub effect: String
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub mutability: bool,
    pub ident: String,
    pub type_annotation: Option<Type>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstStmt {
    pub mutability: bool,
    pub ident: String,
    pub type_annotation: Type,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub path: String,
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
    AddAssign, SubAssign, MulAssign, DivAssign, PowAssign,
    ShlAssign, ShrAssign, BitOrAssign, BitXorAssign, BitAndAssign,
    Eq, Neq, Lt, Lte, Gt, Gte,
    // etc.
    ModAssign,
}

impl From<String> for Op {
    fn from(s: String) -> Self {
        match s.as_str() {
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
            _ => panic!("Unknown operator: {}", s),
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
    pub visibility: Option<Visibility>,
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub effects: Option<Effects>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Effects {
    pub effect_vector: Vec<String>
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub visibility: Option<Visibility>,
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
    pub block: Vec<Stmt, AtomicBump>,
}

impl IntoIterator for Block {
    type Item = Stmt;
    type IntoIter = std::vec::IntoIter<Stmt, AtomicBump>;

    fn into_iter(self) -> Self::IntoIter {
        self.block.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalExprStmt {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Array {
        elements: Vec<Expr>
    },
    ArrayIndex {
        array: Box<Expr>,
        index: Box<Expr>
    },
    ArrayInit {
        array_type: Type,
        num_of_elements: u32
    },
    AddressOf(Box<Expr>),
    Deref(Box<Expr>),
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
    Get {
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
    Assignment {
        lhs: Box<Expr>, 
        op: Op,
        rhs: Box<Expr>
    },
    ExprList {
        exprs: Vec<Expr>,
    },
    RegionInit
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
    Array(Box<Type>, Option<u64>),
    Class(String),
}