use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::{StringPool, VmString};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParamId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Internal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode {
    Unit,
    Boolean,
    String,
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
    Class(VmString),
    Function {
        params: Vec<TypeId>,
        return_ty: Option<TypeId>,
    },
    Array(Box<TypeId>),
    Reference(Box<TypeId>),
    Tuple(Vec<TypeId>),
    Generic(VmString),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamNode {
    pub name: VmString,
    pub ty: Option<TypeId>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtNode {
    Import { path: VmString },
    Let { mutable: bool, name: VmString, ty: Option<TypeId>, value: ExprId },
    Return { value: Option<ExprId> },
    Expr { expr: ExprId },
    If { cond: ExprId, then_branch: Vec<StmtId>, else_branch: Option<Vec<StmtId>> },
    While { cond: ExprId, body: Vec<StmtId> },
    For { var: VmString, iter: ExprId, body: Vec<StmtId> },
    Match { expr: ExprId, arms: Vec<MatchArm> },
    Break,
    Continue,
    
    // Declaration forms
    FuncDecl(FuncDecl),
    ClassDecl(ClassDecl),
    InterfaceDecl(InterfaceDecl),
    ImplDecl(ImplDecl),
    EnumDecl(EnumDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_unsafe: bool,
    pub is_extern: bool,
    pub name: VmString,
    pub params: Vec<ParamId>,
    pub return_type: Option<TypeId>,
    pub body: Option<Vec<StmtId>>, // None => extern or forward decl
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod,
    // Bitwise
    Shl, Shr, BitOr, BitAnd, BitXor,
    // Comparison
    Eq, Neq, Lt, Lte, Gt, Gte,
    // Logical
    And, Or,
    // Assignment variants (used with =)
    AddAssign, SubAssign, MulAssign, DivAssign, ModAssign,
    ShlAssign, ShrAssign, BitOrAssign, BitAndAssign, BitXorAssign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // !
    Deref,  // *
    Ref,    // &
    RefMut, // &mut
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprNode {
    // Literals
    Number(i64),
    Decimal(f64),
    Boolean(bool),
    String(VmString),
    Ident(VmString),
    Unit,
    Array(Vec<ExprId>),
    Tuple(Vec<ExprId>),
    Struct { name: VmString, fields: Vec<(VmString, ExprId)> },
    
    // Operations
    Binary { left: ExprId, op: BinaryOp, right: ExprId },
    Unary { op: UnaryOp, expr: ExprId },
    
    // Calls and access
    Call { callee: ExprId, args: Vec<ExprId> },
    MethodCall { object: ExprId, method: VmString, args: Vec<ExprId> },
    Get { object: ExprId, field: VmString },
    
    // Control flow
    If { cond: ExprId, then_branch: ExprId, else_branch: Option<ExprId> },
    Match { expr: ExprId, arms: Vec<MatchArm> },
    
    // Variables and assignment
    Assignment { target: ExprId, op: Option<BinaryOp>, value: ExprId },
    
    // Other
    Block { stmts: Vec<StmtId>, expr: Option<ExprId> },
    Return(Option<ExprId>),
    Break(Option<ExprId>),
    Continue,
    Lambda { params: Vec<ParamId>, body: ExprId },
}

#[derive(Debug)]
pub struct AstArenas {
    bump: GrowableAtomicBump,
    pub strings: StringPool,

    // Pointers into bump to keep indices; nodes themselves are bump-allocated
    exprs: Vec<*mut ExprNode>,
    stmts: Vec<*mut StmtNode>,
    tys: Vec<*mut TypeNode>,
    params: Vec<*mut ParamNode>,
}

unsafe impl Send for AstArenas {}
unsafe impl Sync for AstArenas {}

impl AstArenas {
    pub fn new() -> Self {
        Self {
            bump: GrowableAtomicBump::with_capacity_and_aligned(1024 * 1024, 16)
                .expect("arena allocation failed"),
            strings: StringPool::new().expect("string pool alloc failed"),
            exprs: Vec::new(),
            stmts: Vec::new(),
            tys: Vec::new(),
            params: Vec::new(),
        }
    }

    #[inline]
    pub fn intern_str(&mut self, s: &str) -> VmString { self.strings.intern(s).expect("intern") }

    #[inline]
    pub fn push_expr(&mut self, node: ExprNode) -> ExprId {
        let p = self.bump.alloc(node).expect("arena expr alloc") as *mut ExprNode;
        let id = ExprId(self.exprs.len() as u32);
        self.exprs.push(p);
        id
    }

    #[inline]
    pub fn push_stmt(&mut self, node: StmtNode) -> StmtId {
        let p = self.bump.alloc(node).expect("arena stmt alloc") as *mut StmtNode;
        let id = StmtId(self.stmts.len() as u32);
        self.stmts.push(p);
        id
    }

    #[inline]
    pub fn push_type(&mut self, node: TypeNode) -> TypeId {
        let p = self.bump.alloc(node).expect("arena type alloc") as *mut TypeNode;
        let id = TypeId(self.tys.len() as u32);
        self.tys.push(p);
        id
    }

    #[inline]
    pub fn push_param(&mut self, node: ParamNode) -> ParamId {
        let p = self.bump.alloc(node).expect("arena param alloc") as *mut ParamNode;
        let id = ParamId(self.params.len() as u32);
        self.params.push(p);
        id
    }

    #[inline]
    pub fn expr(&self, id: ExprId) -> &ExprNode {
        unsafe { &*self.exprs[id.0 as usize] }
    }

    #[inline]
    pub fn expr_mut(&mut self, id: ExprId) -> &mut ExprNode {
        unsafe { &mut *self.exprs[id.0 as usize] }
    }

    #[inline]
    pub fn stmt(&self, id: StmtId) -> &StmtNode {
        unsafe { &*self.stmts[id.0 as usize] }
    }

    #[inline]
    pub fn stmt_mut(&mut self, id: StmtId) -> &mut StmtNode {
        unsafe { &mut *self.stmts[id.0 as usize] }
    }

    #[inline]
    pub fn ty(&self, id: TypeId) -> &TypeNode { unsafe { &*self.tys[id.0 as usize] } }

    #[inline]
    pub fn param(&self, id: ParamId) -> &ParamNode { unsafe { &*self.params[id.0 as usize] } }

    pub fn reset(&mut self) {
        self.bump.reset();
        self.exprs.clear();
        self.stmts.clear();
        self.tys.clear();
        self.params.clear();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<ExprId>,
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Ident(VmString),
    Literal(ExprId),
    Struct { name: VmString, fields: Vec<(VmString, Pattern)> },
    Tuple(Vec<Pattern>),
    Or(Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub visibility: Visibility,
    pub name: VmString,
    pub generics: Option<Vec<GenericParam>>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<StmtId>, // FuncDecl StmtIds
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    pub visibility: Visibility,
    pub name: VmString,
    pub generics: Option<Vec<GenericParam>>,
    pub methods: Vec<StmtId>, // FuncDecl StmtIds
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    pub generics: Option<Vec<GenericParam>>,
    pub interface: Option<VmString>,
    pub target: VmString,
    pub methods: Vec<StmtId>, // FuncDecl StmtIds
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub visibility: Visibility,
    pub name: VmString,
    pub generics: Option<Vec<GenericParam>>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: VmString,
    pub data: Option<EnumVariantData>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantData {
    Tuple(Vec<TypeId>),
    Struct(Vec<FieldDecl>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub visibility: Visibility,
    pub name: VmString,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: VmString,
    pub bounds: Vec<TypeId>,
}

#[derive(Debug)]
pub struct ProgramAst {
    pub stmts: Vec<StmtId>,
    pub arenas: AstArenas,
}

impl ProgramAst {
    pub fn new() -> Self {
        ProgramAst {
            stmts: Vec::new(),
            arenas: AstArenas::new(),
        }
    }
    
    pub fn with_capacity(capacity: usize) -> Self {
        ProgramAst {
            stmts: Vec::with_capacity(capacity),
            arenas: AstArenas::new(),
        }
    }
    
    pub fn add_stmt(&mut self, stmt: StmtNode) -> StmtId {
        let id = self.arenas.push_stmt(stmt);
        self.stmts.push(id);
        id
    }
}
