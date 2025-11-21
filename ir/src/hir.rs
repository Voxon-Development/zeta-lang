use std::fmt::{Display, Formatter};
use std::ops::Deref;
use zetaruntime::string_pool::VmString;
use crate::span::SourceSpan;
// =====================================
// HIR (High-Level IR)
// =====================================

/// A reference to an interned string in the global string pool
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Hir<'a, 'bump>
where
    'bump: 'a,
{
    Module(&'bump HirModule<'a, 'bump>),
    Func(&'bump HirFunc<'a, 'bump>),
    Struct(&'bump HirStruct<'a, 'bump>),
    Interface(&'bump HirInterface<'a, 'bump>),
    Impl(&'bump HirImpl<'a, 'bump>),
    Enum(&'bump HirEnum<'a, 'bump>),
    Const(&'bump ConstStmt<'a, 'bump>),
    Stmt(&'bump HirStmt<'a, 'bump>),
    Expr(&'bump HirExpr<'a, 'bump>),
}

impl<'a, 'bump> Hir<'a, 'bump> {
    pub fn is_func(&self) -> bool {
        if let Hir::Func(_) = self {
            return true;
        }
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct HirModule<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub imports: &'bump [StrId],
    pub items: &'bump [Hir<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirFunc<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub visibility: Visibility,
    pub is_unsafe: bool,
    pub inline: bool,
    pub noinline: bool,
    pub generics: Option<&'bump [HirGeneric<'bump>]>,
    pub params: Option<&'bump [HirParam<'a, 'bump>]>,
    pub return_type: Option<HirType<'a, 'bump> >,
    pub body: Option<HirStmt<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirStruct<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<&'bump [HirGeneric<'bump>]>,
    pub fields: &'bump [HirField<'a, 'bump>],
    pub interfaces: Option<&'bump [StrId]>, // implemented interfaces
    pub methods: Option<&'bump [HirFunc<'a, 'bump>]>,
    pub constants: Option<&'bump [ConstStmt<'a, 'bump>]>,
    pub destructor: Option<&'bump HirStmt<'a, 'bump>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirImpl<'a, 'bump>
where
    'bump: 'a,
{
    pub generics: Option<&'bump [HirGeneric<'bump>]>,
    pub interface: StrId,
    pub target: StrId,
    pub methods: Option<&'bump [HirFunc<'a, 'bump>]>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirInterface<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<&'bump [HirGeneric<'bump>]>,
    pub methods: Option<&'bump [HirFunc<'a, 'bump>]>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirEnum<'a, 'bump> {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<&'bump [HirGeneric<'bump>]>,
    pub variants: &'bump [HirEnumVariant<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirEnumVariant<'a, 'bump> {
    pub name: StrId,
    pub fields: &'bump [HirField<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirField<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub field_type: HirType<'a, 'bump>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirParam<'a, 'bump>
where
    'bump: 'a,
{
    Normal { name: StrId, param_type: HirType<'a, 'bump> },
    This { param_type: Option<HirType<'a, 'bump>> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirGeneric<'bump> {
    pub name: StrId,
    pub constraints: &'bump [StrId],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirType<'a, 'bump>
where
    'bump: 'a,
{
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
    Class(StrId, &'bump [HirType<'a, 'bump>]),
    Interface(StrId, &'bump [HirType<'a, 'bump>]),
    Enum(StrId, &'bump [HirType<'a, 'bump>]),
    Lambda {
        params: &'bump [HirType<'a, 'bump>],
        return_type: &'a HirType<'a, 'bump>,
        concurrent: bool,
    },
    Generic(StrId),
    Void,
    This,
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstStmt<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub ty: HirType<'a, 'bump>,
    pub value: HirExpr<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirStmt<'a, 'bump>
where
    'bump: 'a,
{
    Let {
        name: StrId,
        ty: HirType<'a, 'bump>,
        value: HirExpr<'a, 'bump>,
        mutable: bool,
    },
    Return(Option<&'bump HirExpr<'a, 'bump>>),
    Expr(&'bump HirExpr<'a, 'bump>),
    If {
        cond: HirExpr<'a, 'bump>,
        then_block: &'bump [HirStmt<'a, 'bump>],
        else_block: Option<&'bump HirStmt<'a, 'bump>>,
    },
    While {
        cond: &'bump HirExpr<'a, 'bump>,
        body: &'bump HirStmt<'a, 'bump>,
    },
    For {
        init: Option<&'bump HirStmt<'a, 'bump>>,
        condition: Option<&'bump HirExpr<'a, 'bump>>,
        increment: Option<&'bump HirExpr<'a, 'bump>>,
        body: &'bump HirStmt<'a, 'bump>,
    },
    Match {
        expr: &'bump HirExpr<'a, 'bump>,
        arms: &'bump [HirMatchArm<'a, 'bump>],
    },
    UnsafeBlock {
        body: &'bump HirStmt<'a, 'bump>,
    },
    Block {
        body: &'bump [HirStmt<'a, 'bump>],
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirMatchArm<'a, 'bump>
where
    'bump: 'a,
{
    pub pattern: HirPattern<'bump>,
    pub guard: Option<&'bump HirExpr<'a, 'bump>>,
    pub body: &'bump HirStmt<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirPattern<'bump> {
    Ident(StrId),
    Number(i64),
    String(StrId),
    Tuple(&'bump [HirPattern<'bump>]),
    EnumVariant {
        enum_name: StrId,
        variant: StrId,
        bindings: &'bump [StrId],
    },
    Wildcard,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirExpr<'a, 'bump>
where
    'bump: 'a,
{
    Number(i64),
    String(StrId),
    Boolean(bool),
    Ident(StrId),
    Tuple(&'bump [HirExpr<'a, 'bump>]),
    Decimal(f64),
    Binary {
        left: &'bump HirExpr<'a, 'bump>,
        op: Operator,
        right: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    Call {
        callee: &'bump HirExpr<'a, 'bump>,
        args: &'bump [HirExpr<'a, 'bump>],
    },
    InterfaceCall {
        callee: &'bump HirExpr<'a, 'bump>,
        args: &'bump [HirExpr<'a, 'bump>],
        interface: StrId,
    },
    FieldAccess {
        object: &'bump HirExpr<'a, 'bump>,
        field: StrId,
        span: SourceSpan<'a>
    },
    Assignment {
        target: &'bump HirExpr<'a, 'bump>,
        op: AssignmentOperator,
        value: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    InterpolatedString(&'bump [InterpolationPart<'a, 'bump>]),
    EnumInit {
        enum_name: StrId,
        variant: StrId,
        args: &'bump [HirExpr<'a, 'bump>],
    },
    ExprList { list: &'bump [HirExpr<'a, 'bump>], span: SourceSpan<'a> },
    Get {
        object: &'bump HirExpr<'a, 'bump>,
        field: StrId,
        span: SourceSpan<'a>
    },
    ClassInit {
        name: &'bump HirExpr<'a, 'bump>,
        args: &'bump [HirExpr<'a, 'bump>],
        span: SourceSpan<'a>
    },
    Comparison {
        left: &'bump HirExpr<'a, 'bump>,
        op: Operator,
        right: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>
    },
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
    BitNot,
    LogicalNot,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InterpolationPart<'a, 'bump>
where
    'bump: 'a,
{
    String(StrId),
    Expr(&'bump HirExpr<'a, 'bump>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Module,
    Package,
}

impl<'a, 'bump> Display for HirType<'a, 'bump>
where
    'bump: 'a,
{
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
            HirType::Lambda {
                params,
                return_type,
                concurrent,
            } => {
                let params_str: Vec<_> = params.iter().map(|p| p.to_string()).collect();
                if *concurrent {
                    write!(
                        f,
                        "concurrent fn({}) -> {}",
                        params_str.join(", "),
                        return_type
                    )
                } else {
                    write!(f, "fn({}) -> {}", params_str.join(", "), return_type)
                }
            }
            HirType::Generic(name) => write!(f, "{}", name),
            HirType::Void => write!(f, "void"),
            HirType::This => write!(f, "this"),
            HirType::Null => write!(f, "null"),
        }
    }
}

impl<'a, 'bump> HirType<'a, 'bump>
where
    'bump: 'a,
{
    fn write_args(f: &mut Formatter, name: &StrId, args: &[HirType<'a, 'bump>]) -> std::fmt::Result {
        if args.is_empty() {
            write!(f, "{}", name)
        } else {
            let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
            write!(f, "{}<{}>", name, args_str.join(", "))
        }
    }
}

use std::cell::UnsafeCell;
use crate::ir_hasher::FxHashBuilder;

pub struct HirSlice<'bump, T> {
    inner: UnsafeCell<&'bump [T]>,
}

impl<'bump, T> HirSlice<'bump, T> {
    pub fn new(inner: &'bump [T]) -> Self {
        Self {
            inner: UnsafeCell::new(inner),
        }
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[T] {
        // Safe because shared access doesn't mutate
        unsafe { *self.inner.get() }
    }

    #[inline(always)]
    #[allow(invalid_reference_casting)]
    pub fn as_mut_slice(&self) -> &mut [T] {
        // Unsafe because multiple mutable borrows would be UB,
        // caller must guarantee exclusive access
        unsafe { &mut *(*self.inner.get() as *const [T] as *mut [T]) }
    }
}

// =====================================
// CTRC Integration
// =====================================

/// HIR module with CTRC analysis results attached
#[derive(Debug, Clone)]
pub struct HirModuleWithCTRC<'a, 'bump>
where
    'bump: 'a,
{
    pub module: HirModule<'a, 'bump>,
    pub ctrc_analysis: Option<CTRCAnalysisResult>,
}

/// CTRC analysis result integrated with HIR
#[derive(Debug, Clone)]
pub struct CTRCAnalysisResult {
    pub structs_with_destructors: std::collections::HashSet<StrId, FxHashBuilder>,
    pub droppable_fields: std::collections::HashSet<(StrId, StrId),FxHashBuilder>, // (struct_name, field_name)
    pub variable_aliases: std::collections::HashMap<StrId, usize>, // AliasID
    pub allocation_sites: std::collections::HashMap<usize, usize>, // ProgramPoint -> AliasID
    pub drop_insertions: Vec<DropInsertion>,
    pub destructor_calls: Vec<DestructorCall>,
    pub potential_leaks: Vec<usize>, // AliasID
}

/// Drop insertion point in HIR
#[derive(Debug, Clone)]
pub struct DropInsertion {
    pub program_point: usize,
    pub variable_name: StrId,
    pub alias_id: usize,
    pub location_hint: Option<DropLocation>,
}

/// Destructor call in HIR
#[derive(Debug, Clone)]
pub struct DestructorCall {
    pub program_point: usize,
    pub alias_id: usize,
    pub call_type: DestructorCallType,
    pub location_hint: Option<DropLocation>,
}

/// Type of destructor call
#[derive(Debug, Clone, PartialEq)]
pub enum DestructorCallType {
    AutoDrop,
    ExplicitDrop,
    ScopeDrop,
}

/// Location hint for where drops/destructors should be inserted
#[derive(Debug, Clone, PartialEq)]
pub enum DropLocation {
    BeforeStatement(usize), // Statement index in block
    AfterStatement(usize),  // Statement index in block
    EndOfBlock,
    EndOfFunction,
}

impl CTRCAnalysisResult {
    pub fn new() -> Self {
        Self {
            structs_with_destructors: std::collections::HashSet::with_hasher(FxHashBuilder),
            droppable_fields: std::collections::HashSet::with_hasher(FxHashBuilder),
            variable_aliases: std::collections::HashMap::new(),
            allocation_sites: std::collections::HashMap::new(),
            drop_insertions: Vec::new(),
            destructor_calls: Vec::new(),
            potential_leaks: Vec::new(),
        }
    }
    
    pub fn has_memory_safety_issues(&self) -> bool {
        !self.potential_leaks.is_empty()
    }
    
    pub fn get_drop_points_for_variable(&self, var_name: StrId) -> Vec<usize> {
        self.drop_insertions
            .iter()
            .filter(|drop| drop.variable_name == var_name)
            .map(|drop| drop.program_point)
            .collect()
    }
    
    pub fn get_destructor_calls_at_point(&self, program_point: usize) -> Vec<&DestructorCall> {
        self.destructor_calls
            .iter()
            .filter(|call| call.program_point == program_point)
            .collect()
    }
    
    pub fn get_drop_insertions_at_point(&self, program_point: usize) -> Vec<&DropInsertion> {
        self.drop_insertions
            .iter()
            .filter(|drop| drop.program_point == program_point)
            .collect()
    }
}
