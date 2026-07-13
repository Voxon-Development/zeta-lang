use crate::ast::LambdaModifier;
use crate::ast::MutabilityState;
pub use crate::ast::{FuncModifiers, Path, Visibility};
use crate::hir::DropKind::Undroppable;
use crate::span::SourceSpan;
use std::cell::UnsafeCell;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::ops::Deref;
use std::str::from_utf8_unchecked;
use zetaruntime::string_pool::VmString;

/// A reference to an interned string in the global string pool
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct StrId(pub VmString);

impl fmt::Debug for StrId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Display for StrId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Deref for StrId {
    type Target = VmString;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl StrId {
    pub fn new(vm_string: VmString) -> Self {
        StrId(vm_string)
    }

    pub fn into_inner(self) -> VmString {
        self.0
    }

    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(std::slice::from_raw_parts(self.offset, self.length)) }
    }

    pub fn len(&self) -> usize {
        self.0.length
    }

    /// Check if the string is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl PartialEq<&StrId> for StrId {
    fn eq(&self, other: &&StrId) -> bool {
        self == *other
    }

    fn ne(&self, other: &&StrId) -> bool {
        self != *other
    }
}

impl PartialEq<str> for StrId {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }

    fn ne(&self, other: &str) -> bool {
        self.as_str() != other
    }
}

impl From<VmString> for StrId {
    fn from(vm_string: VmString) -> Self {
        StrId(vm_string)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ProvenanceRoot {
    Var(StrId),
    ThisRoot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ProvenancePathSegment {
    Field(StrId),
    Deref,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub struct ProvenanceAnnotation<'bump> {
    pub root: ProvenanceRoot,
    pub path: &'bump [ProvenancePathSegment],
}

impl fmt::Display for ProvenanceRoot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProvenanceRoot::Var(name) => write!(f, "{name}"),
            ProvenanceRoot::ThisRoot => write!(f, "self"),
        }
    }
}

impl fmt::Display for ProvenancePathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProvenancePathSegment::Field(field) => write!(f, ".{field}"),
            ProvenancePathSegment::Deref => write!(f, "*"),
        }
    }
}

impl<'bump> fmt::Display for ProvenanceAnnotation<'bump> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.root)?;

        for segment in self.path {
            match segment {
                ProvenancePathSegment::Field(field) => {
                    write!(f, ".{field}")?;
                }
                ProvenancePathSegment::Deref => {
                    write!(f, "*")?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct HirModule<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub imports: &'bump [Path<'a, 'bump>],
    pub items: &'bump [Hir<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirFunc<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub function_metadata: FuncModifiers,
    pub generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
    pub params: Option<&'bump [HirParam<'a, 'bump>]>,
    pub return_type: Option<HirType<'a, 'bump>>,
    pub body: Option<HirStmt<'a, 'bump>>,
    pub unmangled_name: StrId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirStruct<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
    pub fields: &'bump [HirField<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirImpl<'a, 'bump>
where
    'bump: 'a,
{
    pub generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
    pub interface: Option<StrId>,
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
    pub generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
    pub methods: Option<&'bump [HirFunc<'a, 'bump>]>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirEnum<'a, 'bump> {
    pub name: StrId,
    pub visibility: Visibility,
    pub generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
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
    /// Generic type parameters for this field
    pub generics: Option<&'bump [HirType<'a, 'bump>]>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ThisPassingKind {
    /// `&this`
    RefConst,
    /// `&mut this`
    RefMut,
    /// `*mut this`
    MutSafePtr,
    /// `*const this`
    ConstSafePtr,
    /// `[*]mut this`
    MutUnsafePtr,
    /// `[*]const this`
    ConstUnsafePtr,
    /// `this`
    Move,
    /// `mut this`
    MoveMut,
}

impl Display for ThisPassingKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ThisPassingKind::RefConst => write!(f, "&this"),
            ThisPassingKind::RefMut => write!(f, "&mut this"),
            ThisPassingKind::MutSafePtr => write!(f, "*mut this"),
            ThisPassingKind::ConstSafePtr => write!(f, "*const this"),
            ThisPassingKind::MutUnsafePtr => write!(f, "[*]mut this"),
            ThisPassingKind::ConstUnsafePtr => write!(f, "[*]const this"),
            ThisPassingKind::Move => write!(f, "this"),
            ThisPassingKind::MoveMut => write!(f, "mut this"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirParam<'a, 'bump>
where
    'bump: 'a,
{
    Normal {
        name: StrId,
        param_type: HirType<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    This {
        kind: ThisPassingKind,
        span: SourceSpan<'a>,
    },
}

impl<'a, 'bump> HirParam<'a, 'bump> {
    pub fn get_type(&self) -> Option<&HirType<'a, 'bump>> {
        match self {
            HirParam::Normal { param_type, .. } => Some(param_type),
            HirParam::This { .. } => Some(&HirType::This),
        }
    }

    /// Returns true when `this` is passed by pointer (all variants except Move/MoveMut).
    pub fn this_is_ptr(&self) -> bool {
        matches!(
            self,
            HirParam::This {
                kind: ThisPassingKind::RefConst
                    | ThisPassingKind::RefMut
                    | ThisPassingKind::MutSafePtr
                    | ThisPassingKind::ConstSafePtr
                    | ThisPassingKind::MutUnsafePtr
                    | ThisPassingKind::ConstUnsafePtr,
                ..
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirGeneric<'a, 'bump> {
    pub name: StrId,
    pub constraints: &'bump [HirType<'a, 'bump>],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    Struct(StrId, &'bump [HirType<'a, 'bump>]),
    DynInterface(StrId, &'bump [HirType<'a, 'bump>]),
    Enum(StrId, &'bump [HirType<'a, 'bump>]),
    SafePointer(&'a HirType<'a, 'bump>),
    Ref {
        inner: &'a HirType<'a, 'bump>,
        mutability_state: MutabilityState,
        provenance: Option<ProvenanceAnnotation<'bump>>,
    },
    UnsafePointer(&'a HirType<'a, 'bump>),
    OwnedPointer(&'bump HirType<'a, 'bump>),
    Lambda {
        params: &'bump [HirType<'a, 'bump>],
        return_type: &'a HirType<'a, 'bump>,
    },
    Generic(StrId),
    Void,
    This,
    Null,
    Char,
    /// Placeholder used when a type couldn't be determined, e.g. after a
    /// type error has already been reported for the expression, so callers
    /// don't cascade a second complaint about the same root cause.
    Unknown,
    Nullable(&'a HirType<'a, 'bump>),
    Dyn {
        bounds: &'bump [HirType<'a, 'bump>],
    },
    Tuple(&'bump [HirType<'a, 'bump>]),
    Array(&'a HirType<'a, 'bump>, usize),
    Slice(&'a HirType<'a, 'bump>),
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
        is_static: bool,
        catch_pattern: Option<HirErrorHandlerPattern<'a, 'bump>>,
        else_block: Option<&'bump HirStmt<'a, 'bump>>,
        span: SourceSpan<'a>,
    },
    Const(&'bump ConstStmt<'a, 'bump>),
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
    Break(Option<&'bump HirExpr<'a, 'bump>>, SourceSpan<'a>),
    Continue(SourceSpan<'a>),
    Defer(&'bump HirStmt<'a, 'bump>),
    Import(Path<'a, 'bump>, SourceSpan<'a>),
    Package(Path<'a, 'bump>, SourceSpan<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirErrorHandlerPattern<'a, 'bump>
where
    'bump: 'a,
{
    Single {
        error_type: HirType<'a, 'bump>,
        binding: Option<StrId>,
        body: &'bump [HirStmt<'a, 'bump>],
    },
    Multiple {
        branches: &'bump [HirErrorHandlerBranch<'a, 'bump>],
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirErrorHandlerBranch<'a, 'bump>
where
    'bump: 'a,
{
    pub error_type: HirType<'a, 'bump>,
    pub binding: Option<StrId>,
    pub body: &'bump [HirStmt<'a, 'bump>],
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
pub struct HirFuncProto<'a, 'bump> {
    pub name: StrId,
    pub params: Option<&'bump [HirParam<'a, 'bump>]>,
    pub return_type: HirType<'a, 'bump>,
    pub function_metadata: FuncModifiers,
    pub generics: Option<&'bump [HirGeneric<'a, 'bump>]>,
    pub unmangled_name: StrId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirFieldInit<'a, 'bump>
where
    'bump: 'a,
{
    pub name: StrId,
    pub name_span: SourceSpan<'a>,
    pub value: HirExpr<'a, 'bump>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HirExpr<'a, 'bump>
where
    'bump: 'a,
{
    Null(SourceSpan<'a>),
    Number(i64, SourceSpan<'a>),
    String(StrId, SourceSpan<'a>),
    Boolean(bool, SourceSpan<'a>),
    Ident(StrId, SourceSpan<'a>),
    Tuple(&'bump [HirExpr<'a, 'bump>], SourceSpan<'a>),
    Decimal(f64, SourceSpan<'a>),
    Binary {
        left: &'bump HirExpr<'a, 'bump>,
        op: Operator,
        right: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    Call {
        callee: &'bump HirExpr<'a, 'bump>,
        args: &'bump [HirExpr<'a, 'bump>],
        type_args: Option<&'bump [HirType<'a, 'bump>]>,
        span: SourceSpan<'a>,
    },
    InterfaceCall {
        callee: &'bump HirExpr<'a, 'bump>,
        args: &'bump [HirExpr<'a, 'bump>],
        interface: StrId,
        span: SourceSpan<'a>,
    },
    FieldAccess {
        object: &'bump HirExpr<'a, 'bump>,
        field: StrId,
        span: SourceSpan<'a>,
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
        span: SourceSpan<'a>,
    },
    ExprList {
        list: &'bump [HirExpr<'a, 'bump>],
        span: SourceSpan<'a>,
    },
    Get {
        object: &'bump HirExpr<'a, 'bump>,
        field: StrId,
        span: SourceSpan<'a>,
    },
    StructInit {
        name: &'bump HirExpr<'a, 'bump>,
        args: &'bump [HirFieldInit<'a, 'bump>],
        type_args: Option<&'bump [HirType<'a, 'bump>]>,
        span: SourceSpan<'a>,
    },
    Comparison {
        left: &'bump HirExpr<'a, 'bump>,
        op: Operator,
        right: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    Deref {
        expr: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    Ref {
        expr: &'bump HirExpr<'a, 'bump>,
        mutable: bool,
        span: SourceSpan<'a>,
    },
    This {
        span: SourceSpan<'a>,
    },
    ModuleAccess(&'bump HirModuleAccess<'a, 'bump>),
    Lambda {
        modifier: Option<LambdaModifier>,
        params: &'bump [HirLambdaParam<'a, 'bump>],
        return_type: &'a HirType<'a, 'bump>,
        body: &'bump HirStmt<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    Index {
        object: &'bump HirExpr<'a, 'bump>,
        index: &'bump HirExpr<'a, 'bump>,
        span: SourceSpan<'a>,
    },
    ArrayLiteral {
        elements: &'bump [HirExpr<'a, 'bump>],
        span: SourceSpan<'a>,
    },
    Undefined {
        span: SourceSpan<'a>,
        ty: HirType<'a, 'bump>,
    },
    GenericIdent(StrId, &'bump [HirType<'a, 'bump>], SourceSpan<'a>),
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
    LogicalAnd,
    LogicalOr,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    DerefUnsafe,
    Deref,
    Ref,
    RefMut,
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

impl<'a, 'bump> Display for HirType<'a, 'bump>
where
    'bump: 'a,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
            HirType::Struct(name, args) => Self::write_args(f, name, args),
            HirType::DynInterface(name, args) => Self::write_args(f, name, args),
            HirType::Enum(name, args) => Self::write_args(f, name, args),
            HirType::Lambda {
                params,
                return_type,
            } => {
                let params_str: Vec<_> = params.iter().map(|p| p.to_string()).collect();
                write!(f, "fn({})", params_str.join(", "))?;

                write!(f, " -> {}", return_type)
            }
            HirType::Generic(name) => write!(f, "{}", name),
            HirType::Void => write!(f, "void"),
            HirType::This => write!(f, "this"),
            HirType::Null => write!(f, "null"),
            HirType::SafePointer(inner) => write!(f, "*{}", inner),
            HirType::UnsafePointer(inner) => write!(f, "[*]{}", inner),
            HirType::Char => write!(f, "char"),
            HirType::Ref {
                inner,
                mutability_state,
                provenance,
            } => {
                write!(f, "&")?;

                if let Some(provenance) = provenance {
                    write!(f, "{} ", provenance)?;
                }

                if let MutabilityState::Mut = mutability_state {
                    write!(f, "mut ")?;
                }

                write!(f, "{inner}")
            }
            HirType::Nullable(hir_type) => write!(f, "{}?", hir_type),
            HirType::Dyn { bounds } => {
                write!(f, "dyn ")?;
                let mut is_start = true;
                let mut iter = bounds.into_iter();
                while let Some(bound) = iter.next() {
                    write!(f, "{}", bound)?;
                    if !is_start {
                        write!(f, " + ")?;
                    }
                    is_start = false;
                }
                Ok(())
            }
            HirType::Unknown => write!(f, "<unknown>"),
            HirType::Tuple(args) => {
                let params_str: String = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({})", params_str)
            }
            HirType::Array(hir_type, len) => write!(f, "&[{}]{}", len, hir_type),
            HirType::Slice(hir_type) => write!(f, "&[]{}", hir_type),
            HirType::OwnedPointer(hir_type) => write!(f, "^{}", hir_type),
        }
    }
}

impl<'a, 'bump> HirType<'a, 'bump>
where
    'bump: 'a,
{
    /// Creates a new safe pointer type that points to the given type (*T)
    pub fn safe_ptr_to(inner: &'a HirType<'a, 'bump>) -> Self {
        HirType::SafePointer(inner)
    }

    /// Creates a new unsafe pointer type that points to the given type ([*]T)
    pub fn unsafe_ptr_to(inner: &'a HirType<'a, 'bump>) -> Self {
        HirType::UnsafePointer(inner)
    }

    /// Returns true if this is a safe pointer type
    pub fn is_safe_pointer(&self) -> bool {
        matches!(self, HirType::SafePointer(_))
    }

    /// Returns true if this is an unsafe pointer type
    pub fn is_unsafe_pointer(&self) -> bool {
        matches!(self, HirType::UnsafePointer(_))
    }

    /// Returns true if this is any pointer type that is literally a pointer (safe or unsafe)
    pub fn is_pointer_literal(&self) -> bool {
        matches!(self, HirType::SafePointer(_) | HirType::UnsafePointer(_))
    }

    /// Returns true if this is any pointer type that functions as a pointer (safe, unsafe or reference)
    pub fn is_pointer_semantics(&self) -> bool {
        matches!(
            self,
            HirType::SafePointer(_) | HirType::UnsafePointer(_) | HirType::Ref { .. }
        )
    }

    pub fn as_safe_pointer(&self) -> Option<&'a HirType<'a, 'bump>> {
        if let HirType::SafePointer(inner) = self {
            Some(inner)
        } else {
            None
        }
    }

    pub fn as_unsafe_pointer(&self) -> Option<&'a HirType<'a, 'bump>> {
        if let HirType::UnsafePointer(inner) = self {
            Some(inner)
        } else {
            None
        }
    }

    /// If this is any pointer type, returns the inner type. Otherwise returns None.
    pub fn as_pointer(&self) -> Option<&'a HirType<'a, 'bump>> {
        match self {
            HirType::SafePointer(inner) => Some(inner),
            HirType::UnsafePointer(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn drop_kind(&self) -> DropKind {
        match self {
            HirType::Struct(name, _) | HirType::Enum(name, _) | HirType::DynInterface(name, _) => {
                DropKind::Type(*name)
            }

            HirType::OwnedPointer(inner) => DropKind::OwnedPointer(Box::new(inner.drop_kind())),

            _ => DropKind::Undroppable,
        }
    }

    pub fn nominal_type_name(&self) -> Option<StrId> {
        match self {
            HirType::Struct(name, _) | HirType::Enum(name, _) | HirType::DynInterface(name, _) => {
                Some(*name)
            }

            HirType::Ref { inner, .. }
            | HirType::SafePointer(inner)
            | HirType::UnsafePointer(inner)
            | HirType::OwnedPointer(inner)
            | HirType::Nullable(inner)
            | HirType::Array(inner, _)
            | HirType::Slice(inner) => inner.nominal_type_name(),

            _ => None,
        }
    }

    fn write_args(f: &mut Formatter, name: &StrId, args: &[HirType<'a, 'bump>]) -> fmt::Result {
        if args.is_empty() {
            write!(f, "{}", name)
        } else {
            let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
            write!(f, "{}<{}>", name, args_str.join(", "))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DropKind {
    Type(StrId),
    OwnedPointer(Box<DropKind>),
    Undroppable,
}

impl DropKind {
    pub fn is_droppable(&self) -> bool {
        match self {
            Undroppable => false,
            _ => true,
        }
    }
}

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
        // SAFETY: multiple mutable borrows would be UB,
        // caller must guarantee exclusive access
        unsafe { &mut *(*self.inner.get() as *const [T] as *mut [T]) }
    }
}

/// A module-qualified access: `std::io.println` or `std::io.File`.
/// `path` is the `::` separated module segments, `member` is the name
/// after the first `.`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirModuleAccess<'a, 'bump> {
    pub path: &'bump [StrId], // ["std", "io"]
    pub member: StrId,        // "println" / "File" / "out"
    pub span: SourceSpan<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HirLambdaParam<'a, 'bump> {
    pub name: StrId,
    pub param_type: Option<HirType<'a, 'bump>>,
    pub span: SourceSpan<'a>,
}
