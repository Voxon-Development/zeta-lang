use smallvec::SmallVec;
use ir::hir::StrId;
use ir::span::SourceSpan;

pub struct Tokens<'a> {
    pub kinds: SmallVec<[TokenKind; 2048]>,
    pub texts: SmallVec<[StrId; 1024]>,
    pub spans: SmallVec<[SourceSpan<'a>; 2048]>,
}

impl<'a> Tokens<'a> {
    pub fn new() -> Tokens<'a> {
        Tokens { kinds: SmallVec::new(), texts: SmallVec::new(), spans: SmallVec::new() }
    }

    pub fn push(&mut self, kind: TokenKind, text: StrId, span: SourceSpan<'a>) {
        self.kinds.push(kind);
        self.texts.push(text);
        self.spans.push(span);
    }

    pub fn push_token(&mut self, kind: TokenKind, span: SourceSpan<'a>) {
        self.kinds.push(kind);
        self.spans.push(span);
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[derive(Debug)]
pub enum TokenKind {
    // ===== Identifiers & Literals =====
    Ident,          // user identifiers
    Number,         // numeric literals
    Decimal,        // decimal literals
    Hexadecimal,    // hexadecimal literals
    Binary,         // binary literals
    String,         // string literals
    BooleanTrue,
    BooleanFalse,

    // ===== Keywords =====
    If,
    Else,
    While,
    For,
    Return,
    Break,
    Continue,
    Enum,
    Struct,
    Interface,
    Impl,
    Import,
    Package,
    Type,
    Const,
    Mut,
    Own,
    Unsafe,
    Inline,
    Noinline,
    Sealed,
    Private,
    Module,
    Extern,
    Static,
    Match,
    Defer,
    Effect,
    Permits,
    Statem,
    Trait,
    Where,
    
    // ======== Types ========
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Char,
    Str,
    Boolean,
    Let,
    Void,
    Func,


    // ===== Punctuation =====
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
    Comma,    // ,
    Dot,      // .
    Semicolon,// ;
    Colon,    // :
    Question, // ?
    Arrow,    // ->
    FatArrow, // =>
    Ellipsis, // ...

    // ===== Operators =====
    InferAssign,       // :=
    Assign,            // =
    AddAssign,         // +=
    SubAssign,         // -=
    MulAssign,         // *=
    DivAssign,         // /=
    ModAssign,         // %=
    ShlAssign,         // <<=
    ShrAssign,         // >>=
    UnsignedShrAssign, // >>>=
    AndAssign,         // &=
    OrAssign,          // |=
    XorAssign,         // ^=
    Eq,                // ==
    Ne,                // ~=
    Lt,                // <
    Gt,                // >
    Le,                // <=
    Ge,                // >=
    Add,               // +
    Sub,               // -
    Mul,               // *
    Div,               // /
    Mod,               // %
    BitNot,            // ~
    BitAnd,            // &
    BitOr,             // |
    BitXor,            // ^
    Shl,               // <<
    Shr,               // >>
    UnsignedShr,       // >>>

    // ===== Logic Operators =====
    AndAnd,         // &&
    OrOr,           // ||
    LogicalNot,     // !

    // ===== Other special tokens =====
    This,
    Underscore,     // _
    ColonAssign,    // :=
    
    // ===== Comments =====
    LineComment,    // // ...
    BlockComment,   // /* ... */
    DocComment,     // /// ...
    
    EOF,
    Unknown,
    NotAssign
}