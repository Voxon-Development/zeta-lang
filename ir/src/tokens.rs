use crate::errors::error::ParserError;
use crate::hir::StrId;
use crate::span::SourceSpan;
use std::fmt;
use std::ops::Index;
use zetaruntime::bump::GrowableBump;

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: Option<StrId>,
    pub span: SourceSpan<'a>,
}

pub struct Tokens<'a> {
    pub slice: &'a [Token<'a>],
}

impl<'a> Index<usize> for Tokens<'a> {
    type Output = Token<'a>;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.slice[index]
    }
}

impl<'a> Tokens<'a> {
    pub fn new(bump: &'a GrowableBump, tokens: Vec<Token<'a>>) -> Self {
        let slice = bump.alloc_slice_copy(&tokens);
        Self { slice }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.slice.len()
    }

    #[inline]
    pub fn get(&self, i: usize) -> Token<'a> {
        self.slice[i]
    }
}

pub struct Cursor<'a> {
    tokens: &'a Tokens<'a>,
    index: usize,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(tokens: &'a Tokens<'a>, index: usize) -> Self {
        Self { tokens, index }
    }

    #[inline]
    pub fn is_eof(&self) -> bool {
        self.peek() == TokenKind::EOF
    }

    #[inline]
    pub fn peek(&self) -> TokenKind {
        let mut idx = self.index;
        while idx < self.tokens.len() {
            match self.tokens[idx].kind {
                TokenKind::LineComment | TokenKind::DocComment => idx += 1,
                kind => return kind,
            }
        }
        TokenKind::EOF
    }

    #[inline]
    pub fn peek_token(&self) -> Token<'a> {
        let mut idx = self.index;
        while idx < self.tokens.len() {
            match self.tokens[idx].kind {
                TokenKind::LineComment | TokenKind::DocComment => idx += 1,
                _ => return self.tokens[idx],
            }
        }
        Token {
            kind: TokenKind::EOF,
            text: None,
            span: Default::default(),
        }
    }

    #[inline]
    pub fn peek_n(&self, n: usize) -> TokenKind {
        self.tokens[self.index + n].kind
    }

    #[inline]
    pub fn advance(&mut self) {
        self.index += 1;
        // Skip over any comments that follow
        while self.index < self.tokens.len() {
            match self.tokens[self.index].kind {
                TokenKind::LineComment | TokenKind::DocComment => self.index += 1,
                _ => break,
            }
        }
    }

    #[inline]
    pub fn index(&self) -> usize {
        self.index
    }

    #[inline]
    pub fn set_index(&mut self, index: usize) {
        self.index = index;
    }

    #[inline]
    pub fn bump(&mut self) -> Token<'a> {
        let tok = self.peek_token();
        self.advance();
        tok
    }

    #[inline]
    pub fn consume(&mut self, kind: TokenKind) -> bool {
        if self.peek() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>, ParserError<'a>> {
        if self.peek() == kind {
            Ok(self.bump())
        } else {
            let tok = self.peek_token();
            Err(ParserError::UnexpectedToken {
                expected: kind,
                found: tok.kind,
                span: tok.span,
            })
        }
    }

    #[inline]
    pub fn expect_ident(&mut self) -> Result<(StrId, SourceSpan<'a>), ParserError<'a>> {
        self.skip_comments();
        let tok = self.peek_token();
        if tok.kind == TokenKind::Ident {
            self.advance();
            Ok((tok.text.expect("ident must have text"), tok.span))
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Ident,
                found: tok.kind,
                span: tok.span,
            })
        }
    }

    /// Skip over comment tokens (LineComment and DocComment)
    #[inline]
    pub fn skip_comments(&mut self) {
        loop {
            match self.peek() {
                TokenKind::LineComment | TokenKind::DocComment => {
                    self.advance();
                }
                _ => break,
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    // ===== Identifiers & Literals =====
    Ident,       // user identifiers
    Number,      // numeric literals
    Decimal,     // decimal literals
    Hexadecimal, // hexadecimal literals
    Binary,      // binary literals
    String,      // string literals
    BooleanTrue,
    BooleanFalse,
    Null,

    // ===== Keywords =====
    If,
    Else,
    While,
    For,
    In,
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
    Unsafe,
    Inline,
    Noinline,
    Sealed,
    Private,
    Module,
    Extern,
    Static,
    Match,
    Case,
    Defer,
    Effect,
    Permits,
    Statem,
    Trait,
    Where,
    Uses,
    Requires,
    Ensures,
    Let,
    Void,
    Fn,

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
    Usize,
    Isize,
    Char,
    Str,
    Boolean,

    // ===== Punctuation =====
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Dot,       // .
    Semicolon, // ;
    Colon,     // :
    Question,  // ?
    Arrow,     // ->
    FatArrow,  // =>
    Ellipsis,  // ...
    DotDot,    // ..
    DotDotLt,  // ..<

    // ===== Operators =====
    Assign,            // =
    ColonAssign,       // :=
    AddAssign,         // +=
    SubAssign,         // -=
    MulAssign,         // *=
    DivAssign,         // /=
    ModAssign,         // %=
    ShlAssign,         // <<=
    ShrAssign,         // >>=
    UnsignedShrAssign, // >>>=
    NotAssign,         // ~=
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
    AndAnd,     // &&
    OrOr,       // ||
    LogicalNot, // !

    // ===== Other special tokens =====
    This,
    Underscore, // _

    // ===== Comments =====
    LineComment, // // ...
    DocComment,  // /// ...

    EOF,
    Unknown,
    Internal,
    ColonColon,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;

        let s = match self {
            // ===== Identifiers & Literals =====
            Ident => "ident",
            Number => "number",
            Decimal => "decimal",
            Hexadecimal => "hexadecimal",
            Binary => "binary",
            String => "string",
            BooleanTrue => "true",
            BooleanFalse => "false",
            Null => "null",

            // ===== Keywords =====
            If => "if",
            Else => "else",
            While => "while",
            For => "for",
            In => "in",
            Return => "return",
            Break => "break",
            Continue => "continue",
            Enum => "enum",
            Struct => "struct",
            Interface => "interface",
            Impl => "impl",
            Import => "import",
            Package => "package",
            Type => "type",
            Const => "const",
            Mut => "mut",
            Unsafe => "unsafe",
            Inline => "inline",
            Noinline => "noinline",
            Sealed => "sealed",
            Private => "private",
            Module => "module",
            Extern => "extern",
            Static => "static",
            Match => "match",
            Case => "case",
            Defer => "defer",
            Effect => "effect",
            Permits => "permits",

            Statem => "statem",
            Trait => "trait",
            Where => "where",
            Uses => "uses",
            Requires => "requires",
            Ensures => "ensures",
            Let => "let",
            Void => "void",
            Fn => "fn",

            // ===== Types =====
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            U128 => "u128",
            I8 => "i8",
            I16 => "i16",
            I32 => "i32",
            I64 => "i64",
            I128 => "i128",
            F32 => "f32",
            F64 => "f64",
            Usize => "usize",
            Isize => "isize",
            Char => "char",
            Str => "str",
            Boolean => "boolean",

            // ===== Punctuation =====
            LParen => "(",
            RParen => ")",
            LBrace => "{",
            RBrace => "}",
            LBracket => "[",
            RBracket => "]",
            Comma => ",",
            Dot => ".",
            Semicolon => ";",
            Colon => ":",
            Question => "?",
            Arrow => "->",
            FatArrow => "=>",
            Ellipsis => "...",
            DotDot => "..",
            DotDotLt => "..<",

            // ===== Operators =====
            Assign => "=",
            AddAssign => "+=",
            SubAssign => "-=",
            MulAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            ShlAssign => "<<=",
            ShrAssign => ">>=",
            UnsignedShrAssign => ">>>=",
            AndAssign => "&=",
            OrAssign => "|=",
            XorAssign => "^=",
            Eq => "==",
            Ne => "~=",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            BitNot => "~",
            BitAnd => "&",
            BitOr => "|",
            BitXor => "^",
            Shl => "<<",
            Shr => ">>",
            UnsignedShr => ">>>",

            // ===== Logic Operators =====
            AndAnd => "&&",
            OrOr => "||",
            LogicalNot => "!",

            // ===== Other =====
            This => "this",
            Underscore => "_",
            ColonAssign => "::=", // as requested

            // ===== Comments =====
            LineComment => "//",
            DocComment => "///",

            // ===== Special =====
            EOF => "eof",
            Unknown => "unknown",
            NotAssign => "!=",
            Internal => "internal",
            ColonColon => "::",
        };

        f.write_str(s)
    }
}
