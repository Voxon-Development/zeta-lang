use crate::errors::error::{DiagnosticError, ParseErrorKind};
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

    /// Returns the kind of the next non-comment token.
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

    /// Returns the next non-comment token (without advancing).
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

    /// Peek at the nth token from the current position, skipping comments.
    ///
    /// `n = 0` is equivalent to `peek()`.
    #[inline]
    pub fn peek_n(&self, n: usize) -> TokenKind {
        let mut idx = self.index;
        let mut skipped = 0;
        while idx < self.tokens.len() {
            match self.tokens[idx].kind {
                TokenKind::LineComment | TokenKind::DocComment => idx += 1,
                kind => {
                    if skipped == n {
                        return kind;
                    }
                    skipped += 1;
                    idx += 1;
                }
            }
        }
        TokenKind::EOF
    }

    /// Advance past the current non-comment token and any trailing comments.
    #[inline]
    pub fn advance(&mut self) {
        // First, skip any comments we're currently sitting on
        while self.index < self.tokens.len() {
            match self.tokens[self.index].kind {
                TokenKind::LineComment | TokenKind::DocComment => self.index += 1,
                _ => break,
            }
        }
        // Now move past the actual token
        if self.index < self.tokens.len() {
            self.index += 1;
        }
        // Skip any comments that follow
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

    /// Return the current non-comment token and advance past it.
    #[inline]
    pub fn bump(&mut self) -> Token<'a> {
        let tok = self.peek_token();
        self.advance();
        tok
    }

    /// Advance and return `true` iff the next non-comment token matches `kind`.
    #[inline]
    pub fn consume(&mut self, kind: TokenKind) -> bool {
        if self.peek() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Advance past `kind` and return the consumed token, or return a
    /// `DiagnosticError` if the next token does not match.
    #[inline]
    pub fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>, DiagnosticError<'a>> {
        let tok = self.peek_token();
        if tok.kind == kind {
            self.advance();
            Ok(tok)
        } else {
            Err(DiagnosticError {
                kind: if tok.kind == TokenKind::EOF {
                    ParseErrorKind::UnexpectedEOF {
                        expected: Some(kind),
                    }
                } else {
                    ParseErrorKind::UnexpectedToken {
                        expected: kind,
                        found: tok.kind,
                    }
                },
                span: tok.span,
                context: vec![],
                notes: vec![],
            })
        }
    }

    /// Advance past `kind` and return the consumed token, or return a
    /// `DiagnosticError` if the next token does not match.
    #[inline]
    pub fn expect_or(
        &mut self,
        kind: TokenKind,
        or: TokenKind,
    ) -> Result<Token<'a>, DiagnosticError<'a>> {
        let tok = self.peek_token();
        if tok.kind == kind || tok.kind == or {
            self.advance();
            Ok(tok)
        } else {
            Err(DiagnosticError {
                kind: if tok.kind == TokenKind::EOF {
                    ParseErrorKind::UnexpectedEOF {
                        expected: Some(kind),
                    }
                } else {
                    ParseErrorKind::UnexpectedTokens {
                        expected: vec![kind, or],
                        found: tok.kind,
                    }
                },
                span: tok.span,
                context: vec![],
                notes: vec![],
            })
        }
    }

    /// Advance past an identifier token and return `(StrId, SourceSpan)`, or
    /// return a `DiagnosticError` if the next token is not an identifier.
    #[inline]
    pub fn expect_ident(&mut self) -> Result<(StrId, SourceSpan<'a>), DiagnosticError<'a>> {
        let tok = self.peek_token();
        match tok.kind {
            TokenKind::Ident => {
                let text = match tok.text {
                    Some(t) if !t.is_empty() => t,
                    _ => {
                        return Err(DiagnosticError {
                            kind: ParseErrorKind::EmptyIdent,
                            span: tok.span,
                            context: vec![],
                            notes: vec![],
                        });
                    }
                };
                self.advance();
                Ok((text, tok.span))
            }
            TokenKind::EOF => Err(DiagnosticError {
                kind: ParseErrorKind::UnexpectedEOF {
                    expected: Some(TokenKind::Ident),
                },
                span: tok.span,
                context: vec![],
                notes: vec![],
            }),
            _ => Err(DiagnosticError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Ident,
                    found: tok.kind,
                },
                span: tok.span,
                context: vec![],
                notes: vec![],
            }),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    // ===== Identifiers & Literals =====
    Ident,
    Number,
    Decimal,
    Hexadecimal,
    Binary,
    String,
    BooleanTrue,
    BooleanFalse,
    Null,

    // ===== Keywords =====
    Comptime,
    If,
    Else,
    While,
    For,
    By,
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
    Catch,
    Void,
    Fn,
    Internal,
    Throws,
    Throw,
    Suspend,
    Nosuspend,
    Blocking,
    Await,
    Dyn,
    Undefined,

    // ===== Types =====
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
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    Comma,      // ,
    Dot,        // .
    Semicolon,  // ;
    Colon,      // :
    Question,   // ?
    Arrow,      // ->
    FatArrow,   // =>
    Ellipsis,   // ...
    DotDot,     // ..
    DotDotLt,   // ..<
    ColonColon, // ::

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
    Ne,                // !=
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

    // ===== Other =====
    This,
    Underscore, // _

    // ===== Comments =====
    LineComment, // ///
    DocComment,  // //

    // ===== Special =====
    EOF,
    Unknown,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        let s = match self {
            Ident => "ident",
            Number => "number",
            Decimal => "decimal",
            Hexadecimal => "hexadecimal",
            Binary => "binary",
            String => "string",
            BooleanTrue => "true",
            BooleanFalse => "false",
            Null => "null",

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
            Dyn => "dyn",

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

            Assign => "=",
            ColonAssign => ":=",
            AddAssign => "+=",
            SubAssign => "-=",
            MulAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            ShlAssign => "<<=",
            ShrAssign => ">>=",
            UnsignedShrAssign => ">>>=",
            NotAssign => "!=",
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

            AndAnd => "&&",
            OrOr => "||",
            LogicalNot => "!",

            This => "this",
            Underscore => "_",

            LineComment => "//",
            DocComment => "///",

            EOF => "eof",
            Unknown => "unknown",
            Internal => "internal",
            ColonColon => "::",
            Throws => "throws",
            Throw => "throw",
            Comptime => "comptime",
            Suspend => "suspend",
            Nosuspend => "nosuspend",
            Blocking => "blocking",
            Await => "await",
            By => "by",
            Catch => "catch",
            Undefined => "undefined",
        };
        f.write_str(s)
    }
}
