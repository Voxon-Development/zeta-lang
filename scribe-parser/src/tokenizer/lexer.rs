use ir::hir::StrId;
use ir::span::SourceSpan;
use ir::tokens::{Token, TokenKind, Tokens};
use smallvec::SmallVec;
use std::fs::File;
use std::io::{self, Error, ErrorKind, Read};
use std::sync::Arc;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

// Maps ASCII bytes 0..128 to a dispatch category.
// Non-ASCII bytes (>= 128) are handled as potential identifier starts.

#[derive(Clone, Copy)]
#[repr(u8)]
enum ByteClass {
    Whitespace, // space, \t, \r
    Newline,    // \n
    Alpha,      // a-z A-Z _
    Digit,      // 0-9
    Quote,      // "
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    Lt,         // <
    Gt,         // >
    Amp,        // &
    Pipe,       // |
    Semi,       // ;
    Colon,      // :
    Tilde,      // ~
    Bang,       // !
    Eq,         // =
    Dot,        // .
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    Comma,      // ,
    Question,   // ?
    Unknown,
}

const JUMP: [ByteClass; 128] = {
    let mut t = [ByteClass::Unknown; 128];
    // whitespace
    t[b' ' as usize] = ByteClass::Whitespace;
    t[b'\t' as usize] = ByteClass::Whitespace;
    t[b'\r' as usize] = ByteClass::Whitespace;
    t[b'\n' as usize] = ByteClass::Newline;
    // alpha + underscore
    let mut i = b'a' as usize;
    while i <= b'z' as usize {
        t[i] = ByteClass::Alpha;
        i += 1;
    }
    i = b'A' as usize;
    while i <= b'Z' as usize {
        t[i] = ByteClass::Alpha;
        i += 1;
    }
    t[b'_' as usize] = ByteClass::Alpha;
    // digits
    i = b'0' as usize;
    while i <= b'9' as usize {
        t[i] = ByteClass::Digit;
        i += 1;
    }
    // single-char punctuation
    t[b'"' as usize] = ByteClass::Quote;
    t[b'+' as usize] = ByteClass::Plus;
    t[b'-' as usize] = ByteClass::Minus;
    t[b'*' as usize] = ByteClass::Star;
    t[b'/' as usize] = ByteClass::Slash;
    t[b'%' as usize] = ByteClass::Percent;
    t[b'^' as usize] = ByteClass::Caret;
    t[b'<' as usize] = ByteClass::Lt;
    t[b'>' as usize] = ByteClass::Gt;
    t[b'&' as usize] = ByteClass::Amp;
    t[b'|' as usize] = ByteClass::Pipe;
    t[b';' as usize] = ByteClass::Semi;
    t[b':' as usize] = ByteClass::Colon;
    t[b'~' as usize] = ByteClass::Tilde;
    t[b'!' as usize] = ByteClass::Bang;
    t[b'=' as usize] = ByteClass::Eq;
    t[b'.' as usize] = ByteClass::Dot;
    t[b'(' as usize] = ByteClass::LParen;
    t[b')' as usize] = ByteClass::RParen;
    t[b'[' as usize] = ByteClass::LBracket;
    t[b']' as usize] = ByteClass::RBracket;
    t[b'{' as usize] = ByteClass::LBrace;
    t[b'}' as usize] = ByteClass::RBrace;
    t[b',' as usize] = ByteClass::Comma;
    t[b'?' as usize] = ByteClass::Question;
    t
};

#[inline(always)]
fn byte_class(b: u8) -> ByteClass {
    if b < 128 {
        JUMP[b as usize]
    } else {
        ByteClass::Alpha
    }
}

pub struct Lexer {
    context: Arc<StringPool>,
}

impl Lexer {
    pub fn new(context: Arc<StringPool>) -> Self {
        Self { context }
    }

    pub fn tokenize_file<'a>(
        &self,
        file_name: &'a str,
        bump: &'a GrowableBump<'a>,
    ) -> std::io::Result<Tokens<'a>> {
        let mut file = File::open(file_name)?;
        let len = file.metadata().map(|m| m.len() as usize).unwrap_or(0);
        let buf: &'a mut [u8] = bump.alloc_bytes(len);

        let bytes_read = retry_on_interrupt(|| file.read(buf))?;
        let src = std::str::from_utf8(&buf[..bytes_read])
            .map_err(|_| Error::new(ErrorKind::InvalidData, "Invalid UTF-8"))?;
        Ok(self.tokenize(src, file_name, bump))
    }

    pub fn tokenize<'a, 'bump>(
        &self,
        src: &str,
        file_name: &'a str,
        bump: &'bump GrowableBump<'bump>,
    ) -> Tokens<'bump>
    where
        'a: 'bump,
    {
        let bytes = src.as_bytes();
        let len = bytes.len();
        #[allow(unused_mut)] // Used by macros like `push`
        let mut tokens: Vec<Token<'bump>> = Vec::with_capacity(len / 4);

        let mut pos = 0usize; // byte offset into src
        let mut line = 1usize;
        let mut column = 1usize;

        macro_rules! span {
            () => {
                SourceSpan::new(file_name, line, column)
            };
        }

        macro_rules! push {
            ($kind:expr) => {
                tokens.push(Token {
                    kind: $kind,
                    text: None,
                    span: span!(),
                });
            };
            ($kind:expr, $id:expr) => {
                tokens.push(Token {
                    kind: $kind,
                    text: Some($id),
                    span: span!(),
                });
            };
        }

        macro_rules! peek {
            ($offset:expr) => {
                if pos + $offset < len {
                    bytes[pos + $offset]
                } else {
                    0
                }
            };
        }

        while pos < len {
            let b = bytes[pos];

            match byte_class(b) {
                ByteClass::Whitespace => {
                    pos += 1;
                    column += 1;
                }

                ByteClass::Newline => {
                    pos += 1;
                    line += 1;
                    column = 1;
                }

                ByteClass::Alpha => {
                    let start = pos;
                    while pos < len && {
                        let c = bytes[pos];
                        c.is_ascii_alphanumeric() || c == b'_'
                    } {
                        pos += 1;
                    }
                    let text = &src[start..pos];
                    column += pos - start;

                    let kind = keyword_or_ident(text);
                    if kind == TokenKind::Ident {
                        let id = self.context.intern_bytes(text.as_bytes());
                        push!(TokenKind::Ident, StrId(id));
                    } else {
                        push!(kind);
                    }
                }

                ByteClass::Digit => {
                    let (kind, id) = lex_number(src, bytes, &mut pos, &mut column, &self.context);
                    push!(kind, id);
                }

                ByteClass::Quote => {
                    pos += 1; // consume opening "
                    column += 1;
                    let id = lex_string(src, bytes, &mut pos, &mut column, &self.context);
                    push!(TokenKind::String, id);
                }

                ByteClass::Plus => {
                    pos += 1;
                    column += 1;
                    if peek!(0) == b'=' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::AddAssign);
                    } else {
                        push!(TokenKind::Add);
                    }
                }

                ByteClass::Minus => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::SubAssign);
                        }
                        b'>' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::Arrow);
                        }
                        _ => {
                            push!(TokenKind::Sub);
                        }
                    }
                }

                ByteClass::Star => {
                    pos += 1;
                    column += 1;
                    if peek!(0) == b'=' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::MulAssign);
                    } else {
                        push!(TokenKind::Mul);
                    }
                }

                ByteClass::Slash => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'/' => {
                            pos += 1;
                            column += 1;
                            let is_doc = peek!(0) == b'/';
                            if is_doc {
                                pos += 1;
                                column += 1;
                            }
                            let id =
                                lex_line_comment(src, bytes, &mut pos, &mut column, &self.context);
                            let kind = if is_doc {
                                TokenKind::DocComment
                            } else {
                                TokenKind::LineComment
                            };
                            push!(kind, id);
                        }
                        b'*' => panic!("Block comments are not supported"),
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::DivAssign);
                        }
                        _ => {
                            push!(TokenKind::Div);
                        }
                    }
                }

                ByteClass::Percent => {
                    pos += 1;
                    column += 1;
                    if peek!(0) == b'=' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::ModAssign);
                    } else {
                        push!(TokenKind::Mod);
                    }
                }

                ByteClass::Caret => {
                    pos += 1;
                    column += 1;
                    if peek!(0) == b'=' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::XorAssign);
                    } else {
                        push!(TokenKind::BitXor);
                    }
                }

                ByteClass::Amp => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'&' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::AndAnd);
                        }
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::AndAssign);
                        }
                        _ => {
                            push!(TokenKind::BitAnd);
                        }
                    }
                }

                ByteClass::Pipe => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'|' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::OrOr);
                        }
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::OrAssign);
                        }
                        _ => {
                            push!(TokenKind::BitOr);
                        }
                    }
                }

                ByteClass::Lt => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'<' => {
                            pos += 1;
                            column += 1;
                            if peek!(0) == b'=' {
                                pos += 1;
                                column += 1;
                                push!(TokenKind::ShlAssign);
                            } else {
                                push!(TokenKind::Shl);
                            }
                        }
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::Le);
                        }
                        _ => {
                            push!(TokenKind::Lt);
                        }
                    }
                }

                ByteClass::Gt => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'>' => {
                            pos += 1;
                            column += 1;
                            match peek!(0) {
                                b'>' => {
                                    pos += 1;
                                    column += 1;
                                    if peek!(0) == b'=' {
                                        pos += 1;
                                        column += 1;
                                        push!(TokenKind::UnsignedShrAssign);
                                    } else {
                                        push!(TokenKind::UnsignedShr);
                                    }
                                }
                                b'=' => {
                                    pos += 1;
                                    column += 1;
                                    push!(TokenKind::ShrAssign);
                                }
                                _ => {
                                    push!(TokenKind::Shr);
                                }
                            }
                        }
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::Ge);
                        }
                        _ => {
                            push!(TokenKind::Gt);
                        }
                    }
                }

                ByteClass::Semi => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::Semicolon);
                }
                ByteClass::LParen => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::LParen);
                }
                ByteClass::RParen => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::RParen);
                }
                ByteClass::LBracket => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::LBracket);
                }
                ByteClass::RBracket => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::RBracket);
                }
                ByteClass::LBrace => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::LBrace);
                }
                ByteClass::RBrace => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::RBrace);
                }
                ByteClass::Comma => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::Comma);
                }
                ByteClass::Question => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::Question);
                }

                ByteClass::Colon => {
                    pos += 1;
                    column += 1;
                    let peeked = peek!(0);
                    if peeked == b'=' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::ColonAssign);
                    } else if peeked == b':' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::ColonColon);
                    } else {
                        push!(TokenKind::Colon);
                    }
                }

                ByteClass::Tilde => {
                    pos += 1;
                    column += 1;
                    if peek!(0) == b'=' {
                        pos += 1;
                        column += 1;
                        push!(TokenKind::NotAssign);
                    } else {
                        push!(TokenKind::BitNot);
                    }
                }

                ByteClass::Bang => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::LogicalNot);
                }

                ByteClass::Eq => {
                    pos += 1;
                    column += 1;
                    match peek!(0) {
                        b'=' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::Eq);
                        }
                        b'>' => {
                            pos += 1;
                            column += 1;
                            push!(TokenKind::FatArrow);
                        }
                        _ => {
                            push!(TokenKind::Assign);
                        }
                    }
                }

                ByteClass::Dot => {
                    pos += 1;
                    column += 1;
                    if peek!(0) == b'.' {
                        pos += 1;
                        column += 1;
                        match peek!(0) {
                            b'<' => {
                                pos += 1;
                                column += 1;
                                push!(TokenKind::DotDotLt);
                            }
                            b'.' => {
                                pos += 1;
                                column += 1;
                                push!(TokenKind::Ellipsis);
                            }
                            _ => {
                                push!(TokenKind::DotDot);
                            }
                        }
                    } else {
                        push!(TokenKind::Dot);
                    }
                }

                ByteClass::Unknown => {
                    pos += 1;
                    column += 1;
                    push!(TokenKind::Unknown);
                }
            }
        }

        push!(TokenKind::EOF);
        Tokens::new(bump, tokens)
    }
}

fn retry_on_interrupt<T, F: FnMut() -> io::Result<T>>(mut f: F) -> io::Result<T> {
    loop {
        match f() {
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {
                // Interrupt means that some sort of signal interrupted the syscall.
                // it may be retried for zeta compiler stability reasons
                // Can this just be replaced by using async I/O?
                // TODO: evaluate if async I/O will actually benefit the zeta compiler long term.
                continue;
            }
            other => return other,
        }
    }
}

#[inline(never)] // large match arm, keep out of hot loop
fn keyword_or_ident(text: &str) -> TokenKind {
    match text {
        // Special case here, not an ident or a keyword but best solution (for now)
        // Is to put this here, just to ease the mind.
        "_" => TokenKind::Underscore,
        "true" => TokenKind::BooleanTrue,
        "false" => TokenKind::BooleanFalse,
        "null" => TokenKind::Null,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "while" => TokenKind::While,
        "for" => TokenKind::For,
        "by" => TokenKind::By,
        "in" => TokenKind::In,
        "return" => TokenKind::Return,
        "break" => TokenKind::Break,
        "continue" => TokenKind::Continue,
        "enum" => TokenKind::Enum,
        "struct" => TokenKind::Struct,
        "interface" => TokenKind::Interface,
        "impl" => TokenKind::Impl,
        "import" => TokenKind::Import,
        "package" => TokenKind::Package,
        "type" => TokenKind::Type,
        "const" => TokenKind::Const,
        "let" => TokenKind::Let,
        "mut" => TokenKind::Mut,
        "match" => TokenKind::Match,
        "case" => TokenKind::Case,
        "defer" => TokenKind::Defer,
        "unsafe" => TokenKind::Unsafe,
        "inline" => TokenKind::Inline,
        "noinline" => TokenKind::Noinline,
        "dyn" => TokenKind::Dyn,
        "sealed" => TokenKind::Sealed,
        "private" => TokenKind::Private,
        "module" => TokenKind::Module,
        "internal" => TokenKind::Internal,
        "comptime" => TokenKind::Comptime,
        "throws" => TokenKind::Throws,
        "throw" => TokenKind::Throw,
        "suspend" => TokenKind::Suspend,
        "nosuspend" => TokenKind::Nosuspend,
        "blocking" => TokenKind::Blocking,
        "await" => TokenKind::Await,
        "extern" => TokenKind::Extern,
        "static" => TokenKind::Static,
        "effect" => TokenKind::Effect,
        "permits" => TokenKind::Permits,
        "statem" => TokenKind::Statem,
        "trait" => TokenKind::Trait,
        "where" => TokenKind::Where,
        "fn" => TokenKind::Fn,
        "catch" => TokenKind::Catch,
        "requires" => TokenKind::Requires,
        "ensures" => TokenKind::Ensures,
        "uses" => TokenKind::Uses,
        "this" => TokenKind::This,
        "u8" => TokenKind::U8,
        "u16" => TokenKind::U16,
        "u32" => TokenKind::U32,
        "u64" => TokenKind::U64,
        "u128" => TokenKind::U128,
        "i8" => TokenKind::I8,
        "i16" => TokenKind::I16,
        "i32" => TokenKind::I32,
        "i64" => TokenKind::I64,
        "i128" => TokenKind::I128,
        "f32" => TokenKind::F32,
        "f64" => TokenKind::F64,
        "usize" => TokenKind::Usize,
        "isize" => TokenKind::Isize,
        "char" => TokenKind::Char,
        "str" => TokenKind::Str,
        "boolean" => TokenKind::Boolean,
        _ => TokenKind::Ident,
    }
}

fn lex_number<'a>(
    src: &str,
    bytes: &[u8],
    pos: &mut usize,
    column: &mut usize,
    ctx: &Arc<StringPool>,
) -> (TokenKind, StrId) {
    let start = *pos;
    let mut kind = TokenKind::Number;
    let mut seen_dot = false;
    let mut seen_exp = false;
    let mut last_under = false;

    // Base prefix
    if bytes[start] == b'0' {
        match bytes.get(start + 1).copied() {
            Some(b'x') | Some(b'X') => {
                *pos += 2;
                *column += 2;
                consume_digits_while(bytes, pos, column, |c| c.is_ascii_hexdigit());
                consume_suffix(bytes, pos, column);
                return finish_number(src, start, *pos, TokenKind::Hexadecimal, ctx);
            }
            Some(b'b') | Some(b'B') => {
                *pos += 2;
                *column += 2;
                consume_digits_while(bytes, pos, column, |c| matches!(c, b'0' | b'1'));
                consume_suffix(bytes, pos, column);
                return finish_number(src, start, *pos, TokenKind::Binary, ctx);
            }
            _ => {}
        }
    }

    *pos += 1;
    *column += 1; // consume first digit

    loop {
        match bytes.get(*pos).copied() {
            Some(c @ b'0'..=b'9') => {
                *pos += 1;
                *column += 1;
                last_under = false;
                let _ = c;
            }
            Some(b'_') if !last_under => {
                *pos += 1;
                *column += 1;
                last_under = true;
            }
            Some(b'.') if !seen_dot && !seen_exp => {
                // don't consume ".." as float
                if bytes.get(*pos + 1).copied() == Some(b'.') {
                    break;
                }
                *pos += 1;
                *column += 1;
                seen_dot = true;
                kind = TokenKind::Decimal;
                last_under = false;
            }
            Some(b'e') | Some(b'E') if !seen_exp => {
                *pos += 1;
                *column += 1;
                seen_exp = true;
                kind = TokenKind::Decimal;
                last_under = false;
                if matches!(bytes.get(*pos).copied(), Some(b'+') | Some(b'-')) {
                    *pos += 1;
                    *column += 1;
                }
            }
            _ => break,
        }
    }

    // strip trailing underscore
    if last_under {
        *pos -= 1;
        *column -= 1;
    }

    consume_suffix(bytes, pos, column);
    finish_number(src, start, *pos, kind, ctx)
}

#[inline]
fn consume_digits_while(
    bytes: &[u8],
    pos: &mut usize,
    column: &mut usize,
    mut valid: impl FnMut(u8) -> bool,
) {
    let mut last_under = false;
    loop {
        match bytes.get(*pos).copied() {
            Some(c) if valid(c) => {
                *pos += 1;
                *column += 1;
                last_under = false;
            }
            Some(b'_') if !last_under => {
                *pos += 1;
                *column += 1;
                last_under = true;
            }
            _ => break,
        }
    }
    if last_under {
        *pos -= 1;
        *column -= 1;
    }
}

#[inline]
fn consume_suffix(bytes: &[u8], pos: &mut usize, column: &mut usize) {
    if bytes.get(*pos).map_or(false, |b| b.is_ascii_alphabetic()) {
        while bytes.get(*pos).map_or(false, |b| b.is_ascii_alphanumeric()) {
            *pos += 1;
            *column += 1;
        }
    }
}

#[inline]
fn finish_number(
    src: &str,
    start: usize,
    end: usize,
    kind: TokenKind,
    ctx: &Arc<StringPool>,
) -> (TokenKind, StrId) {
    let id = ctx.intern_bytes(src[start..end].as_bytes());
    (kind, StrId(id))
}

fn lex_string(
    _src: &str,
    bytes: &[u8],
    pos: &mut usize,
    column: &mut usize,
    ctx: &Arc<StringPool>,
) -> StrId {
    let mut text: SmallVec<u8, 32> = SmallVec::new();
    while *pos < bytes.len() {
        let b = bytes[*pos];
        *pos += 1;
        *column += 1;
        match b {
            b'"' => break,
            b'\\' => {
                let esc = bytes[*pos];
                *pos += 1;
                *column += 1;
                text.push(match esc {
                    b'n' => b'\n',
                    b't' => b'\t',
                    b'r' => b'\r',
                    b'\\' => b'\\',
                    b'"' => b'"',
                    other => other,
                });
            }
            other => text.push(other),
        }
    }
    StrId(ctx.intern_bytes(&text))
}

fn lex_line_comment(
    _src: &str,
    bytes: &[u8],
    pos: &mut usize,
    column: &mut usize,
    ctx: &Arc<StringPool>,
) -> StrId {
    let start = *pos;
    while *pos < bytes.len() && bytes[*pos] != b'\n' {
        *pos += 1;
        *column += 1;
    }
    // safe: comments are ASCII or valid UTF-8 slices of the original source
    StrId(ctx.intern_bytes(&bytes[start..*pos]))
}
