use std::fs::File;
use std::io::{Error, ErrorKind, Read};
use std::iter::Peekable;
use std::str::Chars;
use std::sync::Arc;
use smallvec::SmallVec;
use ir::hir::StrId;
use ir::span::SourceSpan;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;
use crate::tokenizer::tokens::{TokenKind, Tokens};

pub struct Lexer<'a, 'bump> {
    pub tokens: Tokens<'a>,
    context: Arc<StringPool>,
    file_name: &'a str,
    characters: &'bump str,

    #[allow(dead_code)] // If we don't move bump in Lexer, it will UB (lifetime hacks make life easier but the compiler trusts it too much).
    bump: GrowableBump<'bump>,

    pos: usize,
    line: usize,
    column: usize,
}

impl<'a, 'bump> Lexer<'a, 'bump> {
    pub fn new(context: Arc<StringPool>, file_name: &'a str) -> std::io::Result<Self> {
        let mut file = File::open(file_name)?;
        let len = file.metadata().map(|m| m.len() as usize).unwrap_or(0);

        let bump: GrowableBump<'bump> = GrowableBump::new(len, 8);
        let buf: &'bump mut [u8] = bump.alloc_bytes(len);
        let bytes_read = file.read(buf)?;
        let slice = &buf[..bytes_read];

        let string: &'bump str = std::str::from_utf8(slice).map_err(|_| Error::new(ErrorKind::InvalidData, "Invalid UTF-8"))?;

        Ok(Lexer {
            tokens: Tokens::new(),
            context,
            file_name,
            characters: string,
            bump,
            pos: 0,
            line: 1,
            column: 1,
        })
    }

    /// Create a lexer from a string source
    pub fn from_str(src: &'bump str, file_name: &'a str, context: Arc<StringPool>) -> Self {
        let bump: GrowableBump = GrowableBump::new(1024, 8);

        Lexer {
            tokens: Tokens::new(),
            context,
            file_name,
            characters: src,
            bump,
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(mut self) -> Tokens<'a> {
        let mut chars: Peekable<Chars> = self.characters.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch.is_whitespace() {
                continue;
            }

            let len = ch.len_utf8();
            self.pos += len;

            if ch.is_alphabetic() {
                self.tokenize_alphabetic(ch, &mut chars);
                continue;
            }

            if ch.is_numeric() {
                self.tokenize_number(ch, &mut chars);
                continue;
            }

            if ch == '"' {
                self.pos += 1;
                self.tokenize_string(&mut chars);
                continue;
            }

            match ch {
                '+' => {
                    self.detect_assign_or_append(&mut chars, TokenKind::AddAssign, TokenKind::Add);
                }
                '-' => {
                    if let Some(character) = chars.peek() {
                        if *character == '=' {
                            self.pos += 1;
                            self.column += 1;
                            self.push_token(TokenKind::SubAssign);
                        } else if *character == '>' {
                            self.push_token(TokenKind::Arrow);
                        }
                    }
                    self.push_token(TokenKind::Sub);
                }
                '*' => {
                    self.detect_assign_or_append(&mut chars, TokenKind::MulAssign, TokenKind::Mul);
                }
                '/' => {
                    match chars.peek() {
                        Some('/') => {
                            chars.next(); // consume second '/'
                            self.pos += 1;

                            let is_doc = matches!(chars.peek(), Some('/'));
                            if is_doc {
                                chars.next(); // consume third '/'
                                self.pos += 1;
                            }

                            self.tokenize_line_comment(&mut chars, is_doc);
                            continue;
                        }

                        Some('*') => {
                            chars.next(); // consume '*'
                            self.pos += 1;
                            panic!("Block comments are not supported");
                            //continue;
                        }

                        _ => {
                            self.detect_assign_or_append(&mut chars, TokenKind::DivAssign, TokenKind::Div);
                        }
                    }
                }
                '%' => {
                    self.detect_assign_or_append(&mut chars, TokenKind::ModAssign, TokenKind::Mod);
                }
                '^' => {
                    self.detect_assign_or_append(&mut chars, TokenKind::XorAssign, TokenKind::BitXor);
                }
                '<' => {
                    self.process_left_arrow(&mut chars);
                }
                '>' => {
                    self.process_right_arrow(&mut chars);
                }
                '&' => {
                    self.process_and(&mut chars);
                }
                '|' => {
                    self.process_or(&mut chars);
                }
                ';' => {
                    self.push_token(TokenKind::Semicolon);
                }
                ':' => {
                    if let Some(character) = chars.peek() {
                        if *character == '=' {
                            self.pos += 1;
                            self.column += 1;
                            self.push_token(TokenKind::ColonAssign);
                        } else {
                            self.push_token(TokenKind::Colon);
                        }
                    } else {
                        self.push_token(TokenKind::Colon);
                    }
                }
                '~' => {
                    if let Some(character) = chars.peek() {
                        if *character == '=' {
                            self.pos += 1;
                            self.push_token(TokenKind::NotAssign);
                        }
                    }
                    self.push_token(TokenKind::BitNot);
                }
                ')' => {
                    self.push_token(TokenKind::RParen);
                }
                '(' => {
                    self.push_token(TokenKind::LParen);
                }
                '[' => {
                    self.push_token(TokenKind::LBracket);
                }
                ']' => {
                    self.push_token(TokenKind::RBracket);
                }
                '{' => {
                    self.push_token(TokenKind::LBrace);
                }
                '}' => {
                    self.push_token(TokenKind::RBrace);
                }
                ',' => {
                    self.push_token(TokenKind::Comma);
                }
                '.' => {
                    // Check for .. or ..<
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '.' {
                            chars.next(); // consume second dot
                            if let Some(&third_ch) = chars.peek() {
                                if third_ch == '<' {
                                    chars.next(); // consume '<'
                                    self.push_token(TokenKind::DotDotLt);
                                } else if third_ch == '.' {
                                    chars.next(); // consume third dot
                                    self.push_token(TokenKind::Ellipsis);
                                } else {
                                    self.push_token(TokenKind::DotDot);
                                }
                            } else {
                                self.push_token(TokenKind::DotDot);
                            }
                        } else {
                            self.push_token(TokenKind::Dot);
                        }
                    } else {
                        self.push_token(TokenKind::Dot);
                    }
                }
                '_' => {
                    // Check if it's a standalone underscore or part of identifier
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch.is_alphanumeric() || next_ch == '_' {
                            self.tokenize_alphabetic(ch, &mut chars);
                        } else {
                            self.push_token(TokenKind::Underscore);
                        }
                    } else {
                        self.push_token(TokenKind::Underscore);
                    }
                }
                '!' => {
                    self.push_token(TokenKind::LogicalNot);
                }
                '=' => {
                    if let Some(character) = chars.peek() {
                        if *character == '=' {
                            chars.next();
                            self.pos += 1;
                            self.push_token(TokenKind::Eq);
                            continue;
                        } else if *character == '>' {
                            chars.next();
                            self.pos += 1;
                            self.push_token(TokenKind::FatArrow);
                            continue;
                        }
                    }
                    self.push_token(TokenKind::Assign);
                }
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                }
                '\t' | '\r' => {
                    self.column += 1;
                }
                _ => {
                    self.push_token(TokenKind::Unknown);
                }
            }
            self.column += 1;
        }

        self.push_token(TokenKind::EOF);

        self.tokens
    }

    fn process_or(&mut self, chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some('|') => {
                let ch = chars.next().unwrap();
                self.pos += ch.len_utf8();
                self.column += 1;
                self.push_token(TokenKind::OrOr);
            }
            Some('=') => {
                let ch = chars.next().unwrap();
                self.pos += ch.len_utf8();
                self.column += 1;
                self.push_token(TokenKind::OrAssign);
            }
            _ => {
                self.push_token(TokenKind::BitOr);
            }
        }
    }

    fn process_and(&mut self, chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some('&') => {
                let ch = chars.next().unwrap();
                self.pos += ch.len_utf8();
                self.column += 1;
                self.push_token(TokenKind::AndAnd);
            }
            Some('=') => {
                let ch = chars.next().unwrap();
                self.pos += ch.len_utf8();
                self.column += 1;
                self.push_token(TokenKind::AndAssign);
            }
            _ => {
                self.push_token(TokenKind::BitAnd);
            }
        }
    }

    fn process_right_arrow(&mut self, chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some(character) if *character == '>' => {
                chars.next().unwrap();
                self.processs_shr(chars);
            }
            Some(character) if *character == '=' => {
                chars.next().unwrap();
                self.push_token(TokenKind::Ge);
            }
            Some(_) => {
                chars.next().unwrap();
                self.push_token(TokenKind::Gt);
            }
            None => {},
        }
    }

    fn processs_shr(&mut self, chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some(character) if *character == '=' => {
                chars.next().unwrap();
                self.push_token(TokenKind::ShrAssign);
            }
            Some(character) if *character == '>' => {
                self.process_unsigned_shr(chars);
            }
            Some(_) => {
                chars.next().unwrap();
                self.push_token(TokenKind::Shr);
            }
            None => {}
        }
    }

    fn process_unsigned_shr(&mut self, chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some(character) if *character == '=' => {
                chars.next().unwrap();
                self.push_token(TokenKind::UnsignedShrAssign);
            }
            _ => {
                chars.next().unwrap();
                self.push_token(TokenKind::UnsignedShr);
            }
        }
    }

    fn process_left_arrow(&mut self, mut chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some(character) if *character == '<' => {
                chars.next().unwrap();
                self.detect_assign_or_op(&mut chars);
            }
            Some(character) if *character == '=' => {
                chars.next().unwrap();
                self.push_token(TokenKind::Le);
            }
            Some(_) => {
                chars.next().unwrap();
                self.push_token(TokenKind::Lt);
            }
            None => {},
        }
    }

    fn detect_assign_or_op(&mut self, chars: &mut Peekable<Chars>) {
        match chars.peek() {
            Some(character) if *character == '=' => {
                chars.next().unwrap();
                self.push_token(TokenKind::ShlAssign);
            }
            _ => {
                self.push_token(TokenKind::Shl);
            }
        }
    }

    fn detect_assign_or_append(
        &mut self,
        chars: &mut Peekable<Chars>,
        if_assign: TokenKind,
        if_not: TokenKind
    ) {
        if let Some('=') = chars.peek() {
            self.pos += 1;
            self.column += 1;
            self.push_token(if_assign);
        } else {
            self.push_token(if_not);
        }
    }

    fn tokenize_alphabetic(&mut self, ch: char, chars: &mut Peekable<Chars>) {
        let mut text: SmallVec<u8, 16> = SmallVec::from_buf([ch as u8]);
        while let Some(&next_ch) = chars.peek() {
            if next_ch.is_alphanumeric() || next_ch == '_' {
                let ch = chars.next().unwrap(); // Only consume if it's part of the identifier
                self.pos += ch.len_utf8();
                text.push(ch as u8);
            } else {
                break; // Don't consume the character, leave it for the next iteration
            }
        }

        let string = self.context.intern_bytes(text.as_slice());
        let text = self.context.resolve_string(&string);

        match text {
            "true" => self.push_token(TokenKind::BooleanTrue),
            "false" => self.push_token(TokenKind::BooleanFalse),
            "null" => self.push_token(TokenKind::Null),

            "if" => self.push_token(TokenKind::If),
            "else" => self.push_token(TokenKind::Else),
            "while" => self.push_token(TokenKind::While),
            "for" => self.push_token(TokenKind::For),
            "in" => self.push_token(TokenKind::In),
            "return" => self.push_token(TokenKind::Return),
            "break" => self.push_token(TokenKind::Break),
            "continue" => self.push_token(TokenKind::Continue),
            "enum" => self.push_token(TokenKind::Enum),
            "struct" => self.push_token(TokenKind::Struct),
            "interface" => self.push_token(TokenKind::Interface),
            "impl" => self.push_token(TokenKind::Impl),
            "import" => self.push_token(TokenKind::Import),
            "package" => self.push_token(TokenKind::Package),
            "type" => self.push_token(TokenKind::Type),
            "const" => self.push_token(TokenKind::Const),
            "let" => self.push_token(TokenKind::Let),
            "mut" => self.push_token(TokenKind::Mut),
            "own" => self.push_token(TokenKind::Own),
            "match" => self.push_token(TokenKind::Match),
            "defer" => self.push_token(TokenKind::Defer),

            "unsafe" => self.push_token(TokenKind::Unsafe),
            "inline" => self.push_token(TokenKind::Inline),
            "noinline" => self.push_token(TokenKind::Noinline),
            "sealed" => self.push_token(TokenKind::Sealed),
            "private" => self.push_token(TokenKind::Private),
            "module" => self.push_token(TokenKind::Module),
            "extern" => self.push_token(TokenKind::Extern),
            "static" => self.push_token(TokenKind::Static),
            "effect" => self.push_token(TokenKind::Effect),
            "permits" => self.push_token(TokenKind::Permits),
            "statem" => self.push_token(TokenKind::Statem),
            "trait" => self.push_token(TokenKind::Trait),
            "where" => self.push_token(TokenKind::Where),
            "fn" => self.push_token(TokenKind::Fn),
            "requires" => self.push_token(TokenKind::Requires),
            "ensures" => self.push_token(TokenKind::Ensures),
            "uses" => self.push_token(TokenKind::Uses),

            "this" => self.push_token(TokenKind::This),

            "u8" => self.push_token(TokenKind::U8),
            "u16" => self.push_token(TokenKind::U16),
            "u32" => self.push_token(TokenKind::U32),
            "u64" => self.push_token(TokenKind::U64),
            "u128" => self.push_token(TokenKind::U128),
            "i8" => self.push_token(TokenKind::I8),
            "i16" => self.push_token(TokenKind::I16),
            "i32" => self.push_token(TokenKind::I32),
            "i64" => self.push_token(TokenKind::I64),
            "i128" => self.push_token(TokenKind::I128),
            "f32" => self.push_token(TokenKind::F32),
            "f64" => self.push_token(TokenKind::F64),
            "usize" => self.push_token(TokenKind::Usize),
            "isize" => self.push_token(TokenKind::Isize),
            "char" => self.push_token(TokenKind::Char),
            "str" => self.push_token(TokenKind::Str),
            "boolean" => self.push_token(TokenKind::Boolean),

            _ => self.push(TokenKind::Ident, StrId(string))
        }
    }

    fn tokenize_number(&mut self, first: char, chars: &mut Peekable<Chars>) {
        let mut kind = TokenKind::Number;
        let mut text: SmallVec<u8, 16> = SmallVec::from_buf([first as u8]);

        let mut seen_dot = false;
        let mut seen_exp = false;
        let mut last_was_underscore = false;

        // --- base prefixes (0x / 0b) ---
        if first == '0' {
            if let Some(&('x' | 'X')) = chars.peek() {
                let ch = chars.next().unwrap();
                self.pos += 1;
                text.push(ch as u8);
                kind = TokenKind::Hexadecimal;

                self.consume_digits(chars, &mut text, |c| c.is_ascii_hexdigit());
                self.consume_suffix(chars, &mut text);
                self.finish_number(kind, text);
                return;
            }

            if let Some(&('b' | 'B')) = chars.peek() {
                let ch = chars.next().unwrap();
                self.pos += 1;
                text.push(ch as u8);
                kind = TokenKind::Binary;

                self.consume_digits(chars, &mut text, |c| matches!(c, '0' | '1'));
                self.consume_suffix(chars, &mut text);
                self.finish_number(kind, text);
                return;
            }
        }

        // --- decimal / float / scientific ---
        while let Some(&c) = chars.peek() {
            match c {
                '0'..='9' => {
                    let c = chars.next().unwrap();
                    self.pos += 1;
                    text.push(c as u8);
                    last_was_underscore = false;
                }

                '_' if !last_was_underscore => {
                    let c = chars.next().unwrap();
                    self.pos += 1;
                    text.push(c as u8);
                    last_was_underscore = true;
                }

                '.' if !seen_dot && !seen_exp => {
                    let c = chars.next().unwrap();
                    self.pos += 1;
                    text.push(c as u8);
                    seen_dot = true;
                    kind = TokenKind::Decimal;
                    last_was_underscore = false;
                }

                'e' | 'E' if !seen_exp => {
                    let c = chars.next().unwrap();
                    self.pos += 1;
                    text.push(c as u8);
                    seen_exp = true;
                    kind = TokenKind::Decimal;
                    last_was_underscore = false;

                    if let Some(&('+' | '-')) = chars.peek() {
                        let sign = chars.next().unwrap();
                        self.pos += 1;
                        text.push(sign as u8);
                    }
                }

                _ => break,
            }
        }

        // trailing underscore is illegal → stop number before it
        if last_was_underscore {
            text.pop();
        }

        // --- numeric suffix ---
        self.consume_suffix(chars, &mut text);
        self.finish_number(kind, text);
    }

    #[inline]
    fn consume_digits<F>(&mut self, chars: &mut Peekable<Chars>, text: &mut SmallVec<u8, 16>, mut valid: F)
    where
        F: FnMut(char) -> bool,
    {
        let mut last_was_underscore = false;

        while let Some(&c) = chars.peek() {
            if valid(c) {
                let c = chars.next().unwrap();
                self.pos += 1;
                text.push(c as u8);
                last_was_underscore = false;
            } else if c == '_' && !last_was_underscore {
                let c = chars.next().unwrap();
                self.pos += 1;
                text.push(c as u8);
                last_was_underscore = true;
            } else {
                break;
            }
        }

        if last_was_underscore {
            text.pop();
        }
    }

    #[inline]
    fn consume_suffix(&mut self, chars: &mut Peekable<Chars>, text: &mut SmallVec<u8, 16>) {
        if let Some(&c) = chars.peek() {
            if c.is_ascii_alphabetic() {
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() {
                        let c = chars.next().unwrap();
                        self.pos += 1;
                        text.push(c as u8);
                    } else {
                        break;
                    }
                }
            }
        }
    }

    #[inline]
    fn finish_number(&mut self, kind: TokenKind, text: SmallVec<u8, 16>) {
        let string = unsafe { std::str::from_utf8_unchecked(text.as_slice()) };
        let id = self.context.intern_bytes(string.as_bytes());
        self.push(kind, StrId(id));
    }

    fn tokenize_string(&mut self, chars: &mut Peekable<Chars>) {
        let mut text: SmallVec<u8, 16> = SmallVec::new(); // Don't include the opening quote
        while chars.peek().is_some() {
            let ch = chars.next().unwrap();
            if ch == '\\' {
                let ch = chars.next().unwrap();
                match ch {
                    'n' => text.push(b'\n'),
                    't' => text.push(b'\t'),
                    'r' => text.push(b'\r'),
                    '\\' => text.push(b'\\'),
                    '"' => text.push(b'"'),
                    other => {
                        text.push(other as u8);
                    }
                }
                self.pos += 2;
            } else if ch == '"' {
                self.pos += 1;
                break;
            } else {
                self.pos += ch.len_utf8();
                text.push(ch as u8);
            }
        }

        let string = unsafe { str::from_utf8_unchecked(text.as_slice()) };
        let text = self.context
            .intern_bytes(string.as_bytes());
        self.push(TokenKind::String, StrId(text));
    }

    fn tokenize_line_comment(&mut self, chars: &mut Peekable<Chars>, is_doc: bool) {
        let mut text: SmallVec<u8, 128> = SmallVec::new();

        while let Some(&ch) = chars.peek() {
            if ch == '\n' {
                break;
            }
            chars.next();
            self.pos += ch.len_utf8();
            self.column += 1;
            text.push(ch as u8);
        }

        let string = unsafe { str::from_utf8_unchecked(text.as_slice()) };
        let text_id = self.context.intern_bytes(string.as_bytes());

        let kind = if is_doc {
            TokenKind::DocComment
        } else {
            TokenKind::LineComment
        };

        self.push(kind, StrId(text_id));
    }

    fn push(&mut self, kind: TokenKind, text: StrId) {
        let span = SourceSpan::new(
            self.file_name,
            self.pos,
            self.line
        );
        self.tokens.push(kind, text, span);
    }

    fn push_token(&mut self, kind: TokenKind) {
        let span = SourceSpan::new(
            self.file_name,
            self.pos,
            self.line
        );
        self.tokens.push_token(kind, span);
    }
}
