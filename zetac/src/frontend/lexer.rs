use std::fs;
use crate::frontend::tokens;
use crate::frontend::tokens::TokenError;

pub struct Lexer {
    pub input: Vec<char>,
    pub index: usize,
    pub line: usize,
    pub column: usize,
    pub errors: Vec<TokenError>
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input: input.chars().collect::<Vec<char>>(),
            index: 0,
            line: 1,
            column: 1,
            errors: Vec::new()
        }
    }

    pub fn from_file(file_name: &str) -> Lexer {
        Lexer {
            input: fs::read_to_string(file_name).unwrap().chars().collect(),
            index: 0,
            line: 1,
            column: 1,
            errors: Vec::new()
        }
    }

    /// Tokenize the input string. This function will advance the lexer's index
    /// and construct a vector of `Token`s. The `Token`s will be returned in a
    /// `Result`, which will be `Err` if any errors occur during tokenization.
    ///
    /// # Errors
    ///
    /// `tokenize` will return an `Err` if any errors occur during tokenization.
    /// The `Err` will contain a string describing the errors.
    pub fn tokenize(&mut self) -> anyhow::Result<Vec<tokens::Token>> {
        let mut tokens = Vec::new();

        while self.index < self.input.len() {
            let c = self.peek_char(); // Use a helper to peek at the current char

            match c {
                Some(c_val) => {
                    if c_val.is_whitespace() {
                        self.advance(); // Consume the whitespace
                        continue; // Go to the next iteration
                    } else if c_val.is_ascii_digit() {
                        tokens.push(self.tokenize_number());
                    } else if c_val.is_alphanumeric() {
                        tokens.push(self.tokenize_identifiers());
                    } else if c_val == '"' { // Handle strings
                        tokens.push(self.tokenize_string());
                    }
                    // Handle EOL, Semicolon as part of operator or specific checks
                    else if c_val == '\n' {
                        // Advance past the newline character
                        self.advance();
                        // Only add EOL token if it's explicitly part of your grammar
                        // Many languages implicitly handle newlines as whitespace,
                        // or they are handled by semicolon insertion rules.
                        // If you strictly need it, keep this line:
                        tokens.push(tokens::Token::new(String::new(), tokens::TokenType::EOL));
                    }
                    else {
                        // All other non-whitespace, non-alphanumeric, non-digit chars
                        // are handled by tokenize_operators.
                        tokens.push(self.tokenize_operators());
                    }
                },
                None => break, // Reached the end of input
            }
        }

        if !self.errors.is_empty() {
            return Err(anyhow::anyhow!("Unable to tokenize input. Errors: {:?}", self.errors));
        }

        Ok(tokens)
    }

    /// Tokenizes a sequence of characters from the input that form an identifier or keyword.
    ///
    /// This function reads characters from the current position in the input stream
    /// and advances until it encounters a non-alphanumeric character or underscore.
    /// It constructs a string from the collected characters and determines its type:
    /// if the string matches any reserved keywords or types, it returns a `Token` with
    /// the appropriate `TokenType`. Otherwise, it returns a `Token` of type `Identifier`.
    ///
    /// # Returns
    ///
    /// A `tokens::Token` representing the identified keyword, type, or generic identifier.
    pub(crate) fn tokenize_identifiers(&mut self) -> tokens::Token {
        let start_index = self.index;

        while self.index < self.input.len() {
            if let Some(c) = self.peek_char() {
                if c.is_alphanumeric() || c == '_' { // Often identifiers can contain underscores
                    self.advance();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let identifier: String = self.input[start_index..self.index].iter().collect();

        let token_type = match identifier.as_str() {
            "let" => tokens::TokenType::Keyword,
            "fun" => tokens::TokenType::Keyword, // Added fun
            "effect" => tokens::TokenType::Keyword,
            "class" => tokens::TokenType::Keyword,
            "interface" => tokens::TokenType::Keyword,
            "public" => tokens::TokenType::Keyword,
            "private" => tokens::TokenType::Keyword,
            "protected" => tokens::TokenType::Keyword,
            "if" => tokens::TokenType::Keyword,
            "else" => tokens::TokenType::Keyword,
            "while" => tokens::TokenType::Keyword,
            "for" => tokens::TokenType::Keyword,
            "match" => tokens::TokenType::Keyword,
            "return" => tokens::TokenType::Keyword,
            "break" => tokens::TokenType::Keyword,
            "continue" => tokens::TokenType::Keyword,
            "true" => tokens::TokenType::Keyword,
            "false" => tokens::TokenType::Keyword,
            "self" => tokens::TokenType::Keyword,
            "super" => tokens::TokenType::Keyword,
            "enum" => tokens::TokenType::Keyword,
            "import" => tokens::TokenType::Keyword,
            "as" => tokens::TokenType::Keyword,
            "mut" => tokens::TokenType::Keyword,
            "static" => tokens::TokenType::Keyword,
            "const" => tokens::TokenType::Keyword,
            "impl" => tokens::TokenType::Keyword,
            "extern" => tokens::TokenType::Keyword,
            "unsafe" => tokens::TokenType::Keyword,

            // types
            "f32" => tokens::TokenType::F32,
            "f64" => tokens::TokenType::F64,
            "u8" => tokens::TokenType::U8,
            "i8" => tokens::TokenType::I8,
            "u16" => tokens::TokenType::U16,
            "i16" => tokens::TokenType::I16,
            "u32" => tokens::TokenType::U32,
            "i32" => tokens::TokenType::I32,
            "u64" => tokens::TokenType::U64,
            "i64" => tokens::TokenType::I64,
            "i128" => tokens::TokenType::I128,
            "u128" => tokens::TokenType::U128,
            "boolean" => tokens::TokenType::Boolean,
            "str" => tokens::TokenType::String, // This `str` keyword is different from the string literal token type

            _ => tokens::TokenType::Identifier
        };

        tokens::Token::new(identifier, token_type)
    }

    pub(crate) fn tokenize_number(&mut self) -> tokens::Token {
        let start_index = self.index;
        let mut token_type = tokens::TokenType::Int;

        // Consume digits, dot, exponent
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' || c == 'e' || c == 'E' {
                token_type = tokens::TokenType::F64;
                self.advance();
            } else {
                break;
            }
        }

        // Consume suffix if any
        let suffix_start_index = self.index;
        let mut lookahead = 0;
        while let Some(c) = self.peek_char_at(lookahead) {
            if c.is_alphanumeric() {
                lookahead += 1;
            } else {
                break;
            }
        }

        let suffix: String = self.input[suffix_start_index..suffix_start_index + lookahead].iter().collect();

        // Match known suffixes and update type
        match suffix.as_str() {
            "f32" => {
                token_type = tokens::TokenType::F32;
                self.index += 3;
            }
            "f64" => {
                token_type = tokens::TokenType::F64;
                self.index += 3;
            }
            "i8" => {
                token_type = tokens::TokenType::I8;
                self.index += 2;
            }
            "i16" => {
                token_type = tokens::TokenType::I16;
                self.index += 3;
            }
            "i32" => {
                token_type = tokens::TokenType::I32;
                self.index += 3;
            }
            "i64" => {
                token_type = tokens::TokenType::I64;
                self.index += 3;
            }
            "u8" => {
                token_type = tokens::TokenType::U8;
                self.index += 2;
            }
            "u16" => {
                token_type = tokens::TokenType::U16;
                self.index += 3;
            }
            "u32" => {
                token_type = tokens::TokenType::U32;
                self.index += 3;
            }
            "u64" => {
                token_type = tokens::TokenType::U64;
                self.index += 3;
            }
            _ => {} // Unknown suffix, type remains as detected (Int or F64)
        }

        let number_str: String = self.input[start_index..self.index].iter().collect();
        tokens::Token::new(number_str, token_type)
    }

    // Renamed from tokenize_string to match the flow
    pub(crate) fn tokenize_string(&mut self) -> tokens::Token {
        let start_column = self.column;
        let start_line = self.line;
        self.advance(); // Consume the opening '"'

        let mut string_content = String::new();
        while let Some(c) = self.peek_char() {
            if c == '"' {
                break; // Found closing quote
            }
            string_content.push(self.advance().unwrap()); // Consume and add character
        }

        if self.peek_char() != Some('"') {
            // Error: unterminated string literal
            let token_error = TokenError {
                message: "Unterminated string literal".to_string(),
                line: start_line,
                column: start_column,
                token: tokens::Token::new(string_content.clone(), tokens::TokenType::String) // Token with partial string
            };
            self.errors.push(token_error);
            // Even though it's an error, we return a token to allow parsing to continue
            return tokens::Token::new(string_content, tokens::TokenType::Unknown);
        }

        self.advance(); // Consume the closing '"'
        tokens::Token::new(string_content, tokens::TokenType::String)
    }

    /// Tokenize the next operator in the input.
    ///
    /// This function is responsible for tokenizing all operators, including
    /// assignment operators, comparison operators, logical operators, and
    /// arithmetic operators. It handles the case where an operator is followed
    /// by an '=' to form an assignment operator, e.g., '+='.
    ///
    /// # Errors
    ///
    /// If the input contains an unknown symbol, an error is reported and the
    /// lexer advances past the unknown symbol.
    pub(crate) fn tokenize_operators(&mut self) -> tokens::Token {
        let current_char = self.peek_char().expect("Expected char for operator tokenization");
        let start_column = self.column;
        let start_line = self.line;

        let token_type = match current_char {
            '+' => {
                self.advance(); // Consume '+'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '+='
                    tokens::TokenType::AddAssign
                } else {
                    tokens::TokenType::Plus
                }
            }
            '-' => {
                self.advance(); // Consume '-'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '-='
                    tokens::TokenType::SubAssign
                } else {
                    tokens::TokenType::Minus
                }
            }
            '*' => {
                self.advance(); // Consume '*'
                if self.peek_char() == Some('*') {
                    self.advance(); // Consume '*' for '**'
                    if self.peek_char() == Some('=') {
                        self.advance(); // Consume '=' for '**='
                        tokens::TokenType::PowAssign
                    } else {
                        tokens::TokenType::Pow
                    }
                } else if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '*='
                    tokens::TokenType::MulAssign
                } else {
                    tokens::TokenType::Mul
                }
            }
            '/' => {
                self.advance(); // Consume '/'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '/='
                    tokens::TokenType::DivAssign
                } else {
                    tokens::TokenType::Div
                }
            }
            '%' => {
                self.advance(); // Consume '%'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '%='
                    tokens::TokenType::ModAssign
                } else {
                    tokens::TokenType::Mod
                }
            }
            '^' => {
                self.advance(); // Consume '^'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '^='
                    tokens::TokenType::BitXor
                } else {
                    tokens::TokenType::BitXorAssign
                }
            }
            '|' => {
                self.advance(); // Consume '|'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '|='
                    tokens::TokenType::BitOrAssign
                } else {
                    tokens::TokenType::BitOr
                }
            }
            '&' => {
                self.advance(); // Consume '&'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '&='
                    tokens::TokenType::BitAndAssign
                } else {
                    tokens::TokenType::BitAnd
                }
            }
            '~' => {
                self.advance(); // Consume '~'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '~='
                    tokens::TokenType::BitXorAssign
                } else {
                    tokens::TokenType::BitXor
                }
            }
            '=' => {
                self.advance(); // Consume '='
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume the second '=' for '=='
                    tokens::TokenType::IsEqual
                } else {
                    tokens::TokenType::Equal // Assignment operator
                }
            }
            '!' => {
                self.advance(); // Consume '!'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '=' for '!='
                    tokens::TokenType::IsNotEqual
                } else {
                    // This might be an error or another operator if '!' stands alone
                    // For now, treat as Unknown if no '=' follows
                    let token_error = TokenError {
                        message: "Unexpected '!'".to_string(),
                        line: start_line,
                        column: start_column,
                        token: tokens::Token::new(String::from("!"), tokens::TokenType::Unknown)
                    };
                    self.errors.push(token_error);
                    return tokens::Token::new(String::from("!"), tokens::TokenType::Unknown);
                }
            }
            '<' => {
                self.advance(); // Consume '<'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '='
                    tokens::TokenType::LessThanOrEqualTo
                } else if self.peek_char() == Some('<') {
                    self.advance(); // Consume the second '<'
                    tokens::TokenType::ShiftLeft

                } else {
                    tokens::TokenType::LessThan
                }
            }
            '>' => {
                self.advance(); // Consume '>'
                if self.peek_char() == Some('=') {
                    self.advance(); // Consume '='
                    tokens::TokenType::GreaterThanOrEqualTo
                } else if self.peek_char() == Some('>') {
                    self.advance(); // Consume second '>'

                    let mut operator = tokens::TokenType::ShiftRight;
                    if self.peek_char() == Some('>') {
                        self.advance(); // Consume third '>'
                        operator = tokens::TokenType::UnsignedShiftRight;
                    }
                    operator
                } else {
                    tokens::TokenType::GreaterThan
                }
            }
            '(' => tokens::TokenType::LParen,
            ')' => tokens::TokenType::RParen,
            '{' => tokens::TokenType::LBrace,
            '}' => tokens::TokenType::RBrace,
            '[' => tokens::TokenType::LBracket,
            ']' => tokens::TokenType::RBracket,
            ',' => tokens::TokenType::Comma,
            '.' => tokens::TokenType::Dot,
            ';' => tokens::TokenType::Semicolon,
            ':' => tokens::TokenType::Colon,

            _ => {
                // If it's an unknown character, report an error and advance
                let token_error = TokenError {
                    message: format!("Unknown symbol: '{}'", current_char),
                    line: start_line,
                    column: start_column,
                    token: tokens::Token::new(String::from(current_char), tokens::TokenType::Unknown)
                };
                self.errors.push(token_error);
                self.advance(); // Crucially, advance to prevent infinite loop on unknown char
                return tokens::Token::new(String::from(current_char), tokens::TokenType::Unknown);
            }
        };

        // If the token type was determined without needing a lookahead (e.g., '+', '('),
        // we need to advance the index here.
        if token_type != tokens::TokenType::IsEqual && // handled by '=='
            token_type != tokens::TokenType::IsNotEqual && // handled by '!='
            token_type != tokens::TokenType::LessThanOrEqualTo && // handled by '<='
            token_type != tokens::TokenType::GreaterThanOrEqualTo { // handled by '>='
            self.advance(); // Advance past the single character operator
        }

        tokens::Token::new(String::from(current_char), token_type)
    }

    /// Helper to peek at the current character without advancing
    fn peek_char(&self) -> Option<char> {
        self.input.get(self.index).copied()
    }

    /// Helper to peek at a character with an offset
    fn peek_char_at(&self, offset: usize) -> Option<char> {
        self.input.get(self.index + offset).copied()
    }

    /// Advances the lexer to the next character, returning the character
    /// previously at the current index. If the lexer is already at the end of the input.
    ///
    /// It returns `None`. Also updates the lexer's internal line and column numbers if the character was a newline.
    /// The main purpose of this method is to advance the lexer over whitespace and other characters that are not part of the language syntax.
    #[inline(always)]
    fn advance(&mut self) -> Option<char> {
        if self.index >= self.input.len() {
            return None;
        }
        let c = self.input[self.index];
        self.index += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }
}