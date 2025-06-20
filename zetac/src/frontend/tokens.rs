// Lexer

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub value: String,
    pub value_type: TokenType
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Identifiers and keywords
    Identifier,
    Keyword,

    // Constants
    F32,
    F64,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    I128,
    U128,
    
    Int, // unknown at lex time
    Boolean,
    String,

    // Operators
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Power,
    Equal,
    
    // Assignments
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ShlAssign,
    ShrAssign,
    BitOrAssign,
    BitXorAssign,
    BitAndAssign,

    // Comparisons
    IsEqual,
    IsNotEqual,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
    Colon,

    // Other
    EOF,
    EOL,
    AtSymbol,
    
    Unknown, // placed for TokenError
    BitAnd,
    BitXor,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftRight,
    BitOr,
    Pow,
    PowAssign
}


impl Token {
    #[inline]
    pub fn new(name: String, value: TokenType) -> Token {
        Token {
            value: name,
            value_type: value
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct TokenError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub token: Token
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lex time error at line {}, column {}, value: {}\n {}", self.line, self.column, self.token, self.message)
    }
}

// Parser

pub struct ParserError {
    pub message: String,
    pub token: Token
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse time error, value: {}\n {}", self.token, self.message)
    }
}

#[inline(always)]
pub fn comparison_op_from_string(op: &str) -> Option<TokenType> {
    match op {
        "==" => Some(TokenType::IsEqual),
        "!=" => Some(TokenType::IsNotEqual),
        "<" => Some(TokenType::LessThan),
        "<=" => Some(TokenType::LessThanOrEqualTo),
        ">" => Some(TokenType::GreaterThan),
        ">=" => Some(TokenType::GreaterThanOrEqualTo),
        _ => None
    }
}