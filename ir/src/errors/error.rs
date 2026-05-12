use std::error::Error;
use std::fmt;
use std::fmt::{Debug};
use thiserror::Error;
use crate::ast::{Expr, Type, TypeKind};
use crate::hir::StrId;
use crate::span::SourceSpan;
use crate::tokens::TokenKind;

// Represents a type-checking error with detailed diagnostic information.
#[derive(Debug, Error, Clone)]
pub enum TypeError<'a, 'bump>
where 'bump: 'a {
    // Two types could not be unified.
    // Example: trying to assign a string to an integer variable.
    #[error("Type mismatch: expected {expected} but found {found} at {location}")]
    Mismatch {
        expected: Type<'a, 'bump>,    // The type we expected (e.g. int)
        found: Type<'a, 'bump>,       // The type we actually got (e.g. string)
        location: SourceSpan<'a>,
    },

    // A variable or function was used but could not be resolved.
    // Example: calling an undefined function `foo()`.
    #[error("Void function {function_name} returned a value at {location}")]
    VoidFunctionWithNonVoidReturn {
        function_name: StrId,
        return_value: Expr<'a, 'bump>,
        location: SourceSpan<'a>,
    },

    // A variable or function was used but could not be resolved.
    // Example: calling an undefined function `foo()`.
    #[error("Undefined {name} at {location}")]
    UndefinedSymbol {
        name: StrId,
        location: SourceSpan<'a>,
    },

    // A type or trait was not found in the current scope.
    #[error("Unknown type {name} at {location}")]
    UnknownType {
        name: StrId,
        location: SourceSpan<'a>,
    },

    // A function call was made with incorrect argument types or count.
    #[error("Invalid function call: {function} expected {expected_params:?} but got {found_params:?} at {location}")]
    InvalidFunctionCall {
        function: StrId,
        expected_params: &'bump [Type<'a, 'bump>],
        found_params: &'bump [Type<'a, 'bump>],
        location: SourceSpan<'a>,
    },

    // A function call or usage to a non-existing function
    #[error("No function called {function} was found")]
    MissingFunction {
        function: StrId
    },

    // A trait method was called on a type that does not implement it.
    #[error("Type {target_type} does not implement trait {trait_name} at {location}")]
    MissingTraitImpl {
        trait_name: StrId,
        target_type: Type<'a, 'bump>,
        location: SourceSpan<'a>,
    },

    // ABI violation for extern functions (e.g. non-FFI-safe type).
    #[error("Extern function {function} is not FFI-safe: {reason} at {location}")]
    AbiIncompatible {
        function: StrId,
        reason: StrId,
        location: SourceSpan<'a>,
    },

    // A generic type parameter could not be inferred or was ambiguous.
    #[error("Could not infer type for generic parameter {param_name} at {location}")]
    UnresolvedGeneric {
        param_name: StrId,
        location: SourceSpan<'a>,
    },

    // A constraint conflict between two type requirements.
    // Example: T: Copy but also T: !Copy
    #[error("Conflicting constraints: {constraints:?} at {location}")]
    ConflictingConstraints {
        constraints: Vec<StrId>,
        location: SourceSpan<'a>,
    },

    #[error("No such field called {field_name} for type {type_name} at {location}")]
    NoSuchField {
        type_name: StrId,
        field_name: StrId,
        location: SourceSpan<'a>,
    },
    
    #[error("No such field called {method_name} for type {type_name} at {location}")]
    NoSuchMethod {
        type_name: StrId,
        method_name: StrId,
        location: SourceSpan<'a>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    /// A specific token was expected but something else appeared.
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },

    /// A set of tokens was expected (for better "expected one of …" messages).
    UnexpectedTokenOneOf {
        expected: Vec<TokenKind>,
        found: TokenKind,
    },

    /// Input ended before the grammar rule could complete.
    UnexpectedEOF {
        expected: Option<TokenKind>,
    },

    /// An expression could not be started from the current token.
    InvalidExpression { found: TokenKind },

    /// A type position contained a token that cannot begin a type.
    InvalidType { found: TokenKind },

    /// A pattern arm contained an unrecognisable token.
    InvalidPattern { found: TokenKind },

    /// Function name was missing or was not an identifier.
    InvalidFunctionName { found: TokenKind },

    /// An identifier token had empty text (lexer bug).
    EmptyIdent,

    /// The parser entered recovery and could not synchronise.
    RecoveryFailure,

    /// Custom / catch-all for parser infrastructure errors.
    Internal { message: &'static str },
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::UnexpectedToken { expected, found } =>
                write!(f, "expected `{expected:?}`, found `{found:?}`"),
            ParseErrorKind::UnexpectedTokenOneOf { expected, found } => {
                write!(f, "expected one of [")?;
                for (i, t) in expected.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "`{t:?}`")?;
                }
                write!(f, "], found `{found:?}`")
            }
            ParseErrorKind::UnexpectedEOF { expected: Some(e) } =>
                write!(f, "unexpected end of file, expected `{e:?}`"),
            ParseErrorKind::UnexpectedEOF { expected: None } =>
                write!(f, "unexpected end of file"),
            ParseErrorKind::InvalidExpression { found } =>
                write!(f, "expected an expression, found `{found:?}`"),
            ParseErrorKind::InvalidType { found } =>
                write!(f, "expected a type, found `{found:?}`"),
            ParseErrorKind::InvalidPattern { found } =>
                write!(f, "expected a pattern, found `{found:?}`"),
            ParseErrorKind::InvalidFunctionName { found } =>
                write!(f, "expected a function name (identifier), found `{found:?}`"),
            ParseErrorKind::EmptyIdent =>
                write!(f, "identifier token has no text (lexer bug)"),
            ParseErrorKind::RecoveryFailure =>
                write!(f, "parser could not recover from previous errors"),
            ParseErrorKind::Internal { message } =>
                write!(f, "internal parser error: {message}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseContext {
    ParsingTopLevel,
    ParsingModule,
    ParsingStruct,
    ParsingEnum,
    ParsingImplBlock,
    ParsingInterface,
    ParsingMethod,
    ParsingFunction,
    ParsingFunctionParams,
    ParsingReturnType,
    ParsingType,
    ParsingExpression,
    ParsingPattern,
    ParsingMatchArm,
    ParsingBlock,
    ParsingLetStatement,
    ParsingIfStatement,
    ParsingWhileStatement,
    ParsingForStatement,
    ParsingImport,
}

impl fmt::Display for ParseContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ParseContext::ParsingTopLevel       => "top-level declaration",
            ParseContext::ParsingModule         => "module declaration",
            ParseContext::ParsingStruct         => "struct declaration",
            ParseContext::ParsingEnum           => "enum declaration",
            ParseContext::ParsingImplBlock      => "impl block",
            ParseContext::ParsingInterface      => "interface declaration",
            ParseContext::ParsingMethod         => "method declaration",
            ParseContext::ParsingFunction       => "function declaration",
            ParseContext::ParsingFunctionParams => "function parameters",
            ParseContext::ParsingReturnType     => "return type",
            ParseContext::ParsingType           => "type",
            ParseContext::ParsingExpression     => "expression",
            ParseContext::ParsingPattern        => "pattern",
            ParseContext::ParsingMatchArm       => "match arm",
            ParseContext::ParsingBlock          => "block",
            ParseContext::ParsingLetStatement   => "let statement",
            ParseContext::ParsingIfStatement    => "if statement",
            ParseContext::ParsingWhileStatement => "while statement",
            ParseContext::ParsingForStatement   => "for statement",
            ParseContext::ParsingImport         => "import statement",
        };
        f.write_str(s)
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticError<'a> {
    pub kind: ParseErrorKind,
    pub span: SourceSpan<'a>,
    /// Grammar-rule ancestry, innermost first (pushed by `with_context`).
    pub context: Vec<ParseContext>,
    /// Human-readable supplemental notes (e.g. "hint: add a closing brace").
    pub notes: Vec<String>,
}

impl<'a> DiagnosticError<'a> {
    pub fn new(kind: ParseErrorKind, span: SourceSpan<'a>) -> Self {
        DiagnosticError {
            kind,
            span,
            context: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn unexpected_token(expected: TokenKind, found: TokenKind, span: SourceSpan<'a>) -> Self {
        Self::new(ParseErrorKind::UnexpectedToken { expected, found }, span)
    }

    pub fn unexpected_eof(expected: Option<TokenKind>, span: SourceSpan<'a>) -> Self {
        Self::new(ParseErrorKind::UnexpectedEOF { expected }, span)
    }

    pub fn invalid_expr(found: TokenKind, span: SourceSpan<'a>) -> Self {
        Self::new(ParseErrorKind::InvalidExpression { found }, span)
    }

    pub fn empty_ident(span: SourceSpan<'a>) -> Self {
        Self::new(ParseErrorKind::EmptyIdent, span)
    }

    pub fn invalid_function_name(found: TokenKind, span: SourceSpan<'a>) -> Self {
        Self::new(ParseErrorKind::InvalidFunctionName { found }, span)
    }

    /// Render a human-readable single-line summary.
    pub fn message(&self) -> String {
        self.kind.to_string()
    }

    /// Render a multi-line diagnostic (context chain + notes).
    pub fn pretty(&self) -> String {
        let mut out = String::new();

        out.push_str(&format!(
            "error: {}\n --> {}:{}:{}\n",
            self.kind,
            self.span.file_name,
            self.span.line,
            self.span.column,
        ));

        if !self.context.is_empty() {
            out.push_str("\nwhile parsing:\n");
            for ctx in &self.context {
                out.push_str(&format!("  {ctx}\n"));
            }
        }

        for note in &self.notes {
            out.push_str(&format!("\nnote: {note}\n"));
        }

        out
    }
}

impl fmt::Display for DiagnosticError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.pretty())
    }
}

impl std::error::Error for DiagnosticError<'_> {}