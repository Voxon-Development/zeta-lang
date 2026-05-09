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

#[derive(Debug, Clone, Error)]
pub enum ParserError<'a> {
    #[error("Tokenizer error: {0}")]
    LexerError(StrId),

    #[error("Unexpected token: expected {expected} but got {found} at {span}")]
    UnexpectedToken { expected: TokenKind, found: TokenKind, span: SourceSpan<'a> },

    #[error("Unexpected end of file.")]
    UnexpectedEof,

    #[error("Invalid function name: expected an identifier but got {name_type} at {location}")]
    InvalidFunctionName {
        name_type: TokenKind,
        location: SourceSpan<'a>,
    },

    #[error("Invalid identifier: expected text but got an empty identifier at {location}")]
    EmptyIdent {
        location: SourceSpan<'a>,
    },
}