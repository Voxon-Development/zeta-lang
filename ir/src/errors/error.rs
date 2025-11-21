use std::fmt::{Debug};
use thiserror::Error;
use crate::ast::{Expr, Type};
use crate::span::SourceSpan;

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
        function_name: String,
        return_value: Expr<'a, 'bump>,
        location: SourceSpan<'a>,
    },

    // A variable or function was used but could not be resolved.
    // Example: calling an undefined function `foo()`.
    #[error("Undefined {name} at {location}")]
    UndefinedSymbol {
        name: String,
        location: SourceSpan<'a>,
    },

    // A type or trait was not found in the current scope.
    #[error("Unknown type {name} at {location}")]
    UnknownType {
        name: String,
        location: SourceSpan<'a>,
    },

    // A function call was made with incorrect argument types or count.
    #[error("Invalid function call: {function} expected {expected_params:?} but got {found_params:?} at {location}")]
    InvalidFunctionCall {
        function: String,
        expected_params: &'bump [Type<'a, 'bump>],
        found_params: &'bump [Type<'a, 'bump>],
        location: SourceSpan<'a>,
    },

    // A trait method was called on a type that does not implement it.
    #[error("Type {target_type} does not implement trait {trait_name} at {location}")]
    MissingTraitImpl {
        trait_name: String,
        target_type: Type<'a, 'bump>,
        location: SourceSpan<'a>,
    },

    // ABI violation for extern functions (e.g. non-FFI-safe type).
    #[error("Extern function {function} is not FFI-safe: {reason} at {location}")]
    AbiIncompatible {
        function: String,
        reason: String,
        location: SourceSpan<'a>,
    },

    // A generic type parameter could not be inferred or was ambiguous.
    #[error("Could not infer type for generic parameter {param_name} at {location}")]
    UnresolvedGeneric {
        param_name: String,
        location: SourceSpan<'a>,
    },

    // A constraint conflict between two type requirements.
    // Example: T: Copy but also T: !Copy
    #[error("Conflicting constraints: {constraints:?} at {location}")]
    ConflictingConstraints {
        constraints: Vec<String>,
        location: SourceSpan<'a>,
    },

    #[error("No such field called {field_name} for type {type_name} at {location}")]
    NoSuchField {
        type_name: String,
        field_name: String,
        location: SourceSpan<'a>,
    },
    
    #[error("No such field called {method_name} for type {type_name} at {location}")]
    NoSuchMethod {
        type_name: String,
        method_name: String,
        location: SourceSpan<'a>,
    },
}

pub type TypeResult<'a, 'bump, T> = Result<T, TypeError<'a, 'bump>>;