use std::fmt::{Debug, Display, Formatter};
use thiserror::Error;
use crate::midend::ir::hir::HirType;

// Represents a type-checking error with detailed diagnostic information.
#[derive(Debug, Error)]
pub enum TypeError {
    // Two types could not be unified.
    // Example: trying to assign a string to an integer variable.
    #[error("Type mismatch: expected {expected} but found {found} at {location}")]
    Mismatch {
        expected: HirType,    // The type we expected (e.g. int)
        found: HirType,       // The type we actually got (e.g. string)
        location: SourceSpan,
    },

    // A variable or function was used but could not be resolved.
    // Example: calling an undefined function `foo()`.
    #[error("Undefined {name} at {location}")]
    UndefinedSymbol {
        name: String,
        location: SourceSpan,
    },

    // A type or trait was not found in the current scope.
    #[error("Unknown type {name} at {location}")]
    UnknownType {
        name: String,
        location: SourceSpan,
    },

    // A function call was made with incorrect argument types or count.
    #[error("Invalid function call: {function} expected {expected_params:?} but got {found_params:?} at {location}")]
    InvalidFunctionCall {
        function: String,
        expected_params: Vec<HirType>,
        found_params: Vec<HirType>,
        location: SourceSpan,
    },

    // A trait method was called on a type that does not implement it.
    #[error("Type {target_type} does not implement trait {trait_name} at {location}")]
    MissingTraitImpl {
        trait_name: String,
        target_type: HirType,
        location: SourceSpan,
    },

    // ABI violation for extern functions (e.g. non-FFI-safe type).
    #[error("Extern function {function} is not FFI-safe: {reason} at {location}")]
    AbiIncompatible {
        function: String,
        reason: String,
        location: SourceSpan,
    },

    // A generic type parameter could not be inferred or was ambiguous.
    #[error("Could not infer type for generic parameter {param_name} at {location}")]
    UnresolvedGeneric {
        param_name: String,
        location: SourceSpan,
    },

    // A constraint conflict between two type requirements.
    // Example: T: Copy but also T: !Copy
    #[error("Conflicting constraints: {constraints:?} at {location}")]
    ConflictingConstraints {
        constraints: Vec<String>,
        location: SourceSpan,
    },
}

// Represents a source location for better diagnostics.
#[derive(Debug)]
pub struct SourceSpan {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl Display for SourceSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

pub type TypeResult<T> = Result<T, TypeError>;