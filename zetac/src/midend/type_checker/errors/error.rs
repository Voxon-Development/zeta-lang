use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::midend::ir::hir::HirType;

// Represents a type-checking error with detailed diagnostic information.
#[derive(Debug)]
pub enum TypeError {
    // Two types could not be unified.
    // Example: trying to assign a string to an integer variable.
    Mismatch {
        expected: HirType,    // The type we expected (e.g. int)
        found: HirType,       // The type we actually got (e.g. string)
        location: SourceSpan,
    },

    // A variable or function was used but could not be resolved.
    // Example: calling an undefined function `foo()`.
    UndefinedSymbol {
        name: String,
        location: SourceSpan,
    },

    // A type or trait was not found in the current scope.
    UnknownType {
        name: String,
        location: SourceSpan,
    },

    // A function call was made with incorrect argument types or count.
    InvalidFunctionCall {
        function: String,
        expected_params: Vec<HirType>,
        found_params: Vec<HirType>,
        location: SourceSpan,
    },

    // A trait method was called on a type that does not implement it.
    MissingTraitImpl {
        trait_name: String,
        target_type: HirType,
        location: SourceSpan,
    },

    // ABI violation for extern functions (e.g. non-FFI-safe type).
    AbiIncompatible {
        function: String,
        reason: String,
        location: SourceSpan,
    },

    // A generic type parameter could not be inferred or was ambiguous.
    UnresolvedGeneric {
        param_name: String,
        location: SourceSpan,
    },

    // A constraint conflict between two type requirements.
    // Example: T: Copy but also T: !Copy
    ConflictingConstraints {
        constraints: Vec<String>,
        location: SourceSpan,
    },
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for TypeError {}

// Represents a source location for better diagnostics.
#[derive(Debug)]
pub struct SourceSpan {
    file: String,
    line: usize,
    column: usize,
}

pub type TypeResult<T> = Result<T, TypeError>;