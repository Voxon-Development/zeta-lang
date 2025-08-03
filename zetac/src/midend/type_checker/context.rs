use crate::midend::type_checker::errors::reporter::ErrorReporter;
use std::collections::HashMap;
use crate::midend::ir::hir::HirType;

// The global state for type-checking.
pub struct TypeContext<R: ErrorReporter> {
    // Symbol table mapping function names to their signatures.
    // Includes user-defined, standard library, and extern functions.
    functions: HashMap<String, FunctionSig>,

    // Symbol table mapping type names (e.g. "int", "MyStruct") to type definitions.
    types: HashMap<String, HirType>,

    // Trait table mapping trait names to their definition.
    traits: HashMap<String, TraitDef>,

    // Mapping from a type to its implemented traits (for method resolution).
    impls: HashMap<HirType, Vec<TraitImpl>>,

    // Constraints collected during inference (e.g. T must be Copy).
    constraints: Vec<Constraint>,

    // Mapping for generic type variables during inference.
    // Example: T -> Some(int) if inferred.
    type_variables: HashMap<String, Option<HirType>>,

    // Tracks imported extern functions for interop binding.
    extern_functions: HashMap<String, FunctionSig>,

    // Error reporter to collect and track type errors.
    errors: R,
}

// A function signature used for both normal and extern functions.
struct FunctionSig {
    params: Vec<HirType>,   // Parameter types
    return_type: HirType,   // Return type
    is_extern: bool,     // True if this is an extern function
}

// A trait definition with its required methods.
struct TraitDef {
    name: String,
    methods: Vec<FunctionSig>,
}

// A concrete trait implementation for a type.
struct TraitImpl {
    trait_name: String,
    target_type: HirType,
    methods: HashMap<String, FunctionSig>,
}

// A type constraint for inference or trait bounds.
enum Constraint {
    // T must be equal to int, for example.
    Equality { a: HirType, b: HirType },

    // T must implement a trait (e.g. T: Clone).
    TraitBound { type_: HirType, trait_: String },
}
