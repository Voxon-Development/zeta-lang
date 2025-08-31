pub mod parser; // existing pest-based parser (will be deprecated)
pub mod hir_lowerer;

// New arena/bump-allocated AST and custom parser
pub mod arena_ast;
pub mod arena_parser;

// Re-export arena parser API so downstream users can adopt it easily
pub use arena_parser::parse_program_to_ast;