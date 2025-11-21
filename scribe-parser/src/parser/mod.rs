pub mod descent_parser;
pub mod pratt_expr_parser;
pub mod statement_parser;
pub mod declaration_parser;
pub mod parser_types;
mod container_declarations;
mod tests;
mod integration_tests;

pub use descent_parser::parse_program;
