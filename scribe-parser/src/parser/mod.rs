pub mod descent_parser;
mod functions;
mod expressions;
mod statements;
mod declarations;
mod misc;

pub use descent_parser::{parse_program, ParseResult, ParserDiagnostics};