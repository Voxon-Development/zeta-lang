mod declarations;
pub mod descent_parser;
mod expressions;
mod functions;
mod misc;
mod statements;

pub use descent_parser::{ParseResult, ParserDiagnostics, parse_program};
pub use ir::diagnostics_context::ParserDiagnosticsContext;
pub use ir::errors::error::{DiagnosticError, ParseContext, ParseErrorKind};
