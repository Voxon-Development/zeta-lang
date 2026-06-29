use ir::ast::Stmt;
use ir::hir::StrId;
use scribe_parser::parser::ParserDiagnostics;
use std::fmt;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;

#[derive(Clone, Debug)]
pub struct ModuleWithArena<'a, 'bump> {
    /// The arena that owns all AST memory for this file.
    pub bump: Arc<GrowableAtomicBump<'bump>>,
    /// Interned file / module name (used as the `AstModule::name` field).
    pub name: StrId,
    /// Top-level statements produced by the parser.
    pub stmts: &'bump [Stmt<'a, 'bump>],
    /// Non-fatal diagnostics from the parser.
    pub parser_diagnostics: ParserDiagnostics<'a>,
}

#[derive(Debug)]
pub enum CompilerError<'a> {
    SourceNotFound(PathBuf),
    NoSourceFiles,
    FailedToReadFile(PathBuf, io::Error),
    FailedToAllocateBump,
    FailedToAllocateStringPool,
    InvalidFileName(Vec<u8>),
    ParserError(Vec<&'a str>),
    TypeError(String),
    TypeCheckError,
    FinishError(Box<dyn std::error::Error>),
    LinkFailed,
}

impl<'a> fmt::Display for CompilerError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerError::SourceNotFound(p) => write!(f, "Source not found: {}", p.display()),
            CompilerError::NoSourceFiles => write!(f, "No .zeta source files found"),
            CompilerError::FailedToReadFile(p, e) => {
                write!(f, "Failed to read {}: {}", p.display(), e)
            }
            CompilerError::FailedToAllocateBump => write!(f, "Failed to allocate bump arena"),
            CompilerError::FailedToAllocateStringPool => {
                write!(f, "Failed to allocate string pool")
            }
            CompilerError::InvalidFileName(b) => {
                write!(f, "Invalid file name bytes: {:?}", b)
            }
            CompilerError::ParserError(errs) => {
                write!(f, "Parser errors: {:?}", errs)
            }
            CompilerError::TypeError(e) => write!(f, "Type error: {}", e),
            CompilerError::TypeCheckError => write!(f, "Type check failed"),
            CompilerError::FinishError(e) => write!(f, "Backend finish error: {}", e),
            CompilerError::LinkFailed => {
                write!(f, "Backend could not link the stdlib to the binary.")
            }
        }
    }
}
