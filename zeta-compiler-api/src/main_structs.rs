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
    pub bump: Arc<GrowableAtomicBump<'bump>>,
    pub name: StrId,
    pub path: PathBuf,
    pub stmts: &'bump [Stmt<'a, 'bump>],
    pub parser_diagnostics: ParserDiagnostics<'a>,
    pub source: StrId,
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
            CompilerError::TypeCheckError => write!(f, "Type check failed"),
            CompilerError::FinishError(e) => write!(f, "Backend finish error: {}", e),
            CompilerError::LinkFailed => {
                write!(f, "Backend could not link the stdlib to the binary.")
            }
        }
    }
}
