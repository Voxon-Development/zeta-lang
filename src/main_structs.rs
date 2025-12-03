use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;
use engraver_assembly_emit::cranelift::cranelift_backend::EmitError;
use ir::hir::HirModule;
use scribe_parser::parser::{ParserError, ParserDiagnostics};
use zetaruntime::arena::GrowableAtomicBump;

pub struct ModuleWithArena<'a, 'bump> {
    #[allow(dead_code)] // This simply prevents parser and HIR bump from UB, doesn't need to be used
    pub(crate) parse_and_hir_bump: Arc<GrowableAtomicBump<'bump>>,

    pub(crate) module: HirModule<'a, 'bump>,

    pub(crate) parser_diagnostics: ParserDiagnostics,

    pub(crate) valid: bool
}

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Source not found: {0:?}")]
    SourceNotFound(PathBuf),

    #[error("No source files found")]
    NoSourceFiles,

    #[error("Invalid source file: {0}")]
    InvalidSourceFile(String),

    #[error("Failed to read file {0}: {1}")]
    FailedToReadFile(PathBuf, io::Error),

    #[error("Failed to allocate string pool.")]
    FailedToAllocateStringPool,

    #[error("Failed to allocate bump allocator")]
    FailedToAllocateBump,

    #[error("Failed to compile module due to type errors: {0:?}")]
    TypeCheckerErrors(Vec<String>),

    #[error("Invalid UTF-8 filename: {0:?}")]
    InvalidFileName(Vec<u8>),

    #[error("Parser error: {0:#?}")]
    ParserError(Vec<ParserError>),

    #[error("Failed to emit module: {0}")]
    FinishError(EmitError),

    #[error("Linking failed")]
    LinkFailed
}