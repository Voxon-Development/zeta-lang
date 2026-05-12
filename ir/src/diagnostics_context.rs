use crate::errors::error::{DiagnosticError, ParseContext};
use crate::span::SourceSpan;
use crate::tokens::{Cursor, TokenKind};

pub struct ParserDiagnosticsContext<'a> {
    /// All errors collected during this parse run (not just the first).
    errors: Vec<DiagnosticError<'a>>,

    /// All warnings collected (separate from hard errors).
    warnings: Vec<DiagnosticWarning<'a>>,

    /// Currently active grammar-rule ancestry, innermost at the back.
    context_stack: Vec<ParseContext>,

    /// Whether RAII trace scopes should emit output.
    pub tracing_enabled: bool,

    /// How many errors we tolerate before giving up recovery attempts.
    pub max_errors: usize,
}

impl<'a> ParserDiagnosticsContext<'a> {
    pub fn new(tracing_enabled: bool) -> Self {
        ParserDiagnosticsContext {
            errors: Vec::new(),
            warnings: Vec::new(),
            context_stack: Vec::with_capacity(16),
            tracing_enabled,
            max_errors: 20,
        }
    }

    // -----------------------------------------------------------------------
    // Error / warning collection
    // -----------------------------------------------------------------------

    /// Record an error, stamping it with the current context ancestry.
    pub fn record(&mut self, mut err: DiagnosticError<'a>) {
        err.context = self.context_stack.iter().rev().cloned().collect();
        self.errors.push(err);
    }

    pub fn record_warning(&mut self, warn: DiagnosticWarning<'a>) {
        self.warnings.push(warn);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    /// True when we've exceeded the error budget and should stop trying.
    pub fn error_limit_reached(&self) -> bool {
        self.errors.len() >= self.max_errors
    }

    /// Take ownership of collected errors (consumes the context).
    pub fn into_errors(self) -> Vec<DiagnosticError<'a>> {
        self.errors
    }

    pub fn errors(&self) -> &[DiagnosticError] {
        &self.errors
    }

    pub fn warnings(&self) -> &[DiagnosticWarning] {
        &self.warnings
    }

    // -----------------------------------------------------------------------
    // Context stack
    // -----------------------------------------------------------------------

    pub fn push_context(&mut self, ctx: ParseContext) {
        self.context_stack.push(ctx);
    }

    pub fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    /// RAII guard: push a context, run `f`, pop on exit regardless of outcome.
    ///
    /// If `f` returns `Err`, the error is *not* automatically recorded —
    /// the caller decides whether to record-and-recover or to propagate.
    pub fn with_context<T, E, F>(&mut self, ctx: ParseContext, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
    {
        self.push_context(ctx);
        let result = f(self);
        self.pop_context();
        result
    }

    // -----------------------------------------------------------------------
    // Recovery / synchronisation
    // -----------------------------------------------------------------------

    /// Synchronisation tokens: structural anchors that delimit top-level items.
    fn is_sync_token(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Semicolon
                | TokenKind::RBrace
                | TokenKind::Fn
                | TokenKind::Struct
                | TokenKind::Impl
                | TokenKind::Enum
                | TokenKind::Interface
                | TokenKind::Import
                | TokenKind::Module
                | TokenKind::EOF
        )
    }

    /// Panic-mode recovery: advance the cursor until a synchronisation point.
    ///
    /// Returns the `TokenKind` we stopped at so the caller can decide whether
    /// to consume it (e.g. consume `}`) or leave it (e.g. leave `fn`).
    pub fn synchronize(&self, cursor: &mut Cursor<'a>) -> TokenKind {
        loop {
            let kind = cursor.peek();
            if Self::is_sync_token(kind) {
                return kind;
            }
            cursor.advance();
        }
    }

    /// Record `err`, then synchronise the cursor.
    ///
    /// Convenience for the common recovery pattern:
    ///   1. emit the error
    ///   2. skip garbage tokens
    ///   3. continue parsing from the next safe point
    pub fn record_and_recover(
        &mut self,
        err: DiagnosticError<'a>,
        cursor: &mut Cursor<'a>,
    ) -> TokenKind {
        self.record(err);
        self.synchronize(cursor)
    }

    // -----------------------------------------------------------------------
    // Tracing
    // -----------------------------------------------------------------------

    /// Returns a RAII trace scope.  Emit nothing when tracing is disabled.
    pub fn trace_scope(&self, rule: &'static str) -> TraceScope {
        if self.tracing_enabled {
            eprintln!("ENTER {rule}");
        }
        TraceScope {
            rule,
            enabled: self.tracing_enabled,
        }
    }

    /// Emit a single trace token event.
    pub fn trace_token(&self, kind: TokenKind) {
        if self.tracing_enabled {
            eprintln!("  TOKEN {kind:?}");
        }
    }

    /// Emit a trace failure event (called before returning Err).
    pub fn trace_fail(&self, reason: &str) {
        if self.tracing_enabled {
            eprintln!("  FAIL  {reason}");
        }
    }
}

// ---------------------------------------------------------------------------
// TraceScope — RAII guard that emits EXIT on drop
// ---------------------------------------------------------------------------

pub struct TraceScope {
    rule: &'static str,
    enabled: bool,
}

impl Drop for TraceScope {
    fn drop(&mut self) {
        if self.enabled {
            eprintln!("EXIT  {}", self.rule);
        }
    }
}

// ---------------------------------------------------------------------------
// DiagnosticWarning
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct DiagnosticWarning<'a> {
    pub message: String,
    pub span: SourceSpan<'a>,
}

impl<'a> DiagnosticWarning<'a> {
    pub fn new(message: impl Into<String>, span: SourceSpan<'a>) -> Self {
        DiagnosticWarning {
            message: message.into(),
            span,
        }
    }
}

// ---------------------------------------------------------------------------
// ParseResult — the canonical result type for all parser methods
// ---------------------------------------------------------------------------

/// Parser methods should return `ParseResult<T>`.
/// On `Err`, the error has already been recorded in `diag` *and* recovery
/// has run, so the caller can continue without re-recording.
pub type ParseResult<T> = Result<T, RecoveredError>;

/// A sentinel indicating that an error was recorded and recovery ran.
/// Callers propagate this upward with `?`; they do not inspect its contents.
#[derive(Debug, Clone, Copy)]
pub struct RecoveredError;

// ---------------------------------------------------------------------------
// Render helpers
// ---------------------------------------------------------------------------

/// Pretty-print all collected errors to stderr (or a writer).
pub fn render_errors(errors: &[DiagnosticError]) {
    for err in errors {
        eprintln!("{}", err.pretty());
        if errors.len() > 1 {
            eprintln!("{}", "-".repeat(60));
        }
    }
}