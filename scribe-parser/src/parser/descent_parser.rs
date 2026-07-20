use ir::ast::{Stmt, Visibility};
use ir::diagnostics_context::{DiagnosticWarning, ParserDiagnosticsContext};
use ir::errors::error::DiagnosticError;
use ir::hir::StrId;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

use crate::tokenizer::lexer::Lexer;
use ir::tokens::{Cursor, TokenKind, Tokens};

pub struct DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub(crate) cursor: Cursor<'a>,
    pub(crate) bump: &'bump GrowableBump<'bump>,
    pub(crate) string_pool: Arc<StringPool>,
    pub(crate) diag: ParserDiagnosticsContext<'a>,
}

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse(
        string_pool: Arc<StringPool>,
        bump: &'bump GrowableBump<'bump>,
        tokens: &'a Tokens<'a>,
    ) -> Result<Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>, DiagnosticError<'a>> {
        let mut parser = DescentParser {
            cursor: Cursor::new(tokens, 0),
            string_pool,
            bump,
            diag: ParserDiagnosticsContext::new(true),
        };

        parser.parse_toplevel()
    }

    fn parse_toplevel(
        &mut self,
    ) -> Result<Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>, DiagnosticError<'a>> {
        let mut stmts = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::EOF {
            match self.parse_stmt(Visibility::Public) {
                Ok(s) => stmts.push(s),
                Err(e) => {
                    self.diag.record(e);
                    let stop = self.diag.synchronize(&mut self.cursor);
                    if stop == TokenKind::EOF {
                        break;
                    }
                }
            }
        }

        Ok(stmts)
    }

    pub(crate) fn parse_stmt(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let kind = self.cursor.peek();
        match kind {
            TokenKind::Panic => {
                let tok = self.cursor.expect(TokenKind::Panic)?;
                let message = self.parse_expr(0)?;
                self.cursor.expect(TokenKind::Semicolon)?;
                Ok(Stmt::Panic {
                    message,
                    span: tok.span,
                })
            }
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Import => self.parse_import(),
            TokenKind::Package => self.parse_package(),
            TokenKind::Match => self.parse_match_stmt(),
            TokenKind::Defer => self.parse_defer_stmt(),
            TokenKind::Static => {
                self.cursor.advance(); // consume 'static'
                self.parse_static_let_stmt()
            }
            TokenKind::Func => self.parse_function_with_visibility(visibility),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Break => self.parse_break_stmt(),
            TokenKind::Continue => self.parse_continue_stmt(),

            TokenKind::Public => {
                self.cursor.advance();
                self.parse_stmt(Visibility::Private)
            }

            TokenKind::Private => {
                self.cursor.advance();
                self.parse_stmt(Visibility::Private)
            }

            TokenKind::Module => {
                self.cursor.advance();
                // `module Name { ... }` is a module *declaration*.
                // `module fn foo()` / `module struct Bar {}` is a visibility modifier.
                if self.cursor.peek() == TokenKind::Ident
                    && self.cursor.peek_n(1) == TokenKind::LBrace
                {
                    self.parse_module_decl(visibility)
                } else {
                    self.parse_stmt(Visibility::Module)
                }
            }

            TokenKind::Internal => {
                self.cursor.advance();
                self.parse_stmt(Visibility::Internal)
            }

            TokenKind::Inline | TokenKind::Noinline | TokenKind::Unsafe | TokenKind::Extern => {
                self.parse_function_with_visibility(visibility)
            }

            TokenKind::Struct => self.parse_struct_decl(visibility),
            TokenKind::Enum => self.parse_enum_decl(visibility),
            TokenKind::Impl => self.parse_impl_decl(visibility),
            TokenKind::Interface => self.parse_interface_decl(visibility, false),
            TokenKind::Sealed => {
                // sealed interface Name permits X, Y, Z { ... }
                self.cursor.advance();
                self.parse_interface_decl(visibility, true)
            }

            TokenKind::Ident => {
                // Check for shorthand let (ident := expr)
                if self.cursor.peek_n(1) == TokenKind::ColonAssign {
                    self.parse_shorthand_let_stmt()
                } else {
                    self.parse_expr_stmt()
                }
            }

            _ => self.parse_expr_stmt(),
        }
    }
}

pub fn token_to_visibility(token_kind: TokenKind) -> Visibility {
    match token_kind {
        TokenKind::Private => Visibility::Private,
        TokenKind::Module => Visibility::Module,
        TokenKind::Package => Visibility::Internal,
        _ => Visibility::Public,
    }
}

#[derive(Debug)]
pub struct ParseResult<'a, 'bump> {
    pub statements: Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>,
    pub diagnostics: ParserDiagnostics<'a>,
}

pub fn parse_program<'a, 'bump>(
    src: StrId,
    file_name: &'bump str,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> ParseResult<'a, 'bump> {
    let parser_bump = Box::new(GrowableBump::new(4096, 8));
    let parser_bump_ref: &'bump GrowableBump<'bump> = Box::leak(parser_bump);

    let lexer: Lexer = Lexer::new(context.clone());
    let tokens: Tokens<'a> = lexer.tokenize(src.as_str(), file_name, parser_bump_ref);

    let tokenized_source = bump.alloc_value(tokens);

    let mut parser = DescentParser {
        cursor: Cursor::new(tokenized_source, 0),
        string_pool: context,
        bump: parser_bump_ref,
        diag: ParserDiagnosticsContext::new(false),
    };

    let stmts: Vec<Stmt<'_, '_>, &GrowableBump<'_>> = match parser.parse_toplevel() {
        Ok(s) => s,
        Err(e) => {
            parser.diag.record(e);
            Vec::new_in(parser_bump_ref)
        }
    };

    let (errors, warnings) = parser.diag.into_diagnostics();

    ParseResult {
        statements: stmts,
        diagnostics: ParserDiagnostics { errors, warnings },
    }
}

/// Collects parser diagnostics (errors, warnings, notes) instead of panicking
#[derive(Debug, Clone)]
pub struct ParserDiagnostics<'a> {
    pub errors: Vec<DiagnosticError<'a>>,
    pub warnings: Vec<DiagnosticWarning<'a>>,
}

impl<'a> ParserDiagnostics<'a> {
    pub fn new() -> Self {
        ParserDiagnostics {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn add_error(&mut self, error: DiagnosticError<'a>) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: DiagnosticWarning<'a>) {
        self.warnings.push(warning);
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
}
