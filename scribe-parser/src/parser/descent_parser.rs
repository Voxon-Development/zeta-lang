use ir::ast::{Expr, Generic, Stmt, Visibility};
pub(crate) use ir::errors::error::ParserError;
use ir::hir::StrId;
use ir::span::SourceSpan;
use ir::ssa_ir::BinOp;
use std::error::Error;
use std::fmt;
use std::marker::PhantomData;
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
}

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse(
        string_pool: Arc<StringPool>,
        bump: &'bump GrowableBump<'bump>,
        tokens: &'a Tokens<'a>,
    ) -> Result<Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>, ParserError<'a>> {
        let mut parser = DescentParser {
            cursor: Cursor::new(tokens, 0),
            string_pool,
            bump,
        };

        parser.parse_toplevel()
    }

    fn parse_toplevel(
        &mut self,
    ) -> Result<Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>, ParserError<'a>> {
        let mut stmts = Vec::new_in(self.bump);

        while self.cursor.peek() != TokenKind::EOF {
            stmts.push(self.parse_stmt(Visibility::Public)?);
        }

        Ok(stmts)
    }

    pub(crate) fn parse_stmt(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, ParserError<'a>> {
        let kind = self.cursor.peek();
        match kind {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Import => self.parse_import(),
            TokenKind::Package => self.parse_package(),
            TokenKind::Match => self.parse_match_stmt(),
            TokenKind::Defer => self.parse_defer_stmt(),
            TokenKind::Static => {
                self.cursor.advance(); // consume 'static'
                self.parse_static_let_stmt()
            }
            TokenKind::Fn => self.parse_function_with_visibility(visibility),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Break => {
                self.cursor.advance();
                self.cursor.consume(TokenKind::Semicolon);
                Ok(Stmt::Break)
            }
            TokenKind::Continue => {
                self.cursor.advance();
                self.cursor.consume(TokenKind::Semicolon);
                Ok(Stmt::Continue)
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
                    self.parse_module_decl(Visibility::Module)
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
            TokenKind::Impl => self.parse_impl_decl(visibility),
            TokenKind::Interface => self.parse_interface_decl(visibility, false),
            TokenKind::Sealed => {
                // sealed interface Name permits X, Y, Z { ... }
                self.cursor.advance();
                self.cursor.expect(TokenKind::Interface)?;
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

pub struct ParseResult<'a, 'bump> {
    pub statements: Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>,
    pub diagnostics: ParserDiagnostics<'a>,
}

pub fn parse_program<'a, 'bump>(
    src: &'bump str,
    file_name: &'bump str,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>,
) -> ParseResult<'a, 'bump> {
    let mut diagnostics = ParserDiagnostics::new();

    let parser_bump = Box::new(GrowableBump::new(4096, 8));
    let parser_bump_ref: &'bump GrowableBump<'bump> = Box::leak(parser_bump);

    let lexer: Lexer = Lexer::new(context.clone());
    let tokens: Tokens<'a> = lexer.tokenize(src, file_name, parser_bump_ref);

    let tokenized_source = bump.alloc_value(tokens);

    let stmts: Vec<Stmt, &'bump GrowableBump<'bump>> =
        DescentParser::parse(context, parser_bump_ref, tokenized_source).unwrap();

    ParseResult {
        statements: stmts,
        diagnostics,
    }
}

/// Collects parser diagnostics (errors, warnings, notes) instead of panicking
#[derive(Debug, Clone)]
pub struct ParserDiagnostics<'a> {
    pub errors: Vec<ParserError<'a>>,
    pub warnings: Vec<String>,
}

impl<'a> ParserDiagnostics<'a> {
    pub fn new() -> Self {
        ParserDiagnostics {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn add_error(&mut self, error: ParserError<'a>) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: String) {
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
