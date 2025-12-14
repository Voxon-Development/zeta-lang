use ir::ast::Stmt;
use ir::span::SourceSpan;
use std::error::Error;
use std::fmt;
use std::sync::Arc;

use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

use crate::parser::declaration_parser::DeclarationParser;
use crate::parser::statement_parser::StatementParser;
use crate::tokenizer::tokens::Tokens;
use crate::tokenizer::cursor::TokenCursor;
use crate::tokenizer::tokens::TokenKind;
use crate::tokenizer::lexer::Lexer;

pub struct DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    bump: &'bump GrowableBump<'bump>,
    statement_parser: StatementParser<'a, 'bump>,
    declaration_parser: DeclarationParser<'a, 'bump>,
}

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(context: Arc<StringPool>, bump: &'bump GrowableBump<'bump>) -> Self {
        Self {
            bump,
            statement_parser: StatementParser::new(context.clone(), bump),
            declaration_parser: DeclarationParser::new(context.clone(), bump),
        }
    }
    
    /// Get a reference to the bump allocator
    pub fn bump(&self) -> &'bump GrowableBump<'bump> {
        self.bump
    }
    
    /// Parse a program using iterative descent
    pub fn parse(&self, tokens: &'a Tokens<'a>) -> Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>> {
        let mut cursor = TokenCursor::from_tokens(tokens);
        let mut stmts: Vec<Stmt, &GrowableBump> = Vec::new_in(self.bump);
        
        while !cursor.at_end() {
            match cursor.peek_kind() {
                Some(crate::tokenizer::tokens::TokenKind::EOF) => break,
                Some(_) => {
                    if let Some(stmt) = self.parse_top_level(&mut cursor) {
                        stmts.push(stmt);
                    }
                }
                None => break,
            }
        }
        
        stmts
    }
    
    fn parse_top_level(&self, cursor: &mut TokenCursor<'a>) -> Option<ir::ast::Stmt<'a, 'bump>> {
        let kind: TokenKind = cursor.peek_kind()?;
        match kind {
            TokenKind::Import => Some(self.declaration_parser.parse_import(cursor)),
            TokenKind::Package => {
                // Disambiguate: package statement vs package visibility modifier
                if cursor.peek_kind_n(1) == Some(TokenKind::Ident) {
                    // package "name"; - this is a package statement
                    Some(self.declaration_parser.parse_package(cursor))
                } else {
                    // package <something else> - this is a visibility modifier, determine what follows
                    self.parse_declaration_with_visibility(cursor)
                }
            }
            
            // Visibility modifiers - check what declaration follows
            TokenKind::Private | TokenKind::Module => {
                self.parse_declaration_with_visibility(cursor)
            }
            
            // Direct declarations (no visibility specified, defaults to public)
            TokenKind::Enum => Some(self.declaration_parser.parse_enum(cursor)),
            TokenKind::Interface => Some(self.declaration_parser.parse_interface(cursor)),
            TokenKind::Impl => Some(self.declaration_parser.parse_impl_decl(cursor)),
            TokenKind::Struct => Some(self.declaration_parser.parse_struct_decl(cursor)),
            TokenKind::Statem => Some(self.declaration_parser.parse_state_machine(cursor)),
            TokenKind::Effect => Some(self.declaration_parser.parse_effect(cursor)),
            
            TokenKind::Unsafe | TokenKind::Extern | TokenKind::Inline | TokenKind::Noinline | TokenKind::Fn => {
                Some(self.declaration_parser.parse_function(cursor))
            }
            
            _ => Some(self.statement_parser.parse_stmt(cursor)),
        }
    }
    
    fn parse_declaration_with_visibility(&self, cursor: &mut TokenCursor<'a>) -> Option<ir::ast::Stmt<'a, 'bump>> {
        use crate::tokenizer::tokens::TokenKind;
        
        // Look ahead to see what declaration follows the visibility modifier
        let mut lookahead_pos = 0;
        
        // Skip visibility modifiers
        while let Some(kind) = cursor.peek_kind_n(lookahead_pos) {
            match kind {
                TokenKind::Private | TokenKind::Module | TokenKind::Package => {
                    lookahead_pos += 1;
                }
                _ => break,
            }
        }
        
        // Check what declaration type follows the visibility
        match cursor.peek_kind_n(lookahead_pos) {
            Some(TokenKind::Enum) => Some(self.declaration_parser.parse_enum(cursor)),
            Some(TokenKind::Interface) => Some(self.declaration_parser.parse_interface(cursor)),
            Some(TokenKind::Impl) => Some(self.declaration_parser.parse_impl_decl(cursor)),
            Some(TokenKind::Struct) => Some(self.declaration_parser.parse_struct_decl(cursor)),
            Some(TokenKind::Statem) => Some(self.declaration_parser.parse_state_machine(cursor)),
            Some(TokenKind::Effect) => Some(self.declaration_parser.parse_effect(cursor)),
            
            // Function-related tokens (including modifiers and function declarations)
            Some(TokenKind::Unsafe) | Some(TokenKind::Extern) | Some(TokenKind::Inline) | 
            Some(TokenKind::Noinline) | Some(TokenKind::Fn) | Some(TokenKind::Ident) => {
                Some(self.declaration_parser.parse_function(cursor))
            }
            
            // If we can't determine the declaration type, default to function parsing
            _ => Some(self.statement_parser.parse_stmt(cursor)),
        }
    }
}

pub struct ParseResult<'a, 'bump> {
    pub statements: Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>>,
    pub diagnostics: ParserDiagnostics,
}

pub fn parse_program<'a, 'bump>(
    src: &'bump str,
    file_name: &'bump str,
    context: Arc<StringPool>,
    bump: Arc<GrowableAtomicBump<'bump>>
) -> ParseResult<'a, 'bump> {
    let mut diagnostics = ParserDiagnostics::new();
    
    let lexer: Lexer<'a, 'bump> = Lexer::from_str(src, file_name, context.clone());
    let tokens: Tokens<'a> = lexer.tokenize();

    //println!("{:?}", tokens);
    
    let tokenized_source = bump.alloc_value(tokens);
    
    // Create a safe GrowableBump that lives long enough
    // We'll use a Box to ensure it has a stable address
    let parser_bump = Box::new(GrowableBump::new(4096, 8));
    let parser_bump_ref: &'bump GrowableBump<'bump> = Box::leak(parser_bump);
    
    let parser: DescentParser<'a, 'bump> = DescentParser::new(context, parser_bump_ref);
    let stmts: Vec<Stmt, &'bump GrowableBump<'bump>> = parser.parse(tokenized_source);
    
    ParseResult {
        statements: stmts,
        diagnostics,
    }
}

#[derive(Debug, Clone)]
pub enum ParserError {
    LexerError(String),
    UnexpectedToken { expected: String, found: String },
    UnexpectedEof,
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::LexerError(msg) => write!(f, "Lexer error: {}", msg),
            ParserError::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found {}", expected, found)
            }
            ParserError::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}

/// Collects parser diagnostics (errors, warnings, notes) instead of panicking
#[derive(Debug, Clone)]
pub struct ParserDiagnostics {
    pub errors: Vec<ParserError>,
    pub warnings: Vec<String>,
}

impl ParserDiagnostics {
    pub fn new() -> Self {
        ParserDiagnostics {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn add_error(&mut self, error: ParserError) {
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
