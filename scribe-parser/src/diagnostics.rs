use std::sync::Arc;
use zetaruntime::string_pool::StringPool;
use ir::errors::reporter::ErrorReporter;
use ir::span::SourceSpan;
use crate::parser::descent_parser::ParserError;

pub struct ParserDiagnostics<'a> {
    string_pool: Arc<StringPool>,
    file_name: &'a str,
    source_content: &'a str,
}

impl<'a> ParserDiagnostics<'a> {
    pub fn new(string_pool: Arc<StringPool>, file_name: &'a str, source_content: &'a str) -> Self {
        Self {
            string_pool,
            file_name,
            source_content,
        }
    }
    
    pub fn report_parser_errors<'bump>(
        &self,
        errors: &[ParserError],
        reporter: &mut ErrorReporter<'a, 'bump>,
    ) {
        for error in errors {
            self.report_parser_error(error, reporter);
        }
    }
    
    fn report_parser_error<'bump>(
        &self,
        error: &ParserError,
        reporter: &mut ErrorReporter<'a, 'bump>,
    ) {
        match error {
            ParserError::LexerError(msg) => {
                let span = SourceSpan::new(self.file_name, 1, 1);
                reporter.add_parser_error(
                    format!("Lexer error: {}", msg),
                    Some(span),
                );
            }
            ParserError::UnexpectedToken { expected, found } => {
                let span = SourceSpan::new(self.file_name, 1, 1); // TODO: Get actual position
                reporter.add_parser_error(
                    format!("Expected '{}', but found '{}'", expected, found),
                    Some(span),
                );
            }
            ParserError::UnexpectedEof => {
                let lines: Vec<&str> = self.source_content.lines().collect();
                let last_line = lines.len();
                let last_col = lines.last().map(|line| line.len()).unwrap_or(1);
                
                let span = SourceSpan::new(self.file_name, last_line, last_col);
                reporter.add_parser_error(
                    "Unexpected end of file".to_string(),
                    Some(span),
                );
            }
        }
    }
    
    pub fn report_syntax_error<'bump>(
        &self,
        message: &str,
        line: usize,
        col: usize,
        reporter: &mut ErrorReporter<'a, 'bump>,
    ) {
        let span = SourceSpan::new(self.file_name, line, col);
        reporter.add_parser_error(message.to_string(), Some(span));
    }
    
    pub fn report_missing_semicolon<'bump>(
        &self,
        line: usize,
        col: usize,
        reporter: &mut ErrorReporter<'a, 'bump>,
    ) {
        let span = SourceSpan::new(self.file_name, line, col);
        reporter.add_parser_error(
            "Missing semicolon".to_string(),
            Some(span),
        );
    }
    
    pub fn report_missing_brace<'bump>(
        &self,
        brace_type: &str,
        line: usize,
        col: usize,
        reporter: &mut ErrorReporter<'a, 'bump>,
    ) {
        let span = SourceSpan::new(self.file_name, line, col);
        reporter.add_parser_error(
            format!("Missing {}", brace_type),
            Some(span),
        );
    }
    
    pub fn report_invalid_syntax<'bump>(
        &self,
        context: &str,
        line: usize,
        col: usize,
        reporter: &mut ErrorReporter<'a, 'bump>,
    ) {
        let span = SourceSpan::new(self.file_name, line, col);
        reporter.add_parser_error(
            format!("Invalid syntax in {}", context),
            Some(span),
        );
    }
}

#[derive(Debug, Clone)]
pub struct EnhancedParserError {
    pub message: String,
    pub span: SourceSpan<'static>,
    pub error_type: ParserErrorType,
}

#[derive(Debug, Clone)]
pub enum ParserErrorType {
    SyntaxError,
    UnexpectedToken,
    MissingToken,
    InvalidExpression,
    InvalidStatement,
    InvalidDeclaration,
}

impl EnhancedParserError {
    pub fn new(message: String, span: SourceSpan<'static>, error_type: ParserErrorType) -> Self {
        Self {
            message,
            span,
            error_type,
        }
    }
    
    pub fn syntax_error(message: String, span: SourceSpan<'static>) -> Self {
        Self::new(message, span, ParserErrorType::SyntaxError)
    }
    
    pub fn unexpected_token(expected: &str, found: &str, span: SourceSpan<'static>) -> Self {
        Self::new(
            format!("Expected '{}', found '{}'", expected, found),
            span,
            ParserErrorType::UnexpectedToken,
        )
    }
    
    pub fn missing_token(token: &str, span: SourceSpan<'static>) -> Self {
        Self::new(
            format!("Missing '{}'", token),
            span,
            ParserErrorType::MissingToken,
        )
    }
}
