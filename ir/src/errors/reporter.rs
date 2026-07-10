use crate::errors::type_error::{TypeError, TypeErrorKind};
use crate::hir::StrId;
use crate::span::SourceSpan;
use is_terminal::IsTerminal;
use owo_colors::OwoColorize;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
    Note,
}

#[derive(Debug, Clone)]
pub struct Diagnostic<'a> {
    pub level: DiagnosticLevel,
    pub message: String,
    pub span: Option<SourceSpan<'a>>,
    pub notes: Vec<String>,
    pub suggestions: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum CompilerError<'a> {
    TypeError(TypeError<'a>),
    ParserError {
        message: StrId,
        span: Option<SourceSpan<'a>>,
    },
}

pub struct ErrorReporter<'a> {
    pub errors: Vec<CompilerError<'a>>,
    pub source_files: HashMap<String, String>,
    pub use_colors: bool,
}

impl<'a> ErrorReporter<'a> {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            source_files: HashMap::new(),
            use_colors: std::io::stderr().is_terminal(),
        }
    }

    pub fn with_colors(mut self, use_colors: bool) -> Self {
        self.use_colors = use_colors;
        self
    }

    pub fn add_source_file(&mut self, filename: String, content: String) {
        self.source_files.insert(filename, content);
    }

    pub fn add_type_error(&mut self, error: TypeError<'a>) {
        self.errors.push(CompilerError::TypeError(error));
    }

    pub fn add_parser_error(&mut self, message: StrId, span: Option<SourceSpan<'a>>) {
        self.errors
            .push(CompilerError::ParserError { message, span });
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn report_all(&self) {
        for error in &self.errors {
            self.report_error(error);
        }
        if !self.errors.is_empty() {
            let count = self.errors.len();
            if self.use_colors {
                eprintln!(
                    "\n{} Found {} error{}",
                    "error".bright_red().bold(),
                    count,
                    if count == 1 { "" } else { "s" }
                );
            } else {
                eprintln!(
                    "\nFound {} error{}",
                    count,
                    if count == 1 { "" } else { "s" }
                );
            }
        }
    }

    fn report_error(&self, error: &CompilerError<'a>) {
        match error {
            CompilerError::TypeError(te) => {
                let diagnostic = self.type_error_to_diagnostic(te);
                self.print_diagnostic(&diagnostic);
            }
            CompilerError::ParserError { message, span } => {
                self.print_diagnostic(&Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: format!("Parse error: {}", message.as_str()),
                    span: span.clone(),
                    notes: vec![],
                    suggestions: vec![],
                });
            }
        }
    }

    fn type_error_to_diagnostic(&self, error: &TypeError<'a>) -> Diagnostic<'a> {
        let span = if error.has_known_span() {
            Some(error.span)
        } else {
            None
        };

        let (message, suggestions) = match &error.kind {
            TypeErrorKind::TypeMismatch { expected, found } => (
                format!(
                    "Type mismatch: expected '{}' but found '{}'",
                    expected, found
                ),
                vec![format!("Try converting '{}' to '{}'", found, expected)],
            ),
            TypeErrorKind::UndefinedVariable(name) => (
                format!("Undefined variable '{}'", name),
                vec!["Check if the variable is declared and in scope".to_string()],
            ),
            TypeErrorKind::UndefinedType(name) => (
                format!("Unknown type '{}'", name),
                vec!["Check if the type is imported or defined".to_string()],
            ),
            TypeErrorKind::UndefinedFunction(name) => (
                format!("Undefined function '{}'", name),
                vec!["Check if the function is declared and in scope".to_string()],
            ),
            TypeErrorKind::UndefinedFunctionWithSuggestion {
                name,
                suggested_modules,
            } => {
                let msg = if suggested_modules.len() == 1 {
                    format!(
                        "Undefined function '{}', a function with this name exists in module `{}`, did you mean to use `{}.{}`?",
                        name,
                        suggested_modules[0],
                        suggested_modules[0],
                        name.rsplit("::").next().unwrap_or(name),
                    )
                } else {
                    format!(
                        "Undefined function '{}', exists in modules: {}",
                        name,
                        suggested_modules.join(", ")
                    )
                };
                (msg, vec![])
            }
            TypeErrorKind::FieldNotFound { struct_name, field } => (
                format!("No field '{}' found on type '{}'", field, struct_name),
                vec!["Check the field name and type definition".to_string()],
            ),
            TypeErrorKind::MethodNotFound {
                struct_name,
                method,
            } => (
                format!("No method '{}' found on type '{}'", method, struct_name),
                vec!["Check the method name and available methods on this type".to_string()],
            ),
            TypeErrorKind::InvalidFunctionCall {
                expected_args,
                found_args,
            } => (
                format!(
                    "Wrong number of arguments: expected {}, found {}",
                    expected_args, found_args
                ),
                vec![],
            ),
            TypeErrorKind::InvalidReturnType { expected, found } => (
                format!(
                    "Invalid return type: expected '{}', found '{}'",
                    expected, found
                ),
                vec![format!(
                    "Change the return expression to produce a '{}'",
                    expected
                )],
            ),
            TypeErrorKind::InvalidBinaryOp { op, left, right } => (
                format!("Invalid binary operation: '{}' {} '{}'", left, op, right),
                vec![],
            ),
            TypeErrorKind::InvalidUnaryOp { op, operand } => (
                format!("Invalid unary operation: {}{}", op, operand),
                vec![],
            ),
            TypeErrorKind::BreakOutsideLoop => {
                ("Cannot use `break` outside of a loop".to_string(), vec![])
            }
            TypeErrorKind::ContinueOutsideLoop => (
                "Cannot use `continue` outside of a loop".to_string(),
                vec![],
            ),
            TypeErrorKind::Generic(msg) => (msg.clone(), vec![]),
            TypeErrorKind::VariableAlreadyExists { var_name } => (
                format!("Variable already exists: {}", var_name),
                vec![
                    "Rename the variable to another name that can be more descriptive.".to_string(),
                ],
            ),
            TypeErrorKind::IllegalThisParam { func_name } => (
                format!(
                    "Illegal this parameter: free function {} contains `this`",
                    func_name
                ),
                vec![
                    "Remove the `this` param or put this into a struct that may own it."
                        .to_string(),
                ],
            ),
            TypeErrorKind::TypeCannotBeInferred => (
                "Type cannot be inferred".to_string(),
                vec!["please include an explicit type".to_string()],
            ),
        };

        Diagnostic {
            level: DiagnosticLevel::Error,
            message,
            span,
            notes: vec![],
            suggestions,
        }
    }

    fn print_diagnostic(&self, diagnostic: &Diagnostic<'a>) {
        let prefix = match diagnostic.level {
            DiagnosticLevel::Error => {
                if self.use_colors {
                    "error".bright_red().bold().to_string()
                } else {
                    "error".to_string()
                }
            }
            DiagnosticLevel::Warning => {
                if self.use_colors {
                    "warning".bright_yellow().bold().to_string()
                } else {
                    "warning".to_string()
                }
            }
            DiagnosticLevel::Info => {
                if self.use_colors {
                    "info".bright_blue().bold().to_string()
                } else {
                    "info".to_string()
                }
            }
            DiagnosticLevel::Note => {
                if self.use_colors {
                    "note".cyan().bold().to_string()
                } else {
                    "note".to_string()
                }
            }
        };

        eprintln!("{}: {}", prefix, diagnostic.message);

        if let Some(span) = &diagnostic.span {
            self.print_source_snippet(span);
        }
        for note in &diagnostic.notes {
            eprintln!("  note: {}", note);
        }
        for suggestion in &diagnostic.suggestions {
            eprintln!("  help: {}", suggestion);
        }
        eprintln!();
    }

    fn print_source_snippet(&self, span: &SourceSpan<'a>) {
        let Some(source) = self.source_files.get(span.file_name) else {
            return;
        };

        let lines: Vec<&str> = source.lines().collect();

        let start = span.line.saturating_sub(3).max(1);
        let end = (span.line + 2).min(lines.len());

        let width = end.to_string().len();

        for i in start..=end {
            let line = lines.get(i - 1).unwrap_or(&"");

            let gutter = if self.use_colors {
                format!("{:>width$}", i, width = width)
                    .bright_black()
                    .to_string()
            } else {
                format!("{:>width$}", i, width = width)
            };

            eprintln!(" {} | {}", gutter, line);

            if i == span.line {
                let col = span.column.saturating_sub(1).min(line.len());

                let mut marker = String::new();
                marker.push_str(&" ".repeat(width + 3 + col));
                marker.push('^');

                if span.end_line == span.line && span.end_column > span.column {
                    let len = span.end_column.saturating_sub(span.column);
                    marker.push_str(&"~".repeat(len.max(1)));
                }

                if self.use_colors {
                    eprintln!("{}", marker.red().bold());
                } else {
                    eprintln!("{}", marker);
                }
            }
        }
    }
}

impl<'a> Default for ErrorReporter<'a> {
    fn default() -> Self {
        Self::new()
    }
}
