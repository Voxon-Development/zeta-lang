use crate::errors::error::TypeError;
use crate::span::SourceSpan;
use std::collections::HashMap;
use zetaruntime::bump::GrowableBump;

use is_terminal::IsTerminal;
use owo_colors::OwoColorize;

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
pub enum CompilerError<'a, 'bump> {
    TypeError(TypeError<'a, 'bump>),
    ParserError {
        message: String,
        span: Option<SourceSpan<'a>>,
    },
    CTRCError {
        message: String,
        span: Option<SourceSpan<'a>>,
        leak_info: Option<CTRCLeakInfo>,
    },
}

#[derive(Debug, Clone)]
pub struct CTRCLeakInfo {
    pub variable_name: String,
    pub allocation_site: String,
    pub potential_cycles: Vec<String>,
}

pub struct ErrorReporter<'a, 'bump>
where
    'bump: 'a,
{
    pub errors: Vec<CompilerError<'a, 'bump>, GrowableBump<'bump>>,
    pub source_files: HashMap<String, String>,
    pub use_colors: bool,
}

impl<'a, 'bump> ErrorReporter<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            errors: Vec::new_in(GrowableBump::new(
                size_of::<CompilerError>() * 16,
                align_of::<CompilerError>(),
            )),
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

    pub fn add_type_error(&mut self, error: TypeError<'a, 'bump>) {
        self.errors.push(CompilerError::TypeError(error));
    }

    pub fn add_parser_error(&mut self, message: String, span: Option<SourceSpan<'a>>) {
        self.errors.push(CompilerError::ParserError { message, span });
    }

    pub fn add_ctrc_error(&mut self, message: String, span: Option<SourceSpan<'a>>, leak_info: Option<CTRCLeakInfo>) {
        self.errors.push(CompilerError::CTRCError { message, span, leak_info });
    }

    pub fn report_all(&self) {
        for error in &self.errors {
            self.report_error(error);
        }
        if !self.errors.is_empty() {
            let error_count = self.errors.len();
            if self.use_colors {
                eprintln!("\n{} Found {} error{}", "error".bright_red().bold(), error_count, if error_count == 1 { "" } else { "s" });
            } else {
                eprintln!("\nFound {} error{}", error_count, if error_count == 1 { "" } else { "s" });
            }
        }
    }

    fn report_error(&self, error: &CompilerError<'a, 'bump>) {
        match error {
            CompilerError::TypeError(te) => self.report_type_error(te),
            CompilerError::ParserError { message, span } => self.report_parser_error(message, span.as_ref()),
            CompilerError::CTRCError { message, span, leak_info } => self.report_ctrc_error(message, span.as_ref(), leak_info.as_ref()),
        }
    }

    fn report_type_error(&self, error: &TypeError<'a, 'bump>) {
        let diagnostic = self.type_error_to_diagnostic(error);
        self.print_diagnostic(&diagnostic);
    }

    fn type_error_to_diagnostic(&self, error: &TypeError<'a, 'bump>) -> Diagnostic<'a> {
        match error {
            TypeError::Mismatch { expected, found, location } => Diagnostic {
                level: DiagnosticLevel::Error,
                message: format!("Type mismatch: expected '{}' but found '{}'", self.type_to_string(expected), self.type_to_string(found)),
                span: Some(*location),
                notes: vec![],
                suggestions: vec![format!("Try converting '{}' to '{}'", self.type_to_string(found), self.type_to_string(expected))],
            },
            TypeError::UndefinedSymbol { name, location } => Diagnostic {
                level: DiagnosticLevel::Error,
                message: format!("Undefined symbol '{}'", name),
                span: Some(*location),
                notes: vec![],
                suggestions: vec!["Check if the symbol is declared and in scope".to_string()],
            },
            TypeError::UnknownType { name, location } => Diagnostic {
                level: DiagnosticLevel::Error,
                message: format!("Unknown type '{}'", name),
                span: Some(*location),
                notes: vec![],
                suggestions: vec!["Check if the type is imported or defined".to_string()],
            },
            TypeError::NoSuchField { type_name, field_name, location } => Diagnostic {
                level: DiagnosticLevel::Error,
                message: format!("No field '{}' found on type '{}'", field_name, type_name),
                span: Some(*location),
                notes: vec![],
                suggestions: vec!["Check the field name and type definition".to_string()],
            },
            TypeError::NoSuchMethod { type_name, method_name, location } => Diagnostic {
                level: DiagnosticLevel::Error,
                message: format!("No method '{}' found on type '{}'", method_name, type_name),
                span: Some(*location),
                notes: vec![],
                suggestions: vec!["Check the method name and available methods".to_string()],
            },
            _ => Diagnostic {
                level: DiagnosticLevel::Error,
                message: error.to_string(),
                span: None,
                notes: vec![],
                suggestions: vec![],
            },
        } 
    }

    fn report_parser_error(&self, message: &str, span: Option<&SourceSpan<'a>>) {
        self.print_diagnostic(&Diagnostic {
            level: DiagnosticLevel::Error,
            message: format!("Parse error: {}", message),
            span: span.cloned(),
            notes: vec![],
            suggestions: vec![],
        });
    }

    fn report_ctrc_error(&self, message: &str, span: Option<&SourceSpan<'a>>, leak_info: Option<&CTRCLeakInfo>) {
        let mut diagnostic = Diagnostic {
            level: DiagnosticLevel::Error,
            message: format!("Memory safety error: {}", message),
            span: span.cloned(),
            notes: vec![],
            suggestions: vec![],
        };
        if let Some(info) = leak_info {
            diagnostic.notes.push(format!("Variable '{}' allocated at {}", info.variable_name, info.allocation_site));
            if !info.potential_cycles.is_empty() {
                diagnostic.notes.push(format!("Potential reference cycles: {}", info.potential_cycles.join(", ")));
                diagnostic.suggestions.push("Consider using weak references to break cycles".to_string());
            }
        }
        self.print_diagnostic(&diagnostic);
    }

    fn print_diagnostic(&self, diagnostic: &Diagnostic<'a>) {
        match diagnostic.level {
            DiagnosticLevel::Error => {
                eprintln!("{}: {}", "error".bright_red().bold(), diagnostic.message);
            },
            DiagnosticLevel::Warning => {
                eprintln!("{}: {}", "warning".bright_yellow().bold(), diagnostic.message);
            },
            DiagnosticLevel::Info => {
                eprintln!("{}: {}", "info".bright_blue().bold(), diagnostic.message);
            },
            DiagnosticLevel::Note => {
                eprintln!("{}: {}", "note".cyan().bold(), diagnostic.message);
            },
        };

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
        if let Some(source) = self.source_files.get(span.file_name) {
            let lines: Vec<&str> = source.lines().collect();
            let error_line = span.line.saturating_sub(1);
            let start = error_line.saturating_sub(2);
            let end = (error_line + 2).min(lines.len().saturating_sub(1));
            eprintln!("--> {}:{}:{}", span.file_name, span.line, span.column);
            for (i, line) in lines.iter().enumerate().skip(start).take(end - start + 1) {
                let line_no = i + 1;
                if line_no == span.line {
                    eprintln!("{} | {}", line_no, line.red().bold());
                    let mut underline = String::new();
                    for _ in 1..span.column { underline.push(' '); }
                    underline.push('^');
                    eprintln!("{}", underline.red().bold());
                } else {
                    eprintln!("{} | {}", line_no, line.dimmed());
                }
            }
        }
    }

    fn type_to_string(&self, ty: &crate::ast::Type) -> String {
        let mut base = match &ty.kind {
            crate::ast::TypeKind::I32 => "i32".to_string(),
            crate::ast::TypeKind::F32 => "f32".to_string(),
            crate::ast::TypeKind::F64 => "f64".to_string(),
            crate::ast::TypeKind::I64 => "i64".to_string(),
            crate::ast::TypeKind::String => "String".to_string(),
            crate::ast::TypeKind::Boolean => "bool".to_string(),
            crate::ast::TypeKind::U32 => "u32".to_string(),
            crate::ast::TypeKind::U64 => "u64".to_string(),
            crate::ast::TypeKind::Void => "void".to_string(),
            crate::ast::TypeKind::Infer => "<inferred>".to_string(),
            crate::ast::TypeKind::Struct { name, .. } => format!("class {}", name),
            crate::ast::TypeKind::Lambda { .. } => "<lambda>".to_string(),
            _ => "<unknown>".to_string(),
        };
        if ty.error { base = format!("!{}", base); }
        if ty.nullable { base = format!("{}?", base); }
        base
    }

    pub fn has_errors(&self) -> bool { !self.errors.is_empty() }

    pub fn error_count(&self) -> usize { self.errors.len() }
}