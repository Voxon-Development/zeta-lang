use std::collections::HashMap;
use zetaruntime::bump::{GrowableBump};
use crate::errors::error::TypeError;
use crate::span::SourceSpan;

// ANSI color codes for terminal output
mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const BOLD: &str = "\x1b[1m";
    /*pub const RED: &str = "\x1b[31m";
    pub const YELLOW: &str = "\x1b[33m";*/
    pub const BLUE: &str = "\x1b[34m";
    pub const CYAN: &str = "\x1b[36m";
    pub const WHITE: &str = "\x1b[37m";
    pub const BRIGHT_RED: &str = "\x1b[91m";
    pub const BRIGHT_YELLOW: &str = "\x1b[93m";
    pub const BRIGHT_BLUE: &str = "\x1b[94m";
    pub const DIM: &str = "\x1b[2m";
}

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
where 'bump: 'a {
    pub errors: Vec<CompilerError<'a, 'bump>, GrowableBump<'bump>>,
    pub source_files: HashMap<String, String>, // filename -> source content
    pub use_colors: bool,
}

impl<'a, 'bump> ErrorReporter<'a, 'bump> {
    pub fn new() -> ErrorReporter<'a, 'bump> {
        ErrorReporter {
            errors: Vec::new_in(GrowableBump::new(size_of::<CompilerError>() * 512, align_of::<CompilerError>())),
            source_files: HashMap::new(),
            use_colors: atty::is(atty::Stream::Stderr), // Auto-detect if terminal supports colors
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
                eprintln!("\n{}{}Found {} error{}{}\n", 
                         colors::BOLD, colors::BRIGHT_RED, 
                         error_count, 
                         if error_count == 1 { "" } else { "s" },
                         colors::RESET);
            } else {
                eprintln!("\nFound {} error{}\n", 
                         error_count, 
                         if error_count == 1 { "" } else { "s" });
            }
        }
    }
    
    fn report_error(&self, error: &CompilerError<'a, 'bump>) {
        match error {
            CompilerError::TypeError(type_error) => {
                self.report_type_error(type_error);
            }
            CompilerError::ParserError { message, span } => {
                self.report_parser_error(message, span.as_ref());
            }
            CompilerError::CTRCError { message, span, leak_info } => {
                self.report_ctrc_error(message, span.as_ref(), leak_info.as_ref());
            }
        }
    }
    
    fn report_type_error(&self, error: &TypeError<'a, 'bump>) {
        let diagnostic = self.type_error_to_diagnostic(error);
        self.print_diagnostic(&diagnostic);
    }
    
    fn report_parser_error(&self, message: &str, span: Option<&SourceSpan<'a>>) {
        let diagnostic = Diagnostic {
            level: DiagnosticLevel::Error,
            message: format!("Parse error: {}", message),
            span: span.cloned(),
            notes: vec![],
            suggestions: vec![],
        };
        self.print_diagnostic(&diagnostic);
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
    
    fn type_error_to_diagnostic(&self, error: &TypeError<'a, 'bump>) -> Diagnostic<'a> {
        match error {
            TypeError::Mismatch { expected, found, location } => {
                Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: format!("Type mismatch: expected '{}' but found '{}'", 
                                   self.type_to_string(expected), 
                                   self.type_to_string(found)),
                    span: Some(*location),
                    notes: vec![],
                    suggestions: vec![format!("Try converting '{}' to '{}'", 
                                             self.type_to_string(found), 
                                             self.type_to_string(expected))],
                }
            }
            TypeError::UndefinedSymbol { name, location } => {
                Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: format!("Undefined symbol '{}'", name),
                    span: Some(*location),
                    notes: vec![],
                    suggestions: vec!["Check if the symbol is declared and in scope".to_string()],
                }
            }
            TypeError::UnknownType { name, location } => {
                Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: format!("Unknown type '{}'", name),
                    span: Some(*location),
                    notes: vec![],
                    suggestions: vec!["Check if the type is imported or defined".to_string()],
                }
            }
            TypeError::NoSuchField { type_name, field_name, location } => {
                Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: format!("No field '{}' found on type '{}'", field_name, type_name),
                    span: Some(*location),
                    notes: vec![],
                    suggestions: vec!["Check the field name and type definition".to_string()],
                }
            }
            TypeError::NoSuchMethod { type_name, method_name, location } => {
                Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: format!("No method '{}' found on type '{}'", method_name, type_name),
                    span: Some(*location),
                    notes: vec![],
                    suggestions: vec!["Check the method name and available methods".to_string()],
                }
            }
            _ => {
                Diagnostic {
                    level: DiagnosticLevel::Error,
                    message: error.to_string(),
                    span: None,
                    notes: vec![],
                    suggestions: vec![],
                }
            }
        }
    }
    
    fn print_diagnostic(&self, diagnostic: &Diagnostic<'a>) {
        let level_str = match diagnostic.level {
            DiagnosticLevel::Error => if self.use_colors { 
                format!("{}{}{}", colors::BOLD, colors::BRIGHT_RED, "error")
            } else { "error".to_string() },
            DiagnosticLevel::Warning => if self.use_colors { 
                format!("{}{}{}", colors::BOLD, colors::BRIGHT_YELLOW, "warning")
            } else { "warning".to_string() },
            DiagnosticLevel::Info => if self.use_colors { 
                format!("{}{}{}", colors::BOLD, colors::BRIGHT_BLUE, "info")
            } else { "info".to_string() },
            DiagnosticLevel::Note => if self.use_colors { 
                format!("{}{}{}", colors::BOLD, colors::CYAN, "note")
            } else { "note".to_string() },
        };
        
        if self.use_colors {
            eprintln!("{}{}: {}{}", level_str, colors::RESET, colors::WHITE, diagnostic.message);
        } else {
            eprintln!("{}: {}", level_str, diagnostic.message);
        }
        
        if let Some(span) = &diagnostic.span {
            self.print_source_snippet(span);
        }
        
        for note in &diagnostic.notes {
            if self.use_colors {
                eprintln!("  {}{}{} {}{}", colors::BOLD, colors::CYAN, "note:", colors::RESET, note);
            } else {
                eprintln!("  note: {}", note);
            }
        }
        
        for suggestion in &diagnostic.suggestions {
            if self.use_colors {
                eprintln!("  {}{}{} {}{}", colors::BOLD, colors::BRIGHT_BLUE, "help:", colors::RESET, suggestion);
            } else {
                eprintln!("  help: {}", suggestion);
            }
        }
        
        eprintln!(); // Empty line between diagnostics
    }
    
    fn print_source_snippet(&self, span: &SourceSpan<'a>) {
        if let Some(source) = self.source_files.get(span.file_name) {
            let lines: Vec<&str> = source.lines().collect();
            let error_line = span.line.saturating_sub(1); // Convert to 0-based
            
            // Show a few lines of context
            let context_start = error_line.saturating_sub(2);
            let context_end = (error_line + 2).min(lines.len().saturating_sub(1));
            
            if self.use_colors {
                eprintln!("  {}{}--> {}:{}:{}{}", 
                         colors::BLUE, colors::BOLD, 
                         span.file_name, span.line, span.column,
                         colors::RESET);
            } else {
                eprintln!("  --> {}:{}:{}", span.file_name, span.line, span.column);
            }
            
            eprintln!("  {}", if self.use_colors { colors::BLUE } else { "" });
            
            for (i, line) in lines.iter().enumerate().skip(context_start).take(context_end - context_start + 1) {
                let line_num = i + 1;
                let line_num_str = format!("{:3}", line_num);
                
                if line_num == span.line {
                    // This is the error line - highlight it
                    if self.use_colors {
                        eprintln!("{}{} | {}{}{}", 
                                 colors::BLUE, line_num_str, 
                                 colors::RESET, line, colors::RESET);
                        
                        // Add underline for the specific span
                        let mut underline = String::new();
                        for _ in 0..line_num_str.len() + 3 {
                            underline.push(' ');
                        }
                        
                        // Simple underline at the column position
                        for _ in 1..span.column {
                            underline.push(' ');
                        }
                        underline.push('^');
                        
                        eprintln!("{}{}{}{}", colors::BRIGHT_RED, colors::BOLD, underline, colors::RESET);
                    } else {
                        eprintln!("{} | {}", line_num_str, line);
                        
                        // Simple underline without colors
                        let mut underline = String::new();
                        for _ in 0..line_num_str.len() + 3 {
                            underline.push(' ');
                        }
                        
                        for _ in 1..span.column {
                            underline.push(' ');
                        }
                        underline.push('^');
                        
                        eprintln!("{}", underline);
                    }
                } else {
                    // Context line
                    if self.use_colors {
                        eprintln!("{}{} | {}{}", 
                                 colors::BLUE, line_num_str, 
                                 colors::DIM, line);
                    } else {
                        eprintln!("{} | {}", line_num_str, line);
                    }
                }
            }
            
            if self.use_colors {
                eprintln!("  {}", colors::RESET);
            }
        }
    }
    
    fn type_to_string(&self, ty: &crate::ast::Type) -> String {
        // Simplified type to string conversion
        match ty {
            crate::ast::Type::I32 => "i32".to_string(),
            crate::ast::Type::F32 => "f32".to_string(),
            crate::ast::Type::F64 => "f64".to_string(),
            crate::ast::Type::I64 => "i64".to_string(),
            crate::ast::Type::String => "String".to_string(),
            crate::ast::Type::Boolean => "bool".to_string(),
            crate::ast::Type::U32 => "u32".to_string(),
            crate::ast::Type::U64 => "u64".to_string(),
            crate::ast::Type::Void => "void".to_string(),
            crate::ast::Type::Infer => "<inferred>".to_string(),
            crate::ast::Type::Class { name, .. } => format!("class {}", name),
            crate::ast::Type::Lambda { .. } => "<lambda>".to_string(),
            _ => "<unknown>".to_string(),
        }
    }

    // Checks if any errors were reported (for early bailout).
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }
}