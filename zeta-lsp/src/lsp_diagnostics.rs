use ir::errors::reporter::{CompilerError, ErrorReporter};
use lsp_types::{Diagnostic, DiagnosticSeverity};
use std::collections::HashMap;

use crate::text_utils::span_to_range;

pub fn group_by_file<'a>(reporter: &ErrorReporter<'a>) -> HashMap<String, Vec<Diagnostic>> {
    let mut out: HashMap<String, Vec<Diagnostic>> = HashMap::new();

    for err in &reporter.errors {
        let (file_name, message, line, column, end_line, end_column) = match err {
            CompilerError::TypeError(te) => {
                let span = te.span;
                (
                    span.file_name.to_string(),
                    te.kind.to_string(),
                    span.line,
                    span.column,
                    span.end_line,
                    span.end_column,
                )
            }
            CompilerError::ParserError(pe) => (
                pe.span.file_name.to_string(),
                pe.kind.to_string(),
                pe.span.line,
                pe.span.column,
                pe.span.end_line,
                pe.span.end_column,
            ),
        };

        let source = reporter
            .source_files
            .get(&file_name)
            .map(String::as_str)
            .unwrap_or("");
        let range = span_to_range(source, line, column, end_line, end_column);

        out.entry(file_name).or_default().push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("zeta".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        });
    }
    out
}
