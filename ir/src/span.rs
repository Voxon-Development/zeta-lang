use std::fmt;

// Represents a source location for better diagnostics.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SourceSpan<'a> {
    pub file_name: &'a str,
    pub line: usize,
    pub column: usize,
}

impl<'a> SourceSpan<'a> {
    pub fn new(file_name: &'a str, line: usize, column: usize) -> Self {
        SourceSpan { file_name, line, column }
    }
}

impl<'a> fmt::Display for SourceSpan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line number {} at column {} inside of file named {}", self.line, self.column, self.file_name)
    }
}
