use std::fmt;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SourceSpan<'a> {
    pub file_name: &'a str,
    pub line: usize,
    pub column: usize,
}

impl<'a> SourceSpan<'a> {
    pub fn new(file_name: &'a str, line: usize, column: usize) -> Self {
        SourceSpan {
            file_name,
            line,
            column,
        }
    }

    /// Combine two spans into one location for diagnostics spanning a
    /// multi-token construct. Since `SourceSpan` is a single point rather
    /// than a range, this keeps the earlier (start) position
    pub fn merge(self, other: SourceSpan<'a>) -> SourceSpan<'a> {
        if self.file_name != other.file_name
            || (self.line < other.line || (self.line == other.line && self.column <= other.column))
        {
            self
        } else {
            other
        }
    }
}

impl<'a> fmt::Display for SourceSpan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Line number {} at column {} inside of file named {}",
            self.line, self.column, self.file_name
        )
    }
}
