use std::fmt;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SourceSpan<'a> {
    pub file_name: &'a str,

    pub line: usize,
    pub column: usize,

    pub end_line: usize,
    pub end_column: usize,
}

impl<'a> SourceSpan<'a> {
    pub fn new(file_name: &'a str, line: usize, column: usize) -> Self {
        Self {
            file_name,
            line,
            column,
            end_line: line,
            end_column: column,
        }
    }

    /// Combine two spans into one location for diagnostics spanning a
    /// multi-token construct. Since `SourceSpan` is a single point rather
    /// than a range, this keeps the earlier (start) position
    pub fn merge(self, other: SourceSpan<'a>) -> SourceSpan<'a> {
        let (start, end) = if (self.line, self.column) <= (other.line, other.column) {
            (self, other)
        } else {
            (other, self)
        };

        SourceSpan {
            file_name: self.file_name,
            line: start.line,
            column: start.column,
            end_line: end.line,
            end_column: end.column,
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
