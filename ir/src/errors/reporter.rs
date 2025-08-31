use crate::errors::error::TypeError;

#[derive(Default)]
pub struct ErrorReporter {
    pub errors: Vec<TypeError>,
}

// Centralized error reporting for type-checking.
impl ErrorReporter {
    pub const fn new() -> ErrorReporter {
        ErrorReporter {
            errors: Vec::new(),
        }
    }
    
    // Reports a type error with location info and diagnostic hints.
    pub fn add_error(&mut self, error: TypeError) {
        self.errors.push(error);
    }
    
    pub fn report_current_reports(&mut self) {
        for error in &self.errors {
            eprintln!("{}", error);
        }
    }

    // Checks if any errors were reported (for early bailout).
    pub fn has_errors(&self) -> bool {
        self.errors.len() > 0
    }
}