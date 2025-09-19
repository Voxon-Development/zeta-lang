use zetaruntime::bump::{GrowableBump};
use crate::errors::error::TypeError;

pub struct ErrorReporter {
    pub errors: Vec<TypeError, GrowableBump>,
}

// Centralized error reporting for type-checking.
impl ErrorReporter {
    pub fn new() -> ErrorReporter {
        ErrorReporter {
            errors: Vec::new_in(GrowableBump::new(1024, align_of::<TypeError>())),
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