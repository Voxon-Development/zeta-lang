use crate::midend::type_checker::errors::error::TypeError;

// Centralized error reporting for type-checking.
pub trait ErrorReporter {
    // Reports a type error with location info and diagnostic hints.
    fn report(&mut self, error: TypeError);

    // Checks if any errors were reported (for early bailout).
    fn has_errors(&self) -> bool;
}