use zetaruntime::string_pool::StringPool;
use crate::errors::reporter::ErrorReporter;

pub struct Context<'a> {
    pub string_pool: &'a mut StringPool,
    pub error_reporter: ErrorReporter
}