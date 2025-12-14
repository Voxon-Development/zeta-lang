#![feature(allocator_api)]

pub mod type_checker;
pub mod type_context;
pub mod type_error;

pub use type_checker::TypeChecker;
pub use type_context::TypeContext;
pub use type_error::{TypeError, TypeCheckResult};