pub mod virtualmachine;
pub mod profiler;
pub mod functions;
pub mod eventloop;
mod fibers;
mod memory;
mod println;
mod interpreter;
pub mod instruction_handlers;
mod utils;

// Re-export commonly used types
pub use instruction_handlers::class_ops::ClassInitHandler;
pub use zetac::codegen::ir::ir_compiler::{ClassTable, CompressedClassTable, ClassLayout};
