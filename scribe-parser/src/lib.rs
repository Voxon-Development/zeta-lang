#![feature(allocator_api)]
extern crate core;

pub mod tokenizer;
pub mod parser;
pub mod hir_lowerer;
pub mod diagnostics;
mod ctrc_integration_demo;