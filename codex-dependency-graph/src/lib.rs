#![feature(allocator_api)]

pub mod dep_graph;
pub mod module_collection_builder;
pub mod symbol_table;
pub mod topo;

#[cfg(test)]
mod dep_graph_tests;

pub use dep_graph::{DepGraph, DepNode, NodeIdx, NodeKind};
