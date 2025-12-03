#![feature(allocator_api)]

pub mod symbol_table;
pub mod module_collection_builder;
pub mod topo;
pub mod dep_graph;

#[cfg(test)]
mod dep_graph_tests;

pub use dep_graph::{DepGraph, NodeIdx, NodeKind, DepNode, TypeKey, InstanceKey};