#![feature(likely_unlikely)]
#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(portable_simd)]

extern crate core;

pub mod string_pool;
pub mod bump;
pub mod arena;