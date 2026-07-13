#![feature(likely_unlikely)]
#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(portable_simd)]

extern crate core;

pub mod arena;
pub mod bump;
pub mod string_pool;
