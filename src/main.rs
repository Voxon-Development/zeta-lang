#![feature(likely_unlikely)]
#![feature(unsafe_cell_access)]

mod bump;

use std::alloc::alloc;
use bump::Bump;
use std::hint::black_box;
use std::time::Instant;
use crate::bump::Arena;

#[derive(Debug)]
struct Person<'bump> {
    name: &'bump str,
    age: u32,
    height: f32,
    weight: f32,
    salary: f32,
    address: &'bump str,
    phone_number: &'bump str,
}

// Generate varying Person data

fn main() {
    const N: usize = 100_000;

    // Bump allocator benchmark
    let bump = Bump::with_capacity(N * (size_of::<Person>() * 2));
    let start_bump = Instant::now();

    for i in 0..N {
        let mut buf = arrayvec::ArrayString::<64>::new();
        buf.push_str("Person ");
        buf.push_str(&i.to_string());
        let name = bump.alloc_str(buf.as_str());

        let mut buf = arrayvec::ArrayString::<64>::new();
        buf.push_str("Address: ");
        buf.push_str(&i.to_string());
        let address = bump.alloc_str(buf.as_str());

        let phone_number = bump.alloc_str(&format!("+201000000{:04}", i % 10000));

        let person = bump.alloc(Person {
            name,
            age: i as u32,
            height: 150.0 + (i % 50) as f32,
            weight: 50.0 + (i % 30) as f32,
            salary: (i as f32) * 1000.0,
            address,
            phone_number,
        });

        black_box(person);
    }

    drop(bump);
    let elapsed_bump = start_bump.elapsed();
    println!("Bump: Time elapsed: {} ns", elapsed_bump.as_nanos());
    println!("Bump: Time elapsed: {} ms", elapsed_bump.as_millis());

    // Heap benchmark
    let start_heap = Instant::now();
    for i in 0..N {
        let person = Box::new(Person {
            name: Box::leak(format!("Person {}", i).into_boxed_str()),
            age: i as u32,
            height: 150.0 + (i % 50) as f32,
            weight: 50.0 + (i % 30) as f32,
            salary: (i as f32) * 1000.0,
            address: Box::leak(format!("Address {}", i).into_boxed_str()),
            phone_number: Box::leak(format!("+201000000{:04}", i % 10000).into_boxed_str()),
        });
        black_box(person);
    }
    let elapsed_heap = start_heap.elapsed();
    println!("Heap: Time elapsed: {} ns", elapsed_heap.as_nanos());
    println!("Heap: Time elapsed: {} ms", elapsed_heap.as_millis());
}
