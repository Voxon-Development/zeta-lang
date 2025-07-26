mod bump;

use bump::Bump;
use std::hint::black_box;
use std::time::Instant;

#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
    height: f32,
    weight: f32,
    salary: f32,
    address: String,
    phone_number: String,
}

// Generate varying Person data
fn create_person(i: u32) -> Person {
    Person {
        name: format!("Person {}", i),
        age: i,
        height: 150.0 + (i % 50) as f32,
        weight: 50.0 + (i % 30) as f32,
        salary: (i as f32) * 1000.0,
        address: format!("Address {}", i),
        phone_number: format!("+201000000{:04}", i % 10000),
    }
}

fn main() {
    const N: usize = 100_000;

    // Bump allocator benchmark
    let mut bump = Bump::with_capacity(N);
    let start_bump = Instant::now();

    for i in 0..N {
        let person = bump.alloc_struct(create_person(i as u32));
        black_box(person); // Prevent optimization
    }

    drop(bump); // Free all at once
    let elapsed_bump = start_bump.elapsed();
    println!("Bump: Time elapsed: {} ns", elapsed_bump.as_nanos());
    println!("Bump: Time elapsed: {} ms", elapsed_bump.as_millis());

    // Global heap benchmark (Box)
    let start_heap = Instant::now();

    for i in 0..N {
        let person = Box::new(create_person(i as u32));
        black_box(person); // Prevent optimization
        // Drop at the end of the block
    }

    let elapsed_heap = start_heap.elapsed();
    println!("Heap: Time elapsed: {} ns", elapsed_heap.as_nanos());
    println!("Heap: Time elapsed: {} ms", elapsed_heap.as_millis());

    match elapsed_bump.cmp(&elapsed_heap) {
        std::cmp::Ordering::Less => println!("Bump is faster!"),
        std::cmp::Ordering::Greater => println!("Heap is faster!"),
        std::cmp::Ordering::Equal => println!("Both took the same time!"),
    }
}
