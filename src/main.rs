use bumpalo::Bump;
use std::time::Instant;

fn main() {
    let bump = Bump::with_capacity(1_000_000);
    let start = Instant::now();
    for i in 0..1_000_000 {
        bump.alloc(i); // bump alloc
    }
    drop(bump);
    println!("Bump: {:?}", start.elapsed());

    let start = Instant::now();
    for i in 0..1_000_000 {
        let x = Box::new(i); // heap alloc
        drop(x);
    }
    println!("Box::new: {:?}", start.elapsed());
}
