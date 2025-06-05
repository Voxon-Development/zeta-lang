use bumpalo::Bump;
use std::time::Instant;

fn main() {
    let start = Instant::now();
    println!("Hello, world!");
    
    let arr: [i64; 3] = [1,2,3];
    let mut i = 0;
    while (i < 3) {
        println!("{}", arr[i]);
        i += 1;
    }
    
    println!("Time elapsed: {} ns", start.elapsed().as_nanos());
}
