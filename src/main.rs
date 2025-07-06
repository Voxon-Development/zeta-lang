use rand::TryRngCore;
use ir::bump;

#[derive(Debug)]
struct A {
    a: i32,
    b: i32,
    c: i32,
}

fn main() {
    // random int
    let random_int = rand::rngs::OsRng.try_next_u64().unwrap();
    let start = std::time::Instant::now();
    let bumpalo = bumpalo::Bump::new();

    for i in 0..10000 {
        let s = bumpalo.alloc(A {
            a: i as i32,
            b: i as i32,
            c: i as i32,
        });
        println!("{:?}", s);
    }
    drop(bumpalo);
    let end1 = start.elapsed().as_millis();

    let start = std::time::Instant::now();
    for i in 0..10000 {
        let s = Box::new(A {
            a: i as i32,
            b: i as i32,
            c: i as i32,
        });
        println!("{:?}", s);
    }
    let end2 = start.elapsed().as_millis();

    let start = std::time::Instant::now();
    let bump = bump::Bump::new();
    let max: i32 = 10000;
    for i in 0..max {
        let s = bump.alloc(size_of::<A>(), align_of::<A>());
        unsafe {
            *(s as *mut A) = A {
                a: i as i32,
                b: i as i32,
                c: i as i32,
            };
        }

        println!("{:?}", unsafe { &*s });
    }
    drop(bump);
    let end3 = start.elapsed().as_millis();

    println!("bumpalo: {}ms", end1);
    println!("box: {}ms", end2);
    println!("custom: {}ms", end3);
}

pub fn use_i32(i: i32) -> i32 {
    i
}