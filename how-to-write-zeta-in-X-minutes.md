```rs
// Single line comment
// A package clause starts every source file.
package my::app;

import zeta::utils::array_list;
import zeta::utils::mallocator.RawMallocator;
import zeta::utils::mallocator.Mallocator;

func main() {
  	// No stdout for now
    // It'd look like this
    // StdOutWriter would be a zero-sized struct
    let writer: StdOutWriter = zeta::io.out();
    writer.writeln("Hello, world!");

    // for explicit buffering
    let buf_writer: BufWriter = BufWriter::new(zeta::io.out());
    buf_writer.write("Hello");
    buf_writer.writeln(", world!");
    // you may choose to `buf_writer.flush()` or wait until the scope ends and Drop semantics does what it does best.
    // Doesn't exist for now though
  
  	// Call another function within this package.
  	beyond_hello();
}

// Functions have parameters in parentheses.
// If there are no parameters, empty parentheses are still required.
func beyond_hello() {
  	let x: int = 3;     // Variable assignment.
  	// "Short" declarations use := to infer the type, declare, and assign.
  	y := 4;
  	learn_types();
}

// Some built-in types and literals.
func learn_types() {
  	// Short declaration usually gives you what you want.
  	a_string := "Learn Zeta!"; // string type.
  
  	f := 3.14159; // f64, an 64-bit floating point number.
  
  	// var syntax with initializers.
  	let u: u32 = 7; // Unsigned, but implementation dependent size as with int.
  	let pi: f32 = 22 / 7;
    
  	learn_flow_control(); // Back in the flow.
}

func learn_flow_control() {
    // If statements will execute if condition in `if (condition)` equals true, for example, `1 == 1` returns true, while `1 == 2` returns false
  	if (true) {
    		// told you!
  	}
  	if (false) { // <----------------------------------------------|
  		// how?!                                                  \./
  	} else { // these else statements will execute if the `if (condition)` above does not execute because condition equaled false.
    	// told you!
  	}

    let u: boolean = true;
    let a: boolean = false;
    x := 42.0;

    match (x) {
        case 1 -> {},
        case 2 -> {},
        case _ -> {},
    }

    for (mut x := 0; x < 3; x += 1) {
        // use x
  	}

    borrow_checking_and_move_semantics();
}

interface Stringer {
  	func to_string(): str;
}

// Define Pair as a struct with two fields, ints named x and y.
struct Pair {
  	x: i32,
    y: i32
}

impl Stringer for Pair {
    func to_string(): str {
        return ...; // If you're new to programming, know ... means a placeholder, it is not a real value.
    }
}

impl Pair {
    func get_x(&this): i32 {
        return this.x;
    }

    func set_x(&mut this, x: i32) {
        this.x = x;
    }
}

func borrow_checking_and_move_semantics() {
    // Values in Zeta have a single owner.
    // When a value goes out of scope, it is automatically dropped.
    let mut pair: Pair = Pair {
        x: 1,
        y: 2,
    };

    // A borrow is a temporary reference to a value owned by someone else.
    // `&T` creates a shared (read-only) borrow.
    //
    // Multiple shared borrows may exist at the same time because none of
    // them can modify the value.
    let a: &Pair = &pair;
    let b: &Pair = &pair;

    a.get_x();
    b.get_x();

    // `&mut T` creates a mutable borrow.
    //
    // Unlike Rust, Zeta does NOT immediately reject creating this borrow
    // while `a` and `b` still exist.
    //
    // Instead, Zeta checks whether shared and mutable borrows are ever
    // *used* at the same time.
    let c: &mut Pair = &mut pair;

    // Since `a` and `b` are never used again after this point, the mutable
    // borrow is allowed.
    c.set_x(42);

    // The same rule also applies to multiple mutable borrows.
    //
    // Creating several mutable borrows is allowed.
    // What matters is that they are never used simultaneously.
    let mut mut_pair: Pair = Pair {
        x: 10,
        y: 20,
    };

    let left: &mut Pair = &mut mut_pair;
    let right: &mut Pair = &mut mut_pair;

    // `left` is completely finished before `right` is used,
    // so there is no conflict.
    left.set_x(15);
    right.set_x(30);

    // Some values own heap memory.
    //
    // ArrayList owns its allocation and it is a heap allocated list that can grow dynamically, and it is a move-only type.
    // This is more useful sometimes than an array because it grows itself elegantly, but heap allocating has a higher overhead.
    // You use an ArrayList most of the time, but you should be cautious when writing high performance hot code with this though (due to the heap allocations)
    // or if you want an array that never grows for correctness/performance reasons.
    let list: ArrayList<i32> = ArrayList.new();

    // This does NOT copy the ArrayList.
    //
    // Ownership is moved from `list` into `moved`.
    // After this assignment, `moved` becomes the new owner.
    let moved: ArrayList<i32> = list;

    // Since ownership has moved away, `list` is no longer valid.
    //
    // Uncommenting the following line would be a compile-time error.
    //
    // list.push(456);

    // `moved` is now the only owner of the ArrayList.
    // moved.push(123);

    // Primitive numeric types implement Copy.
    //
    // Assigning them copies the value instead of moving ownership.
    // Something that is Copy should be trivial to copy in memory, such as numbers, pointers, references, etc.
    let x: i32 = 123;
    let y: i32 = x;

    // Since `x` was copied rather than moved,
    // both variables remain valid.
    let z: i32 = x + y;

    // The following function intentionally contains examples that fail to
    // compile, demonstrating what Zeta's borrow checker prevents.
    error_with_borrow_checking_and_move_semantics();
}

func borrow_checking_and_move_semantics_with_arrays() {
    // Arrays are values that directly contain their elements.
    //
    // This array lives entirely on the stack because its size is known
    // at compile time.
    let mut arr: [4]i64 = [1, 2, 3, 4];

    // A shared borrow allows reading the array without taking ownership.
    let a: &[4]i64 = &arr;
    let b: &[4]i64 = &arr;

    // Multiple shared borrows may exist simultaneously and use it
    let first: &i64 = &a[0];
    let second: &i64 = &b[1];

    // A mutable borrow allows modifying the array.
    //
    // Like all borrows in Zeta, conflicts are checked when the borrows
    // are used, not when they are created.
    let c: &mut [4]i64 = &mut arr;

    // `a` and `b` are never used again, so this is accepted.
    c[0] = 42;

    // Multiple mutable borrows may also exist.
    let mut other: [4]i64 = [10, 20, 30, 40];

    let left: &mut [4]i64 = &mut other;
    let right: &mut [4]i64 = &mut other;

    // Since `left` mutates a thing in the array,
    // `right` may safely modify different things in the same array.
    left[0] = 15;
    right[1] = 25;

    // Both variables remain valid because every element was copied.
    let x: i64 = arr[0];
    let y: i64 = copied[0];

    // Modifying one array does not affect the other.
    arr[0] = 100;
}

func error_with_borrow_checking_and_move_semantics() {
    let mut pair: Pair = Pair {
        x: 1,
        y: 2,
    };

    let read: &Pair = &pair;
    let write: &mut Pair = &mut pair;

    // ERROR:
    // `read` and `write` are both used here.
    read.get_x();
    write.set_x(10);
    read.get_x();

    let mut pair2: Pair = Pair {
        x: 5,
        y: 6,
    };

    let a: &mut Pair = &mut pair2;
    let b: &mut Pair = &mut pair2;

    // ERROR:
    // Two mutable borrows are active at the same use site.
    a.set_x(1);
    b.set_x(2);

    let mut value: Pair = Pair {
        x: 0,
        y: 0,
    };

    let borrow: &Pair = &value;

    // ERROR:
    // Cannot move while a borrow is later used.
    let moved = value;
    borrow.get_x();

    let list: ArrayList<i32> = ArrayList.new();

    let other: ArrayList<i32> = list;

    // ERROR:
    // Ownership was moved into `other`.
    list.push(5);

    let mut pair3: Pair = Pair {
        x: 1,
        y: 2,
    };

    let borrow2: &mut Pair = &mut pair3;

    // ERROR:
    // Cannot move while mutably borrowed.
    let moved_pair: Pair = pair3;
    borrow2.set_x(10);
}
```
