# Zeta's memory model inside out

Here we'll dive into the memory model of Zeta, inspired by Rust, made to work for regions.

This memory model encourages fast code via encouraging developers to always use regions, zero-cost RAII and achieve memory safety while not compromising on fearless concurrency.

# Memory Model
For a memory model to truly be memory-safe, it has to follow these rules:
- No data races by default
- Thread-safety markers
- Safe aliasing rules
- Seamless lifetime management
- No implicit sharing of mutable state
- Undefined behavior isolation
- Well-defined memory ordering
- Escape hatches for low-level work

## How does it work?
So with regions and the heap, you can allocate on the stack and on the heap, but Zeta also includes first-class regions

We got inspired by Rust, but we removed some things to make it work better for regions, and make it simpler:
- Ownership and move semantics no longer apply, making this code no longer a problem
```
mut x := 5;
y := x;

x = 6;

println(x); // prints 6
println(y); // prints 5
```
- Since move semantics and ownership no longer exists, we removed references, and we replaced them with pointers such as `*mut MyStruct` and `*MyStruct`

```
mut x := 5;

immutable_ptr := *x;
mutable_ptr: *mut i32 := *mut x;

x = 6; // Compiles
*mutable_ptr = 6; // Compiles
*immutable_ptr = 6; // Does not compile
```

- Since we removed references, we also removed the need for `&` and `&mut`

- If there is no ownership, there is no need for reference counting for "multiple owners" and hence no need for `Arc` and `Rc` or libraries trying to optimize reference counting with `Trc` or `SharedTrc`

Otherwise, the rest of the memory model is the same, RAII, memory safety and concurrency

## Yeah, but wouldn't fearless concurrency be a problem?
On normal situations, yes, it is a problem, but we found a simple solution

it is hard to interact with threads or fibers on high-level (and sometimes mid-level like Rust) without lambdas or closures, so not only have we embraced this entirely, we even made it safer for working with concurrency.

You see, since we have no ownership, there is no need for reference counting for "multiple owners" and hence no need for `Arc` and `Rc` or libraries trying to optimize reference counting with `Trc` or `SharedTrc`

and that means we can't Send concurrently and we need to Sync concurrently... right?

We removed Send because we have no ownership or move semantics, but we kept Sync.

To answer the question: No, Sync is no longer a problem if we give Channels (and for less common scenarios, Mutexes) a Sync trait,
We can also simply give atomic values a Sync trait, which means we can use it with threads and fibers without any problems.

`lambda` can take in any value, while `concurrent lambda` needs to take in Sync values, or use `unsafe concurrent lambda` to ensure fast performance at the cost of memory safety.

and In Sha Allah we can make lambda usages as easy as `() -> {}` or (only for concurrency reasons) `unsafe () -> {}`, but if someone tries to mutate outer data inside of a `concurrent lambda` without Sync trait or unsafe, we can't compile the piece of code.

No data races by default? Check. Thread-safety markers? Check

## Safe aliasing and seamless lifetime management
So we need to make sure that we have safe aliasing and good lifetime management, which means:
- No double free
- No dangling pointers
- No use-after-free

So how do we do this?
Well, we have regions, and we can combine them to make it work for regions, which means we can have lifetimes that work for regions, simple, if the lifetime of the region ends, so does all the data inside the region

but what if we need the heap? it's a little more complicated then

We must rely on compile time RAII and tracking references and deletions.

If we want to make sure use-after-free, dangling pointers or double free is impossible here from the heap, we would want to make a tracker that tracks aliases, and deletions, or even just rely on RAII

For example, simple RAII:
```
mut x := 5;

immutable_ptr := *x;
mutable_ptr: *mut i32 := *mut x;

x = 6; // Compiles
*mutable_ptr = 6; // Compiles
*immutable_ptr = 6; // Does not compile

// Everything is freed automatically here
```

Since it's easy to detect that pointers are no longer needed, we can just free the data and know that all these pointers won't be used

This could be anywhere; the data could be stored inside the region, on the heap or on the stack, but we can easily prove short-lived data is no longer needed

but what if we are returning stack allocated data? region allocated data, or heap allocated data?

It gets pretty spicy because we can't delete anything now:

```
class MySQLPlayerRepository(/* fields */) {
    // Initialization, fields, etc
    
    foo() {
        person := fetchPerson();
        // Use person.
    }
    
    fetchPerson(id: u32) -> Option<Box<Person>> {
        preparedStatement := database.prepare("SELECT * FROM person WHERE id = ? LIMIT 1;");
        preparedStatement.setU32(1, id);
        
        resultSet := preparedStatement.execute();
        personData := resultSet.next() ?? return None; // Returns Option<Data>
       
        return Some(Box::new(Person {
            id: personData.getU32("id"),
            name: personData.getString("name"),
            age: personData.getU32("age"),
            ...
        }));
    }
}
```

What if we just did this?
```
resultSet := preparedStatement.execute();

unsafe { delete resultSet; } // Equivalent to rust `drop(resultSet);`

// this needs to access a null pointer, that's undefined behavior, or a runtime exception which is also not good.
personData := resultSet.next() ?? return None; // Returns Option<Data>
```

So now we need the compiler to track resultSet, if it were to be deleted just like that then it wouldn't compile, this way, you can deallocate and handle even regular deallocations just fine!

(By the way, `delete` would call Drop#drop)

Safe aliasing rules? Check, Seamless lifetime management? Check

## No implicit sharing of mutable state
Self-explanatory!

No implicit sharing of mutable state? Check

## Undefined behavior isolation
Any "unsafe" operation (manual memory management, pointer arithmetic, raw concurrency primitives) must be explicitly marked.

You cannot call unsafe operations unless you know 100% that it is unsafe.

Undefined behavior isolation? Check

## Well-defined memory ordering
Well-defined memory ordering means to ensure that:
- The language should specify clear rules for atomic operations and memory visibility.
- Default shared access is sequentially consistent unless explicitly relaxed.
- Non-atomic shared mutable state is forbidden in safe code.

The design for Zeta ensures this.

Well-defined memory ordering? Check

## Escape hatches for low-level work
Even with all this memory safety, we still need to escape hatches for low-level work.

This includes but is not limited to:
- Pointer arithmetic
- Raw concurrency primitives
- Manual memory management
- FFI and hardware access
- Manual synchronization.

And this should all be under `unsafe` since Zeta cannot (or should not) guarantee safety for all of these operations.

Escape hatches for low-level work? Check

## We have achieved memory safety and fearless concurrency!

So now the language is memory safe, but we still don't know one thing and we need to specify this:

# Regions, What are regions?
Regions are backed by bump allocators internally.

A bump allocator works by allocating a big chunk of memory instead of exactly what you need, which means you need significantly more memory at the cost of O(1) deallocation of the ENTIRE bump/region + O(1) allocation

It works by:

```rust
#[inline(always)]
pub fn alloc<T>(&mut self, val: T) -> Option<&mut T> {
    let align = align_of::<T>();
    let size = size_of::<T>();

    let ptr = self.ptr.as_ptr() as usize;
    let base = ptr + self.offset;
    let aligned = unsafe { align_up(base, align) };
    let end = aligned + size;
    let new_offset = end - ptr;

    if new_offset > self.capacity {
        return None;
    }

    self.offset = new_offset;
    let ptr = aligned as *mut T;
    unsafe {
        ptr.write(val);
        Some(&mut *ptr)
    }
}
```

This is just a bunch of math, and alignment to make it safer, but it boils down to:
```rust
// Get the memory location
let ptr = unsafe { (self.ptr.as_ptr() as *mut u8).add(self.offset) as *mut T };

// Update offset
self.offset += size;

// Write the value into memory and return a reference
unsafe {
	ptr.write(val);
	Some(&mut *ptr)
}
```

Which is a little bit of pointer arithmetic to get the memory location and write data to it, then we would increment offset so that future allocations can be done.

Keep in mind the real version (above) has real alignment and checks, and optimization for cold paths (such as allocation fails)

So yeah, O(1) allocation, it is literally just taking a pointer you have from your global allocator and adding `offset` to it for allocation.

and when you deallocate, guess what, it is literally just deallocating one big pointer and/or setting offset to 0 :D

## How does it work if you need it to grow?
So, what I showed you in the previous section was a regular bump allocator, what if we needed it to grow?

Resizing is very slow, just use the heap instead.

## What should it be used for?
Well, you should use it every time you want to do short-lived allocations, but due to the cons of arena/bump allocation, you also shouldn't do it always.
You can also do it when you need append-only data

For when region allocation does not work, just use the heap or even slab allocation if you need ultra-high performance

## Pros of Regions

- O(1) bulk deallocation
- O(1) allocation

## Cons of Regions

- No individual deallocation
- Memory usage is higher (Short-term)
- No resizing