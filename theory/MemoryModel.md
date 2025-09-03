# Zeta's memory model inside out

Here we'll dive into the memory model of Zeta, inspired by Rust, made to work for custom allocators with memory safety.

This memory model encourages fast code via encouraging developers to write clean and safe code, zero-cost RAII and achieve memory safety while not compromising on fearless concurrency.

# Memory Model
For a memory model to truly be memory-safe, it has to follow these rules:
- No data races by default
- Thread-safety markers
- Safe aliasing rules
- Seamless lifetime management
- No implicit sharing of mutable state
- Undefined behavior isolation
- Well-defined memory ordering
- Explicit (not implicit) escape hatches for low-level work

## How does it work?

To make memory safety simpler from a typical Rust borrow checker:
- Ownership and move semantics no longer apply, making this code no longer a problem
```
mut String x = String::from("five");
String y = x;

x.append("six");

println(x); // prints fivesix
println(y); // prints five
```
- Since move semantics is optional and the language copies by default, we removed references, and we replaced them with pointers such as `mut Ptr<MyStruct>` and `Ptr<MyStruct>`

```
mut u32 x = 5;

Ptr<u32> immutablePtr = asPtr(x);
mut Ptr<u32> mutablePtr = asPtr(x);

x = 6; // Compiles

// Auto dereference, since by default pointers are safe and lifetime proven unless you explicitly use unsafe apis
mutablePtr = 6; // Compiles
immutablePtr = 6; // Does not compile
```

- There is no need for reference counting for "multiple owners" now, and hence no need for `Arc` and `Rc` or libraries trying to optimize reference counting with `Trc` or `SharedTrc` (Except for cyclic data, but Zeta *wants* to take a different approach on cyclic data)

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

We must rely on safe alias tracking, lifetimes and move semantics

If we want to make sure use-after-free, dangling pointers or double free is impossible here from the heap, we would want to make a tracker that tracks aliases, and deletions, or even just rely on RAII

For example, simple RAII:
```
mut x := 5;

Ptr<u32> immutablePtr = Ptr.new(x);
mut Ptr<u32> mutablePtr = Ptr.new(x);

x = 6; // Compiles

// Auto deref pointers to values for getting or setting
mutablePtr = 6; // Compiles
immutablePtr = 6; // Does not compile

// Everything is freed automatically here
```

Since it's easy to detect that pointers are no longer needed, we can just free the data and know that all these pointers won't be used

This could be anywhere; the data could be stored inside the region, on the heap or on the stack, but we can easily prove short-lived data is no longer needed

but what if we are returning stack allocated data? region allocated data, or heap allocated data?

It gets pretty spicy because we can't delete anything now:

```
struct MySQLPlayerRepository(/* fields */) {
    // Initialization, fields, etc
    
    foo() {
        person := fetchPerson();
        // Use person.
    }
    
	// Data? is equivalent to Option<Data> at compile time
    fetchPerson(u32 id) -> Ptr<Person>? {
        preparedStatement := database.prepare("SELECT * FROM person WHERE id = ? LIMIT 1;");
        preparedStatement.setUnsignedInt(1, id);
        
        resultSet := preparedStatement.execute();
        personData := resultSet.next() ?? return None; // Returns Data?

        // We could just as easily use a custom allocator, and by default the compiler should be able to delete it from the correct allocator
        return Box.new(Person {
            id: personData.getU32("id"),
            name: personData.getString("name"),
            age: personData.getU32("age"),
            ...
        }); // Auto dereferences to a pointer
    }
}
```

What if we just did this?
```
resultSet := preparedStatement.execute();

drop(resultSet);

// resultSet is now explicitly moved (because move semantics are optional and not enforced like in rust)
personData := resultSet.next() ?? return None; // Returns Option<Data>
```

~~So now we need the compiler to track resultSet, if it were to be deleted just like that then it wouldn't compile, this way, you can deallocate and handle even regular deallocations just fine!~~

The language now can track move semantics which greatly simplify the compiler work, though it is not enforced, `drop` and deallocation moves the data.

Though by default, pointers will be auto dropped (auto free but it doesn't just free, but it calls Drop.drop to release resources too!) at the end of scopes by default.

## What about long-lived lifetimes?
You may ask aswell, What about lifetimes of pointers?

Rust might have `&'a HashMap<...>`, and you usually don't use lifetimes like this... until when you put references inside structs.

Zeta works harder to infer lifetimes of pointers by analyzing how pointers are used, only when the compiler finds inconsistencies then you have to put lifetimes in pointers:
```
struct MySQLDatabaseRepository<'a>(Ptr<ConnectionPool, 'a> connectionPool) { ... }
```

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

## Explicit escape hatches for low-level work
Even with all this memory safety, we still need to escape hatches for low-level work.

This includes but is not limited to:
- Pointer arithmetic
- Raw concurrency primitives
- Manual memory management
- FFI and hardware access
- Manual synchronization.

And this should all be under `unsafe` since Zeta cannot (or should not) guarantee safety for all of these operations.

Escape hatches for low-level work? Check
