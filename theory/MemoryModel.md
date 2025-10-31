# Zeta's memory model inside out

Here we'll dive into the memory model of Zeta, inspired by Rust, made to work for custom allocators with memory safety.

This memory model encourages fast code via encouraging developers to write clean and safe code, zero-cost RAII and achieve memory safety while not compromising on fearless concurrency.



---

# What is safe alias tracking? Compiler algorithm for **CTRC** (Compile-Time Reference Counting)

---

## Overview of Safe Alias Tracking/Compile-Time Reference Counting/CTRC

### Every value has a reference counter, but it’s solved by the compiler, not the CPU.
A compiler pipeline that transforms source -> typed IR -> SSA value flow graph -> CTRC constraints -> flow-sensitive solver -> compile-time RC proof -> runtime RC elimination (for provable cases) or error

The entire memory model is reference-counted *at compile time*: every allocation, move, copy, and destruction contributes to an inferred static count equation. If all counts resolve statically, runtime RC ops are removed; achieving **manual-C-level efficiency with automatic safety**.

---

# Passes (in order)

### 1. Parsing + AST

Produce AST with explicit ownership operators:

```
alloc, move, copy, Drop#drop, borrow, asPtr, free, clone
```

Every literal or expression has a *value kind*:

```
{Stack, Heap, Ref, Borrowed, Copy, Ptr, Rc}
```

---

### 2. Name Resolution & Hygiene

Same as before, resolve identifiers, imports, namespaces, types.

---

### 3. Type Checking & Kind Inference

Each symbol receives both a **Type** and a **ValueKind**

If `Copy` -> guaranteed stack lifetime.
If not -> heap allocated, subject to CTRC constraints:

```
TypeInfo {
  base: Type
  kind: {Stack, Box, Ptr, Borrow, Ref, Rc}
  mutability: Mut|Const
}
```

---

### 4. Lowering -> CTRC IR (SSA form)

Convert to low-level SSA IR where every operation that can affect lifetime is explicit:

```
alloc, free, move, copy, Drop#drop, borrow, asPtr, call, return
```

All heap allocations are **symbolically named** (e.g. `%alloc_12`).

---

### 5. Build **Pointer/Value Flow Graph (PVG)**

Every SSA value that can hold or propagate an allocation reference is a **PVG node**.

```
PVGNode { id, kind: {Alloc, Var, Temp, Param, Return}, alias: Option<AliasID> }
PVGEdge { src, dst, kind: {Copy, Move, Borrow, Return, ParamPass} }
```

This graph describes all potential alias flows, including copies, container stores, function arguments, and returns.

---

### 6. Alias Set & Region Inference

Group nodes into **AliasSets**, all nodes that can reference the same allocation.
Infer region start/end boundaries (lexical or control-flow inferred).

```
AliasSet {
  id: AliasID
  type: Type
  origin: AllocationSite
  escape: bool
  concurrent: bool
}
```

---

### 7. Generate **CTRC Constraints**

For each alias set `A`, emit *count constraints* expressing reference count evolution per program point:

```
Count(A, p) = Count(A, pred) + Σ(copies) - Σ(moves) - Σ(drops)
```

And invariants:

* At entry: Count(A) = 0
* After `alloc`: Count(A) = 1
* At function exit: Count(A) = 0 (must be freed or returned)

Operations:

```
copy(p)   -> Count += 1
move(p)   -> Count unchanged, ownership transferred
Drop#drop(p)   -> Count -= 1
store(p)  -> if container escapes, Count unbounded (mark dynamic)
borrow(p) -> no Count change; emit exclusivity guarantee
```

---

### 8. Flow-sensitive CTRC Solver

Perform **abstract interpretation** on constraints to find provable bounds.

```
for each alias A:
    count_interval[A][pp] = [lower, upper]
```

Transfer rules:

* Copy -> upper += 1
* Move -> upper unchanged, rebind owner
* Drop -> both lower, upper -= 1 (clamped)
* Branch/merge -> φ-join(max, min)

The CTRC solver uses a conservative widening strategy for loops (This prevents infinite iteration in the solver by generalizing the count range when convergence stalls): unresolved loops widen to “unbounded” and reporting that a runtime RC would not be needed. To avoid unnecessary fallbacks, the solver will attempt loop-boundedness proofs (constant iteration counts, single induction variable, user annotations). If proof succeeds, counts remain static; otherwise runtime RC is selected.

If at any program point:

```
Count(A) proven always == 1  -> static unique
Count(A) proven always >= 1, bounded -> static shared
Count(A) unbounded or unknown -> dynamic RC required
```

---

### 9. `borrow` Guarantees (checked restrict)

Unlike lifetimed borrows, `borrow` in CTRC does not affect reference counts.
Instead, the solver enforces **no conflicting use** over its lexical scope.

Borrows are modeled as flow intervals (program-point ranges) rather than purely lexical tokens.

Borrow exclusivity constraints are flow-sensitive and use φ-joins at merge points; borrows across branches are merged conservatively; spurious rejections are reduced by refining intervals (block-level splitting) or by requiring an explicit annotation when necessary.

you can borrow A mutably, but not while another alias exists; the solver enforces this like C’s restrict

These are enforced symbolically, same as C’s `restrict`, but with static diagnostics.

Allowing for heavy performance optimization, and a guarantee for concurrency

---

If the solver can’t prove safety because a variable’s lifetime spans too many branches, we split and retry

### 10. Lifetime Splitting & Re-analysis

If a variable fails static proof due to coarse granularity, split lifetimes by basic block or variable scope.

Re-run solver locally until each subregion is statically provable or marked dynamic.

---

### 11. Classification & Codegen Transformation

For each alias set `A`:

| Case                             | Proven                                      | Codegen                           |
| -------------------------------- | ------------------------------------------- | --------------------------------- |
| `Count(A)` known statically      | Fully elide RC; insert deterministic `free` | Stack- or scoped-free             |
| `Count(A)` requires tracking     | Must use a runtime Rc<> explicitly            |                                   |
| `Count(A)` unknown or may escape | Must use a runtime Rc<> explicitly            |                                   |

---

### 12. Codegen & Runtime Hooks

Lower to backend IR with:

* static frees (no RC ops)
* explicit runtime Rc<> ops for unresolved sets
* `borrow` -> no code emission (purely compile-time)

---

# Key Data Structures (CTRC representation)

```
CTRCGraph {
  aliases: Map<AliasID, AliasSet>
  pvg_nodes: [PVGNode]
  pvg_edges: [PVGEdge]
  constraints: [Constraint]
  solutions: Map<AliasID, CountRange>
}
```

```
Constraint {
  alias: AliasID
  point: ProgramPoint
  delta: +n / -n
  reason: {Copy, Drop, Move, Borrow}
}

CountRange { lower: int | 0, upper: int | ∞ }
```

---

# CTRC Solver Sketch

```
solve_CTRC(MIR):
    G = build_PVG(MIR)
    C = gen_constraints(G)
    S = init_state(C)
    repeat
        for each pp in topo_order:
            S[pp] = transfer(S, pp, C)
        until fixpoint or widen
    classify_aliases(S)
```

At fixpoint, check:

```
∀ A: CountExit(A) == 0 (no leaks)
∀ A: Count never < 0 (no use-after-free)
```

If all true, program proven memory-safe; runtime RC removed.

---

# Representation of the CTRC idea (compact summary)

* Every allocation (heap, pointer, struct containing pointers) has a **compile-time reference count variable**.
* The compiler builds a **value flow graph** where each assignment, copy, and Drop#drop corresponds to a symbolic count delta.
* A **flow-sensitive solver** ensures that counts stay balanced (no leaks or premature frees).
* If counts resolve statically, no runtime RC ops are emitted.
* If counts cannot be proven (e.g. due to escaping containers, dynamic dispatch, non-static analyzable cyclic graphs, etc); force explicit Rc<>
* The `borrow` keyword is a **checked restrict**: guarantees exclusivity during its lexical scope, with no runtime tracking.
* The end result: **C-level performance**, **Rust-level safety**, **Python-level ergonomics**, achieved by performing **reference counting at compile time**.

---

# Conceptual difference (CTRC vs traditional RC / borrow)

| Concept            | CTRC                  | Rust borrow                | Runtime RC            |
| ------------------ |-----------------------| -------------------------- | --------------------- |
| Reference tracking | Compile-time symbolic | Lifetime-based             | Runtime counter       |
| Borrow model       | Checked restrict      | Lifetimed, ownership-based | N/A                   |
| Runtime cost       | Zero                  | Zero                       | Nonzero               |
| Failure mode       | Compile-time error    | Compile-time error         | Runtime leak or cycle |
| Applicability      | Whole program         | Lexical region             | Any                   |
| Escapes / dynamic  | Forbidden or unsafe   | Forbidden or unsafe        | Native behavior       |

---

# Summary of CTRC Compiler Flow

```
Source
  ↓
AST + types
  ↓
MIR with ownership ops
  ↓
PVG (pointer/value graph)
  ↓
Constraint generation (Count equations)
  ↓
Flow-sensitive CTRC solver
  ↓
[Proven static] -> elide RC + emit `Drop#drop`
[Unproven]      -> emit runtime RC ops
  ↓
Codegen
```

---

##  Allocator Provenance in CTRC

In languages like C or Zig, allocator knowledge is manual (and thus “free”).
In Rust, allocator handles are often passed or stored at runtime (e.g., `Box<T, A>`), which adds an indirection and increases memory size.

Zeta’s design can achieve the same safety as Rust *without the runtime cost* by making **allocators part of the region system**.

---

### Key Idea: Allocators Are Regions

Every allocator is treated as a **region anchor**; a root region that owns its allocations.
So when you allocate through `std.pageAllocator`, you’re not passing a runtime reference; you’re creating a pointer tied to the **region representing that allocator**.

In other words:

```
Ptr<Person, A=std.pageAllocator>
```

is known statically, but at runtime it just knows how to free it.

You could think of this as:

```zeta
region PageAllocator : Allocator
region Stack : Allocator
region Arena<'a> : Allocator with parent<'a>
```

Though it is not real code.

The compiler then ties each allocation site to its allocator’s region node in the **Pointer Value Graph (PVG)**.

---

### How CTRC Uses This

When CTRC analyzes ownership and reference counts, it already tracks pointer origins (`src -> dst` edges).
Allocator provenance becomes an additional static attribute of the region  not a runtime field.

Example of constraint inference:

```zeta
return std.pageAllocator.alloc(Person { ... })
```

CTRC derives:

```
Ptr<Person, A=std.pageAllocator>
PVGEdge { src = std.pageAllocator, dst = callerRegion, kind = Borrow|Own }
```

This gives the compiler enough information to:

* **Free from the correct allocator** when the pointer drops,
* **Prevent cross-allocator frees** (e.g., freeing stack data in heap region),
* **Inline allocator operations** when safe (so no function pointer indirection),
* And **statically elide allocator references** in data structures.

---

### Result

You now get:

*  **Allocator safety** (no freeing with wrong allocator)
*  **Compile-time provenance tracking** (CTRC + PVG handle it)
*  **Zero runtime overhead** (no allocator handles or vtables)
*  **No syntax clutter** (allocators inferred from region flow)

---

### Example: How it All Fits

```zeta
Ptr<Person>? fetchPerson(u32 id) {
    resultSet := ...
    row := resultSet.next() ?? return null

    // Allocation site automatically binds to region `PageAllocator` and gets inferred as `Ptr<Person, A=PageAllocator>`
    return std.pageAllocator.alloc(Person {
        id: row.getUnsignedInt("id"),
        name: row.getString("name"),
        age: row.getUnsignedInt("age"),
    })
}
```

At compile-time, the function’s type is inferred as:

```
Ptr<Person, A=std.pageAllocator>? fetchPerson(id: u32)
```

When returning it, CTRC verifies that:

* The `std.pageAllocator>` outlives the returned pointer’s region,
* Or, if not, automatically promotes it (heapifies) to a globally safe allocator.

---

each allocator can define static capabilities via traits like:

```zeta
interface Allocator {
    Ptr<T, This> alloc<T>()
    void free<T>(Ptr<T, This>)
}
```

But note: `This` here is purely compile-time; there’s still **no runtime allocator object**.
This keeps it inlined, zero-cost, and fully analyzable by CTRC.

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
- (implicit) move semantics no longer apply, making this code no longer a problem
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

## Fearless Concurrency Without Ownership Syntax

In most languages, interacting with threads or fibers safely requires explicit ownership syntax, lifetime annotations, and wrappers such as `Arc`, `Rc`, `Mutex`, or `RefCell`, or they can abandon safety for ease of use and freedom.
Zeta removes all of these through **CTRC-based ownership inference** and **region verification**.

### Unified Ownership & Lifetime Model

Ownership and lifetimes are not written; they are *inferred and statically proven*.
CTRC tracks aliasing, references, and mutations across all regions, guaranteeing safety *without runtime reference counting*.

As a result:

* No `Arc`, `Rc`, or `RefCell` needed for shared data.
* No explicit `Send` or `Sync` traits required for thread transfer or sharing.
* No manual lifetime syntax; region relationships are deduced from data flow.

Multiple “owners” are statically permitted when proven non-conflicting; otherwise, CTRC promotes the reference to a runtime-verified variant only when necessary.

---

### Concurrency Model

Zeta’s concurrency system treats threads and fibers as **regioned execution contexts**.

you pass **function literals** directly to concurrency APIs such as `fibers.spawn`.

```zeta
fibers.spawn {
    process(data); // must be thread-safe, channel, mutex, etc
}
```

This block is simply a **typed function literal**, with ownership and region constraints checked by CTRC.

If the block captures shared data, CTRC ensures that:

* The captured values are `Sync` (safe for concurrent access), or
* The compiler promotes them to thread-local copies if shared mutability cannot be proven safe.

For lower-level control, Zeta also allows **unsafe concurrent execution**, which bypasses static `Sync` verification for maximum performance:

```zeta
fibers.spawn(unsafe {
    fastOp(raw);
});
```

Here, the programmer assumes responsibility for ensuring thread safety; the compiler only checks basic region consistency.

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

Ptr<u32> immutablePtr = asPtr(x);
mut Ptr<u32> mutablePtr = asPtr(x);

x = 6; // Compiles

// Auto deref pointers to values for getting or setting
mutablePtr = 6; // Compiles
immutablePtr = 6; // Does not compile

// Everything is freed automatically here
```

Since it's easy to detect that pointers are no longer needed, we can just free the data and know that all these pointers won't be used

This could be anywhere; the data could be stored inside the region, on the heap or on the stack, but we can easily prove short-lived data is no longer needed

Returning data that would otherwise outlive its originating region triggers region promotion). When the compiler can cheaply and safely copy the value (small Copy types), it may implicitly copy; for non-Copy values the compiler will reject the code with a clear borrow/escape diagnostic. This ensures no dangling pointers are produced.

```
struct MySQLPlayerRepository(/* fields */) {
    // Initialization, fields, etc
    
    foo() {
        person := fetchPerson()
        // Use person.
    }
    
	// Data? is equivalent to Option<Data> at compile time
    fetchPerson(u32 id) -> Ptr<Person>? {
        preparedStatement := database.prepare("SELECT * FROM person WHERE id = ? LIMIT 1;")
        preparedStatement.setUnsignedInt(1, id)
        
        resultSet := preparedStatement.execute()
        personData := resultSet.next() ?? return null // resultSet.next() returns Row?

        // We could just as easily use a custom allocator, and by default the compiler should be able to delete it from the correct allocator
        return std.pageAllocator.alloc(Person {
            id: personData.getUnsignedInt("id"),
            name: personData.getString("name"),
            age: personData.getUnsignedInt("age"),
            ...
        })
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
