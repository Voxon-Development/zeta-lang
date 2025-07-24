# WIT - When it's time.

This is a new proposal to the way the runtime of languages work, to optimize, accelerate, and improve
the runtime of programs.

## JIT
Just in time, usually includes an interpreter, dynamic dispatch, profile-guided
optimization, and then it runs native-code

Runs like:

Lexer -> Parser -> Semantic Analysis -> Bytecode -> Interpretation
Hot path -> Optimize -> Compile to machine code

## AOT
Ahead of time, is not an interpreter, and is usually optimized fully at compile-time,
and emits direct machine code directly executed by the computer

Runs like:
Lexer -> Parser -> Semantic Analysis -> IR -> Optimization -> Compile to machine code

## Compare
AOT is more efficient, but JIT is more flexible and easier to debug
JIT is also sometimes more efficient than AOT in the correct setting

# How WIT works:
WIT, short for When it's time, is a new way to run programs in the computer.

Instead of optimizing to the max then compiling to machine code ahead of time,
or using JIT to interpret and optimize at run-time, We take a hybrid approach.

We compile to machine code at run time as SOON as specific functions are needed, but we also include profiler calls inside the machine code, like:

It's a bigger delay but the code is usually as fast as unoptimized C with profiling calls.
```asm
_ZsomeFunction:
    call profiler_start
    // instructions
    call profiler_end
```

The profiler is a function that is called at the start and end of each function, to record the
time it took to run, and how many times it ran.

Usually if a function is a few instructions then the compiler would max-optimize this function to remove the profiler calls
since stuff like Math operations are usually fast, we make them faster to reduce profiler overhead

# Why WIT?

WIT is a special kind to combine the pros of AOT and JIT.

## AOT
We can compile to machine code at compile time, and then run it at run time
Pros:
- Extremely fast, no runtime or virtual machine overhead
- Fully optimized binary for release mode
- Much lower memory usage
Cons:
- No JIT, no optimization, no profiler
- Cannot have more high level stuff like reflection.

## JIT
JIT is more complex, it requires a virtual machine and profiler
Pros:
- Can have more high level stuff like reflection.
- Can have more low level stuff like profiler and optimization
- Can optimize at runtime to even rival AOT compiled languages like C/C++/Rust/Zig/Go
Cons:
- Much higher memory usage (Java, C#, V8, they have very big memory usages compared to other AOT Languages)
- Interpreted for a long time
- Slower than AOT compiled languages until they get optimized by JIT

## WIT Internals

Both JIT and AOT have very considerable cons even with their pros, but WIT has
the best of both worlds.

WIT combines these by lazy compiling functions to machine code at runtime using
a lightweight backend to transmit to machine code, and a profiler to record
how much time each function took to run and how many times it ran.

We also need IR at runtime which should be a 1:1 port from machine code to IR and vice versa.

So not only can you basically run machine code but you can also optimize at runtime JUST like a JIT.

Pros over JIT:
- Extremely fast, no runtime or virtual machine overhead, especially in warmup times
- Fully optimized binary when paths are hot, Unoptimized machine code when paths are cold (Still much faster than most interpreters)
- Can still have more high level stuff like reflection.
- Same memory usage as hot paths if not lower due to avoiding vtables, virtualization, unnecessary boxing, zero branch prediction, indirections, type checks and poor CPU utilization in general.

Pros over AOT:
- Can use profile-guided optimization at runtime to even rival AOT compiled languages like C/C++/Rust/Zig/Go
- WIT can optimize only what runs often — ideal for large programs where 90% of code is cold

Pros over Both:
- WIT IR can be pure and direct. You don’t need intermediate stack-based bytecode or virtual machine state juggling, which reduces complexity and overhead.
- Since IR is a 1:1 with machine code, tooling can inspect generated IR or code for optimizations, static errors, dead code, etc, yet it has no performance overhead (only memory overhead)
WIT Cons:
- Higher memory usage (Significantly lower than JIT especially at warmup times)
- Still has profiler overhead (Optimized away in hot paths)
- Still has warmup overhead due to lazy compilation (Compilers, especially like [zeta-lang](https://github.com/Voxon-Development/zeta-lang) is *extremely* fast for a few functions at a time, usually outweighs interpretation)