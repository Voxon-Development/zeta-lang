# ALPHA STAGES
# zeta-lang 
New generation of JIT like you've never seen, High-performance, fearless concurrency, and memory safe systems programming without the pain.

Example:

```
main() {
    println("Hello, World!!!")
    println(49)

    let something: boolean = true
    if (something) {
        let num: i32 = 49 + (23 * 39)
        println_int(num)
    }
}
```

A memory-safe, low-level, WIT-compiled, to be made in a new generation of languages like you've never seen, with fast compilation.

# Short recap of WIT compilation

WIT (When it's time) compilation is the hybrid process of JIT and AOT.

AOT compiles machine code and optimizes it at compile time

JIT interprets, profiles hot paths and optimizes hot paths (while cold paths are not as optimized)

Both have their disadvantages and advantages, but what if a language started with machine code, but could profile machine code? That's WIT compilation.

WIT compilation is the process of compiling the language to an IR then to machine code, unlike AOT we store the IR at runtime for profiling and optimization, the machine code has profiler calls injected to know what's hot and what's cold

then it goes through tiered JIT, where the final tier will optimize away profiling unless deoptimization occurs.

Check out more detail [here](https://github.com/Voxon-Development/zeta-lang/blob/main/theory/WIT.md)

A language made to **touch the realms of possibility** in JIT, memory management and performance

| Category | **Zeta’s Uniqueness** |
|---------|----------------------|
| **Concurrency** | Combines simplicity with fine-grained control (fiber-level scheduling, profiling) |
| **Performance Tuning** | Explicit memory regions, no GC, JIT profile-directed optimizations |
| **Memory Safety** | Combines region sets with typed pointer rules |
| **Metaprogramming** | AST-level, powerful compile-time capabilities without runtime overhead |
