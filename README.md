# zeta-lang
New generation of JIT like you've never seen.

Example:

```
main() {
    println_str("Hello, World!!!")
    println_int(49)

    let something: boolean = true
    if (something) {
        let num: i32 = 49 + (23 * 39)
        println_int(num)
    }
}
```

Visibility modifiers are optional.

Current stuff to be done:
- [x] OOP (Currently limited)
- [x] Functions (Return values too)
- [x] Type inference
- [ ] Extensive FFI and Importing (Dependent on OOP)
- [ ] SDK (Dependent on Extensive FFI and Importing and Arrays)
- [ ] Optimizations for generated IR
- [x] If statements
- [x] While statements
- [x] For statements
- [x] Match statements
- [ ] Arrays
