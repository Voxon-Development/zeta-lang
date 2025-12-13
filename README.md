# Introduction
High-performance, fearless concurrency, and memory safe systems programming without the pain.

A language made to **touch the realms of cutting-edge possibility** in safety, correctness and performance

Example:

enum GameError {
    IO(IOError),
    Game(String)
}

interface Printable {
    fn print(): !void
}

struct GuessGame {
    target: i32,
    attempts: i32,
}

impl Printable for GuessGame {
    fn print(): !void {
        try std.out.println("Welcome to Guess the Number!")
    }
}

fn random_number(min: i32, max: i32): i32 {
    return min + (std.random.int() % (max - min + 1))
}

fn play_game(game: *mut GuessGame): IOError!void {
    for _ in 0..5 {
        try std.out.print("Enter your guess: ")
        guess := try std.io.read().parse<i32>();
        
        match guess {
            g if g == game.target => {
                try std.out.println("Correct! You win!")
                return
            }
            g if g < game.target => try std.out.println("Too low!"),
            g if g > game.target => try std.out.println("Too high!"),
        }
        game.attempts += 1
    }
    try std.out.println("Out of attempts! Game over.")
    return GameError.Game("Out of attempts!");
}

fn main(): IOError!void {
    // Allocate the game on the heap
    let mut game: *mut GuessGame = try std.mem.malloc(size_of<GuessGame>())
    game = GuessGame { target: random_number(1, 10), attempts: 0 }

    game.print()
    try play_game(game)

    try std.out.println("Thanks for playing!")

    // auto drop game
}


A memory-safe, systems programming languages, to be made in a new generation of languages like you've never seen.

## Roadmap

- Stdlib
  - Core collections, I/O, concurrency primitives, time, math
  - Small, well-documented standard library shipped with the compiler/runtime
- Bootstrapping
  - Self-hosting compiler roadmap (write the compiler in zeta)
  - Clear bootstrapping plan with intermediate toolchains
- Bootstrapping toolchain & release process
  - Reproducible builds and toolchain pinning

- Memory safety guarantees
  - Borrowing/ownership model or equivalent system to avoid use-after-free and data races
  - Clear semantics and ergonomic patterns
- Fearless concurrency
  - Message-passing and/or structured concurrency primitives
  - Session types (see advanced research features)
- Type system foundations
  - Strong static typing, with plans for advanced type theory features
  - Option for checking effects or resource usage

- Separation logic
  - Research integration options for formal reasoning about mutable state
- Intuitionistic type theory
  - Experimental modules or proof-oriented features for theorem-proving use-cases
- Session types
  - Optional language-level support for protocol-safe concurrency
  - Library-first approach, with language-level support evaluated later

  Absolutely! Here’s a solid draft for your `# Contributing` and `# License` sections, tailored for your Zeta-Lang project:

---

### # Contributing

We’re excited you want to contribute to Zeta-Lang! By joining our community of developers, you’ll help shape a high-performance, memory-safe, and fearless concurrency language.

**How to Contribute:**

1. **Join the Discord**: Start by joining our [Discord server](https://discord.gg/VXGk2jjuzc) to discuss ideas, report issues, and get help.
2. **Report Bugs**: Open an issue on GitHub if you encounter a bug or unexpected behavior. Include a minimal reproducible example.
3. **Submit Pull Requests**:

   * Fork the repository and create a feature branch (`git checkout -b feature/YourFeature`).
   * Write clear, concise, and well-documented code.
   * Include tests for new features or bug fixes.
   * Ensure code passes existing tests before submitting.
4. **Code Style**: Follow consistent formatting and naming conventions. Use `snake_case` for variables, `PascalCase` for types, and proper indentation.
5. **Documentation**: Update docs when adding features or changing behavior. Clear documentation is crucial for Zeta-Lang’s adoption.
6. **Community Etiquette**: Be respectful and collaborative. We’re here to build a language together.

We welcome contributions of **all sizes**, from fixing typos to implementing major features. Your help brings Zeta-Lang closer to the cutting edge of research systems programming.

---

### # License

Zeta-Lang is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.

```
GNU GENERAL PUBLIC LICENSE
Version 3, 29 June 2007

Copyright (C) 2025 Eyad Amr

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

**Summary:** You are free to use, modify, and distribute Zeta-Lang, but any derivative works must also be released under the GPL-3.0 license.
