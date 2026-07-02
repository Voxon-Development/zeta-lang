# Introduction
High-performance, fearless concurrency, and memory safe systems programming without the pain.

Example:

```rs
public enum GameError {
    Game { msg: String },
    ParseFailed { err: ParseFailedError },
    Io { err: IOError }
}    
    
public interface Printable {    
    fn print() throws IOError -> void    
}    
    
public struct GuessGame {    
    target: i32
}

impl GuessGame {
    fn play_game(&this) throws GameError -> void {    
        for _ in 0..5 {    
            input := try std::io.out.readln("Enter your guess: "");
            guess := try input.parse<i32>();
            match guess {    
                g if g == this.target => {    
                    try std::io.out.println("Correct! You win!");
                    return    
                }    
                g if g < this.target => try std::io.out.println("Too low!"),  
                g if g > this.target => try std::io.out.println("Too high!"),
                _ => unsafe { assume_unreachable() }
            }    
        }
        throw GameError.Game("Out of attempts! Game over.");
    }
} 
    
impl Printable for GuessGame {    
    fn print() throws IOError -> void {    
        try std::io.out.println("Welcome to Guess the Number!")    
    }    
}    
    
fn random_number(min: i32, max: i32) throws RandomNumberError -> i32 {    
    return min + (try std::random.next_int() % (max - min + 1))    
}    
    
fn main() throws GameError, RandomNumberError -> void {    
    let game: Box<GuessGame> = Box.new(GuessGame { target: try random_number(1, 10), attempts: 0 });
    
    try *game.print()
    *game.play_game() else (err) {
        match (err) {
            case Game { msg } => {
                try std::io.out.println(msg);
            },
            case Io { err } => {
                try std::io.out.println("\{}", err);
            },
            case ParseFailed { err } => {
                try std::io.out.println("Please input a real number!");
            }
        }
    }
    
    try std::io.out.println("Thanks for playing!");
}
```

### Contributing

We’re excited you want to contribute to Zeta-Lang!

**How to Contribute:**

1. **Report Bugs**: Open an issue on GitHub if you encounter a bug or unexpected behavior. Include a minimal reproducible example.
2. **Submit Pull Requests**:
   * Fork the repository and create a feature branch (`git checkout -b feature/YourFeature`).
   * Write clear, concise, and well-documented code.
   * Include tests for new features or bug fixes.
   * Ensure code passes existing tests before submitting.
3. **Code Style**: Follow consistent formatting and naming conventions. Use `snake_case` for variables, `PascalCase` for types, and proper indentation.
4. **Documentation**: Update docs when adding features or changing behavior.
5. **Community Etiquette**: Be respectful and collaborative. We’re here to build a language together.

We welcome contributions of **all sizes**, from fixing typos to implementing major features.
