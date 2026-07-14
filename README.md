# Introduction
Research systems programming language on concurrency and memory safety.
**Linux x86_64 only for now**

## Building from Source (Linux)

Zeta ships two binaries you'll want on your `PATH`: the compiler CLI
(`zeta-compiler`) and the language server (`zeta-lsp`, used by editor
integrations like the Zed extension).

### Prerequisites

- A recent stable Rust toolchain (`rustup install stable`)
- `git`

### 1. Clone and build

```bash
git clone https://github.com/OWNER/REPO.git
cd REPO
cargo build --release --bin zeta-compiler
cargo build --release --bin zeta-lsp
```

This produces:

target/release/zeta-compiler
target/release/zeta-lsp

### 2. Install to your PATH

Copy both binaries somewhere on your `PATH`. `~/.local/bin` is a good
default on most distros (already on `PATH` by default on Ubuntu/Fedora;
otherwise add it yourself):

```bash
mkdir -p ~/.local/bin
cp target/release/zeta-compiler ~/.local/bin/
cp target/release/zeta-lsp ~/.local/bin/

# only needed if ~/.local/bin isn't already on your PATH —
# add this line to your ~/.bashrc / ~/.zshrc if `zeta-compiler --version`
# doesn't work after this step:
export PATH="$HOME/.local/bin:$PATH"
```

Verify:

```bash
zeta-compiler --version
zeta-lsp --version
```

### 3. Keeping up to date

Once installed this way, you can upgrade in place without re-cloning or
re-running `cargo build`:

```bash
zetac upgrade
```

This checks the latest GitHub release against your installed version and,
if newer, downloads and replaces both binaries in the directory they're
currently installed in.

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
