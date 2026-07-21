# Introduction

Research systems programming language focused on concurrency and memory safety.

**Linux x86_64 only for now.**

# Install Zeta (Linux)

The recommended way to install Zeta is through the official release archive.

The archive contains the complete Zeta toolchain:

* `zeta-lang` -- Zeta compiler
* `zeta-lsp` -- Language server for editor integrations
* `zetaup` -- Zeta toolchain updater
* `lib/` -- Zeta standard library

## Quick Install

Download the latest release:

```bash
curl -LO https://github.com/Voxon-Development/zeta-lang/releases/latest/download/zeta-linux-x86_64.tar.gz
```

Extract it:

```bash
tar -xzf zeta-linux-x86_64.tar.gz
cd zeta-linux-x86_64
```

Install Zeta:

```bash
mkdir -p ~/.local/bin
mkdir -p ~/.local/share/zeta

cp zeta-lang zeta-lsp zetaup ~/.local/bin/
cp -r lib ~/.local/share/zeta/
```

Add `~/.local/bin` to your `PATH` if needed:

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

Verify your installation:

```bash
zeta-lang --version
zeta-lsp --version
zetaup --version
```

You are now ready to use Zeta.

---

# Updating Zeta

Zeta includes a toolchain updater that keeps the compiler, language server, and standard library synchronized.

Run:

```bash
zetaup upgrade
```

The updater will:

* Check for the latest Zeta release
* Download updated binaries
* Update `zeta-lang`
* Update `zeta-lsp`
* Update the standard library

To see more details:

```bash
zetaup --verbose upgrade
```

**zetaup must be updated manually using the steps above, zetaup cannot update itself.

---

# Building from Source (Linux)

Building from source is intended for contributors and compiler developers.

## Prerequisites

* A recent nightly Rust toolchain: (Zeta depends on stuff such as allocator_api)

```bash
rustup install nightly
```

* `git`

## Clone and Build

```bash
git clone https://github.com/Voxon-Development/zeta-lang.git
cd zeta-lang

cargo build --release --bin zeta-lang
cargo build --release --bin zeta-lsp
cargo build --release --bin zetaup
```

This produces:

```text
target/release/
--- zeta-lang
--- zeta-lsp
--- zetaup
```

You can install the locally built binaries:

```bash
mkdir -p ~/.local/bin

cp target/release/zeta-lang ~/.local/bin/
cp target/release/zeta-lsp ~/.local/bin/
cp target/release/zetaup ~/.local/bin/
```

The standard library is available in the repository under:

```text
lib/
```

---

# Contributing

We’re excited you want to contribute to Zeta-Lang!

## How to Contribute

### Report Bugs

Open an issue on GitHub if you encounter a bug or unexpected behavior.

Please include:

* A minimal reproducible example
* Expected behavior
* Actual behavior
* Zeta version

### Submit Pull Requests

1. Fork the repository and create a feature branch:

```bash
git checkout -b feature/YourFeature
```

2. Write clear and documented code.

3. Include tests for new features or bug fixes.

4. Ensure existing tests pass before submitting.

### Code Style

Follow consistent formatting and naming conventions:

* Use `snake_case` for variables and functions.
* Use `PascalCase` for types.
* Keep formatting consistent.

### Documentation

Update documentation when adding features or changing behavior.

### Community Etiquette

Be respectful and collaborative.

Contributions of all sizes are welcome, from fixing typos to implementing major compiler features.
