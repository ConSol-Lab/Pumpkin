# Setting Up Your Environment

Before working with Pumpkin, make sure your environment is properly set up. This section provides steps to prepare your Rust toolchain and explore the Pumpkin codebase.

### Install Rust

Install Rust using the official installer:

<https://rust-lang.org/tools/install/>

This will install:

- the Rust compiler (`rustc`)
- `cargo`, Rust’s package manager and build tool

### Clone the repository

Clone the course repository:

```bash
git clone https://github.com/ConSol-Lab/CS4535.git
```

### Verify your Rust installation

Navigate into the repository and build the project:

```bash
cd CS4535
cargo build
```

This compiles the solver and confirms that your Rust toolchain is functioning correctly.

### Generate the documentation

Pumpkin includes extensive inline documentation. You can build and view it locally using:

```bash
cargo doc --no-deps --open --document-private-items
```

This opens a local documentation page in your browser, which is helpful when navigating the codebase.

### TODO
- Discuss Clippy.
- A few words on IDEs. Suggest that running Clippy upon save (CRTL+S) is a good setup (vscode specific?).
- How to add git hooks.