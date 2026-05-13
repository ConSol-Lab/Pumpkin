# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Pumpkin is a constraint programming (CP) solver written in pure Rust, based on the lazy clause generation paradigm. It supports proof logging via DRAT (SAT) and DRCP (CP) certificates.

## Build & Development Commands

```bash
# Build
cargo build
cargo build --release

# Test (standard CI command)
cargo test --release --no-fail-fast --features pumpkin-solver/check-propagations

# Run a single test
cargo test --release --features pumpkin-solver/check-propagations <test_name>

# Format (requires nightly)
cargo +nightly fmt
cargo +nightly fmt --check  # check only

# Lint (requires nightly)
cargo +nightly clippy --all-targets -- -Dwarnings

# Docs
cargo doc --no-deps

# License check
cargo deny check

# WASM tests (for pumpkin-crates/core)
wasm-pack test --release --node

# Python tests
cd pumpkin-solver-py && pytest
```

Scripts in `scripts/` (`fmt.sh`, `clippy.sh`, `documentation.sh`, `deny.sh`) mirror the CI commands and are also run by the pre-commit hook (`.githooks/pre-commit`).

## Workspace Architecture

The project is a Cargo workspace (edition 2024, resolver 2). Crates are grouped by role:

### Core Engine (`pumpkin-crates/`)
- **`core`** — The main solver engine. Contains the CDCL loop, propagation engine, nogood learning, branching heuristics, and proof logging infrastructure. This is the heart of the solver.
- **`checking`** — Shared types used by both `core` and `pumpkin-checker` (avoids circular deps).
- **`propagators`** — Implementations of CP propagators (arithmetic, cumulative, disjunctive, element, etc.).
- **`conflict-resolvers`** — Pluggable conflict analysis strategies for nogood derivation.
- **`constraints`** — High-level constraint API built on top of `core`.

### Interfaces
- **`pumpkin-solver`** — CLI binary. Accepts CNF, WCNF (MaxSAT), and FlatZinc input formats.
- **`pumpkin-solver-py`** — Python bindings via PyO3.

### Proof Infrastructure
- **`drcp-format`** — Reading/writing the DRCP proof certificate format.
- **`pumpkin-checker`** — Standalone proof verification tool.
- **`pumpkin-proof-processor`** — Proof transformation/preprocessing utility.
- **`drcp-debugger`** — Debugging tool for DRCP proofs.

### Parsing & Utilities
- **`fzn-rs`** / **`fzn-rs-derive`** — FlatZinc parser and derive macros.
- **`pumpkin-macros`** — Procedural macros used across the workspace.
- **`minizinc/`** — MiniZinc integration (solver plugin).

## Key Design Concepts

- **Lazy Clause Generation (LCG)**: The solver operates on a hybrid SAT/CP model. CP propagators generate explanations (clauses/nogoods) on demand during conflict analysis.
- **Proof Logging**: Every inference can be certified. The `core` crate threads proof-logging through propagators via a `Proof` type. The `check-propagations` feature enables runtime validation of propagator explanations during tests.
- **Propagator Interface**: Custom propagators implement the `Propagator` trait in `pumpkin-crates/core`. They must provide both `propagate` and `explain` methods for proof soundness.
- **Feature Flags**: `pumpkin-solver/check-propagations` enables expensive correctness assertions — always enable this when running tests.

## Code Style

- **Rust edition 2024**, stable toolchain (see `rust-toolchain.toml`), but formatting/linting requires nightly.
- Line length: 120 chars for comments (`rustfmt.toml`).
- Imports: grouped (std / external / crate), single-line style enforced.
- Workspace-level lints are configured in the root `Cargo.toml` — check there before suppressing warnings locally.
