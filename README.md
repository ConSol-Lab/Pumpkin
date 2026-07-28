# Pumpkin

[![Build Status](https://github.com/ConSol-Lab/pumpkin/actions/workflows/ci.yml/badge.svg)](https://github.com/ConSol-Lab/pumpkin/actions/workflows/ci.yml)
[![License: MIT OR Apache-2.0](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue.svg)](#license)
[![Pumpkin on crates.io](https://img.shields.io/crates/v/pumpkin-solver?label=pumpkin-solver)](https://crates.io/crates/pumpkin-solver)
[![docs.rs](https://img.shields.io/docsrs/pumpkin-solver)](https://docs.rs/pumpkin-solver)
[![DRCP Format on crates.io](https://img.shields.io/crates/v/drcp-format?label=drcp-format)](https://crates.io/crates/drcp-format)
[![Pumpkin on PyPI](https://img.shields.io/pypi/v/pumpkin_solver)](https://pypi.org/project/pumpkin-solver/)

<p align="center">
    <img align="left" width="50%" src="https://github.com/ConSol-Lab/Pumpkin/raw/main/pumpkin.svg" alt="ASCII art pumpkin logo">
</p>

<p align="left">
<br>

Pumpkin is a combinatorial optimisation solver developed within the [ConSol Lab](https://github.com/ConSol-Lab) at TU Delft. It is based on the (lazy clause generation) constraint programming paradigm.

Our goal is to keep the solver efficient, easy to use, and well-documented. The solver is written in pure Rust and follows Rust best practices, making it straightforward to download and compile.

A unique feature of Pumpkin is that it can produce certificates of infeasibility and optimality, which can be checked independently of the solver using our formally verified checker. See our [CP'24](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2024.11) and [CP'26](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2026.24) papers for details.

### 🥈🥉 Silver (fixed search) and bronze (free search) at the [2026 MiniZinc Challenge](https://www.minizinc.org/challenge/2026/results/)
### 🥉 Bronze (fixed search) at the [2025 MiniZinc Challenge](https://www.minizinc.org/challenge/2025/results/)

</p>

<br clear="left"/>

## Table of Contents

- [Features](#features)
- [Installation](#installation)
  - [Building from Source](#building-from-source)
- [Examples](#examples)
- [MiniZinc](#minizinc)
- [Components](#components)
- [Contributing](#contributing)
- [Team](#team)
- [Publications Involving Pumpkin](#publications-involving-pumpkin)
- [Citing](#citing)
- [License](#license)

## Features

Pumpkin can produce certificates of infeasibility and optimality, checkable using our formally verified checker.

Pumpkin currently supports integer variables and the following (global) constraints:

- [Cumulative global constraint](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/propagators/src/propagators/cumulative)
- [Disjunctive global constraint](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/propagators/src/propagators/disjunctive)
- [Element global constraint](https://github.com/ConSol-Lab/Pumpkin/blob/main/pumpkin-crates/propagators/src/propagators/element.rs)
- Arithmetic constraints: [linear integer (in)equalities](https://github.com/ConSol-Lab/Pumpkin/blob/main/pumpkin-crates/propagators/src/propagators/arithmetic/linear_less_or_equal.rs), [integer division](https://github.com/ConSol-Lab/Pumpkin/blob/main/pumpkin-crates/propagators/src/propagators/arithmetic/integer_division.rs), [integer multiplication](https://github.com/ConSol-Lab/Pumpkin/blob/main/pumpkin-crates/propagators/src/propagators/arithmetic/integer_multiplication.rs), [maximum](https://github.com/ConSol-Lab/Pumpkin/blob/main/pumpkin-crates/propagators/src/propagators/arithmetic/maximum.rs), [absolute value](https://github.com/ConSol-Lab/Pumpkin/blob/main/pumpkin-crates/propagators/src/propagators/arithmetic/absolute_value.rs)
- Clausal constraints

We are actively developing Pumpkin and would be happy to hear from you — feel free to open an [issue](https://github.com/ConSol-Lab/Pumpkin/issues) or start a [discussion](https://github.com/ConSol-Lab/Pumpkin/discussions) with any questions or feature requests!

## Installation

To use Pumpkin as a library, use cargo to install it with:
```sh
cargo add pumpkin-solver
```

Pumpkin is also the command-line interface to the library. It provides support for (W)CNF and FlatZinc files. Obtaining the solver binary can also be done with cargo:
```sh
cargo install pumpkin-solver
```

### Building from Source

To clone the project, run:
```sh
git clone https://github.com/ConSol-Lab/Pumpkin.git
```

Since Pumpkin is written in pure Rust, it is easy to install! After cloning, you can build the project using a version of [Rust](https://www.rust-lang.org/tools/install) (1.72.1+) using the following commands:

```sh
cargo build           # Creates a non-optimized build with debug info
cargo build --release # Creates an optimized build
```

## Examples

Examples of how to use the solver are included in the [documentation](https://docs.rs/pumpkin-solver) of the different components. For more concrete, runnable examples of solving example problems with Pumpkin, see the [examples folder](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-solver/examples), which includes `bibd`, `nqueens`, and disjunctive scheduling.

## MiniZinc

Pumpkin serves as a backend solver for the [MiniZinc](https://www.minizinc.org/) modelling language.

To use it as a backend, follow these steps:

- Step 1: Clone the repository and build it using `cargo build --release`.
- Step 2: Install MiniZinc using the [appropriate executable](https://www.minizinc.org/resources/) or [binary archive](https://www.minizinc.org/downloads/).
- Step 3: Add the following to the `MZN_SOLVER_PATH` environment variable: `<path_to_pumpkin>/minizinc` (see [this thread](https://askubuntu.com/questions/58814/how-do-i-add-environment-variables) on how to do this using a shell).
- Step 4: Check whether the installation worked using the command `minizinc --help pumpkin`.

This will add Pumpkin and PumpkinProof (which uses a flattening library specific for proof logging).

## Components

Pumpkin consists of several different components:

- The crates contained in [pumpkin-crates](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates):
    - [pumpkin-core](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/core); defines the API through which the solver can be used via Rust.
    - [pumpkin-propagators](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/propagators); contains (most of) the propagators used by Pumpkin.
    - [pumpkin-constraints](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/constraints); contains convenient ways to add one or more propagators modelling certain constraints to the solver.
    - [pumpkin-conflict-resolvers](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/conflict-resolvers); contains the conflict resolvers (e.g., 1UIP or All-Decision conflict resolvers) used by Pumpkin.
    - [pumpkin-checking](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-crates/checking); contains the types used for checking the soundness of propagators in Pumpkin.
- The CLI contained in [pumpkin-solver](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-solver).
- The Python bindings contained in [pumpkin-solver-py](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-solver-py).
- The proof logging contained in [drcp-format](https://github.com/ConSol-Lab/Pumpkin/tree/main/drcp-format); a file reader and writer for the DRCP proof format (the proof format used by Pumpkin).
- The (unverified) proof processor contained in [pumpkin-proof-processor](https://github.com/ConSol-Lab/Pumpkin/tree/main/pumpkin-proof-processor).
- A debugger for DRCP proofs contained in [drcp-debugger](https://github.com/ConSol-Lab/Pumpkin/tree/main/drcp-debugger).

The easiest way to get to know the different modules is through the documentation. This documentation can be created automatically using the command:
```sh
cargo doc --no-deps
```

## Contributing

We encourage contributions to Pumpkin via pull requests and issues. When contributing, please ensure that you adhere to the following guidelines.

### Documentation

One of the development goals of Pumpkin is to ensure that the solver is easy to use and well-documented. To this end, it is required that any external contribution is well-documented (both the structs/enums/methods and the implementation itself)!

### Pre-commit Hooks

To ensure certain standards, we make use of [pre-commit hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks). The hooks that we use can be registered using the following command:
```sh
cp .githooks/pre-commit .git/hooks
```

### Formatting

To make use of these formatting rules, we require the [nightly toolchain](https://doc.rust-lang.org/beta/book/appendix-07-nightly-rust.html). *Note that we only use the nightly toolchain for formatting.* The nightly version can be installed using the following command:
```sh
rustup toolchain install --component rustfmt -- nightly
```
The formatting can then be run using:
```sh
cargo +nightly fmt
```

## Team

Pumpkin is developed within the [ConSol Lab](https://github.com/ConSol-Lab) at TU Delft.

- **Emir Demirović** — Principal Investigator
- **Maarten Flippo**, **Imko Marijnissen** — Core developers, with [contributions from others](https://github.com/ConSol-Lab/Pumpkin/graphs/contributors?all=1)

## Publications Involving Pumpkin

### Led by the ConSol Lab
- [Formally Verified Certification of Constraint Programming Proofs](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2026.24) — Flippo, Sidorov, ten Brink, Pit-Claudel, Demirović; CP 2026
- [From Literals to Atomic Constraints: Generalising Conflict-Driven Clause Learning for Constraint Programming](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2026.42) — Marijnissen, Flippo, Demirović; CP 2026
- [Domain-Independent Dynamic Programming with Constraint Propagation](https://ojs.aaai.org/index.php/ICAPS/article/view/42826) — Marijnissen, Beck, Demirović, Kuroiwa; ICAPS 2026
- [Resolution Meets Cutting Planes: Introducing Hypercube Linear Resolution](https://link.springer.com/chapter/10.1007/978-3-032-27242-3_10) — Flippo, Stuckey, Demirović; CPAIOR 2026
- [Conflict Analysis Based on Cutting-Planes for Constraint Programming](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2025.4) — Baauw, Flippo, Demirović; CP 2025
- [Unite and Lead: Finding Disjunctive Cliques for Scheduling Problems](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2025.35) — Sidorov, Marijnissen, Demirović; CP 2025
- [A Multi-Stage Proof Logging Framework to Certify the Correctness of CP Solvers](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2024.11) — Flippo, Sidorov, Marijnissen, Smits, Demirović; CP 2024

### Led by others
- [Towards Step-Wise Explanations of Large Search Trees](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2026.62) — Bleukx, Stuckey, Guns; CP 2026
- [On Inferring Cumulative Constraints](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2026.50) — Sidorov; CP 2026
- [Using Certifying Constraint Solvers for Generating Step-wise Explanations](https://ojs.aaai.org/index.php/AAAI/article/view/38432) — Bleukx, Flippo, Bogaerts, Demirović, Guns; AAAI 2026

## Citing

Please cite Pumpkin using the following citation:

```bibtex
@inproceedings{marijnissen_et_al:LIPIcs.CP.2026.42,
  author = {Marijnissen, Imko and Flippo, Maarten and Demirovi\'{c}, Emir},
  title = {{From Literals to Atomic Constraints: Generalising Conflict-Driven Clause Learning for Constraint Programming}},
  booktitle = {32nd International Conference on Principles and Practice of Constraint Programming (CP 2026)},
  pages = {42:1--42:21},
  series = {Leibniz International Proceedings in Informatics (LIPIcs)},
  isbn = {978-3-95977-432-1},
  issn = {1868-8969},
  year = {2026},
  volume = {379},
  editor = {Beldiceanu, Nicolas},
  publisher = {Schloss Dagstuhl -- Leibniz-Zentrum f{\"u}r Informatik},
  address = {Dagstuhl, Germany},
  url = {https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2026.42},
  urn = {urn:nbn:de:0030-drops-266744},
  doi = {10.4230/LIPIcs.CP.2026.42},
  annote = {Keywords: LCG, CP, CDCL, Lazy Literal, Conflict Analysis, Nogood Propagation},
}
```

If you are using the proof-logging capabilities of Pumpkin, please **additionally** include the following citation:

```bibtex
@InProceedings{flippo_et_al:LIPIcs.CP.2024.11,
  author =  {Flippo, Maarten and Sidorov, Konstantin and Marijnissen, Imko and Smits, Jeff and Demirovi\'{c}, Emir},
  title =   {{A Multi-Stage Proof Logging Framework to Certify the Correctness of CP Solvers}},
  booktitle =   {30th International Conference on Principles and Practice of Constraint Programming (CP 2024)},
  pages =   {11:1--11:20},
  series =  {Leibniz International Proceedings in Informatics (LIPIcs)},
  ISBN =    {978-3-95977-336-2},
  ISSN =    {1868-8969},
  year =    {2024},
  volume =  {307},
  editor =  {Shaw, Paul},
  publisher =   {Schloss Dagstuhl -- Leibniz-Zentrum f{\"u}r Informatik},
  address = {Dagstuhl, Germany},
  URL =     {https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2024.11},
  URN =     {urn:nbn:de:0030-drops-206969},
  doi =     {10.4230/LIPIcs.CP.2024.11},
  annote =  {Keywords: proof logging, formal verification, constraint programming}
}
```

## License

Pumpkin is dual-licensed under either of

- [Apache License, Version 2.0](LICENSE-APACHE)
- [MIT license](LICENSE-MIT)

at your option.