# Pumpkin

[![Build Status](https://github.com/ConSol-Lab/pumpkin/actions/workflows/ci.yml/badge.svg)](https://github.com/ConSol-Lab/pumpkin/actions/workflows/ci.yml)
![DRCP Format on crates.io](https://img.shields.io/badge/drcp--format-v0.1.0-green)
![Pumpkin on crates.io](https://img.shields.io/badge/pumpkin--solver-v0.1.0-green)

<p align="center">
    <img align="left" width="50%" src="./pumpkin.svg", alt="ASCII art pumpkin logo">
</p>

<p align="left">
<br>
Pumpkin is a combinatorial optimisation solver developed by the ConSol Lab at TU Delft. It is based on the (lazy clause generation) constraint programming paradigm.

Our goal is to keep the solver efficient, easy-to-use, and well-documented. The solver is written in pure Rust and follows Rust best practices. As a result, downloading and compiling Pumpkin is straight-forward!

A unique feature of Pumpkin is that it can produce a certificate of unsatisfiability. See [our CP’24 paper](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.CP.2024.11) for more details.
</p>

<br clear="left"/>

The solver currently supports integer variables and a number of (global) constraints:

- [Cumulative global constraint](./pumpkin-solver/src/propagators/cumulative/).
- [Element global constraint](./pumpkin-solver/src/propagators/element.rs).
- [Arithmetic constraints](./pumpkin-solver/src/propagators/arithmetic/): [linear integer (in)equalities](./pumpkin-solver/src/propagators/arithmetic/linear_less_or_equal.rs), [integer division](./pumpkin-solver/src/propagators/arithmetic/division.rs), [integer multiplication](./pumpkin-solver/src/propagators/arithmetic/integer_multiplication.rs), [maximum](./pumpkin-solver/src/propagators/arithmetic/maximum.rs), [absolute value](./pumpkin-solver/src/propagators/arithmetic/absolute_value.rs).
- Clausal constraints.

We are actively developing Pumpkin and would be happy to hear from you should you have any questions or feature requests!



# Usage

## Building
To clone the project, run:
```sh 
git clone https://github.com/ConSol-Lab/pumpkin
```

Since Pumpkin is written in pure Rust, it is easy to install! After cloning, you can build the project using a version of [Rust](https://www.rust-lang.org/tools/install) (1.72.1+) using the following commands:

```sh
cargo build           # Creates a non-optimized build with debug info
cargo build --release # Creates an optimized build
```

## MiniZinc
Pumpkin serves as a backend solver for the [MiniZinc](https://www.minizinc.org/) modelling language.

To use it as such a backend, follow the following steps:

- Step 1: Clone the repository and build it using `cargo build --release`.
- Step 2: Install MiniZinc using the [appropriate executable](https://www.minizinc.org/resources/) or [binary archive](https://www.minizinc.org/downloads/).
- Step 3: Add the following to the `MZN_SOLVER_PATH` environment variable: `<path_to_pumpkin>/pumpkin-cli/minizinc` (see [this thread](https://askubuntu.com/questions/58814/how-do-i-add-environment-variables) on how to do this using a shell).
- Step 4: Check whether the installation worked using the command `minizinc --help pumpkin`.

## Components
Pumpkin consists of 3 different crates:

- The library contained in [pumpkin-solver](./pumpkin-solver/); defines the API through which the solver can be used via Rust.
- The CLI contained in [here](./pumpkin-solver/src/bin); defines the usage of Pumpkin through a command line.
- The proof logging contained in [drcp-format](./drcp-format/); defines proof logging which can be used in combination with Pumpkin.
- The python bindings contained in [pumpkin-py](./pumpkin-py/); defines the python interface for Pumpkin

The easiest way to get to know the different modules is through the documentation. This documentation can be created automatically using the command:
```sh
cargo doc --no-deps
```

## Examples
There are several examples of how to use the solver specified in the documentation of the different components. For more concrete examples of how to use Pumpkin to solve a set of example problems, we refer to the [examples folder](./pumpkin-solver/examples/) which contains examples such as bibd, nqueens, and disjunctive scheduling.

# Contributing
We encourage contributions to Pumpkin by merge requests and issues. When contributing please ensure that you adhere to the following guidelines.

## Documentation
One of the development goals of Pumpkin is to ensure that the solver is easy-to-use and well-documented. To this end, it is required that any external contribution is well-documented (both the structs/enums/methods and the implementation itself)!

## Pre-commit Hooks
To ensure certain standards, we make use of [pre-commit hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks). The hooks that we use can be registered using the following command:
```sh
cp .githooks/pre-commit .git/hooks
```

## Formatting
To make use of these formatting rules, we require the [nightly toolchain](https://doc.rust-lang.org/beta/book/appendix-07-nightly-rust.html). *Note that we only use the nightly toolchain for formatting.* The nightly version which can be installed using the following command:
```sh
rustup toolchain install --component rustfmt -- nightly
```
The formatting can then be run using:
```sh
cargo +nightly fmt
```


