# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.3](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.2...pumpkin-solver-v0.1.3) (2024-10-31)


### Features

* implement consistency check interface in linear less than propagator ([#87](https://github.com/ConSol-Lab/Pumpkin/issues/87)) ([089a083](https://github.com/ConSol-Lab/Pumpkin/commit/089a083432a064c5eb001e6b78293eac85d0e0f1))

## [0.1.2](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.1...pumpkin-solver-v0.1.2) - 2024-10-30

### Added

- incremental backtracking for the cumulative constraint ([#60](https://github.com/ConSol-Lab/Pumpkin/pull/60))

### Fixed

- do not print intermediate solution when flag is not set + solution callback for satisfaction problems ([#77](https://github.com/ConSol-Lab/Pumpkin/pull/77))
- issue with time-point ending up between profiles for pointwise sequence explanation ([#78](https://github.com/ConSol-Lab/Pumpkin/pull/78))
- take ownership of the propagation context in propagators ([#85](https://github.com/ConSol-Lab/Pumpkin/pull/85))
- complete proof when propagator initialization identifies root-level conflict ([#80](https://github.com/ConSol-Lab/Pumpkin/pull/80))
- float stat into one line ([#83](https://github.com/ConSol-Lab/Pumpkin/pull/83))

## [0.1.1] - 2024-10-17

### Added

- Documentation to the containers module.
- Installation instructions through cargo to the README.

### Fixed

- Link to crates.io in README.

## [0.1.0] - 2024-10-17

### Added

- Lazy clause generation with eager SAT literals for all integer domains.
- VSIDS variable selection with solution-guided/phase-saving value selection.
- Support for the FlatZinc builtins for integer and boolean variables.
- Support for the `cumulative` global constraint through time-table filtering.
- Support for MiniZinc search annotations.
- Example model for disjunctive scheduling using the Rust API.
- Logging of various solver statistics.
- Start of a programmable search API.
