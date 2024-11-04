# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.3...pumpkin-solver-v0.2.0) (2024-11-04)


### ⚠ BREAKING CHANGES

* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98))

### Features

* adding synchronisation for the cumulative constraint ([#86](https://github.com/ConSol-Lab/Pumpkin/issues/86)) ([5ef9d56](https://github.com/ConSol-Lab/Pumpkin/commit/5ef9d56f34e5d77cc3ccb753070f09cae93e7311))
* implement consistency check interface in linear less than propagator ([#87](https://github.com/ConSol-Lab/Pumpkin/issues/87)) ([089a083](https://github.com/ConSol-Lab/Pumpkin/commit/089a083432a064c5eb001e6b78293eac85d0e0f1))
* incremental backtracking for the cumulative constraint ([#60](https://github.com/ConSol-Lab/Pumpkin/issues/60)) ([571f99f](https://github.com/ConSol-Lab/Pumpkin/commit/571f99f013cec0c2148b1d3578fa3efdaff396fb))
* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98)) ([e5ae6c2](https://github.com/ConSol-Lab/Pumpkin/commit/e5ae6c25ac6d9e5407d3b1ed963c20ef25e88d18))


### Bug Fixes

* complete proof when propagator initialization identifies root-level conflict ([#80](https://github.com/ConSol-Lab/Pumpkin/issues/80)) ([2ecfe4d](https://github.com/ConSol-Lab/Pumpkin/commit/2ecfe4da0ddda4b22d552c7eb789652c014c1127))
* do not print intermediate solution when flag is not set + solution callback for satisfaction problems ([#77](https://github.com/ConSol-Lab/Pumpkin/issues/77)) ([e853d05](https://github.com/ConSol-Lab/Pumpkin/commit/e853d05b403256229a14276e5ed5233691fcb6ad))
* float stat into one line ([#83](https://github.com/ConSol-Lab/Pumpkin/issues/83)) ([d0c459d](https://github.com/ConSol-Lab/Pumpkin/commit/d0c459d285324e53d34a8a738448a51d06251d6a))
* issue with time-point ending up between profiles for pointwise sequence explanation ([#78](https://github.com/ConSol-Lab/Pumpkin/issues/78)) ([2fcb011](https://github.com/ConSol-Lab/Pumpkin/commit/2fcb0117fc437a9ce11df2688bace2a5395a1a02))
* take ownership of the propagation context in propagators ([#85](https://github.com/ConSol-Lab/Pumpkin/issues/85)) ([13f44a0](https://github.com/ConSol-Lab/Pumpkin/commit/13f44a01b746c6b2387be5e0737d9b37b5e8a527))

## [0.1.3](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.2...pumpkin-solver-v0.1.3) (2024-10-31)


### Features

* adding synchronisation for the cumulative constraint ([#86](https://github.com/ConSol-Lab/Pumpkin/issues/86)) ([5ef9d56](https://github.com/ConSol-Lab/Pumpkin/commit/5ef9d56f34e5d77cc3ccb753070f09cae93e7311))
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
