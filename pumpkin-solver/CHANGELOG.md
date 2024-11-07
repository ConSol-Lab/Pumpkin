# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.3...pumpkin-solver-v0.2.0) (2024-11-07)


### ⚠ BREAKING CHANGES

* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98))

### Features

* allow logging of statistics to arbitrary writers ([#73](https://github.com/ConSol-Lab/Pumpkin/issues/73)) ([136e03a](https://github.com/ConSol-Lab/Pumpkin/commit/136e03a6440f7e07e24e0e2f4e79ceb837c67a2d))
* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98)) ([e5ae6c2](https://github.com/ConSol-Lab/Pumpkin/commit/e5ae6c25ac6d9e5407d3b1ed963c20ef25e88d18))


### Bug Fixes

* cumulative holes in domain incorrectly used cached profile ([#109](https://github.com/ConSol-Lab/Pumpkin/issues/109)) ([4c86a8b](https://github.com/ConSol-Lab/Pumpkin/commit/4c86a8ba5a6b291c21da62be3fc4ed0e8321eda9))
* cumulative sequence generation found less profiles than it should + fixing issues with debug propagation ([#110](https://github.com/ConSol-Lab/Pumpkin/issues/110)) ([f17222d](https://github.com/ConSol-Lab/Pumpkin/commit/f17222db5dd2e8fd01a1c55c1c08e4c557de50e6))
* ensure drcp-format version is the published one ([#101](https://github.com/ConSol-Lab/Pumpkin/issues/101)) ([2f6df0c](https://github.com/ConSol-Lab/Pumpkin/commit/2f6df0c403bce7951c41ee0e275b9bdbef1cf9c4))
* use propagate for "&lt;=" constraint + using solution reference from solution rather than solver  ([#108](https://github.com/ConSol-Lab/Pumpkin/issues/108)) ([668bd0d](https://github.com/ConSol-Lab/Pumpkin/commit/668bd0df2b5856d7f74b8f58a280660bd93daebc))
* wrong profile found for over-interval conflict explanation ([#111](https://github.com/ConSol-Lab/Pumpkin/issues/111)) ([4ba1d50](https://github.com/ConSol-Lab/Pumpkin/commit/4ba1d50276c4b189380e5a2072570297a5bfff0b))

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
