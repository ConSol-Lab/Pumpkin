# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.2](https://github.com/ConSol-Lab/Pumpkin/releases/tag/pumpkin-propagators-v0.2.2) - 2026-02-10

### Added

- Calculate minimal profile for explaining time-table conflict ([#353](https://github.com/ConSol-Lab/Pumpkin/pull/353))
- *(pumpkin-core)* Propagator for the hypercube linear constraint ([#347](https://github.com/ConSol-Lab/Pumpkin/pull/347))
- Extract conflict resolvers and nogood minimisation into a separate crate ([#341](https://github.com/ConSol-Lab/Pumpkin/pull/341))
- *(pumpkin-core)* If the `check-propagations` flag is enabled, the state will run inference checkers on all propagations immediately ([#340](https://github.com/ConSol-Lab/Pumpkin/pull/340))
- Extract propagators into separate crate ([#337](https://github.com/ConSol-Lab/Pumpkin/pull/337))

### Fixed

- Use saturating multiplication in integer multiplication propagator ([#350](https://github.com/ConSol-Lab/Pumpkin/pull/350))
- Explanation check for Cumulative after full explanation is created ([#349](https://github.com/ConSol-Lab/Pumpkin/pull/349))

### Other

- `InferenceCode` wraps constraint tag and inference label directory ([#339](https://github.com/ConSol-Lab/Pumpkin/pull/339))
