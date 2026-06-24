# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [0.4.0](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-propagators-v0.3.0...pumpkin-propagators-v0.4.0) - 2026-06-23

### Fixed

- *(pumpkin-core)* Creation and Insertion in Sparse Set ([#395](https://github.com/ConSol-Lab/Pumpkin/pull/395))
- *(pumpkin-solver)* Update packages and fix duplicate dependencies ([#412](https://github.com/ConSol-Lab/Pumpkin/pull/412))
- Time-table checker not detecting conflicts properly ([#405](https://github.com/ConSol-Lab/Pumpkin/pull/405))

### Other

- Use central version number for all pumpkin-* crates ([#470](https://github.com/ConSol-Lab/Pumpkin/pull/470))
- *(deps)* bump enumset from 1.1.12 to 1.1.13 ([#452](https://github.com/ConSol-Lab/Pumpkin/pull/452))
- *(deps)* bump enumset from 1.1.11 to 1.1.12 ([#446](https://github.com/ConSol-Lab/Pumpkin/pull/446))
- *(deps)* bump enumset from 1.1.10 to 1.1.11 ([#441](https://github.com/ConSol-Lab/Pumpkin/pull/441))
- *(pumpkin-solver)* Remove vergen as a dependency ([#438](https://github.com/ConSol-Lab/Pumpkin/pull/438))
- *(pumpkin-core)* Attach `InferenceCode` instead of `Predicate` ([#433](https://github.com/ConSol-Lab/Pumpkin/pull/433))
- *(deps)* bump convert_case from 0.8.0 to 0.11.0 ([#428](https://github.com/ConSol-Lab/Pumpkin/pull/428))
- *(deps)* bump bitfield-struct from 0.9.5 to 0.13.0 ([#421](https://github.com/ConSol-Lab/Pumpkin/pull/421))
- *(pumpkin-propagators)* Move away from TestSolver in propagator tests ([#387](https://github.com/ConSol-Lab/Pumpkin/pull/387))
- *(pumpkin-core)* Cleanup creation of propagator conflict ([#399](https://github.com/ConSol-Lab/Pumpkin/pull/399))
- add utility method fixed value and replace is_fixed wherever possible ([#393](https://github.com/ConSol-Lab/Pumpkin/pull/393))

## [0.3.0](https://github.com/consol-lab/pumpkin/releases/tag/pumpkin-propagators-v0.3.0) - 2026-02-10

### Added

- Calculate minimal profile for explaining time-table conflict ([#353](https://github.com/consol-lab/pumpkin/pull/353))
- Calculate minimal explanation time-table propagation ([#356](https://github.com/consol-lab/pumpkin/pull/356))
- Extract propagators into separate crate ([#337](https://github.com/consol-lab/pumpkin/pull/337))

### Fixed

- Use saturating multiplication in integer multiplication propagator ([#350](https://github.com/consol-lab/pumpkin/pull/350))
- Explanation check for Cumulative after full explanation is created ([#349](https://github.com/consol-lab/pumpkin/pull/349))
- Calculate minimal explanation time-table propagation ([#356](https://github.com/consol-lab/pumpkin/pull/356))
