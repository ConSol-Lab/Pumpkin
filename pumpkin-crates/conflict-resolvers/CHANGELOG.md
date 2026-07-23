# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [0.5.0](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-conflict-resolvers-v0.4.0...pumpkin-conflict-resolvers-v0.5.0) - 2026-07-23

### Added

- *(pumpkin-solver,pumpkin-core)* Implement extended nogood propagation and CPIP nogood learning ([#454](https://github.com/ConSol-Lab/Pumpkin/pull/454))

## [0.4.0](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-conflict-resolvers-v0.3.0...pumpkin-conflict-resolvers-v0.4.0) - 2026-06-23

### Added

- *(pumpkin-solver)* Check derived nogoods during search ([#373](https://github.com/ConSol-Lab/Pumpkin/pull/373))

### Fixed

- *(pumpkin-core)* Creation and Insertion in Sparse Set ([#395](https://github.com/ConSol-Lab/Pumpkin/pull/395))

### Other

- Use central version number for all pumpkin-* crates ([#470](https://github.com/ConSol-Lab/Pumpkin/pull/470))

## [0.3.0](https://github.com/consol-lab/pumpkin/releases/tag/pumpkin-conflict-resolvers-v0.3.0) - 2026-02-10

### Added

- Extract conflict resolvers and nogood minimisation into a separate crate ([#341](https://github.com/consol-lab/pumpkin/pull/341))
