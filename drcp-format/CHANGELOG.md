# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0](https://github.com/ConSol-Lab/Pumpkin/compare/drcp-format-v0.2.1...drcp-format-v0.3.0) (2025-04-11)


### ⚠ BREAKING CHANGES

* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98))

### Features

* implement a tool to combine a proof with its literal mapping ([#157](https://github.com/ConSol-Lab/Pumpkin/issues/157)) ([e4cb2cc](https://github.com/ConSol-Lab/Pumpkin/commit/e4cb2ccef1aa821e7dae6e162b53fba4682aeca2))
* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98)) ([e5ae6c2](https://github.com/ConSol-Lab/Pumpkin/commit/e5ae6c25ac6d9e5407d3b1ed963c20ef25e88d18))


### Bug Fixes

* addressing clippy warnings ([#143](https://github.com/ConSol-Lab/Pumpkin/issues/143)) ([20c76bb](https://github.com/ConSol-Lab/Pumpkin/commit/20c76bb551588cff776c8c7154da8bcac4f2497c))
* **drcp-format:** An empty nogood with hints can now be parsed. ([e4cb2cc](https://github.com/ConSol-Lab/Pumpkin/commit/e4cb2ccef1aa821e7dae6e162b53fba4682aeca2))
* root level assignments are properly explained in the proof when logging inferences ([#158](https://github.com/ConSol-Lab/Pumpkin/issues/158)) ([857f84c](https://github.com/ConSol-Lab/Pumpkin/commit/857f84cbb39ab896bcbb36238fef29757d7536f3)), closes [#118](https://github.com/ConSol-Lab/Pumpkin/issues/118) [#119](https://github.com/ConSol-Lab/Pumpkin/issues/119) [#156](https://github.com/ConSol-Lab/Pumpkin/issues/156)

## [0.2.1](https://github.com/consol-lab/pumpkin/compare/drcp-format-v0.2.0...drcp-format-v0.2.1) (2025-04-11)


### Bug Fixes

* an empty nogood with hints can now be parsed. ([e4cb2cc](https://github.com/consol-lab/pumpkin/commit/e4cb2ccef1aa821e7dae6e162b53fba4682aeca2))

## [0.2.0](https://github.com/ConSol-Lab/Pumpkin/compare/drcp-format-v0.1.0...drcp-format-v0.2.0) (2024-11-05)


### ⚠ BREAKING CHANGES

* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98))

### Features

* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98)) ([e5ae6c2](https://github.com/ConSol-Lab/Pumpkin/commit/e5ae6c25ac6d9e5407d3b1ed963c20ef25e88d18))

## [0.1.0] - 2024-10-16

### Added

- Parsing/reading of DRCP files.
- Writing of DRCP files.
- Reading of literal definition files.
- Writing of literal definition files.
