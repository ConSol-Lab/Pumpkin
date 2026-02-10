# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.1](https://github.com/consol-lab/pumpkin/compare/drcp-format-v0.3.0...drcp-format-v0.3.1) - 2026-02-10

### Other

- Update rust edition to 2024 ([#311](https://github.com/consol-lab/pumpkin/pull/311))
- *(drcp-format)* Update README
- *(drcp-format)* Improve parser speed ([#293](https://github.com/consol-lab/pumpkin/pull/293))

## [0.3.0](https://github.com/ConSol-Lab/Pumpkin/compare/drcp-format-v0.2.1...drcp-format-v0.3.0) (2025-07-10)


### Features

* Update to the new format, which inlines atomic constraint definitions into the proof steps ([#204](https://github.com/ConSol-Lab/Pumpkin/issues/204)) ([5cdc747](https://github.com/ConSol-Lab/Pumpkin/commit/5cdc747d8f241859dfea806297595f7feb12fa7e))
* Do mapping from codes to atomic constraints inside the crate ([#204](https://github.com/ConSol-Lab/Pumpkin/issues/204)) ([5cdc747](https://github.com/ConSol-Lab/Pumpkin/commit/5cdc747d8f241859dfea806297595f7feb12fa7e))

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
