# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.5](https://github.com/consol-lab/pumpkin/compare/pumpkin-solver-v0.1.4...pumpkin-solver-v0.1.5) (2025-04-11)


### Features

* Add logging for branchers ([#135](https://github.com/consol-lab/pumpkin/issues/135)) ([d8814f1](https://github.com/consol-lab/pumpkin/commit/d8814f12fe7e8bd38ccdfc02bdb049ee84d7fe16))
* add method for retrieving relevant brancher events ([#138](https://github.com/consol-lab/pumpkin/issues/138)) ([8253580](https://github.com/consol-lab/pumpkin/commit/8253580ef0a5e222abb8d81fa88a5e1d2e66bf89))
* Adding lower-bounding search to the solver ([#132](https://github.com/consol-lab/pumpkin/issues/132)) ([57bde78](https://github.com/consol-lab/pumpkin/commit/57bde7879c20e02d2d59eeae627178eecd02460e))
* adding random variable selector + sparse_set fixes ([#139](https://github.com/consol-lab/pumpkin/issues/139)) ([1c4b85f](https://github.com/consol-lab/pumpkin/commit/1c4b85fe9fb802c9409df52741c0dff8c6b68d4a))
* debug clone does not synchronise at level 0 + random splitter edge cases ([#146](https://github.com/consol-lab/pumpkin/issues/146)) ([bba1abb](https://github.com/consol-lab/pumpkin/commit/bba1abbd1a24f4dd6fc9d09f387383aa2fd36eb1))
* enable proof logging in appropriate test cases ([#154](https://github.com/consol-lab/pumpkin/issues/154)) ([914f1bc](https://github.com/consol-lab/pumpkin/commit/914f1bc37cead8a6d8945b715af4551e4c11437b))
* expose the optimisation API in the python wrapper ([#148](https://github.com/consol-lab/pumpkin/issues/148)) ([ab9a463](https://github.com/consol-lab/pumpkin/commit/ab9a4632d08343ef685bf0223f400cdfe666a829))
* Flatzinc parse boolean & negative int arrays as well ([#142](https://github.com/consol-lab/pumpkin/issues/142)) ([68aef75](https://github.com/consol-lab/pumpkin/commit/68aef7575a2316d24d3652618fe630f135cb028b))
* improve element propagator ([#140](https://github.com/consol-lab/pumpkin/issues/140)) ([b9bcfd3](https://github.com/consol-lab/pumpkin/commit/b9bcfd318c44725df0b90661ac7da1e1ca8fe07c))
* lazy explanation context ([#123](https://github.com/consol-lab/pumpkin/issues/123)) ([0781357](https://github.com/consol-lab/pumpkin/commit/0781357aae2fc4753df67d469056e397ecc46dd6))
* no additional variables during constraint construction ([#131](https://github.com/consol-lab/pumpkin/issues/131)) ([19dd45b](https://github.com/consol-lab/pumpkin/commit/19dd45b97ab7cffb08f61500ff80c8e270145eab))
* remove explicit literals ([#115](https://github.com/consol-lab/pumpkin/issues/115)) ([ab74e20](https://github.com/consol-lab/pumpkin/commit/ab74e20d511856dea3469aea067902db1d6a1d1f))
* stateful integer ([#125](https://github.com/consol-lab/pumpkin/issues/125)) ([eb01ed3](https://github.com/consol-lab/pumpkin/commit/eb01ed34fd0a62a3b34060a7f9f7418390c7e26c))


### Bug Fixes

* addressing clippy warnings ([#143](https://github.com/consol-lab/pumpkin/issues/143)) ([20c76bb](https://github.com/consol-lab/pumpkin/commit/20c76bb551588cff776c8c7154da8bcac4f2497c))
* allow reification of lazy reasons ([#152](https://github.com/consol-lab/pumpkin/issues/152)) ([c7418ec](https://github.com/consol-lab/pumpkin/commit/c7418ec1669050d78bea3f79dd7770f004f22593))
* also log the inference that led to an empty domain ([#165](https://github.com/consol-lab/pumpkin/issues/165)) ([7799119](https://github.com/consol-lab/pumpkin/commit/7799119a4ab65635636dac69bec50373b2e76494))
* alternating brancher did not call the methods correctly ([#128](https://github.com/consol-lab/pumpkin/issues/128)) ([6019d86](https://github.com/consol-lab/pumpkin/commit/6019d8606478625d40e225c8dab2bf19a8160b15))
* disjunctive_scheduling example had incorrect negation constraint ([#114](https://github.com/consol-lab/pumpkin/issues/114)) ([fd007a0](https://github.com/consol-lab/pumpkin/commit/fd007a0af37d1674e42741eed97eb22e2a8ef3aa))
* don't debug propagate deleted nogoods ([#147](https://github.com/consol-lab/pumpkin/issues/147)) ([e3a6527](https://github.com/consol-lab/pumpkin/commit/e3a65279389f731dcd856325353b6d5d0167648b))
* explain root-level assignments in the proof log during conflict analysis ([#163](https://github.com/consol-lab/pumpkin/issues/163)) ([c877b06](https://github.com/consol-lab/pumpkin/commit/c877b0663fc9140ba55fd75e4dd7bc018d16bfa5))
* fixing erronous conflict check when using incrementality ([#137](https://github.com/consol-lab/pumpkin/issues/137)) ([07c253f](https://github.com/consol-lab/pumpkin/commit/07c253f568bce25cba9bc15e44656a7df1a95753))
* give reason to root-level propagation in nogood propagator ([#124](https://github.com/consol-lab/pumpkin/issues/124)) ([14868d5](https://github.com/consol-lab/pumpkin/commit/14868d5f69e159a528e881a78efdbb4e33449bca))
* identify more places where root-level facts need to be logged to the proof ([#153](https://github.com/consol-lab/pumpkin/issues/153)) ([3ac5019](https://github.com/consol-lab/pumpkin/commit/3ac50196d50314b268cf1ef2b178a64aa71a1b5f))
* issue with integer multiplication + correct path in msc file ([#117](https://github.com/consol-lab/pumpkin/issues/117)) ([ce25710](https://github.com/consol-lab/pumpkin/commit/ce25710071e58af84b6cdd0925b4099ab0d924a9))
* post process new domain ([#129](https://github.com/consol-lab/pumpkin/issues/129)) ([26a3b46](https://github.com/consol-lab/pumpkin/commit/26a3b46d45164b62ec5b58941065f956c9801c4f))
* print statistics at the end of solving process + make pumpkin-solver default bin ([#162](https://github.com/consol-lab/pumpkin/issues/162)) ([7d30ac8](https://github.com/consol-lab/pumpkin/commit/7d30ac83e55c07143f7afec20ce82ac00e014d71))
* remove paste dependency ([#159](https://github.com/consol-lab/pumpkin/issues/159)) ([ea53fed](https://github.com/consol-lab/pumpkin/commit/ea53fed1ee19fe8b902a407fea1fd2500ae4ae13))
* root level assignments are properly explained in the proof when logging inferences ([#158](https://github.com/consol-lab/pumpkin/issues/158)) ([857f84c](https://github.com/consol-lab/pumpkin/commit/857f84cbb39ab896bcbb36238fef29757d7536f3)), closes [#118](https://github.com/consol-lab/pumpkin/issues/118) [#119](https://github.com/consol-lab/pumpkin/issues/119) [#156](https://github.com/consol-lab/pumpkin/issues/156)

## [0.1.4](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.3...pumpkin-solver-v0.1.4) (2024-11-07)


### Features

* allow logging of statistics to arbitrary writers ([#73](https://github.com/ConSol-Lab/Pumpkin/issues/73)) ([136e03a](https://github.com/ConSol-Lab/Pumpkin/commit/136e03a6440f7e07e24e0e2f4e79ceb837c67a2d))
* introduce inference-nogoods in drcp-format ([#98](https://github.com/ConSol-Lab/Pumpkin/issues/98)) ([e5ae6c2](https://github.com/ConSol-Lab/Pumpkin/commit/e5ae6c25ac6d9e5407d3b1ed963c20ef25e88d18))
* update to drcp-format v0.2.0 ([#101](https://github.com/ConSol-Lab/Pumpkin/issues/101)) ([2f6df0c](https://github.com/ConSol-Lab/Pumpkin/commit/2f6df0c403bce7951c41ee0e275b9bdbef1cf9c4))

### Bug Fixes

* cumulative holes in domain incorrectly used cached profile ([#109](https://github.com/ConSol-Lab/Pumpkin/issues/109)) ([4c86a8b](https://github.com/ConSol-Lab/Pumpkin/commit/4c86a8ba5a6b291c21da62be3fc4ed0e8321eda9))
* cumulative sequence generation found less profiles than it should + fixing issues with debug propagation ([#110](https://github.com/ConSol-Lab/Pumpkin/issues/110)) ([f17222d](https://github.com/ConSol-Lab/Pumpkin/commit/f17222db5dd2e8fd01a1c55c1c08e4c557de50e6))
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
