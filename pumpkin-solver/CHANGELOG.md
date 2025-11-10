# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.2](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.2.1...pumpkin-solver-v0.2.2) (2025-11-10)


### Features

* Adding logging of brancher statistics on completion ([#257](https://github.com/ConSol-Lab/Pumpkin/issues/257)) ([9d26842](https://github.com/ConSol-Lab/Pumpkin/commit/9d268429bc29fecb4070d6680d4f83a74c7c895f))
* Allow unbounded integers ([#285](https://github.com/ConSol-Lab/Pumpkin/issues/285)) ([826fe58](https://github.com/ConSol-Lab/Pumpkin/commit/826fe58fdba2b81c47a64953b2308cff8275fc23))
* Avoid unnecessary traversals of profiles in time-tabling + Improving Explanations ([#282](https://github.com/ConSol-Lab/Pumpkin/issues/282)) ([f7a4fdb](https://github.com/ConSol-Lab/Pumpkin/commit/f7a4fdb2dae8003b0021de70f514c9df6b30acea))
* **pumpkin-solver:** Adding edge-finding for the disjunctive constraint ([#275](https://github.com/ConSol-Lab/Pumpkin/issues/275)) ([606a7d8](https://github.com/ConSol-Lab/Pumpkin/commit/606a7d8eb36edf0b2a059c44a4f50f5ca40fc572))
* **pumpkin-solver:** Write proofs with gzip encoding ([#264](https://github.com/ConSol-Lab/Pumpkin/issues/264)) ([83f76da](https://github.com/ConSol-Lab/Pumpkin/commit/83f76da17efc4a5ba1904dd144b89f0ff41cf326))
* Remove unnecessary variables introduced by bool2int ([#232](https://github.com/ConSol-Lab/Pumpkin/issues/232)) ([cf0b2fe](https://github.com/ConSol-Lab/Pumpkin/commit/cf0b2fe3831db904345e96266ae41b6836b8057f))


### Bug Fixes

* Allow bool in int search strategy ([#308](https://github.com/ConSol-Lab/Pumpkin/issues/308)) ([a3fb929](https://github.com/ConSol-Lab/Pumpkin/commit/a3fb9291285664dfefd9380638c4f4baf75c7d16))
* Correctly reporting unknown solution in optimisation and solution enumeration ([#247](https://github.com/ConSol-Lab/Pumpkin/issues/247)) ([8c0f27c](https://github.com/ConSol-Lab/Pumpkin/commit/8c0f27c7cb298329ac637294c2e0cd9f680f82f4))
* **pumpkin-solver:** Only compile solution checkers for integration tests ([#278](https://github.com/ConSol-Lab/Pumpkin/issues/278)) ([a5e4c2c](https://github.com/ConSol-Lab/Pumpkin/commit/a5e4c2c95fcce96463f3e684267bd458092d2cfc))
* **pumpkin-solver:** Pass  between pumpkin-solver and pumpkin-core ([#276](https://github.com/ConSol-Lab/Pumpkin/issues/276)) ([270c179](https://github.com/ConSol-Lab/Pumpkin/commit/270c1790908fb6b1beed6d590e94f6de7e557213))
* **pumpkin-solver:** Remove the terms with zero coefficients from linear constraints ([#269](https://github.com/ConSol-Lab/Pumpkin/issues/269)) ([576887e](https://github.com/ConSol-Lab/Pumpkin/commit/576887e10116787372d293afb618c02e3a7df03c))
* Reduce cost of predicate notification ([#259](https://github.com/ConSol-Lab/Pumpkin/issues/259)) ([d38e990](https://github.com/ConSol-Lab/Pumpkin/commit/d38e990dd42dd81f5d22200e8d6bff7ff688ad64))
* Removing leading number from statistics ([#261](https://github.com/ConSol-Lab/Pumpkin/issues/261)) ([f9b996b](https://github.com/ConSol-Lab/Pumpkin/commit/f9b996b7725d200228f31f5eb1c0a54f569cdea9))
* Support warm_start and ignore constraint_name search annotations ([#235](https://github.com/ConSol-Lab/Pumpkin/issues/235)) ([d7e8bb0](https://github.com/ConSol-Lab/Pumpkin/commit/d7e8bb028c04fc7cbf21400ef422c4c82673806e))

## [0.2.1](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.2.0...pumpkin-solver-v0.2.1) (2025-07-10)


### Features

* Branch on objective variable first if not everything is assigned after the FlatZinc search strategy ([#230](https://github.com/ConSol-Lab/Pumpkin/issues/230)) ([d676430](https://github.com/ConSol-Lab/Pumpkin/commit/d67643031e5ef7144953885608c15d94c10223e8))
* Arena allocation for the Nogood propagator ([#212](https://github.com/ConSol-Lab/Pumpkin/issues/212)) ([6f647e8](https://github.com/ConSol-Lab/Pumpkin/commit/6f647e80eae28862b78cf05d99449241ae50e25b))
* Enforce constraint ID and inference label are provided in propagators ([#179](https://github.com/ConSol-Lab/Pumpkin/issues/179)) ([74126a1](https://github.com/ConSol-Lab/Pumpkin/commit/74126a13adf08fdaca3f56bea4ee9c0d3098f5ce))
* Expose a timeout flag in the Python API ([#195](https://github.com/ConSol-Lab/Pumpkin/issues/195)) ([7d4a74f](https://github.com/ConSol-Lab/Pumpkin/commit/7d4a74fe14b7f26e53d3dacd9729780574318ac8))
* Implement *_imp FlatZinc constraints for linear arithmetic ([#219](https://github.com/ConSol-Lab/Pumpkin/issues/219)) ([c00050f](https://github.com/ConSol-Lab/Pumpkin/commit/c00050f963a1b07b1018b7808f297560c16a16aa))
* Implement the table and negative table constraints ([#194](https://github.com/ConSol-Lab/Pumpkin/issues/194)) ([49344d6](https://github.com/ConSol-Lab/Pumpkin/commit/49344d6c10d2489b8cabcc5e9abcbb241e1490ec))
* Improving notification system for nogood propagator ([#190](https://github.com/ConSol-Lab/Pumpkin/issues/190)) ([b04ac58](https://github.com/ConSol-Lab/Pumpkin/commit/b04ac587089e26c49b8e0cd299145fad50600469))
* Merge equivalence classes of variables in int_eq as a preprocessing step ([#221](https://github.com/ConSol-Lab/Pumpkin/issues/221)) ([9b5c1cc](https://github.com/ConSol-Lab/Pumpkin/commit/9b5c1ccc85a4a8f3c7551d53f37951a0aa846afa))
* Propagator for binary arithmetic constraints `int_ne` and `int_eq` ([#223](https://github.com/ConSol-Lab/Pumpkin/issues/223)) ([00a16e0](https://github.com/ConSol-Lab/Pumpkin/commit/00a16e06f934a808413ac869f5392027f1301a8a))
* Remove unused variables from the FlatZinc AST ([#222](https://github.com/ConSol-Lab/Pumpkin/issues/222)) ([223a26a](https://github.com/ConSol-Lab/Pumpkin/commit/223a26ad33b5cff250afe20d87798004e902ba54))
* Rewrite `int_lin_eq` and `int_lin_ne` over two variables into their binary equivalents ([#240](https://github.com/ConSol-Lab/Pumpkin/issues/240)) ([a370866](https://github.com/ConSol-Lab/Pumpkin/commit/a370866149683befbf8d529a54064cadeee20738))


### Bug Fixes

* Create sparse domains properly for a sparse FlatZinc domain ([#243](https://github.com/ConSol-Lab/Pumpkin/issues/243)) ([6225ff5](https://github.com/ConSol-Lab/Pumpkin/commit/6225ff5406f35960827c1b68f44180c0b6589b0d))
* Disable recursive minimisation when logging the full proof ([#215](https://github.com/ConSol-Lab/Pumpkin/issues/215)) ([7a0a08e](https://github.com/ConSol-Lab/Pumpkin/commit/7a0a08e4080221682438adf478f105d1d291b9ae))
* Don't log same domain inference multiple times per deduction ([#227](https://github.com/ConSol-Lab/Pumpkin/issues/227)) ([30a7b53](https://github.com/ConSol-Lab/Pumpkin/commit/30a7b53dd8e0a540317539dc9f2b8a804ff34e28))
* Fix lazy explanations for the element propagator ([#191](https://github.com/ConSol-Lab/Pumpkin/issues/191)) ([60e2bea](https://github.com/ConSol-Lab/Pumpkin/commit/60e2bea7ba403990b147a0b3a27546fccd27ee73))
* Log the correct proofs when optimising ([#216](https://github.com/ConSol-Lab/Pumpkin/issues/216)) ([ca6e452](https://github.com/ConSol-Lab/Pumpkin/commit/ca6e4528d341d14060868d3135fa365586ea63d2))
* Log the initial bounds in the full proof as inferences ([#211](https://github.com/ConSol-Lab/Pumpkin/issues/211)) ([7624035](https://github.com/ConSol-Lab/Pumpkin/commit/7624035b39c3b5a10b99ad3f3ae7a3a37afbdb43))
* Properly explain root-level explanations that are not considered during conflict analysis ([#176](https://github.com/ConSol-Lab/Pumpkin/issues/176)) ([d237293](https://github.com/ConSol-Lab/Pumpkin/commit/d237293fddb8970df8ad0e5b89eaf98b19d4013b))
* Update outdated documentation from before the v0.2 release ([#177](https://github.com/ConSol-Lab/Pumpkin/issues/177)) ([8e4009f](https://github.com/ConSol-Lab/Pumpkin/commit/8e4009fe45838f244e130ff4f6e4d3aa1f75ea5e))

## [0.2.0](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-solver-v0.1.4...pumpkin-solver-v0.2.0) (2025-04-11)


### Features

* Add logging for branchers ([#135](https://github.com/ConSol-Lab/Pumpkin/issues/135)) ([d8814f1](https://github.com/ConSol-Lab/Pumpkin/commit/d8814f12fe7e8bd38ccdfc02bdb049ee84d7fe16))
* Add method for retrieving relevant brancher events ([#138](https://github.com/ConSol-Lab/Pumpkin/issues/138)) ([8253580](https://github.com/ConSol-Lab/Pumpkin/commit/8253580ef0a5e222abb8d81fa88a5e1d2e66bf89))
* Adding lower-bounding search to the solver ([#132](https://github.com/ConSol-Lab/Pumpkin/issues/132)) ([57bde78](https://github.com/ConSol-Lab/Pumpkin/commit/57bde7879c20e02d2d59eeae627178eecd02460e))
* Adding random variable selector + sparse_set fixes ([#139](https://github.com/ConSol-Lab/Pumpkin/issues/139)) ([1c4b85f](https://github.com/ConSol-Lab/Pumpkin/commit/1c4b85fe9fb802c9409df52741c0dff8c6b68d4a))
* Debug clone does not synchronise at level 0 + random splitter edge cases ([#146](https://github.com/ConSol-Lab/Pumpkin/issues/146)) ([bba1abb](https://github.com/ConSol-Lab/Pumpkin/commit/bba1abbd1a24f4dd6fc9d09f387383aa2fd36eb1))
* Enable proof logging in appropriate test cases ([#154](https://github.com/ConSol-Lab/Pumpkin/issues/154)) ([914f1bc](https://github.com/ConSol-Lab/Pumpkin/commit/914f1bc37cead8a6d8945b715af4551e4c11437b))
* Expose the optimisation API in the python wrapper ([#148](https://github.com/ConSol-Lab/Pumpkin/issues/148)) ([ab9a463](https://github.com/ConSol-Lab/Pumpkin/commit/ab9a4632d08343ef685bf0223f400cdfe666a829))
* Flatzinc parse boolean & negative int arrays as well ([#142](https://github.com/ConSol-Lab/Pumpkin/issues/142)) ([68aef75](https://github.com/ConSol-Lab/Pumpkin/commit/68aef7575a2316d24d3652618fe630f135cb028b))
* Improve element propagator ([#140](https://github.com/ConSol-Lab/Pumpkin/issues/140)) ([b9bcfd3](https://github.com/ConSol-Lab/Pumpkin/commit/b9bcfd318c44725df0b90661ac7da1e1ca8fe07c))
* Lazy explanation context ([#123](https://github.com/ConSol-Lab/Pumpkin/issues/123)) ([0781357](https://github.com/ConSol-Lab/Pumpkin/commit/0781357aae2fc4753df67d469056e397ecc46dd6))
* No additional variables during constraint construction ([#131](https://github.com/ConSol-Lab/Pumpkin/issues/131)) ([19dd45b](https://github.com/ConSol-Lab/Pumpkin/commit/19dd45b97ab7cffb08f61500ff80c8e270145eab))
* Nemove explicit literals ([#115](https://github.com/ConSol-Lab/Pumpkin/issues/115)) ([ab74e20](https://github.com/ConSol-Lab/Pumpkin/commit/ab74e20d511856dea3469aea067902db1d6a1d1f))
* Stateful integer ([#125](https://github.com/ConSol-Lab/Pumpkin/issues/125)) ([eb01ed3](https://github.com/ConSol-Lab/Pumpkin/commit/eb01ed34fd0a62a3b34060a7f9f7418390c7e26c))


### Bug Fixes

* Addressing clippy warnings ([#143](https://github.com/ConSol-Lab/Pumpkin/issues/143)) ([20c76bb](https://github.com/ConSol-Lab/Pumpkin/commit/20c76bb551588cff776c8c7154da8bcac4f2497c))
* Allow reification of lazy reasons ([#152](https://github.com/ConSol-Lab/Pumpkin/issues/152)) ([c7418ec](https://github.com/ConSol-Lab/Pumpkin/commit/c7418ec1669050d78bea3f79dd7770f004f22593))
* Also log the inference that led to an empty domain ([#165](https://github.com/ConSol-Lab/Pumpkin/issues/165)) ([7799119](https://github.com/ConSol-Lab/Pumpkin/commit/7799119a4ab65635636dac69bec50373b2e76494))
* Alternating brancher did not call the methods correctly ([#128](https://github.com/ConSol-Lab/Pumpkin/issues/128)) ([6019d86](https://github.com/ConSol-Lab/Pumpkin/commit/6019d8606478625d40e225c8dab2bf19a8160b15))
* Disjunctive scheduling example had incorrect negation constraint ([#114](https://github.com/ConSol-Lab/Pumpkin/issues/114)) ([fd007a0](https://github.com/ConSol-Lab/Pumpkin/commit/fd007a0af37d1674e42741eed97eb22e2a8ef3aa))
* Don't debug propagate deleted nogoods ([#147](https://github.com/ConSol-Lab/Pumpkin/issues/147)) ([e3a6527](https://github.com/ConSol-Lab/Pumpkin/commit/e3a65279389f731dcd856325353b6d5d0167648b))
* Explain root-level assignments in the proof log during conflict analysis ([#163](https://github.com/ConSol-Lab/Pumpkin/issues/163)) ([c877b06](https://github.com/ConSol-Lab/Pumpkin/commit/c877b0663fc9140ba55fd75e4dd7bc018d16bfa5))
* Fixing erronous conflict check when using incrementality ([#137](https://github.com/ConSol-Lab/Pumpkin/issues/137)) ([07c253f](https://github.com/ConSol-Lab/Pumpkin/commit/07c253f568bce25cba9bc15e44656a7df1a95753))
* Give reason to root-level propagation in nogood propagator ([#124](https://github.com/ConSol-Lab/Pumpkin/issues/124)) ([14868d5](https://github.com/ConSol-Lab/Pumpkin/commit/14868d5f69e159a528e881a78efdbb4e33449bca))
* Identify more places where root-level facts need to be logged to the proof ([#153](https://github.com/ConSol-Lab/Pumpkin/issues/153)) ([3ac5019](https://github.com/ConSol-Lab/Pumpkin/commit/3ac50196d50314b268cf1ef2b178a64aa71a1b5f))
* Issue with integer multiplication + correct path in msc file ([#117](https://github.com/ConSol-Lab/Pumpkin/issues/117)) ([ce25710](https://github.com/ConSol-Lab/Pumpkin/commit/ce25710071e58af84b6cdd0925b4099ab0d924a9))
* Post process new domain ([#129](https://github.com/ConSol-Lab/Pumpkin/issues/129)) ([26a3b46](https://github.com/ConSol-Lab/Pumpkin/commit/26a3b46d45164b62ec5b58941065f956c9801c4f))
* Print statistics at the end of solving process + make pumpkin-solver default bin ([#162](https://github.com/ConSol-Lab/Pumpkin/issues/162)) ([7d30ac8](https://github.com/ConSol-Lab/Pumpkin/commit/7d30ac83e55c07143f7afec20ce82ac00e014d71))
* Remove paste dependency ([#159](https://github.com/ConSol-Lab/Pumpkin/issues/159)) ([ea53fed](https://github.com/ConSol-Lab/Pumpkin/commit/ea53fed1ee19fe8b902a407fea1fd2500ae4ae13))
* Root level assignments are properly explained in the proof when logging inferences ([#158](https://github.com/ConSol-Lab/Pumpkin/issues/158)) ([857f84c](https://github.com/ConSol-Lab/Pumpkin/commit/857f84cbb39ab896bcbb36238fef29757d7536f3)), closes [#118](https://github.com/ConSol-Lab/Pumpkin/issues/118) [#119](https://github.com/ConSol-Lab/Pumpkin/issues/119) [#156](https://github.com/ConSol-Lab/Pumpkin/issues/156)

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
