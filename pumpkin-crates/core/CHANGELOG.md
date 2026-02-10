# Changelog

## [0.2.3](https://github.com/consol-lab/pumpkin/compare/pumpkin-core-v0.2.2...pumpkin-core-v0.2.3) - 2026-02-10

### Added

- *(pumpkin-core)* Propagator for the hypercube linear constraint ([#347](https://github.com/consol-lab/pumpkin/pull/347))
- Extract conflict resolvers and nogood minimisation into a separate crate ([#341](https://github.com/consol-lab/pumpkin/pull/341))
- *(pumpkin-core)* If the `check-propagations` flag is enabled, the state will run inference checkers on all propagations immediately ([#340](https://github.com/consol-lab/pumpkin/pull/340))
- *(pumpkin-solver-py)* Support warm starting for optimisation models ([#344](https://github.com/consol-lab/pumpkin/pull/344))
- *(pumpkin-solver-py)* Add incrementality to python interface ([#315](https://github.com/consol-lab/pumpkin/pull/315))
- Extract propagators into separate crate ([#337](https://github.com/consol-lab/pumpkin/pull/337))
- *(pumpkin-core)* Crash if a propagator constructor does not register anything ([#338](https://github.com/consol-lab/pumpkin/pull/338))
- *(pumpkin-core)* Make propagator API public ([#333](https://github.com/consol-lab/pumpkin/pull/333))
- Add interface for nogood minimiser ([#326](https://github.com/consol-lab/pumpkin/pull/326))
- Implement State API ([#319](https://github.com/consol-lab/pumpkin/pull/319))
- *(pumpkin-core)* WebAssembly support for pumpkin-core ([#327](https://github.com/consol-lab/pumpkin/pull/327))

### Fixed

- Fix off-by-one error in initial bounds calculation ([#361](https://github.com/consol-lab/pumpkin/pull/361))
- *(pumpkin-solver-py)* Forward brancher events in PythonBrancher ([#358](https://github.com/consol-lab/pumpkin/pull/358))
- *(pumpkin-core)* Do not check for event registration when already panicking ([#346](https://github.com/consol-lab/pumpkin/pull/346))
- Sign error in greater_than constraint ([#334](https://github.com/consol-lab/pumpkin/pull/334))
- Generate constraint tags via State ([#335](https://github.com/consol-lab/pumpkin/pull/335))
- *(pumpkin-core)* Allow propagators to register for predicates becoming true ([#331](https://github.com/consol-lab/pumpkin/pull/331))
- Off-by-one error when explaining empty domain conflict ([#322](https://github.com/consol-lab/pumpkin/pull/322))
- *(pumpkin-core)* Don't use binary equality when logging proof ([#320](https://github.com/consol-lab/pumpkin/pull/320))
- *(pumpkin-core)* Declare solving after conflict resolution ([#317](https://github.com/consol-lab/pumpkin/pull/317))

### Other

- Calculate minimal explanation time-table propagation ([#356](https://github.com/consol-lab/pumpkin/pull/356))
- *(pumpkin-checker)* Add test cases for verifiers ([#352](https://github.com/consol-lab/pumpkin/pull/352))
- Stop watching predicates without any watchers in nogood propagator ([#351](https://github.com/consol-lab/pumpkin/pull/351))
- Simplify predicate notification system ([#348](https://github.com/consol-lab/pumpkin/pull/348))
- `InferenceCode` wraps constraint tag and inference label directory ([#339](https://github.com/consol-lab/pumpkin/pull/339))
- *(pumpkin-core)* Explicitly register predicates in NogoodPropagator ([#332](https://github.com/consol-lab/pumpkin/pull/332))
- *(pumpkin-solver)* Run proof checker on integration tests ([#329](https://github.com/consol-lab/pumpkin/pull/329))
- *(pumpkin-core)* Use clippy to detect disallowed types ([#325](https://github.com/consol-lab/pumpkin/pull/325))
- *(pumpkin-core)* Empty domain conflict is a predicate and reason ([#314](https://github.com/consol-lab/pumpkin/pull/314))
- *(pumpkin-core)* Separate conflict resolvers further from solver ([#310](https://github.com/consol-lab/pumpkin/pull/310))
- Update rust edition to 2024 ([#311](https://github.com/consol-lab/pumpkin/pull/311))

## [0.2.2](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-core-v0.2.1...pumpkin-core-v0.2.2) (2025-11-10)


### Features

* Adding logging of brancher statistics on completion ([#257](https://github.com/ConSol-Lab/Pumpkin/issues/257)) ([9d26842](https://github.com/ConSol-Lab/Pumpkin/commit/9d268429bc29fecb4070d6680d4f83a74c7c895f))
* Post equality as single predicate instead of splitting in two ([#305](https://github.com/ConSol-Lab/Pumpkin/issues/305)) ([8f43208](https://github.com/ConSol-Lab/Pumpkin/commit/8f43208e447add20808a8472ccb57cdf051acab6))
* Adding edge-finding for the disjunctive constraint ([#275](https://github.com/ConSol-Lab/Pumpkin/issues/275)) ([606a7d8](https://github.com/ConSol-Lab/Pumpkin/commit/606a7d8eb36edf0b2a059c44a4f50f5ca40fc572))
* Lazy explanation for linear less than or equals ([#290](https://github.com/ConSol-Lab/Pumpkin/issues/290)) ([3515c7a](https://github.com/ConSol-Lab/Pumpkin/commit/3515c7af018a2d57230f5a9f333a90cd1d6775d3))
* Write proofs with gzip encoding ([#264](https://github.com/ConSol-Lab/Pumpkin/issues/264)) ([83f76da](https://github.com/ConSol-Lab/Pumpkin/commit/83f76da17efc4a5ba1904dd144b89f0ff41cf326))
* Remove unnecessary variables introduced by bool2int ([#232](https://github.com/ConSol-Lab/Pumpkin/issues/232)) ([cf0b2fe](https://github.com/ConSol-Lab/Pumpkin/commit/cf0b2fe3831db904345e96266ae41b6836b8057f))
* Use lazy explanations for BinaryEq ([#231](https://github.com/ConSol-Lab/Pumpkin/issues/231)) ([4a89a5e](https://github.com/ConSol-Lab/Pumpkin/commit/4a89a5e2090f63726e6bacfd5c86ca656a318413))
* Support warm_start and ignore constraint_name search annotations ([#235](https://github.com/ConSol-Lab/Pumpkin/issues/235)) ([d7e8bb0](https://github.com/ConSol-Lab/Pumpkin/commit/d7e8bb028c04fc7cbf21400ef422c4c82673806e))


### Bug Fixes

* Add pumpkin-crates to default-members ([#286](https://github.com/ConSol-Lab/Pumpkin/issues/286)) ([bf7ff80](https://github.com/ConSol-Lab/Pumpkin/commit/bf7ff80ca231c7bc4e25dcc6028b0c6c9a4798d3))
* Correctly reporting unknown solution in optimisation and solution enumeration ([#247](https://github.com/ConSol-Lab/Pumpkin/issues/247)) ([8c0f27c](https://github.com/ConSol-Lab/Pumpkin/commit/8c0f27c7cb298329ac637294c2e0cd9f680f82f4))
* Create correct reason for preprocessing permanent nogood + Removing unassigned status for PredicateId ([#250](https://github.com/ConSol-Lab/Pumpkin/issues/250)) ([98ba136](https://github.com/ConSol-Lab/Pumpkin/commit/98ba1360844ceb23d3b3bea4d22a615cd30ac8ea))
* Provide `SelectionContext` instead of `Assignments` to `Brancher::synchronise` ([#312](https://github.com/ConSol-Lab/Pumpkin/issues/312)) ([9548114](https://github.com/ConSol-Lab/Pumpkin/commit/9548114df84ed3ddd2214270de9e2ab459c1621e))
* Remove debug printing ([#254](https://github.com/ConSol-Lab/Pumpkin/issues/254)) ([0ca93a2](https://github.com/ConSol-Lab/Pumpkin/commit/0ca93a2f89637600002a63a06d29a46aa454c2db))
* Remove the terms with zero coefficients from linear constraints ([#269](https://github.com/ConSol-Lab/Pumpkin/issues/269)) ([576887e](https://github.com/ConSol-Lab/Pumpkin/commit/576887e10116787372d293afb618c02e3a7df03c))
* Take root-propagation into account in the LUS optimiser ([#253](https://github.com/ConSol-Lab/Pumpkin/issues/253)) ([d30ed06](https://github.com/ConSol-Lab/Pumpkin/commit/d30ed060d7efd6abc8bca980919ddaa676704f84))
* Reduce cost of predicate notification ([#259](https://github.com/ConSol-Lab/Pumpkin/issues/259)) ([d38e990](https://github.com/ConSol-Lab/Pumpkin/commit/d38e990dd42dd81f5d22200e8d6bff7ff688ad64))

### Refactors

* Avoid unnecessary traversals of profiles in time-tabling + Improving Explanations ([#282](https://github.com/ConSol-Lab/Pumpkin/issues/282)) ([f7a4fdb](https://github.com/ConSol-Lab/Pumpkin/commit/f7a4fdb2dae8003b0021de70f514c9df6b30acea))
* Don't use binary equals when logging the full proof ([#252](https://github.com/ConSol-Lab/Pumpkin/issues/252)) ([4454d93](https://github.com/ConSol-Lab/Pumpkin/commit/4454d93a9e70f5c6d5b442a8402775957a0184f4))
