# Changelog

## [0.2.2](https://github.com/ConSol-Lab/Pumpkin/compare/pumpkin-core-v0.2.1...pumpkin-core-v0.2.2) (2025-10-17)


### Features

* Adding logging of brancher statistics on completion ([#257](https://github.com/ConSol-Lab/Pumpkin/issues/257)) ([9d26842](https://github.com/ConSol-Lab/Pumpkin/commit/9d268429bc29fecb4070d6680d4f83a74c7c895f))
* Allow unbounded integers ([#285](https://github.com/ConSol-Lab/Pumpkin/issues/285)) ([826fe58](https://github.com/ConSol-Lab/Pumpkin/commit/826fe58fdba2b81c47a64953b2308cff8275fc23))
* Avoid unnecessary traversals of profiles in time-tabling + Improving Explanations ([#282](https://github.com/ConSol-Lab/Pumpkin/issues/282)) ([f7a4fdb](https://github.com/ConSol-Lab/Pumpkin/commit/f7a4fdb2dae8003b0021de70f514c9df6b30acea))
* **pumpkin-solver:** Adding edge-finding for the disjunctive constraint ([#275](https://github.com/ConSol-Lab/Pumpkin/issues/275)) ([606a7d8](https://github.com/ConSol-Lab/Pumpkin/commit/606a7d8eb36edf0b2a059c44a4f50f5ca40fc572))
* **pumpkin-solver:** Don't use binary equals when logging the full proof ([#252](https://github.com/ConSol-Lab/Pumpkin/issues/252)) ([4454d93](https://github.com/ConSol-Lab/Pumpkin/commit/4454d93a9e70f5c6d5b442a8402775957a0184f4))
* **pumpkin-solver:** Lazy explanation for linear less than or equals ([#290](https://github.com/ConSol-Lab/Pumpkin/issues/290)) ([3515c7a](https://github.com/ConSol-Lab/Pumpkin/commit/3515c7af018a2d57230f5a9f333a90cd1d6775d3))
* **pumpkin-solver:** Write proofs with gzip encoding ([#264](https://github.com/ConSol-Lab/Pumpkin/issues/264)) ([83f76da](https://github.com/ConSol-Lab/Pumpkin/commit/83f76da17efc4a5ba1904dd144b89f0ff41cf326))
* Remove unnecessary variables introduced by bool2int ([#232](https://github.com/ConSol-Lab/Pumpkin/issues/232)) ([cf0b2fe](https://github.com/ConSol-Lab/Pumpkin/commit/cf0b2fe3831db904345e96266ae41b6836b8057f))
* Use lazy explanations for BinaryEq ([#231](https://github.com/ConSol-Lab/Pumpkin/issues/231)) ([4a89a5e](https://github.com/ConSol-Lab/Pumpkin/commit/4a89a5e2090f63726e6bacfd5c86ca656a318413))


### Bug Fixes

* Add pumpkin-crates to default-members ([#286](https://github.com/ConSol-Lab/Pumpkin/issues/286)) ([bf7ff80](https://github.com/ConSol-Lab/Pumpkin/commit/bf7ff80ca231c7bc4e25dcc6028b0c6c9a4798d3))
* Broken links in README ([#307](https://github.com/ConSol-Lab/Pumpkin/issues/307)) ([13b1587](https://github.com/ConSol-Lab/Pumpkin/commit/13b15873ded00004132bbf8bb905b78f12e4589b))
* Correctly reporting unknown solution in optimisation and solution enumeration ([#247](https://github.com/ConSol-Lab/Pumpkin/issues/247)) ([8c0f27c](https://github.com/ConSol-Lab/Pumpkin/commit/8c0f27c7cb298329ac637294c2e0cd9f680f82f4))
* Create correct reason for preprocessing permanent nogood + Removing unassigned status for PredicateId ([#250](https://github.com/ConSol-Lab/Pumpkin/issues/250)) ([98ba136](https://github.com/ConSol-Lab/Pumpkin/commit/98ba1360844ceb23d3b3bea4d22a615cd30ac8ea))
* **pumpkin-solver:** Remove debug printing ([#254](https://github.com/ConSol-Lab/Pumpkin/issues/254)) ([0ca93a2](https://github.com/ConSol-Lab/Pumpkin/commit/0ca93a2f89637600002a63a06d29a46aa454c2db))
* **pumpkin-solver:** Remove the terms with zero coefficients from linear constraints ([#269](https://github.com/ConSol-Lab/Pumpkin/issues/269)) ([576887e](https://github.com/ConSol-Lab/Pumpkin/commit/576887e10116787372d293afb618c02e3a7df03c))
* **pumpkin-solver:** Take root-propagation into account in the LUS optimiser ([#253](https://github.com/ConSol-Lab/Pumpkin/issues/253)) ([d30ed06](https://github.com/ConSol-Lab/Pumpkin/commit/d30ed060d7efd6abc8bca980919ddaa676704f84))
* Reduce cost of predicate notification ([#259](https://github.com/ConSol-Lab/Pumpkin/issues/259)) ([d38e990](https://github.com/ConSol-Lab/Pumpkin/commit/d38e990dd42dd81f5d22200e8d6bff7ff688ad64))
* Support warm_start and ignore constraint_name search annotations ([#235](https://github.com/ConSol-Lab/Pumpkin/issues/235)) ([d7e8bb0](https://github.com/ConSol-Lab/Pumpkin/commit/d7e8bb028c04fc7cbf21400ef422c4c82673806e))

## Changelog

See changelog for `pumpkin-solver`.
