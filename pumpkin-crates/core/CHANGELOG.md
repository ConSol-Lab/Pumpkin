# Changelog

## 0.1.0 (2025-07-10)


### Features

* Arena allocation for the Nogood propagator ([#212](https://github.com/ConSol-Lab/Pumpkin/issues/212)) ([6f647e8](https://github.com/ConSol-Lab/Pumpkin/commit/6f647e80eae28862b78cf05d99449241ae50e25b))
* Rewrite `int_lin_eq` and `int_lin_ne` into their binary equivalents ([#240](https://github.com/ConSol-Lab/Pumpkin/issues/240)) ([a370866](https://github.com/ConSol-Lab/Pumpkin/commit/a370866149683befbf8d529a54064cadeee20738))


### Bug Fixes

* Correct order of re-used domain inferences in deduction step ([#241](https://github.com/ConSol-Lab/Pumpkin/issues/241)) ([5935846](https://github.com/ConSol-Lab/Pumpkin/commit/59358466bff2ea1014cd58da4f54de19f36d40bd))
* Performing lazy synchronisation for autonomous search + not evaluating predicate upon synchronisation ([#224](https://github.com/ConSol-Lab/Pumpkin/issues/224)) ([a1742ea](https://github.com/ConSol-Lab/Pumpkin/commit/a1742eae5d5206a4406fb80efcb5d9d8bb8562b3))
* Remove unnecessary notifier actions ([#233](https://github.com/ConSol-Lab/Pumpkin/issues/233)) ([3414292](https://github.com/ConSol-Lab/Pumpkin/commit/3414292228ba3f7467d2d17edc689f5c1d4fba9f))
* Reporting conflict if there is a task which uses more resource than capacity ([#236](https://github.com/ConSol-Lab/Pumpkin/issues/236)) ([18b6a21](https://github.com/ConSol-Lab/Pumpkin/commit/18b6a21223a18fc4de5ed628a9cd1154560099e7))
* Use synchronise instead of notify backtrack for binary equals ([#237](https://github.com/ConSol-Lab/Pumpkin/issues/237)) ([e808d1c](https://github.com/ConSol-Lab/Pumpkin/commit/e808d1cd8579c86bf9aa8caf41be117f8cee8da4))
