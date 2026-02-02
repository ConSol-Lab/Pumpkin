#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]

use crate::Propagator;
use crate::propagators::LINEAR_INSTANCES;
use crate::propagators::ProofTestRunner;

#[test]
fn linear_conflict_test_0() {
    let runner =
        ProofTestRunner::new_runner(LINEAR_INSTANCES[0], Propagator::Linear).check_conflicts_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn linear_conflict_test_1() {
    let runner =
        ProofTestRunner::new_runner(LINEAR_INSTANCES[1], Propagator::Linear).check_conflicts_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn linear_conflict_test_2() {
    let runner =
        ProofTestRunner::new_runner(LINEAR_INSTANCES[2], Propagator::Linear).check_conflicts_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn linear_conflict_test_3() {
    let runner =
        ProofTestRunner::new_runner(LINEAR_INSTANCES[3], Propagator::Linear).check_conflicts_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}
