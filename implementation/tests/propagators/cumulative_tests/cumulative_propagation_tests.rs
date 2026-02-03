#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]

use crate::Propagator;
use crate::propagators::ProofTestRunner;
use crate::propagators::RCPSP_INSTANCES;

#[test]
fn cumulative_propagation_test_0() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[0], Propagator::Cumulative)
        .check_propagations_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_propagation_test_1() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[1], Propagator::Cumulative)
        .check_propagations_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_propagation_test_2() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[2], Propagator::Cumulative)
        .check_propagations_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_propagation_test_3() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[3], Propagator::Cumulative)
        .check_propagations_only();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}
