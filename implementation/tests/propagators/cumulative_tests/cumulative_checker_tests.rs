#![cfg(test)]

use crate::Propagator;
use crate::propagators::ProofTestRunner;
use crate::propagators::RCPSP_INSTANCES;

#[test]
fn cumulative_checker_test_0() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[0], Propagator::Cumulative);
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_1() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[1], Propagator::Cumulative);
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_2() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[2], Propagator::Cumulative);
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_3() {
    let runner = ProofTestRunner::new_runner(RCPSP_INSTANCES[3], Propagator::Cumulative);
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_0_invalid() {
    let runner =
        ProofTestRunner::new_runner(RCPSP_INSTANCES[0], Propagator::Cumulative).invalid_checks();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_1_invalid() {
    let runner =
        ProofTestRunner::new_runner(RCPSP_INSTANCES[1], Propagator::Cumulative).invalid_checks();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_2_invalid() {
    let runner =
        ProofTestRunner::new_runner(RCPSP_INSTANCES[2], Propagator::Cumulative).invalid_checks();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn cumulative_checker_test_3_invalid() {
    let runner =
        ProofTestRunner::new_runner(RCPSP_INSTANCES[3], Propagator::Cumulative).invalid_checks();
    let result = runner.run();

    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}
