#![cfg(test)]

use crate::Propagator;
use crate::propagators::LINEAR_INSTANCES;
use crate::run_instance;

#[test]
fn linear_checker_test_0() {
    let result = run_instance(LINEAR_INSTANCES[0], Propagator::Linear);
    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn linear_checker_test_1() {
    let result = run_instance(LINEAR_INSTANCES[1], Propagator::Linear);
    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn linear_checker_test_2() {
    let result = run_instance(LINEAR_INSTANCES[2], Propagator::Linear);
    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}

#[test]
fn linear_checker_test_3() {
    let result = run_instance(LINEAR_INSTANCES[3], Propagator::Linear);
    if let Err(e) = result {
        panic!("Failed to check inference: {e:#?}");
    }
}
