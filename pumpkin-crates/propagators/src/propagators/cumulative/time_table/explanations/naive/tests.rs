use pumpkin_core::predicate;
use pumpkin_core::predicates::PropositionalConjunction;

use crate::cumulative::time_table::CumulativeExplanationType;
use crate::propagators::cumulative::time_table::propagation_handler::test_propagation_handler::TestPropagationHandler;

#[test]
fn test_naive_explanation_lower_bound() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
    let (reason, x, y) = propagation_handler.set_up_example_lower_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x >= 11),
        predicate!(y >= 15),
        predicate!(y <= 16),
    ]
    .into();
    assert_eq!(reason, expected_reason);
}

#[test]
fn test_naive_explanation_lower_bound_sequence() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
    let (reason, x, y, z) = propagation_handler.set_up_example_sequence_lower_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x >= 11),
        predicate!(y >= 15),
        predicate!(y <= 16),
        predicate!(z >= 15),
        predicate!(z <= 19),
    ]
    .into();
    assert_eq!(reason, expected_reason);
}

#[test]
fn test_naive_explanation_upper_bound() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
    let (reason, x, y) = propagation_handler.set_up_example_upper_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x <= 16),
        predicate!(y >= 15),
        predicate!(y <= 16),
    ]
    .into();
    assert_eq!(reason, expected_reason);
}

#[test]
fn test_naive_explanation_upper_bound_sequence() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
    let (reason, x, y, z) = propagation_handler.set_up_example_sequence_upper_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x <= 16),
        predicate!(y >= 15),
        predicate!(y <= 16),
        predicate!(z >= 7),
        predicate!(z <= 9),
    ]
    .into();
    assert_eq!(reason, expected_reason);
}

#[test]
fn test_conflict_naive() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
    let (reason, y) = propagation_handler.set_up_conflict_example();
    let expected_reason: PropositionalConjunction =
        vec![predicate!(y >= 15), predicate!(y <= 16)].into();
    assert_eq!(reason, expected_reason);
}
