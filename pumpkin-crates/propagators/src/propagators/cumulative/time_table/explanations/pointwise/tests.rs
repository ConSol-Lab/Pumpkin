use pumpkin_core::predicate;
use pumpkin_core::predicates::PropositionalConjunction;

use crate::cumulative::time_table::CumulativeExplanationType;
use crate::propagators::cumulative::time_table::propagation_handler::test_propagation_handler::TestPropagationHandler;

#[test]
fn test_pointwise_explanation_lower_bound() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
    let (reason_last_propagation, x, y) = propagation_handler.set_up_example_lower_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x >= 13),
        predicate!(y >= 15),
        predicate!(y <= 18),
    ]
    .into();
    assert_eq!(reason_last_propagation, expected_reason);

    let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x >= 17));
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x >= 11),
        predicate!(y >= 13),
        predicate!(y <= 16),
    ]
    .into();
    assert_eq!(reason_first_propagation, expected_reason);
}

#[test]
fn test_pointwise_explanation_lower_bound_sequence() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
    let (reason_last_propagation, x, y, z) =
        propagation_handler.set_up_example_sequence_lower_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x >= 16),
        predicate!(z >= 15),
        predicate!(z <= 21),
    ]
    .into();
    assert_eq!(reason_last_propagation, expected_reason);

    let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x >= 17));
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x >= 11),
        predicate!(y >= 13),
        predicate!(y <= 16),
    ]
    .into();
    assert_eq!(reason_first_propagation, expected_reason);
}

#[test]
fn test_pointwise_explanation_upper_bound() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
    let (reason_last_propagation, x, y) = propagation_handler.set_up_example_upper_bound();
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x <= 16),
        predicate!(y >= 13),
        predicate!(y <= 16),
    ]
    .into();
    assert_eq!(reason_last_propagation, expected_reason);
}

#[test]
fn test_pointwise_explanation_upper_bound_sequence() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
    let (reason_last_propagation, x, y, z) =
        propagation_handler.set_up_example_sequence_upper_bound();
    let expected_reason: PropositionalConjunction =
        vec![predicate!(x <= 9), predicate!(z >= 4), predicate!(z <= 9)].into();
    assert_eq!(reason_last_propagation, expected_reason);

    let reason_middle_propagation = propagation_handler.get_reason_for(predicate!(x <= 4));
    let expected_reason: PropositionalConjunction =
        vec![predicate!(x <= 10), predicate!(z >= 5), predicate!(z <= 10)].into();
    assert_eq!(reason_middle_propagation, expected_reason);

    let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x <= 10));
    let expected_reason: PropositionalConjunction = vec![
        predicate!(x <= 16),
        predicate!(y >= 13),
        predicate!(y <= 16),
    ]
    .into();
    assert_eq!(reason_first_propagation, expected_reason);
}

#[test]
fn test_conflict_point_wise() {
    let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
    let (reason, y) = propagation_handler.set_up_conflict_example();
    let expected_reason: PropositionalConjunction =
        vec![predicate!(y >= 13), predicate!(y <= 16)].into();
    assert_eq!(reason, expected_reason);
}
