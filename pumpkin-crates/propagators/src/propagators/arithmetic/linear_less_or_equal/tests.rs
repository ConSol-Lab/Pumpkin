use pumpkin_checking::VariableState;
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::state::State;

use super::LinearLessOrEqualDescription;
use super::LinearLessOrEqualPropagatorArgs;
use crate::StateExt;

#[test]
fn test_bounds_are_propagated() {
    let mut state = State::default();
    let x = state.new_interval_variable(1, 5, None);
    let y = state.new_interval_variable(0, 10, None);

    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
        constraint_description: LinearLessOrEqualDescription {
            terms: [x, y].into(),
            bound: 7,
        },
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(x, 1, 5);
    state.assert_bounds(y, 0, 6);
}

#[test]
fn test_explanations() {
    let mut state = State::default();
    let x = state.new_interval_variable(1, 5, None);
    let y = state.new_interval_variable(0, 10, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
        constraint_description: LinearLessOrEqualDescription {
            terms: [x, y].into(),
            bound: 7,
        },
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![y <= 6],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();

    assert_eq!(conjunction!([x >= 1]), reason);
}

#[test]
fn overflow_leads_to_conflict() {
    let mut state = State::default();

    let x = state.new_interval_variable(i32::MAX, i32::MAX, None);
    let y = state.new_interval_variable(1, 1, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
        constraint_description: LinearLessOrEqualDescription {
            terms: [x, y].into(),
            bound: i32::MAX,
        },
        constraint_tag,
    });
    let _ = state
        .propagate_to_fixed_point()
        .expect_err("Expected overflow to be detected");
}

#[test]
fn underflow_leads_to_no_propagation() {
    let mut state = State::default();

    let x = state.new_interval_variable(i32::MIN, i32::MIN, None);
    let y = state.new_interval_variable(-1, -1, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
        constraint_description: LinearLessOrEqualDescription {
            terms: [x, y].into(),
            bound: i32::MIN,
        },
        constraint_tag,
    });
    state
        .propagate_to_fixed_point()
        .expect("Expected no error to be detected");
}

#[test]
fn a_linear_inequality_is_decided_by_the_bounds() {
    let mut state = State::default();
    let x = state.new_interval_variable(-100, 100, None);
    let y = state.new_interval_variable(-100, 100, None);
    let domains = VariableState::prepare_for_conflict_check(
        [
            predicate![x >= 0],
            predicate![x <= 3],
            predicate![y >= 1],
            predicate![y <= 2],
        ],
        None,
    )
    .expect("the predicates are consistent");

    let check = |bound| {
        LinearLessOrEqualDescription {
            terms: Box::from([x, y]),
            bound,
        }
        .check_solution(&domains)
    };

    assert_eq!(check(5), SolutionCheck::ConstraintSatisfied);
    assert_eq!(check(0), SolutionCheck::ConstraintViolated);
    assert_eq!(check(3), SolutionCheck::Unknown);
}
