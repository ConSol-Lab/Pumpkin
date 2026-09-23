use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::State;
use pumpkin_core::variables::TransformableVariable;

use super::*;
use crate::StateExt;

#[test]
fn test_value_is_removed() {
    let mut state = State::default();
    let x = state.new_interval_variable(2, 2, None);
    let y = state.new_interval_variable(1, 5, None);

    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearNotEqualPropagatorArgs {
        terms: [x.scaled(1), y.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("non-empty domain");

    state.assert_bounds(x, 2, 2);
    state.assert_bounds(y, 1, 5);
    assert!(!state.contains(y, 2));
}

#[test]
fn test_empty_domain_is_detected() {
    let mut state = State::default();
    let x = state.new_interval_variable(2, 2, None);
    let y = state.new_interval_variable(2, 2, None);

    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearNotEqualPropagatorArgs {
        terms: [x.scaled(1), y.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
    });
    let err = state.propagate_to_fixed_point().expect_err("empty domain");

    let expected = conjunction!([x == 2] & [y == 2]);

    match err {
        Conflict::EmptyDomain(_) => panic!("expected an explicit conflict"),
        Conflict::Propagator(conflict) => assert_eq!(expected, conflict.conjunction),
    }
}

#[test]
fn explanation_for_propagation() {
    let mut state = State::default();
    let x = state.new_interval_variable(2, 2, None).scaled(1);
    let y = state.new_interval_variable(1, 5, None).scaled(-1);

    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearNotEqualPropagatorArgs {
        terms: [x, y].into(),
        rhs: 0,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("non-empty domain");

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![y != -2],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();

    assert_eq!(conjunction!([x == 2]), reason);
}

#[test]
fn satisfied_constraint_does_not_trigger_conflict() {
    let mut state = State::default();
    let x = state.new_interval_variable(0, 3, None);
    let y = state.new_interval_variable(0, 3, None);

    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(LinearNotEqualPropagatorArgs {
        terms: [x.scaled(1), y.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
    });

    let _ = state.post(predicate![x != 0]).unwrap();
    let _ = state.post(predicate![x != 2]).unwrap();
    let _ = state.post(predicate![x != 3]).unwrap();

    let _ = state.post(predicate![y != 0]).unwrap();
    let _ = state.post(predicate![y != 1]).unwrap();
    let _ = state.post(predicate![y != 2]).unwrap();

    state.propagate_to_fixed_point().expect("non-empty domain");
}
