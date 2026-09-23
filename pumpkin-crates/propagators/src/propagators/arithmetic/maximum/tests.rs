use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::state::State;

use super::MaximumArgs;
use crate::StateExt;

#[test]
fn upper_bound_of_rhs_matches_maximum_upper_bound_of_array_at_initialise() {
    let mut state = State::default();

    let a = state.new_interval_variable(1, 3, None);
    let b = state.new_interval_variable(1, 4, None);
    let c = state.new_interval_variable(1, 5, None);

    let rhs = state.new_interval_variable(1, 10, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(MaximumArgs {
        array: [a, b, c].into(),
        rhs,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(rhs, 1, 5);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![rhs <= 5],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([a <= 5] & [b <= 5] & [c <= 5]), reason);
}

#[test]
fn lower_bound_of_rhs_is_maximum_of_lower_bounds_in_array() {
    let mut state = State::default();

    let a = state.new_interval_variable(3, 10, None);
    let b = state.new_interval_variable(4, 10, None);
    let c = state.new_interval_variable(5, 10, None);

    let rhs = state.new_interval_variable(1, 10, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(MaximumArgs {
        array: [a, b, c].into(),
        rhs,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(rhs, 5, 10);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![rhs >= 5],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([c >= 5]), reason);
}

#[test]
fn upper_bound_of_all_array_elements_at_most_rhs_max_at_initialise() {
    let mut state = State::default();

    let array = (1..=5)
        .map(|idx| state.new_interval_variable(1, 4 + idx, None))
        .collect::<Box<_>>();

    let rhs = state.new_interval_variable(1, 3, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(MaximumArgs {
        array: array.clone(),
        rhs,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domain");

    for var in array.iter() {
        state.assert_bounds(*var, 1, 3);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![var <= 3],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason: PropositionalConjunction = reason_buffer.into();
        assert_eq!(conjunction!([rhs <= 3]), reason);
    }
}

#[test]
fn single_variable_propagate() {
    let mut state = State::default();

    let array = (1..=5)
        .map(|idx| state.new_interval_variable(1, 1 + 10 * idx, None))
        .collect::<Box<_>>();

    let rhs = state.new_interval_variable(45, 60, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(MaximumArgs {
        array: array.clone(),
        rhs,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(*array.last().unwrap(), 45, 51);
    state.assert_bounds(rhs, 45, 51);
}
