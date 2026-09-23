use pumpkin_core::state::State;

use super::*;
use crate::StateExt;

#[test]
fn absolute_bounds_are_propagated_at_initialise() {
    let mut state = State::default();

    let signed = state.new_interval_variable(-3, 4, None);
    let absolute = state.new_interval_variable(-2, 10, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(absolute, 0, 4);
}

#[test]
fn signed_bounds_are_propagated_at_initialise() {
    let mut state = State::default();

    let signed = state.new_interval_variable(-5, 5, None);
    let absolute = state.new_interval_variable(0, 3, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(signed, -3, 3);
}

#[test]
fn absolute_lower_bound_can_be_strictly_positive() {
    let mut state = State::default();

    let signed = state.new_interval_variable(3, 6, None);
    let absolute = state.new_interval_variable(0, 10, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(absolute, 3, 6);
}

#[test]
fn strictly_negative_signed_value_can_propagate_lower_bound_on_absolute() {
    let mut state = State::default();

    let signed = state.new_interval_variable(-5, -3, None);
    let absolute = state.new_interval_variable(1, 5, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(absolute, 3, 5);
}

#[test]
fn lower_bound_on_absolute_can_propagate_negative_upper_bound_on_signed() {
    let mut state = State::default();

    let signed = state.new_interval_variable(-5, 0, None);
    let absolute = state.new_interval_variable(1, 5, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(signed, -5, -1);
}

#[test]
fn lower_bound_on_absolute_can_propagate_positive_lower_bound_on_signed() {
    let mut state = State::default();

    let signed = state.new_interval_variable(1, 5, None);
    let absolute = state.new_interval_variable(3, 5, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(signed, 3, 5);
}
