use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

use super::LexLessOrEqualArgs;
use super::LexLessOrEqualDescription;
use crate::StateExt;

fn post(state: &mut State, x: Vec<DomainId>, y: Vec<DomainId>) {
    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(LexLessOrEqualArgs {
        constraint_description: LexLessOrEqualDescription {
            x: x.into(),
            y: y.into(),
        },
        constraint_tag,
    });
}

#[test]
fn the_first_position_is_bounded_by_the_other_side() {
    let mut state = State::default();
    let x0 = state.new_interval_variable(2, 5, None);
    let x1 = state.new_interval_variable(0, 9, None);
    let y0 = state.new_interval_variable(0, 3, None);
    let y1 = state.new_interval_variable(0, 9, None);
    post(&mut state, vec![x0, x1], vec![y0, y1]);
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(x0, 2, 3);
    state.assert_bounds(y0, 2, 3);
    state.assert_bounds(x1, 0, 9);
}

#[test]
fn an_infeasible_tail_makes_the_first_position_strict() {
    let mut state = State::default();
    let x0 = state.new_interval_variable(1, 3, None);
    let x1 = state.new_interval_variable(4, 5, None);
    let y0 = state.new_interval_variable(1, 3, None);
    let y1 = state.new_interval_variable(0, 3, None);
    post(&mut state, vec![x0, x1], vec![y0, y1]);
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(x0, 1, 2);
    state.assert_bounds(y0, 2, 3);
}

#[test]
fn a_fixed_equal_prefix_moves_the_propagation_on() {
    let mut state = State::default();
    let x0 = state.new_interval_variable(2, 2, None);
    let y0 = state.new_interval_variable(2, 2, None);
    let x1 = state.new_interval_variable(3, 5, None);
    let y1 = state.new_interval_variable(0, 4, None);
    post(&mut state, vec![x0, x1], vec![y0, y1]);
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(x1, 3, 4);
    state.assert_bounds(y1, 3, 4);
}

#[test]
fn forcing_the_first_position_equal_continues_with_the_next() {
    // x0 in 3..4 and y0 = 3 force x0 = 3; then x1 <= y1 applies.
    let mut state = State::default();
    let x0 = state.new_interval_variable(3, 4, None);
    let y0 = state.new_interval_variable(3, 3, None);
    let x1 = state.new_interval_variable(5, 9, None);
    let y1 = state.new_interval_variable(0, 6, None);
    post(&mut state, vec![x0, x1], vec![y0, y1]);
    state.propagate_to_fixed_point().expect("no empty domain");

    state.assert_bounds(x0, 3, 3);
    state.assert_bounds(x1, 5, 6);
    state.assert_bounds(y1, 5, 6);
}

#[test]
fn a_greater_fixed_vector_is_a_conflict() {
    let mut state = State::default();
    let x0 = state.new_interval_variable(3, 3, None);
    let y0 = state.new_interval_variable(2, 2, None);
    post(&mut state, vec![x0], vec![y0]);
    assert!(state.propagate_to_fixed_point().is_err());
}

#[test]
fn the_explanation_of_a_strict_bound_cites_the_deciding_tail() {
    let mut state = State::default();
    let x0 = state.new_interval_variable(1, 3, None);
    let x1 = state.new_interval_variable(4, 5, None);
    let y0 = state.new_interval_variable(1, 3, None);
    let y1 = state.new_interval_variable(0, 3, None);
    post(&mut state, vec![x0, x1], vec![y0, y1]);
    state.propagate_to_fixed_point().expect("no empty domain");

    let mut reason: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(predicate![x0 <= 2], &mut reason, CurrentNogood::empty());
    let reason: PropositionalConjunction = reason.into();
    assert_eq!(conjunction!([y0 <= 3] & [x1 >= 4] & [y1 <= 3]), reason);
}

#[test]
fn backtracking_restores_the_first_undecided_position() {
    let mut state = State::default();
    let x0 = state.new_interval_variable(0, 2, None);
    let y0 = state.new_interval_variable(0, 2, None);
    let x1 = state.new_interval_variable(3, 5, None);
    let y1 = state.new_interval_variable(0, 4, None);
    post(&mut state, vec![x0, x1], vec![y0, y1]);
    state.propagate_to_fixed_point().expect("no empty domain");

    state.new_checkpoint();
    let _ = state.post(predicate![x0 == 1]).expect("1 is in the domain");
    let _ = state.post(predicate![y0 == 1]).expect("1 is in the domain");
    state.propagate_to_fixed_point().expect("no empty domain");
    state.assert_bounds(x1, 3, 4);
    state.assert_bounds(y1, 3, 4);

    let _ = state.restore_to(0);
    state.propagate_to_fixed_point().expect("no empty domain");
    state.assert_bounds(x1, 3, 5);
    state.assert_bounds(y1, 0, 4);
}
