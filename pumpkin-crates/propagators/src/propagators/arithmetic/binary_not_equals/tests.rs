use pumpkin_core::state::State;

use crate::StateExt;
use crate::propagators::arithmetic::BinaryNotEqualsPropagatorArgs;

#[test]
fn detects_conflict() {
    let mut state = State::default();
    let a = state.new_interval_variable(0, 0, None);
    let b = state.new_interval_variable(0, 0, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
        a,
        b,
        constraint_tag,
    });
    let _ = state
        .propagate_to_fixed_point()
        .expect_err("Expected conflict to be detected");
}

#[test]
fn propagate_when_one_is_fixed() {
    let mut state = State::default();
    let a = state.new_interval_variable(0, 0, None);
    let b = state.new_interval_variable(0, 1, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
        a,
        b,
        constraint_tag,
    });
    state
        .propagate_to_fixed_point()
        .expect("Expected no conflict to be detected");

    state.assert_bounds(b, 1, 1);
}

#[allow(deprecated, reason = "Uses TestSolver for EnqueueDecision assertions")]
#[test]
fn incremental_propagation() {
    use pumpkin_core::TestSolver;
    use pumpkin_core::propagation::EnqueueDecision;

    let mut solver = TestSolver::default();
    let a = solver.new_variable(0, 0);
    let b = solver.new_variable(0, 10);
    let constraint_tag = solver.new_constraint_tag();

    let propagator = solver
        .new_propagator(BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        })
        .expect("Expected no conflict to be detected");

    solver.assert_bounds(b, 1, 10);

    solver.new_checkpoint();

    let should_enqueue = solver.decrease_upper_bound_and_notify(propagator, 1, b, 5);
    assert_eq!(should_enqueue, EnqueueDecision::Skip);

    solver.synchronise(0);
    let should_enqueue = solver.decrease_upper_bound_and_notify(propagator, 1, b, 1);
    assert_eq!(should_enqueue, EnqueueDecision::Enqueue);
}

#[test]
fn non_overlapping_is_ok() {
    let mut state = State::default();
    let a = state.new_interval_variable(0, 5, None);
    let b = state.new_interval_variable(6, 10, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
        a,
        b,
        constraint_tag,
    });
    state
        .propagate_to_fixed_point()
        .expect("Expected no conflict to be detected");

    state.assert_bounds(a, 0, 5);
    state.assert_bounds(b, 6, 10);
}
