use pumpkin_core::state::State;

use crate::StateExt;
use crate::propagators::arithmetic::BinaryEqualsPropagatorArgs;

#[test]
fn test_propagation_of_bounds() {
    let mut state = State::default();
    let a = state.new_interval_variable(0, 5, None);
    let b = state.new_interval_variable(3, 7, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(BinaryEqualsPropagatorArgs {
        a,
        b,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no conflict");

    state.assert_bounds(a, 3, 5);
    state.assert_bounds(b, 3, 5);
}

#[test]
fn test_propagation_of_holes() {
    let mut state = State::default();
    let a = state.new_sparse_variable(vec![2, 4, 6, 9], None);
    let b = state.new_sparse_variable(vec![3, 4, 7, 9], None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(BinaryEqualsPropagatorArgs {
        a,
        b,
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no conflict");

    state.assert_bounds(a, 4, 9);
    state.assert_bounds(b, 4, 9);

    for i in 5..=8 {
        assert!(!state.contains(a, i));
        assert!(!state.contains(b, i));
    }
}

#[allow(deprecated, reason = "Uses TestSolver for EnqueueDecision assertions")]
#[test]
fn test_propagation_of_holes_incremental() {
    use pumpkin_core::TestSolver;
    use pumpkin_core::propagation::EnqueueDecision;

    let mut solver = TestSolver::default();
    let a = solver.new_variable(2, 9);
    let b = solver.new_variable(3, 9);
    let constraint_tag = solver.new_constraint_tag();

    let propagator = solver
        .new_propagator(BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        })
        .expect("Expected result to be okay");

    solver.assert_bounds(a, 3, 9);
    solver.assert_bounds(b, 3, 9);

    let should_enqueue = solver.remove_and_notify(propagator, a, 5);
    assert_eq!(should_enqueue, EnqueueDecision::Enqueue);

    let should_enqueue = solver.remove_and_notify(propagator, a, 6);
    assert_eq!(should_enqueue, EnqueueDecision::Enqueue);

    let should_enqueue = solver.remove_and_notify(propagator, b, 4);
    assert_eq!(should_enqueue, EnqueueDecision::Enqueue);

    let result = solver.propagate(propagator);
    assert!(result.is_ok());

    assert!(!solver.contains(b, 5));
    assert!(!solver.contains(b, 6));
    assert!(!solver.contains(a, 4));
}

#[test]
fn test_conflict() {
    let mut state = State::default();
    let a = state.new_interval_variable(0, 5, None);
    let b = state.new_interval_variable(6, 9, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(BinaryEqualsPropagatorArgs {
        a,
        b,
        constraint_tag,
    });
    let _ = state
        .propagate_to_fixed_point()
        .expect_err("expected conflict");
}
