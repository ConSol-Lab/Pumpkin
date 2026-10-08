use std::num::NonZero;

use super::*;
use crate::predicate;
use crate::propagators::hypercube_linear::Hypercube;
use crate::propagators::hypercube_linear::LinearInequality;
use crate::state::State;

#[test]
fn conflict_detected() {
    let mut state = State::default();

    let x = state.new_interval_variable(2, 10, Some("x".into()));
    let y = state.new_interval_variable(2, 10, Some("y".into()));
    let z = state.new_interval_variable(2, 5, Some("z".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");
    // x + y + z <= 5.
    let linear = LinearInequality::new(
        [
            (NonZero::new(1).unwrap(), x),
            (NonZero::new(1).unwrap(), y),
            (NonZero::new(1).unwrap(), z),
        ],
        5,
    )
    .expect("not trivially true");

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_err());
}

#[test]
fn incremental_hypercube_evaluation() {
    let mut state = State::default();

    let x = state.new_interval_variable(0, 10, Some("x".into()));
    let y = state.new_interval_variable(0, 10, Some("y".into()));
    let z = state.new_interval_variable(0, 5, Some("z".into()));

    let hypercube = Hypercube::new([predicate![x >= 2], predicate![y >= 2], predicate![z <= 3]])
        .expect("not inconsistent");

    let linear = LinearInequality::trivially_false();

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_ok());

    let _ = state.post(predicate![x >= 2]).expect("domain not empty");
    assert!(state.propagate_to_fixed_point().is_ok());
    let _ = state.post(predicate![y >= 2]).expect("domain not empty");
    let _ = state.post(predicate![z <= 3]).expect("domain not empty");
    assert!(state.propagate_to_fixed_point().is_err());
}

#[test]
fn empty_hypercube_simplifies_to_linear_conflict() {
    let mut state = State::default();

    let x = state.new_interval_variable(2, 10, Some("x".into()));
    let y = state.new_interval_variable(2, 10, Some("y".into()));
    let z = state.new_interval_variable(2, 5, Some("z".into()));

    let hypercube = Hypercube::new([]).expect("not inconsistent");
    // x + y + z <= 5.
    let linear = LinearInequality::new(
        [
            (NonZero::new(1).unwrap(), x),
            (NonZero::new(1).unwrap(), y),
            (NonZero::new(1).unwrap(), z),
        ],
        5,
    )
    .expect("not trivially true");

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_err());
}

#[test]
fn conflicting_linear_propagates_last_unassigned_hypercube_bound_to_false() {
    let mut state = State::default();

    let x = state.new_interval_variable(0, 10, Some("x".into()));
    let y = state.new_interval_variable(2, 5, Some("y".into()));
    let z = state.new_interval_variable(2, 10, Some("z".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

    // y + z <= 3.
    let linear = LinearInequality::new(
        [(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
        3,
    )
    .expect("not trivially true");

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_ok());

    assert_eq!(1, state.upper_bound(x));
}

#[test]
fn propagate_weaker_than_unassigned_predicate_in_hypercube() {
    let mut state = State::default();

    let x = state.new_interval_variable(0, 10, Some("x".into()));
    let y = state.new_interval_variable(2, 5, Some("y".into()));
    let z = state.new_interval_variable(0, 10, Some("z".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

    // x + y + z <= 5.
    let linear = LinearInequality::new(
        [
            (NonZero::new(1).unwrap(), x),
            (NonZero::new(1).unwrap(), y),
            (NonZero::new(1).unwrap(), z),
        ],
        5,
    )
    .expect("not trivially true");

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_ok());

    assert_eq!(3, state.upper_bound(x));
}

#[test]
fn linear_component_propagates_if_hypercube_is_satisfied() {
    let mut state = State::default();

    let x = state.new_interval_variable(0, 10, Some("x".into()));
    let y = state.new_interval_variable(0, 10, Some("y".into()));
    let z1 = state.new_interval_variable(0, 10, Some("z1".into()));
    let z2 = state.new_interval_variable(0, 10, Some("z2".into()));
    let z3 = state.new_interval_variable(0, 10, Some("z3".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

    // z1 + z2 + z3 <= 10.
    let linear = LinearInequality::new(
        [
            (NonZero::new(1).unwrap(), z1),
            (NonZero::new(1).unwrap(), z2),
            (NonZero::new(1).unwrap(), z3),
        ],
        10,
    )
    .expect("not trivially true");

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_ok());

    assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());

    assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());

    assert!(state.post(predicate![z1 >= 2]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());
    assert_eq!(state.upper_bound(z2), 8);
    assert_eq!(state.upper_bound(z3), 8);
}

#[test]
fn backtracking_does_not_break_the_propagator() {
    let mut state = State::default();

    let x = state.new_interval_variable(0, 10, Some("x".into()));
    let y = state.new_interval_variable(0, 10, Some("y".into()));
    let z1 = state.new_interval_variable(0, 10, Some("z1".into()));
    let z2 = state.new_interval_variable(0, 10, Some("z2".into()));
    let z3 = state.new_interval_variable(0, 10, Some("z3".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

    // z1 + z2 + z3 <= 10.
    let linear = LinearInequality::new(
        [
            (NonZero::new(1).unwrap(), z1),
            (NonZero::new(1).unwrap(), z2),
            (NonZero::new(1).unwrap(), z3),
        ],
        10,
    )
    .expect("not trivially true");

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(HypercubeLinearConstructor {
        hypercube,
        linear,
        constraint_tag,
    });

    assert!(state.propagate_to_fixed_point().is_ok());

    state.new_checkpoint();

    assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());
    assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());
    assert!(state.post(predicate![z1 >= 2]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());

    let _ = state.restore_to(0);

    assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
    assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
    assert!(state.post(predicate![z1 >= 4]).expect("not empty domain"));
    assert!(state.propagate_to_fixed_point().is_ok());

    assert_eq!(state.upper_bound(z2), 6);
    assert_eq!(state.upper_bound(z3), 6);
}
