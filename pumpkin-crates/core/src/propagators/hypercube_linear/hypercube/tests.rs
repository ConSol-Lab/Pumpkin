use super::*;
use crate::containers::HashSet;
use crate::predicate;
use crate::state::State;

#[test]
fn consistent_hypercube_can_be_created() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));
    let y = state.new_interval_variable(1, 10, Some("y".into()));

    let maybe_hypercube = Hypercube::new([predicate![x >= 2], predicate![y >= 2]]);

    assert!(maybe_hypercube.is_ok());
}

#[test]
fn inconsistent_hypercube_can_be_created() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));
    let y = state.new_interval_variable(1, 10, Some("y".into()));

    let error = Hypercube::new([predicate![x >= 2], predicate![y >= 2], predicate![x <= 1]])
        .expect_err("hypercube is inconsistent");

    assert_eq!(InconsistentHypercube(x), error);
}

#[test]
fn hypercube_iters_predicates_from_constructor() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));
    let y = state.new_interval_variable(1, 10, Some("y".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

    assert_eq!(
        [predicate![x >= 2], predicate![y >= 2]]
            .into_iter()
            .collect::<HashSet<_>>(),
        hypercube.iter_predicates().collect::<HashSet<_>>(),
    );
}

#[test]
fn iterating_predicates_ignores_subsumed_predicates() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));

    let hypercube =
        Hypercube::new([predicate![x >= 2], predicate![x >= 4]]).expect("not inconsistent");

    assert_eq!(
        [predicate![x >= 4]].into_iter().collect::<HashSet<_>>(),
        hypercube.iter_predicates().collect::<HashSet<_>>(),
    );
}
