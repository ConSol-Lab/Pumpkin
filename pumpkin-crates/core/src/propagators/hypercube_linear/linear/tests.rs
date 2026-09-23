use super::*;
use crate::containers::HashSet;
use crate::state::State;

#[test]
fn terms_are_iterable() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));
    let y = state.new_interval_variable(1, 10, Some("y".into()));

    let linear = LinearInequality::new(
        [(NonZero::new(2).unwrap(), x), (NonZero::new(3).unwrap(), y)],
        8,
    )
    .expect("not trivially true");

    let iterated_terms = linear.terms().collect::<HashSet<_>>();

    assert_eq!(
        [x.scaled(2), y.scaled(3)]
            .into_iter()
            .collect::<HashSet<_>>(),
        iterated_terms
    );
}

#[test]
fn terms_for_same_variable_are_merged() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));

    let linear = LinearInequality::new(
        [(NonZero::new(2).unwrap(), x), (NonZero::new(3).unwrap(), x)],
        8,
    )
    .expect("not trivially true");

    let iterated_terms = linear.terms().collect::<HashSet<_>>();

    assert_eq!(
        [x.scaled(5)].into_iter().collect::<HashSet<_>>(),
        iterated_terms
    );
}

#[test]
fn trivially_satisfied_linear_inequalities_are_not_created() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));

    let linear = LinearInequality::new(
        [
            (NonZero::new(2).unwrap(), x),
            (NonZero::new(-2).unwrap(), x),
        ],
        8,
    );

    assert!(linear.is_none());
}

#[test]
fn trivially_unsatisfied_linear_are_okay() {
    let mut state = State::default();

    let x = state.new_interval_variable(1, 10, Some("x".into()));

    let linear = LinearInequality::new(
        [
            (NonZero::new(2).unwrap(), x),
            (NonZero::new(-2).unwrap(), x),
        ],
        -1,
    )
    .expect("not trivially satisfiable");

    assert_eq!(
        Vec::<AffineView<DomainId>>::new(),
        linear.terms().collect::<Vec<_>>()
    );
}
