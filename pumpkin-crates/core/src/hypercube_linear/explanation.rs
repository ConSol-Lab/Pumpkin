use std::fmt::Display;

use itertools::Itertools;

use crate::{
    hypercube_linear::{BoundPredicate, Hypercube, LinearInequality},
    predicate,
    predicates::Predicate,
    state::State,
    variables::IntegerVariable,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct HypercubeLinear {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
}

impl Display for HypercubeLinear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.hypercube, self.linear,)
    }
}

#[derive(Clone)]
pub(super) enum HypercubeLinearExplanation {
    Proper(HypercubeLinear),
    Conjunction(Vec<Predicate>),
}

impl Default for HypercubeLinearExplanation {
    fn default() -> Self {
        HypercubeLinearExplanation::Conjunction(vec![])
    }
}

impl HypercubeLinearExplanation {
    pub(super) fn into_clause(
        self,
        state: &State,
        pivot: Predicate,
        trail_position: usize,
    ) -> Vec<Predicate> {
        let hypercube_linear = match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => hypercube_linear,
            HypercubeLinearExplanation::Conjunction(predicates) => return predicates,
        };

        let mut clause = vec![!pivot];

        clause.extend(
            hypercube_linear
                .hypercube
                .iter_predicates()
                // Any predicate in the hypercube that is implied by !pivot should be
                // excluded. This is due to the propagation rule that allows a weaker
                // hypercube predicate to be propagated by the linear.
                .filter(|&p| !(!pivot).implies(p)),
        );

        clause.extend(hypercube_linear.linear.terms().map(|term| {
            let term_bound = term.lower_bound_at_trail_position(&state.assignments, trail_position);

            predicate![term >= term_bound]
        }));

        clause
    }

    pub(super) fn weaken_to_zero(self, bound: BoundPredicate) -> Option<Self> {
        match self {
            HypercubeLinearExplanation::Proper(mut hypercube_linear) => {
                hypercube_linear.linear =
                    std::mem::take(&mut hypercube_linear.linear).weaken_to_zero(bound)?;

                hypercube_linear.hypercube = std::mem::take(&mut hypercube_linear.hypercube)
                    .with_predicate(bound.into())
                    .expect("should never construct inconsistent hypercube");

                Some(HypercubeLinearExplanation::Proper(hypercube_linear))
            }

            HypercubeLinearExplanation::Conjunction(predicates) => {
                Some(HypercubeLinearExplanation::Conjunction(predicates))
            }
        }
    }

    pub(super) fn iter_predicates(&self) -> impl Iterator<Item = Predicate> {
        match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => {
                itertools::Either::Left(hypercube_linear.hypercube.iter_predicates())
            }
            HypercubeLinearExplanation::Conjunction(predicates) => {
                itertools::Either::Right(predicates.iter().copied())
            }
        }
    }
}

impl Display for HypercubeLinearExplanation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => {
                write!(f, "{hypercube_linear}")
            }
            HypercubeLinearExplanation::Conjunction(predicates) => {
                write!(f, "{} ->  <= -1", predicates.iter().format(" & "))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;

    #[test]
    fn weaken_to_zero_conjunction_is_passthrough() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));

        let predicates = vec![predicate![x >= 3], predicate![y >= 2]];
        let explanation = HypercubeLinearExplanation::Conjunction(predicates.clone());

        let bound = BoundPredicate::new(predicate![x >= 3]).expect("bound predicate");
        let result = explanation
            .weaken_to_zero(bound)
            .expect("not trivially satisfiable");

        let HypercubeLinearExplanation::Conjunction(result_predicates) = result else {
            panic!("expected Conjunction variant");
        };

        assert_eq!(result_predicates, predicates);
    }

    #[test]
    fn weaken_to_zero_proper_removes_term_and_tightens_hypercube() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));

        // x + y <= 5
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            5,
        )
        .expect("not trivially satisfiable");

        let explanation = HypercubeLinearExplanation::Proper(HypercubeLinear {
            hypercube: Hypercube::new([]).expect("not inconsistent"),
            linear,
        });

        // Weaken on x >= 3: removes x from linear (bound becomes 5 - 1*3 = 2).
        let bound = BoundPredicate::new(predicate![x >= 3]).expect("bound predicate");
        let result = explanation
            .weaken_to_zero(bound)
            .expect("not trivially satisfiable");

        let HypercubeLinearExplanation::Proper(result_hl) = result else {
            panic!("expected Proper variant");
        };

        // x should be removed; only y with bound 2 remains.
        let expected_linear =
            LinearInequality::new([(NonZero::new(1).unwrap(), y)], 2).expect("not trivially sat");
        assert_eq!(result_hl.linear, expected_linear);

        // The bound predicate x >= 3 should have been added to the hypercube.
        assert!(
            result_hl
                .hypercube
                .iter_predicates()
                .any(|p| p == predicate![x >= 3])
        );
    }

    #[test]
    fn weaken_to_zero_proper_with_domain_absent_from_linear_leaves_linear_unchanged() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z = state.new_interval_variable(0, 10, Some("z".into()));

        // y + z <= 5 — x is NOT in the linear
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
            5,
        )
        .expect("not trivially satisfiable");

        let explanation = HypercubeLinearExplanation::Proper(HypercubeLinear {
            hypercube: Hypercube::new([]).expect("not inconsistent"),
            linear: linear.clone(),
        });

        // Weaken on x >= 3: x is not in the linear, so the linear should be unchanged.
        let bound = BoundPredicate::new(predicate![x >= 3]).expect("bound predicate");
        let result = explanation
            .weaken_to_zero(bound)
            .expect("not trivially satisfiable");

        let HypercubeLinearExplanation::Proper(result_hl) = result else {
            panic!("expected Proper variant");
        };

        assert_eq!(result_hl.linear, linear);
    }
}
