use std::fmt::Display;

use itertools::Itertools;

use crate::{
    hypercube_linear::{BoundPredicate, Hypercube, LinearInequality},
    predicate,
    predicates::Predicate,
    state::State,
    variables::IntegerVariable,
};

#[derive(Clone)]
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
    pub(super) fn into_clause(self, state: &State, trail_position: usize) -> Vec<Predicate> {
        let mut hypercube_linear = match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => hypercube_linear,
            HypercubeLinearExplanation::Conjunction(predicates) => return predicates,
        };

        let mut clause = hypercube_linear
            .hypercube
            .iter_predicates()
            .collect::<Vec<_>>();

        while !hypercube_linear.linear.is_trivially_false() {
            let next_term = hypercube_linear
                .linear
                .terms()
                .next()
                .expect("conversion to clause only possible when linear is conflicting");

            let term_bound =
                next_term.lower_bound_at_trail_position(&state.assignments, trail_position);

            let predicate_to_weaken_on = BoundPredicate::new(predicate![next_term >= term_bound])
                .expect("the predicate is a bound predicate");

            hypercube_linear.linear = std::mem::take(&mut hypercube_linear.linear)
                .weaken_to_zero(predicate_to_weaken_on)
                .expect("conversion to clause only possible when linear is conflicting");

            clause.push(predicate_to_weaken_on.into());
        }

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
    fn into_clause_conjunction_returns_predicates_directly() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));

        let predicates = vec![predicate![x >= 3], predicate![y >= 2], predicate![x <= 7]];
        let explanation = HypercubeLinearExplanation::Conjunction(predicates.clone());

        let clause = explanation.into_clause(&state, state.trail_len());

        assert_eq!(clause, predicates);
    }

    #[test]
    fn into_clause_proper_trivially_false_linear_returns_only_hypercube_predicates() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 10, Some("a".into()));
        let b = state.new_interval_variable(0, 10, Some("b".into()));

        let hypercube =
            Hypercube::new([predicate![a >= 1], predicate![b <= 5]]).expect("not inconsistent");
        let linear = LinearInequality::trivially_false();

        let explanation = HypercubeLinearExplanation::Proper(HypercubeLinear { hypercube, linear });

        let clause = explanation.into_clause(&state, state.trail_len());

        assert_eq!(clause.len(), 2);
        assert!(clause.contains(&predicate![a >= 1]));
        assert!(clause.contains(&predicate![b <= 5]));
    }

    #[test]
    fn into_clause_proper_converts_conflicting_linear_terms_to_bound_predicates() {
        // Tests that a Proper explanation with a conflicting linear is converted to a clause
        // consisting of the hypercube predicates plus the lower-bound predicate for each term.
        let mut state = State::default();
        let a = state.new_interval_variable(0, 10, Some("a".into()));
        // y has initial lb=2, z has initial lb=3 so that y + z <= 4 (slack=-1) is conflicting.
        let y = state.new_interval_variable(2, 10, Some("y".into()));
        let z = state.new_interval_variable(3, 5, Some("z".into()));

        // y + z <= 4 with lb(y)=2, lb(z)=3: slack = 4 - 2 - 3 = -1 (conflicting)
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
            4,
        )
        .expect("not trivially satisfiable");

        let hypercube = Hypercube::new([predicate![a >= 1]]).expect("not inconsistent");

        let explanation = HypercubeLinearExplanation::Proper(HypercubeLinear { hypercube, linear });

        // trail_len() gives a position past all trail entries so lower_bound_at_trail_position
        // returns the current (initial) lower bounds.
        let clause = explanation.into_clause(&state, state.trail_len());

        // Clause = [a >= 1] (hypercube) + [y >= 2, z >= 3] (from weakening the linear)
        assert_eq!(clause.len(), 3);
        assert!(clause.contains(&predicate![a >= 1]));
        assert!(clause.contains(&predicate![y >= 2]));
        assert!(clause.contains(&predicate![z >= 3]));
    }

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
