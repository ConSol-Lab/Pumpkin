use std::fmt::Display;

use itertools::Itertools;

use crate::hypercube_linear::BoundPredicate;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::InconsistentHypercube;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::trail_view::TrailView;
use crate::hypercube_linear::trail_view::affine_lower_bound_at;
use crate::predicate;
use crate::predicates::Predicate;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HypercubeLinear {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
}

impl From<LinearInequality> for HypercubeLinear {
    fn from(linear: LinearInequality) -> Self {
        HypercubeLinear {
            hypercube: Hypercube::default(),
            linear,
        }
    }
}

impl Display for HypercubeLinear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.hypercube, self.linear,)
    }
}

impl HypercubeLinear {
    pub fn from_clause(
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<Self, InconsistentHypercube> {
        let hypercube = Hypercube::new(predicates)?;

        Ok(HypercubeLinear {
            hypercube,
            linear: LinearInequality::trivially_false(),
        })
    }
}

#[derive(Clone)]
pub(crate) enum HypercubeLinearExplanation {
    Proper(HypercubeLinear),
    Conjunction(Vec<Predicate>),
}

impl From<HypercubeLinear> for HypercubeLinearExplanation {
    fn from(hypercube_linear: HypercubeLinear) -> Self {
        HypercubeLinearExplanation::Proper(hypercube_linear)
    }
}

impl From<LinearInequality> for HypercubeLinearExplanation {
    fn from(linear: LinearInequality) -> Self {
        HypercubeLinearExplanation::Proper(HypercubeLinear::from(linear))
    }
}

impl<T: Into<Vec<Predicate>>> From<T> for HypercubeLinearExplanation {
    fn from(conjunction: T) -> Self {
        HypercubeLinearExplanation::Conjunction(conjunction.into())
    }
}

impl Default for HypercubeLinearExplanation {
    fn default() -> Self {
        HypercubeLinearExplanation::Conjunction(vec![])
    }
}

impl HypercubeLinearExplanation {
    pub(crate) fn into_clause<T: TrailView + ?Sized>(
        self,
        trail: &T,
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
            let term_bound = affine_lower_bound_at(trail, term, trail_position - 1);
            predicate![term >= term_bound]
        }));

        clause
    }

    pub(crate) fn weaken_to_zero(self, bound: BoundPredicate) -> Option<Self> {
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

    pub(crate) fn iter_predicates(&self) -> impl Iterator<Item = Predicate> {
        match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => {
                itertools::Either::Left(hypercube_linear.hypercube.iter_predicates())
            }
            HypercubeLinearExplanation::Conjunction(predicates) => {
                itertools::Either::Right(predicates.iter().copied())
            }
        }
    }

    pub(crate) fn take_linear(&mut self) -> LinearInequality {
        match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => {
                std::mem::take(&mut hypercube_linear.linear)
            }
            HypercubeLinearExplanation::Conjunction(_) => LinearInequality::trivially_false(),
        }
    }

    pub(crate) fn linear(&self) -> Option<&LinearInequality> {
        match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => Some(&hypercube_linear.linear),
            HypercubeLinearExplanation::Conjunction(_) => None,
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
    use crate::state::State;

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
