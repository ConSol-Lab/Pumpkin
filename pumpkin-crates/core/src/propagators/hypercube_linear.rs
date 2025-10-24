use std::num::NonZero;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::declare_inference_label;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::DomainEvents;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::TransformableVariable;

declare_inference_label!(HypercubeLinearInference);

#[derive(Clone, Debug)]
pub struct HypercubeLinear {
    hypercube: Vec<Predicate>,
    linear_terms: Box<[AffineView<DomainId>]>,
    linear_rhs: i32,
}

impl HypercubeLinear {
    pub fn new(
        mut hypercube: Vec<Predicate>,
        mut linear_terms: Vec<(NonZero<i32>, DomainId)>,
        linear_rhs: i32,
    ) -> Self {
        // Remove duplicates from the hypercube.
        hypercube.sort();
        hypercube.dedup();

        // Merge domain IDs with multiple weights.
        linear_terms.sort_by_key(|(_, domain)| *domain);
        let mut linear_terms =
            linear_terms
                .into_iter()
                .fold(vec![], |mut acc, (weight, domain)| {
                    // Get the last term in the linear component.
                    let Some(last_term) = acc.last_mut() else {
                        acc.push(domain.scaled(weight.get()));
                        return acc;
                    };

                    // If it is the same domain as the current domain, then add the weights
                    // together.
                    if domain == last_term.inner {
                        last_term.scale += weight.get();
                        return acc;
                    }

                    // Otherwise, add the current term to the accumulator.
                    acc.push(domain.scaled(weight.get()));
                    acc
                });

        linear_terms.retain(|view| view.scale != 0);

        Self {
            hypercube,
            linear_terms: linear_terms.into(),
            linear_rhs,
        }
    }

    /// Get an iterator over the hypercube of this constraint.
    pub fn iter_hypercube(&self) -> impl ExactSizeIterator<Item = Predicate> + '_ {
        self.hypercube.iter().copied()
    }

    /// Get an iterator over the terms in the linear component of the constraint. Each [`DomainId`]
    /// is guaranteed to be present only once.
    pub fn iter_linear_terms(
        &self,
    ) -> impl ExactSizeIterator<Item = (NonZero<i32>, DomainId)> + '_ {
        self.linear_terms
            .iter()
            .copied()
            .map(|view| (view.scale.try_into().unwrap(), view.inner))
    }

    pub fn linear_rhs(&self) -> i32 {
        self.linear_rhs
    }

    /// Get the weight for the given domain in the linear component of the hypercube linear.
    ///
    /// If the domain is not in the linear component, this returns `None`.
    pub(crate) fn variable_weight(&self, domain: DomainId) -> Option<NonZero<i32>> {
        self.linear_terms.iter().find_map(|view| {
            if view.inner == domain {
                Some(view.scale.try_into().expect("scales should be non-zero"))
            } else {
                None
            }
        })
    }
}

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinearPropagatorArgs {
    pub(crate) hypercube_linear: HypercubeLinear,
    pub(crate) constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for HypercubeLinearPropagatorArgs {
    type PropagatorImpl = HypercubeLinearPropagator;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let HypercubeLinearPropagatorArgs {
            hypercube_linear,
            constraint_tag,
        } = self;

        for predicate in hypercube_linear.hypercube.iter().copied() {
            context.register_predicate(predicate);
        }

        for (i, x_i) in hypercube_linear.linear_terms.iter().enumerate() {
            context.register(*x_i, DomainEvents::LOWER_BOUND, LocalId::from(i as u32));
        }

        HypercubeLinearPropagator {
            hypercube_linear,
            inference_code: context.create_inference_code(constraint_tag, HypercubeLinearInference),
        }
    }
}

/// Propagator for the constraint `/\ hypercube -> \sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinearPropagator {
    pub(crate) hypercube_linear: HypercubeLinear,
    inference_code: InferenceCode,
}

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        // If not all bounds of the hypercube are satisfied, don't propagate.
        if !self
            .hypercube_linear
            .hypercube
            .iter()
            .all(|&predicate| context.evaluate_predicate(predicate) == Some(true))
        {
            return Ok(());
        }

        let lower_bound_left_hand_side = self
            .hypercube_linear
            .linear_terms
            .iter()
            .map(|term| context.lower_bound(term))
            .sum::<i32>();

        for (i, term) in self.hypercube_linear.linear_terms.iter().enumerate() {
            let bound = self.hypercube_linear.linear_rhs
                - (lower_bound_left_hand_side - context.lower_bound(term));

            let reason: PropositionalConjunction = self
                .hypercube_linear
                .hypercube
                .iter()
                .copied()
                .chain(
                    self.hypercube_linear
                        .linear_terms
                        .iter()
                        .enumerate()
                        .filter_map(|(j, x_j)| {
                            if j != i {
                                Some(predicate![x_j >= context.lower_bound(x_j)])
                            } else {
                                None
                            }
                        }),
                )
                .collect();

            context.post(predicate![term <= bound], reason, self.inference_code)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_solver::TestSolver;

    #[test]
    fn test_bounds_are_not_propagated_if_hypercube_not_satisfied() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear {
                    hypercube: conjunction!([x >= 2]).into(),
                    linear_terms: [x.into(), y.into()].into(),
                    linear_rhs: 7,
                },
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 10);
    }

    #[test]
    fn test_bounds_are_propagated_if_hypercube_satisfied() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear {
                    hypercube: conjunction!([x >= 2]).into(),
                    linear_terms: [x.into(), y.into()].into(),
                    linear_rhs: 7,
                },
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 2, 5);
        solver.assert_bounds(y, 0, 5);
    }
}
