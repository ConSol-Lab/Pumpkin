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
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::variables::IntegerVariable;

declare_inference_label!(HypercubeLinearInference);

#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinear<Var> {
    pub(crate) hypercube: PropositionalConjunction,
    pub(crate) linear_terms: Box<[Var]>,
    pub(crate) linear_rhs: i32,
}

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinearPropagatorArgs<Var> {
    pub(crate) hypercube_linear: HypercubeLinear<Var>,
    pub(crate) constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor for HypercubeLinearPropagatorArgs<Var> {
    type PropagatorImpl = HypercubeLinearPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let HypercubeLinearPropagatorArgs {
            hypercube_linear,
            constraint_tag,
        } = self;

        for predicate in hypercube_linear.hypercube.iter().copied() {
            context.register_predicate(predicate);
        }

        for (i, x_i) in hypercube_linear.linear_terms.iter().enumerate() {
            context.register(
                x_i.clone(),
                DomainEvents::LOWER_BOUND,
                LocalId::from(i as u32),
            );
        }

        HypercubeLinearPropagator {
            hypercube_linear,
            inference_code: context.create_inference_code(constraint_tag, HypercubeLinearInference),
        }
    }
}

/// Propagator for the constraint `/\ hypercube -> \sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinearPropagator<Var> {
    hypercube_linear: HypercubeLinear<Var>,
    inference_code: InferenceCode,
}

impl<Var: IntegerVariable + 'static> Propagator for HypercubeLinearPropagator<Var> {
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
                    hypercube: conjunction!([x >= 2]),
                    linear_terms: [x, y].into(),
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
                    hypercube: conjunction!([x >= 2]),
                    linear_terms: [x, y].into(),
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
