use std::rc::Rc;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::predicate;

#[derive(Debug)]
pub(crate) struct LinearNotEqualConstructor<Var> {
    /// The terms which sum to the left-hand side.
    terms: Box<[Var]>,
    /// The right-hand side.
    rhs: i32,
}

impl<Var> LinearNotEqualConstructor<Var> {
    pub(crate) fn new(terms: Box<[Var]>, rhs: i32) -> Self {
        LinearNotEqualConstructor { terms, rhs }
    }
}

/// Domain consistent propagator for the constraint `reif => \sum x_i != rhs`, where `x_i` are
/// integer variables and `rhs` is an integer constant.
#[derive(Debug)]
pub(crate) struct LinearNotEqualPropagator<Var> {
    terms: Rc<[Var]>,
    rhs: i32,
}

impl<Var> PropagatorConstructor for LinearNotEqualConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type Propagator = LinearNotEqualPropagator<Var>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let x: Rc<[_]> = self
            .terms
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(x_i.clone(), DomainEvents::ASSIGN, LocalId::from(i as u32))
            })
            .collect();
        LinearNotEqualPropagator {
            terms: x,
            rhs: self.rhs,
        }
    }
}

impl<Var> Propagator for LinearNotEqualPropagator<Var>
where
    Var: IntegerVariable + 'static,
{
    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearNe"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        // TODO: This is a great candidate to potentially make incremental. We can only propagate
        //  when there is one unfixed variable.

        let num_fixed = self
            .terms
            .iter()
            .filter(|&x_i| context.is_fixed(x_i))
            .count();
        if num_fixed < self.terms.len() - 1 {
            return Ok(());
        }

        let lhs = self
            .terms
            .iter()
            .map(|var| {
                if context.is_fixed(var) {
                    context.lower_bound(var)
                } else {
                    0
                }
            })
            .sum::<i32>();

        if num_fixed == self.terms.len() - 1 {
            let value_to_remove = self.rhs - lhs;

            let unfixed_x_i = self
                .terms
                .iter()
                .position(|x_i| !context.is_fixed(x_i))
                .unwrap();
            let terms = Rc::clone(&self.terms);
            context.remove(
                &self.terms[unfixed_x_i],
                value_to_remove,
                move |context: &PropagationContext| {
                    let predicates = terms
                        .iter()
                        .enumerate()
                        .filter(|&(i, _)| i != unfixed_x_i)
                        .map(|(_, x_i)| predicate![x_i == context.lower_bound(x_i)])
                        .collect::<Vec<_>>();
                    predicates.into()
                },
            )?;
        } else if num_fixed == self.terms.len() && lhs == self.rhs {
            // Conflict was found, either the constraint is not reified or the reification
            // variable is already true

            let failure_reason: PropositionalConjunction = self
                .terms
                .iter()
                .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                .collect();

            return Err(failure_reason.into());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::engine::variables::TransformableVariable;

    #[test]
    fn test_value_is_removed() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(1, 5);

        let mut propagator = solver
            .new_propagator(LinearNotEqualConstructor::new(
                [x.scaled(1), y.scaled(-1)].into(),
                0,
            ))
            .expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 2, 2);
        solver.assert_bounds(y, 1, 5);
        assert!(!solver.contains(y, 2));
    }

    #[test]
    fn test_empty_domain_is_detected() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(2, 2);

        let err = solver
            .new_propagator(LinearNotEqualConstructor::new(
                [x.scaled(1), y.scaled(-1)].into(),
                0,
            ))
            .expect_err("empty domain");

        let expected: Inconsistency = conjunction!([x == 2] & [y == 2]).into();
        assert_eq!(expected, err);
    }

    #[test]
    fn explanation_for_propagation() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2).scaled(1);
        let y = solver.new_variable(1, 5).scaled(-1);

        let mut propagator = solver
            .new_propagator(LinearNotEqualConstructor::new(
                [x.clone(), y.clone()].into(),
                0,
            ))
            .expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y != -2].try_into().unwrap());

        assert_eq!(conjunction!([x == 2]), *reason);
    }

    #[test]
    fn satisfied_constraint_does_not_trigger_conflict() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 3);
        let y = solver.new_variable(0, 3);

        let mut propagator = solver
            .new_propagator(LinearNotEqualConstructor::new(
                [x.scaled(1), y.scaled(-1)].into(),
                0,
            ))
            .expect("non-empty domain");

        solver.remove(x, 0).expect("non-empty domain");
        solver.remove(x, 2).expect("non-empty domain");
        solver.remove(x, 3).expect("non-empty domain");

        solver.remove(y, 0).expect("non-empty domain");
        solver.remove(y, 1).expect("non-empty domain");
        solver.remove(y, 2).expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");
    }
}
