use std::rc::Rc;

use crate::basic_types::variables::IntVar;
use crate::basic_types::ConflictInfo;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::DomainEvents;
use crate::engine::LocalId;
use crate::engine::PropagationContext;
use crate::engine::PropagationContextMut;
use crate::engine::PropagatorConstructorContext;
use crate::engine::PropagatorVariable;
use crate::engine::ReadDomains;
use crate::predicate;

pub struct LinearNe<Var> {
    /// The terms which sum to the left-hand side.
    pub terms: Box<[Var]>,
    /// The right-hand side.
    pub rhs: i32,
}

/// Domain consistent propagator for the constraint `\sum x_i != rhs`, where `x_i` are integer variables
/// and `rhs` is an integer constant.
pub struct LinearNeProp<Var> {
    terms: Rc<[PropagatorVariable<Var>]>,
    rhs: i32,
}

impl<Var> CPPropagatorConstructor for LinearNe<Var>
where
    Var: IntVar + 'static,
{
    type Propagator = LinearNeProp<Var>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let x: Rc<[_]> = self
            .terms
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(x_i.clone(), DomainEvents::ASSIGN, LocalId::from(i as u32))
            })
            .collect();

        LinearNeProp {
            terms: x,
            rhs: self.rhs,
        }
    }
}

impl<Var> ConstraintProgrammingPropagator for LinearNeProp<Var>
where
    Var: IntVar + 'static,
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.debug_propagate_from_scratch(context)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearNe"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        // TODO: This is a great candidate to potentially make incremental. We can only propagate
        //  when there is one unfixed variable.
        let num_fixed = self
            .terms
            .iter()
            .filter(|x_i| context.is_fixed(x_i))
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
                    terms
                        .iter()
                        .enumerate()
                        .filter(|&(i, _)| i != unfixed_x_i)
                        .map(|(_, x_i)| predicate![x_i == context.lower_bound(x_i)])
                        .collect()
                },
            )?;
        } else if lhs == self.rhs {
            debug_assert_eq!(num_fixed, self.terms.len());

            let failure_reason = self
                .terms
                .iter()
                .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                .collect();

            return Err(Inconsistency::Other(ConflictInfo::Explanation(
                failure_reason,
            )));
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

    #[test]
    fn test_value_is_removed() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(1, 5);

        let mut propagator = solver
            .new_propagator(LinearNe {
                terms: [x.scaled(1), y.scaled(-1)].into(),
                rhs: 0,
            })
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
            .new_propagator(LinearNe {
                terms: [x.scaled(1), y.scaled(-1)].into(),
                rhs: 0,
            })
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
            .new_propagator(LinearNe {
                terms: [x.clone(), y.clone()].into(),
                rhs: 0,
            })
            .expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y != -2]);

        assert_eq!(conjunction!([x == 2]), *reason);
    }

    #[test]
    fn satisfied_constraint_does_not_trigger_conflict() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 3);
        let y = solver.new_variable(0, 3);

        let mut propagator = solver
            .new_propagator(LinearNe {
                terms: [x.scaled(1), y.scaled(-1)].into(),
                rhs: 0,
            })
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
