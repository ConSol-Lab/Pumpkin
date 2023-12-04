use crate::engine::DomainEvents;
use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, LocalId,
        PropagationContext, PropagatorConstructorContext, PropagatorVariable,
    },
    predicate,
};

pub struct LinearNeArgs<Var> {
    /// The terms which sum to the left-hand side.
    pub terms: Box<[Var]>,
    /// The right-hand side.
    pub rhs: i32,
}

/// Domain consistent propagator for the constraint `\sum x_i != rhs`, where `x_i` are integer variables
/// and `rhs` is an integer constant.
pub struct LinearNe<Var> {
    terms: Box<[PropagatorVariable<Var>]>,
    rhs: i32,
}

impl<Var> CPPropagatorConstructor for LinearNe<Var>
where
    Var: IntVar + std::fmt::Debug + 'static,
{
    type Args = LinearNeArgs<Var>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        let x: Box<[_]> = args
            .terms
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(x_i.clone(), DomainEvents::ASSIGN, LocalId::from(i as u32))
            })
            .collect();

        Box::new(LinearNe {
            terms: x,
            rhs: args.rhs,
        })
    }
}

impl<Var> ConstraintProgrammingPropagator for LinearNe<Var>
where
    Var: IntVar,
{
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        self.debug_propagate_from_scratch(context)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn get_reason_for_propagation(
        &mut self,
        context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        let i = delta.affected_local_id().unpack();
        let change = self.terms[i as usize].unpack(delta);

        let DomainChange::Removal(_) = change else {
            unreachable!()
        };

        self.terms
            .iter()
            .enumerate()
            .filter_map(|(j, x_j)| {
                if j as u32 != i {
                    Some(predicate![x_j == context.lower_bound(x_j)])
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .into()
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearNe"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        // TODO: This is a great candidate to potentially make incremental. We can only propagate
        // when there is one unfixed variable.
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
                .find(|x_i| !context.is_fixed(x_i))
                .unwrap();
            context.remove(unfixed_x_i, value_to_remove)?;
        } else if lhs == self.rhs {
            debug_assert_eq!(num_fixed, self.terms.len());

            let failure_reason = self
                .terms
                .iter()
                .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                .collect::<Vec<_>>();

            return Err(failure_reason.into());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{basic_types::Inconsistency, conjunction, engine::test_helper::TestSolver};

    use super::*;

    #[test]
    fn test_value_is_removed() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(1, 5);

        let mut propagator = solver.new_propagator::<LinearNe<_>>(LinearNeArgs {
            terms: [x.scaled(1), y.scaled(-1)].into(),
            rhs: 0,
        });

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

        let mut propagator = solver.new_propagator::<LinearNe<_>>(LinearNeArgs {
            terms: [x.scaled(1), y.scaled(-1)].into(),
            rhs: 0,
        });

        let err = solver.propagate(&mut propagator).expect_err("empty domain");

        assert_eq!(Inconsistency::Other(conjunction!([x == 2] & [y == 2])), err);
    }

    #[test]
    fn explanation_for_propagation() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(1, 5);

        let mut propagator = solver.new_propagator::<LinearNe<_>>(LinearNeArgs {
            terms: [x.scaled(1), y.scaled(-1)].into(),
            rhs: 0,
        });

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::Removal(2)),
        );

        assert_eq!(conjunction!([x == 2]), reason);
    }

    #[test]
    fn satisfied_constraint_does_not_trigger_conflict() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 3);
        let y = solver.new_variable(0, 3);

        let mut propagator = solver.new_propagator::<LinearNe<_>>(LinearNeArgs {
            terms: [x.scaled(1), y.scaled(-1)].into(),
            rhs: 0,
        });

        solver.remove(x, 0).expect("non-empty domain");
        solver.remove(x, 2).expect("non-empty domain");
        solver.remove(x, 3).expect("non-empty domain");

        solver.remove(y, 0).expect("non-empty domain");
        solver.remove(y, 1).expect("non-empty domain");
        solver.remove(y, 2).expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");
    }
}
