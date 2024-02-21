use std::rc::Rc;

use crate::basic_types::variables::IntVar;
use crate::basic_types::ConflictInfo;
use crate::basic_types::Inconsistency;
use crate::basic_types::Literal;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
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

#[derive(Debug)]
pub struct LinearNe<Var> {
    /// The terms which sum to the left-hand side.
    terms: Box<[Var]>,
    /// The right-hand side.
    rhs: i32,
    /// The reification variable
    reif: Option<Literal>,
}

impl<Var> LinearNe<Var> {
    pub fn new(terms: Box<[Var]>, rhs: i32) -> Self {
        LinearNe {
            terms,
            rhs,
            reif: None,
        }
    }

    pub fn reified(terms: Box<[Var]>, rhs: i32, reif: Literal) -> Self {
        LinearNe {
            terms,
            rhs,
            reif: Some(reif),
        }
    }
}

/// Domain consistent propagator for the constraint `reif => \sum x_i != rhs`, where `x_i` are
/// integer variables and `rhs` is an integer constant.
#[derive(Debug)]
pub struct LinearNeProp<Var> {
    terms: Rc<[PropagatorVariable<Var>]>,
    rhs: i32,
    pub reif: Option<PropagatorVariable<Literal>>,
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
            reif: self.reif.map(|literal| {
                context.register_literal(
                    literal,
                    DomainEvents::ANY_BOOL,
                    LocalId::from(self.terms.len() as u32),
                )
            }),
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
        let reified = self.reif.is_some();

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

        if num_fixed == self.terms.len() - 1
            && (!reified || context.is_literal_true(self.reif.as_ref().unwrap()))
        {
            let value_to_remove = self.rhs - lhs;

            let unfixed_x_i = self
                .terms
                .iter()
                .position(|x_i| !context.is_fixed(x_i))
                .unwrap();
            let terms = Rc::clone(&self.terms);
            let reif = self.reif.clone();
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
                    PropositionalConjunction::new(
                        predicates.into(),
                        reif.map(|var| var.get_literal()).iter().copied().collect(),
                    )
                },
            )?;
        } else if lhs == self.rhs {
            if reified && !context.is_literal_fixed(self.reif.as_ref().unwrap()) {
                // Conflict was found but we can set the reified literal to false to satisfy the
                // constraint
                let reason: PropositionalConjunction = self
                    .terms
                    .iter()
                    .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                    .collect();

                context.assign_literal(self.reif.as_ref().unwrap(), false, reason)?;
            } else if !reified || context.is_literal_true(self.reif.as_ref().unwrap()) {
                // Conflict was found, either the constraint is not reified or the reification
                // variable is already true

                let failure_reason: Vec<_> = self
                    .terms
                    .iter()
                    .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                    .collect();

                return Err(Inconsistency::Other(ConflictInfo::Explanation(
                    PropositionalConjunction::new(
                        failure_reason.into(),
                        self.reif
                            .as_ref()
                            .map(|var| var.get_literal())
                            .iter()
                            .copied()
                            .collect(),
                    ),
                )));
            }
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
            .new_propagator(LinearNe::new([x.scaled(1), y.scaled(-1)].into(), 0))
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
            .new_propagator(LinearNe::new([x.scaled(1), y.scaled(-1)].into(), 0))
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
            .new_propagator(LinearNe::new([x.clone(), y.clone()].into(), 0))
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
            .new_propagator(LinearNe::new([x.scaled(1), y.scaled(-1)].into(), 0))
            .expect("non-empty domain");

        solver.remove(x, 0).expect("non-empty domain");
        solver.remove(x, 2).expect("non-empty domain");
        solver.remove(x, 3).expect("non-empty domain");

        solver.remove(y, 0).expect("non-empty domain");
        solver.remove(y, 1).expect("non-empty domain");
        solver.remove(y, 2).expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");
    }

    #[test]
    fn literal_is_propagated_when_reified() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(2, 2);
        let reif = solver.new_literal();

        let _ = solver
            .new_propagator(LinearNe::reified(
                [x.scaled(1), y.scaled(-1)].into(),
                0,
                reif,
            ))
            .expect("Non empty domain");

        assert!(solver.is_literal_false(reif));
    }

    #[test]
    fn conflict_is_found_when_reified() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2);
        let y = solver.new_variable(2, 2);
        let reif = solver.new_literal();

        solver.set_literal(reif, true);

        let err = solver
            .new_propagator(LinearNe::reified(
                [x.scaled(1), y.scaled(-1)].into(),
                0,
                reif,
            ))
            .expect_err("Non empty domain");

        let expected: Inconsistency = PropositionalConjunction::new(
            vec![predicate!(x == 2), predicate!(y == 2)].into(),
            vec![reif].into(),
        )
        .into();
        assert_eq!(expected, err);
    }

    #[test]
    fn explanation_for_propagation_reified() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2).scaled(1);
        let y = solver.new_variable(1, 5).scaled(-1);
        let reif = solver.new_literal();

        solver.set_literal(reif, true);

        let mut propagator = solver
            .new_propagator(LinearNe::reified([x.clone(), y.clone()].into(), 0, reif))
            .expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y != -2]);

        assert_eq!(
            PropositionalConjunction::new(vec![predicate!(x == 2)].into(), vec![reif].into()),
            *reason
        );
    }

    #[test]
    fn no_propagation_reified_unfixed() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 2).scaled(1);
        let y = solver.new_variable(1, 5).scaled(-1);
        let reif = solver.new_literal();

        let mut propagator = solver
            .new_propagator(LinearNe::reified([x.clone(), y.clone()].into(), 0, reif))
            .expect("non-empty domain");

        solver.propagate(&mut propagator).expect("non-empty domain");

        assert!(solver.contains(y, -2));
    }
}
