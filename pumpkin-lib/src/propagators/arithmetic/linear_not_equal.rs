use std::rc::Rc;

use enumset::enum_set;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::engine::IntDomainEvent;
use crate::predicate;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

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

/// Propagator for the constraint `\sum x_i != rhs`, where `x_i` are
/// integer variables and `rhs` is an integer constant.
#[derive(Debug)]
pub(crate) struct LinearNotEqualPropagator<Var> {
    /// The terms of the sum
    terms: Rc<[Var]>,
    /// The right-hand side of the sum
    rhs: i32,

    /// The number of fixed terms; note that this constraint can only propagate when there is a
    /// single unfixed variable and can only detect conflicts if all variables are assigned
    number_of_fixed_terms: usize,
    /// The sum of the values of the fixed terms
    fixed_lhs: i32,
    /// Indicates whether the single unfixed variable has been updated; if this is the case then
    /// the propagator is not scheduled again
    unfixed_variable_has_been_updated: bool,
    /// Indicates whether the value of [`LinearNotEqualPropagator::fixed_lhs`] is invalid and
    /// should be recalculated
    should_recalculate_lhs: bool,
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
                let _ =
                    context.register(x_i.clone(), DomainEvents::ASSIGN, LocalId::from(i as u32));
                context.register_for_backtrack_events(
                    x_i.clone(),
                    DomainEvents::create_with_int_events(enum_set!(
                        IntDomainEvent::Assign | IntDomainEvent::Removal
                    )),
                    LocalId::from(i as u32),
                )
            })
            .collect();
        LinearNotEqualPropagator {
            terms: x,
            rhs: self.rhs,
            number_of_fixed_terms: 0,
            fixed_lhs: 0,
            unfixed_variable_has_been_updated: false,
            should_recalculate_lhs: false,
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

    fn notify(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        // We update the number of fixed variables
        self.number_of_fixed_terms += 1;
        // We update the value of the left-hand side with the value of the newly fixed variable
        self.fixed_lhs += context.lower_bound(&self.terms[local_id.unpack() as usize]);

        // Either the number of fixed variables is the number of terms - 1 in which case we can
        // propagate if it has not been updated before; if it has been updated then we don't need to
        // remove the value from its domain again.
        let can_propagate = self.number_of_fixed_terms == self.terms.len() - 1
            && !self.unfixed_variable_has_been_updated;
        // Otherwise the number of fixed variables is equal to the number of terms in the following
        // cases:
        // - Either we can report a conflict
        // - Or the sum of the values of the left-hand side is inaccurate and we should recalculate
        let is_conflicting_or_outdated = self.number_of_fixed_terms == self.terms.len()
            && (self.should_recalculate_lhs || self.fixed_lhs == self.rhs);
        if can_propagate || is_conflicting_or_outdated {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    fn notify_backtrack(
        &mut self,
        _context: &PropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) {
        if matches!(
            self.terms[local_id.unpack() as usize].unpack_event(event),
            IntDomainEvent::Assign
        ) {
            // An assign has been undone, we can decrease the
            // number of fixed variables
            self.number_of_fixed_terms -= 1;

            // We don't keep track of the old bound to which this variable was assigned so we simply
            // indicate that our lhs is out-of-date
            self.should_recalculate_lhs = true;
        } else {
            // A removal has been undone
            pumpkin_assert_moderate!(matches!(
                self.terms[local_id.unpack() as usize].unpack_event(event),
                IntDomainEvent::Removal
            ));

            // We set the flag whether the unfixed variable has been updated
            self.unfixed_variable_has_been_updated = false;
        }
    }

    fn initialise_at_root(
        &mut self,
        context: PropagationContext,
    ) -> Result<(), PropositionalConjunction> {
        self.recalculate_fixed_variables(context);
        self.check_for_conflict(context)?;
        Ok(())
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        // If the left-hand side is out of date then we simply recalculate from scratch; we only do
        // this when we can propagate or check for a conflict
        if self.should_recalculate_lhs && self.number_of_fixed_terms >= self.terms.len() - 1 {
            self.recalculate_fixed_variables(context.as_readonly());
            self.should_recalculate_lhs = false;
        }
        pumpkin_assert_extreme!(self.is_propagator_state_consistent(context.as_readonly()));

        // If there is only 1 unfixed variable, then we can propagate
        if self.number_of_fixed_terms == self.terms.len() - 1 {
            pumpkin_assert_simple!(!self.should_recalculate_lhs);

            // The value which would cause a conflict if the current variable would be set equal to
            // this
            let value_to_remove = self.rhs - self.fixed_lhs;

            // We find the value which is unfixed
            // We could make use of a sparse-set to determine this, if necessary
            let unfixed_x_i = self
                .terms
                .iter()
                .position(|x_i| !context.is_fixed(x_i))
                .unwrap();

            if context.contains(&self.terms[unfixed_x_i], value_to_remove) {
                // We keep track of whether we have removed the value which could cause a conflict
                // from the unfixed variable
                self.unfixed_variable_has_been_updated = true;

                // Then we remove the conflicting value from the unfixed variable
                let reason = self
                    .terms
                    .iter()
                    .enumerate()
                    .filter(|&(i, _)| i != unfixed_x_i)
                    .map(|(_, x_i)| predicate![x_i == context.lower_bound(x_i)])
                    .collect::<PropositionalConjunction>();
                context.remove(&self.terms[unfixed_x_i], value_to_remove, reason)?;
            }
        } else if self.number_of_fixed_terms == self.terms.len() {
            pumpkin_assert_simple!(!self.should_recalculate_lhs);
            // Otherwise we check for a conflict
            self.check_for_conflict(context.as_readonly())?;
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
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

            let reason = self
                .terms
                .iter()
                .enumerate()
                .filter(|&(i, _)| i != unfixed_x_i)
                .map(|(_, x_i)| predicate![x_i == context.lower_bound(x_i)])
                .collect::<PropositionalConjunction>();
            context.remove(&self.terms[unfixed_x_i], value_to_remove, reason)?;
        } else if num_fixed == self.terms.len() && lhs == self.rhs {
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

impl<Var: IntegerVariable + 'static> LinearNotEqualPropagator<Var> {
    /// This method is used to calculate the fixed left-hand side of the equation and keep track of
    /// the number of fixed variables.
    ///
    /// Note that this method always sets the `unfixed_variable_has_been_updated` to true; this
    /// might be too lenient as it could be the case that synchronisation does not lead to the
    /// re-adding of the removed value.
    fn recalculate_fixed_variables(&mut self, context: PropagationContext) {
        self.unfixed_variable_has_been_updated = false;
        (self.fixed_lhs, self.number_of_fixed_terms) =
            self.terms
                .iter()
                .fold((0, 0), |(fixed_lhs, number_of_fixed_terms), term| {
                    if context.is_fixed(term) {
                        (
                            fixed_lhs + context.lower_bound(term),
                            number_of_fixed_terms + 1,
                        )
                    } else {
                        (fixed_lhs, number_of_fixed_terms)
                    }
                })
    }

    /// Determines whether a conflict has occurred and calculate the reason for the conflict
    fn check_for_conflict(
        &self,
        context: PropagationContext,
    ) -> Result<(), PropositionalConjunction> {
        pumpkin_assert_simple!(!self.should_recalculate_lhs);
        if self.number_of_fixed_terms == self.terms.len() && self.fixed_lhs == self.rhs {
            let failure_reason: PropositionalConjunction = self
                .terms
                .iter()
                .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                .collect();

            return Err(failure_reason);
        }
        Ok(())
    }

    /// Checks whether the number of fixed terms is equal to the number of fixed terms in the
    /// provided [`PropagationContext`] and whether the value of the fixed lhs is the same as in the
    /// provided [`PropagationContext`].
    fn is_propagator_state_consistent(&self, context: PropagationContext) -> bool {
        let expected_number_of_fixed_terms = self
            .terms
            .iter()
            .filter(|&x_i| context.is_fixed(x_i))
            .count();
        let number_of_fixed_terms_is_correct =
            self.number_of_fixed_terms == expected_number_of_fixed_terms;

        let expected_fixed_lhs = self
            .terms
            .iter()
            .filter_map(|x_i| {
                if context.is_fixed(x_i) {
                    Some(context.lower_bound(x_i))
                } else {
                    None
                }
            })
            .sum();
        let lhs_is_outdated_or_correct =
            self.should_recalculate_lhs || self.fixed_lhs == expected_fixed_lhs;

        number_of_fixed_terms_is_correct && lhs_is_outdated_or_correct
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

        solver.notify_propagator(&mut propagator);

        solver.propagate(&mut propagator).expect("non-empty domain");
    }
}
