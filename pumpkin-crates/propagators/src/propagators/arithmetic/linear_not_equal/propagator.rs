use std::rc::Rc;

use pumpkin_core::asserts::pumpkin_assert_extreme;
use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

/// Propagator for the constraint `\sum x_i != rhs`, where `x_i` are
/// integer variables and `rhs` is an integer constant.
#[derive(Clone, Debug)]
pub struct LinearNotEqualPropagator<Var> {
    /// The terms of the sum
    pub(super) terms: Rc<[Var]>,
    /// The right-hand side of the sum
    pub(super) rhs: i32,

    /// The inference code for this propagator.
    pub(super) inference_code: InferenceCode,

    /// The number of fixed terms; note that this constraint can only propagate when there is a
    /// single unfixed variable and can only detect conflicts if all variables are assigned
    pub(super) number_of_fixed_terms: usize,
    /// The sum of the values of the fixed terms
    pub(super) fixed_lhs: i32,
    /// Indicates whether the single unfixed variable has been updated; if this is the case then
    /// the propagator is not scheduled again
    pub(super) unfixed_variable_has_been_updated: bool,
    /// Indicates whether the value of [`LinearNotEqualPropagator::fixed_lhs`] is invalid and
    /// should be recalculated
    pub(super) should_recalculate_lhs: bool,
}

impl<Var> Propagator for LinearNotEqualPropagator<Var>
where
    Var: IntegerVariable + 'static,
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "LinearNe"
    }

    fn notify(
        &mut self,
        context: NotificationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        // If the updated term is fixed then we update the number of fixed variables
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

    fn notify_backtrack(&mut self, _context: Domains, local_id: LocalId, event: OpaqueDomainEvent) {
        if matches!(
            self.terms[local_id.unpack() as usize].unpack_event(event),
            DomainEvent::Assign
        ) {
            pumpkin_assert_simple!(
                self.number_of_fixed_terms >= 1,
                "The number of fixed terms should never be negative"
            );
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
                DomainEvent::Removal
            ));

            // We set the flag whether the unfixed variable has been updated
            self.unfixed_variable_has_been_updated = false;
        }
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        // If the left-hand side is out of date then we simply recalculate from scratch; we only do
        // this when we can propagate or check for a conflict
        if self.should_recalculate_lhs && self.number_of_fixed_terms >= self.terms.len() - 1 {
            self.recalculate_fixed_variables(context.domains());
            self.should_recalculate_lhs = false;
        }
        pumpkin_assert_extreme!(self.is_propagator_state_consistent(context.domains()));

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

                context.post(
                    predicate![self.terms[unfixed_x_i] != value_to_remove],
                    (
                        self.terms
                            .iter()
                            .enumerate()
                            .filter(|&(i, _)| i != unfixed_x_i)
                            .map(|(_, x_i)| predicate![x_i == context.lower_bound(x_i)])
                            .collect::<PropositionalConjunction>(),
                        &self.inference_code,
                    ),
                )?;
            }
        } else if self.number_of_fixed_terms == self.terms.len() {
            pumpkin_assert_simple!(!self.should_recalculate_lhs);
            // Otherwise we check for a conflict
            self.check_for_conflict(context.domains())?;
        }

        Ok(())
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
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
            .map(|var| context.fixed_value(var).unwrap_or_default() as i64)
            .sum::<i64>();

        if num_fixed == self.terms.len() - 1 {
            let value_to_remove = self.rhs as i64 - lhs;

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
            context.post(
                predicate![
                    self.terms[unfixed_x_i]
                        != value_to_remove
                            .try_into()
                            .expect("Expected to be able to fit i64 into i32")
                ],
                (reason, &self.inference_code),
            )?;
        } else if num_fixed == self.terms.len() && lhs == self.rhs as i64 {
            let conjunction = self
                .terms
                .iter()
                .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                .collect();

            return Err(PropagatorConflict {
                conjunction,
                inference_code: self.inference_code.clone(),
            }
            .into());
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
    pub(super) fn recalculate_fixed_variables(&mut self, context: Domains) {
        self.unfixed_variable_has_been_updated = false;
        (self.fixed_lhs, self.number_of_fixed_terms) =
            self.terms
                .iter()
                .fold((0, 0), |(fixed_lhs, number_of_fixed_terms), term| {
                    if let Some(fixed_term) = context.fixed_value(term) {
                        (fixed_lhs + fixed_term, number_of_fixed_terms + 1)
                    } else {
                        (fixed_lhs, number_of_fixed_terms)
                    }
                })
    }

    /// Determines whether a conflict has occurred and calculate the reason for the conflict
    fn check_for_conflict(&self, context: Domains) -> Result<(), PropagatorConflict> {
        pumpkin_assert_simple!(!self.should_recalculate_lhs);
        if self.number_of_fixed_terms == self.terms.len() && self.fixed_lhs == self.rhs {
            let conjunction = self
                .terms
                .iter()
                .map(|x_i| predicate![x_i == context.lower_bound(x_i)])
                .collect();

            return Err(PropagatorConflict {
                conjunction,
                inference_code: self.inference_code.clone(),
            });
        }
        Ok(())
    }

    /// Checks whether the number of fixed terms is equal to the number of fixed terms in the
    /// provided [`PropagationContext`] and whether the value of the fixed lhs is the same as in the
    /// provided [`PropagationContext`].
    fn is_propagator_state_consistent(&self, context: Domains) -> bool {
        let expected_number_of_fixed_terms = self
            .terms
            .iter()
            .filter(|&x_i| context.is_fixed(x_i))
            .count();
        let number_of_fixed_terms_is_correct =
            self.number_of_fixed_terms == expected_number_of_fixed_terms;

        let expected_fixed_lhs: i32 = self
            .terms
            .iter()
            .filter_map(|x_i| context.fixed_value(x_i))
            .sum();
        let lhs_is_outdated_or_correct =
            self.should_recalculate_lhs || self.fixed_lhs == expected_fixed_lhs;

        number_of_fixed_terms_is_correct && lhs_is_outdated_or_correct
    }
}
