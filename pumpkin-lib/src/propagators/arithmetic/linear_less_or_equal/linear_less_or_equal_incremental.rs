use super::linear_less_or_equal_constructor::LinearLessOrEqualConstructor;
use super::linear_less_or_equal_regular::propagate_linear_less_or_equal_from_scratch;
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
use crate::propagators::SparseSet;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;

/// Propagator for the constraint `reif => \sum x_i <= rhs`.
///
/// This propagator is incremental and attempts to reason about when at least 1 propagation is
/// **guaranteed** to occur. It does this by keeping track of the "Slack" which has the following
/// formula:
///
/// `Slack(x_i) = ub(x_i) - (rhs - (lb_lhs - lb(x_i)))`
/// where `lb_lhs` is the lower-bound of the left-hand side.
///
/// If the slack is *positive* then it means that `ub(x_i) > rhs - lb_lhs - lb(x_i)` which means
/// that a bound update can take place (since the upper-bound is larger than its maximum
/// allowed value).
///
/// If the slack is *non-positive* then it means that `ub(x_i) <= rhs -
/// lb_lhs - lb(x_i)` which means that no propagation can take place since the upper-bound of
/// `x_i` is in its allowed range.
///
/// Thus, this propagator keeps track of an approximation of the maximum slack value without
/// recalculating it in every iteration; a propagation is thus guaranteed to not be possible if the
/// propagator is **not** scheduled but the propagator is not guaranteed to propagate anything if it
/// **is** scheduled due to the approximative nature of the maximum slack value.
///
/// Additionally, the propagator keeps track of the unfixed terms in a sparse-set \[1\] which are
/// updated during search/propagation so that this propagator only iterates over terms which it is
/// possible to update when checking for propagations.
///
/// Finally, the propagator keeps track of the upper-bound of the left-hand side; if this value is
/// lower than the right-hand side of the inequality then this means that no propagation can take
/// place and the propagator should never be scheduled.
///
/// # Bibliography
/// \[1\] V. le C. de Saint-Marcq, P. Schaus, C. Solnon, and C. Lecoutre, ‘Sparse-sets for domain
/// implementation’, in CP workshop on Techniques foR Implementing Constraint programming Systems
/// (TRICS), 2013, pp. 1–10.
#[derive(Debug)]
pub(crate) struct IncrementalLinearLessOrEqualPropagator<Var> {
    /// The terms in the linear inequality (also called the left-hand side), often represented by
    /// `x_i`.
    terms: Box<[Var]>,
    /// The right-hand side of the linear inequality, a constant term which does not change
    /// throughout the search.
    rhs: i32,

    /// The terms in [`LinearLessOrEqualPropagator::terms`] whose domain is not fixed. Note that we
    /// simply store the [`LocalId`] since we require a way to map the elements onto the original
    /// size of the array.
    unfixed_terms: SparseSet<LocalId>,
    /// The known bounds of each term in [`LinearLessOrEqualPropagator::terms`].
    bounds: Vec<(i32, i32)>,
    /// The lower-bound of the left-hand side (i.e. the [`LinearLessOrEqualPropagator::terms`]) of
    /// the linear inequality.
    lb_lhs: i32,
    /// The slack for each term; this slack represents how close a term is to having its
    /// upper-bound propagated. See [`LinearLessOrEqualPropagator`] for more information.
    slacks: Vec<i32>,
    /// An approximation of the maximum slack value in [`LinearLessOrEqualPropagator::slacks`]
    /// which is used to determine when to schedule the propagator.
    maximum_slack: i32,
    /// The upper-bound of the left-hand side (i.e. the [`LinearLessOrEqualPropagator::terms`]) of
    /// the linear inequality. Used to determine when the inequality is trivially satisfied.
    ub_lhs: i32,
    /// Indicates whether backtracking has occurred; this variable is kept to determine when to
    /// recalculate from scratch but it lazily performs this operation.
    should_recalculate: bool,
}

impl<Var> PropagatorConstructor
    for LinearLessOrEqualConstructor<Var, IncrementalLinearLessOrEqualPropagator<Var>>
where
    Var: IntegerVariable + 'static,
{
    type Propagator = IncrementalLinearLessOrEqualPropagator<Var>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let x: Box<[_]> = self
            .x
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(x_i.clone(), DomainEvents::BOUNDS, LocalId::from(i as u32))
            })
            .collect();

        IncrementalLinearLessOrEqualPropagator::<Var> {
            terms: x,
            rhs: self.c,

            unfixed_terms: SparseSet::new(
                (0..self.x.len())
                    .map(|index| LocalId::from(index as u32))
                    .collect::<Vec<_>>(),
                |local_id| local_id.unpack() as usize,
            ),
            bounds: vec![(i32::MIN, i32::MAX); self.x.len()],
            lb_lhs: 0,
            ub_lhs: 0,
            slacks: vec![i32::MIN; self.x.len()],
            maximum_slack: i32::MIN,
            should_recalculate: false,
        }
    }
}

impl<Var> Propagator for IncrementalLinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable + 'static,
{
    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "IncrementalLinearLeq"
    }

    fn initialise_at_root(
        &mut self,
        context: PropagationContext,
    ) -> Result<(), PropositionalConjunction> {
        self.create_bounds_lhs_and_set_known_bounds(&context);
        self.check_for_conflict(&context)?;
        self.calculate_slack();

        Ok(())
    }

    fn synchronise(&mut self, _context: &PropagationContext) {
        self.should_recalculate = true;
    }

    fn notify(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if self.should_recalculate {
            // We have synchronized and need to recalculate
            return EnqueueDecision::Enqueue;
        }
        if self.ub_lhs <= self.rhs {
            // The upper-bound of the left-hand side can never reach the value of the right-hand
            // side, we do not need to be enqueued
            return EnqueueDecision::Skip;
        }

        let index = local_id.unpack() as usize;
        let term = &self.terms[index];

        if matches!(term.unpack_event(event), IntDomainEvent::LowerBound)
            && self.bounds[index].0 < context.lower_bound(term)
        {
            // A lower-bound update has occurred which does not update the slack of the current term
            // but might update the slack of other terms.
            pumpkin_assert_moderate!(self.bounds[index].0 < context.lower_bound(term));

            if context.is_fixed(term) {
                // If the term is fixed due to the update, then we remove it from the fixed terms
                self.unfixed_terms.remove(&local_id);
            }

            // We calculate the difference between the updated lower-bound and the new lower-bound
            // since this is the change in `lb_lhs`.
            let difference_in_lower_bound = context.lower_bound(term) - self.bounds[index].0;
            self.lb_lhs += difference_in_lower_bound;

            self.bounds[index].0 = context.lower_bound(term);

            // Note that the slack of this term does not change since the increase in `lb_lhs` is
            // the same as the increase of the lower-bound.
            // However, the slack of other terms could change.

            // We increase the maximum slack by the difference in the lower-bound; note that this
            // might lead to the propagator being scheduled even though it cannot propagate if the
            // maximum slack is equal to the slack of the current `term` being updated.
            self.maximum_slack += difference_in_lower_bound;

            if self.maximum_slack.is_positive() || self.lb_lhs > self.rhs {
                // A large enough change has occurred that the maximum slack has become positive
                // which means that an update can take place OR it a conflict has occurred which
                // means the propagator should be enqueued.
                EnqueueDecision::Enqueue
            } else {
                // The update was not large enough for propagation to occur;
                // note that this value could be inaccurate if the maximum slack is of the current
                // term being updated, however this would lead to being enqueued more often then
                // necessary which is still correct

                EnqueueDecision::Skip
            }
        } else if self.bounds[index].1 > context.upper_bound(term) {
            // An upper-bound update has occurred which updates the slack of the current term (but
            // not of the others)
            pumpkin_assert_moderate!(self.bounds[index].1 > context.upper_bound(term),);

            // We decrease the upper-bound of the left-hand side by the difference between the
            // current stored upper-bound and the new upper-bound
            let difference_in_upper_bound = self.bounds[index].1 - context.upper_bound(term);
            self.ub_lhs -= difference_in_upper_bound;

            if self.ub_lhs <= self.rhs {
                // The upper-bound of the left-hand side can never reach the value of the
                // right-hand side, we do not need to be enqueued
                return EnqueueDecision::Skip;
            }

            if context.is_fixed(term) {
                // If the term is fixed due to the update, then we remove it from the fixed terms
                self.unfixed_terms.remove(&local_id);
            }

            self.bounds[index].1 = context.upper_bound(term);

            // Completely recalculate the slack of the updated term
            self.slacks[index] =
                self.bounds[index].1 - (self.rhs - (self.lb_lhs - self.bounds[index].0));

            self.maximum_slack = self.maximum_slack.max(self.slacks[index]);

            if self.slacks[index].is_positive() {
                // The updated slack is positive, which means that we could propagate
                EnqueueDecision::Enqueue
            } else {
                // The updated slack is negative which means that we can not propagate this value
                EnqueueDecision::Skip
            }
        } else {
            // We have received a notification which was caused by our own propagation, we do not
            // need to update anything
            EnqueueDecision::Skip
        }
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        pumpkin_assert_extreme!(
            self.lb_lhs
                == self
                    .terms
                    .iter()
                    .map(|var| context.lower_bound(var))
                    .sum::<i32>(),
        );
        pumpkin_assert_extreme!(
            self.ub_lhs
                == self
                    .terms
                    .iter()
                    .map(|var| context.upper_bound(var))
                    .sum::<i32>()
        );

        if self.should_recalculate {
            self.should_recalculate = false;
            self.create_bounds_lhs_and_set_known_bounds(&context.as_readonly());
            self.calculate_slack();
        }

        if self.ub_lhs <= self.rhs {
            // The upper-bound of the left-hand side can never reach the value of the right-hand
            // side, we do not need to propagate
            return Ok(());
        }

        // We first check whether a conflict has occurred
        self.check_for_conflict(&context.as_readonly())?;

        // We reset the maximum slack since it will be updated
        self.maximum_slack = i32::MIN;

        // Then we go over the unfixed terms and determine whether a propagation could take place
        let mut term_index = 0;
        while term_index < self.unfixed_terms.len() {
            let index = self.unfixed_terms.get(term_index).unpack() as usize;
            let x_i = &self.terms[index];
            pumpkin_assert_extreme!(
                self.bounds[index] == (context.lower_bound(x_i), context.upper_bound(x_i)),
            );

            // This is the maximum value that `x_i` can take without causing a left-hand side which
            // is greater than `c`.
            let maximum_bound = self.rhs - (self.lb_lhs - context.lower_bound(x_i));

            // Now we check whether we can update the upper-bound
            if context.upper_bound(x_i) > maximum_bound {
                let reason: PropositionalConjunction = self
                    .terms
                    .iter()
                    .enumerate()
                    .filter_map(|(j, x_j)| {
                        if j != index {
                            Some(predicate![x_j >= context.lower_bound(x_j)])
                        } else {
                            None
                        }
                    })
                    .collect();

                context.set_upper_bound(x_i, maximum_bound, reason)?;

                // We update the upper-bound of the left-hand side after the update
                self.ub_lhs -= self.bounds[index].1 - context.upper_bound(x_i);
                if self.ub_lhs <= self.rhs {
                    // The linear inequality is trivially satisfied, we can stop updating
                    return Ok(());
                }
                self.bounds[index].1 = context.upper_bound(x_i);
            }

            // We remove the term from consideration if it is fixed; otherwise we simply move to the
            // next element
            if context.is_fixed(x_i) {
                self.unfixed_terms.remove(&LocalId::from(index as u32));
            } else {
                term_index += 1;
            }

            // We update the slack to its new value and we recalculate the maximum slack
            self.slacks[index] =
                context.upper_bound(x_i) - (self.rhs - (self.lb_lhs - context.lower_bound(x_i)));
            self.maximum_slack = self.maximum_slack.max(self.slacks[index]);

            pumpkin_assert_moderate!(self.slacks[index] <= 0);
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        propagate_linear_less_or_equal_from_scratch(&self.terms, &mut context, self.rhs)
    }
}

impl<Var: IntegerVariable> IncrementalLinearLessOrEqualPropagator<Var> {
    /// Determines whether a conflict has occurred and calculates the reason for the conflict.
    fn check_for_conflict(
        &mut self,
        context: &PropagationContext,
    ) -> Result<(), PropositionalConjunction> {
        // If the lower-bound of the left-hand side exceeds the right-hand side then we can report a
        // conflict
        if self.lb_lhs > self.rhs {
            let reason: PropositionalConjunction = self
                .terms
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect();
            return Err(reason);
        }
        Ok(())
    }

    /// Calculates the lower-bound and upper-bound on the left-hand side of the inequality and
    /// initializes the bounds to the values stored in `context`. Additionally, initializes the
    /// `[LinearLessOrEqualPropagator::unfixed_terms]` to contain only unfixed terms.
    fn create_bounds_lhs_and_set_known_bounds(&mut self, context: &PropagationContext) {
        self.lb_lhs = 0;
        self.ub_lhs = 0;

        self.terms.iter().enumerate().for_each(|(index, term)| {
            if context.is_fixed(term) {
                // If it is fixed then we remove it from the unfixed terms
                self.unfixed_terms.remove(&LocalId::from(index as u32));
            } else {
                // If it is not fixed then we restore it if necessary
                self.unfixed_terms.restore(&LocalId::from(index as u32));
            }
            // We store the currently known bounds
            self.bounds[index] = (context.lower_bound(term), context.upper_bound(term));

            // We keep track of the bounds of the left-hand side
            self.ub_lhs += context.upper_bound(term);
            self.lb_lhs += context.lower_bound(term);
        });
    }

    /// Calculates the slack for each variable which indicates how far the upper-bound of a term is
    /// from requiring an update; specifically, the slack for a term `x_i` is calculated as
    /// follows:
    ///
    /// `Slack(x_i) = ub(x_i) - (rhs - (lb_lhs - lb(x_i)))`
    /// where `lb_lhs` is the lower-bound of the left-hand side.
    ///
    /// If the slack is *positive* then it means that `ub(x_i) > rhs - lb_lhs - lb(x_i)` which means
    /// that a bound update can take place (since the upper-bound is larger than its maximum
    /// allowed value).
    ///
    /// If the slack is *non-positive* then it means that `ub(x_i) <= rhs -
    /// lb_lhs - lb(x_i)` which means that no propagation can take place since the upper-bound of
    /// `x_i` is in its allowed range.
    fn calculate_slack(&mut self) {
        (0..self.terms.len()).for_each(|index| {
            let slack = self.bounds[index].1 - (self.rhs - (self.lb_lhs - self.bounds[index].0));

            // We track the maximum slack to determine when it become positive
            self.maximum_slack = self.maximum_slack.max(slack);

            self.slacks[index] = slack;
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::propagators::linear_less_or_equal::linear_less_or_equal_constructor::IncrementalLinearLessOrEqual;

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(IncrementalLinearLessOrEqual::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_explanations() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(IncrementalLinearLessOrEqual::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y <= 6].try_into().unwrap());

        assert_eq!(conjunction!([x >= 1]), *reason);
    }
}
