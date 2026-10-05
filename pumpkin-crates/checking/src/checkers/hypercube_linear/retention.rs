use std::fmt::Debug;

use super::HypercubeLinearChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::IntExt;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::VariableState;

/// Mirrors one pass of the propagation of the hypercube linear propagator from scratch.
///
/// When every predicate of the hypercube is true, the linear inequality bounds each of its terms by
/// the slack. When exactly one predicate is not yet true, a negative slack falsifies it, and
/// otherwise the term over the variable of that predicate, if there is one, is bounded by the
/// slack.
impl<Atomic, Var> RetentionChecker<Atomic> for HypercubeLinearChecker<Atomic, Var>
where
    Atomic: AtomicConstraint + Clone + Debug,
    Var: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> RetentionCheck {
        if self
            .hypercube
            .iter()
            .any(|atomic| state.is_true(&atomic.negate()))
        {
            // A false predicate makes the hypercube linear hold trivially.
            return RetentionCheck::NothingToPropagate;
        }

        let not_true = self
            .hypercube
            .iter()
            .filter(|&atomic| !state.is_true(atomic))
            .collect::<Vec<_>>();

        if not_true.len() > 1 {
            return RetentionCheck::NothingToPropagate;
        }

        let lower_bound_sum = self
            .terms
            .iter()
            .map(|term| IntExt::<i64>::from(term.induced_lower_bound(state)))
            .sum::<IntExt<i64>>();
        let slack = i64::from(self.bound) - lower_bound_sum;

        if let [unassigned] = not_true.as_slice() {
            if slack < 0 {
                log::error!(
                    "The predicate {unassigned:?} could be falsified since the linear inequality {:?} <= {} is violated",
                    self.terms,
                    self.bound
                );
                return RetentionCheck::PropagationMissed;
            }

            let Some(term) = self
                .terms
                .iter()
                .find(|term| term.does_atomic_constrain_self(unassigned))
            else {
                return RetentionCheck::NothingToPropagate;
            };

            return match tightened_upper_bound(term, slack, state) {
                UpperBound::Tightened => RetentionCheck::PropagationMissed,
                UpperBound::Tight | UpperBound::BeyondI32 => RetentionCheck::NothingToPropagate,
            };
        }

        // Every predicate of the hypercube is true, so the linear inequality has to hold.
        if self.terms.is_empty() {
            if slack < 0 {
                log::error!("The linear inequality 0 <= {} is violated", self.bound);
            }
            return RetentionCheck::missed_if(slack < 0);
        }

        for term in self.terms.iter() {
            match tightened_upper_bound(term, slack, state) {
                UpperBound::Tight => {}
                UpperBound::Tightened => return RetentionCheck::PropagationMissed,
                // The propagator stops at the first bound beyond i32::MAX, so the terms after it
                // are not propagated in this pass.
                UpperBound::BeyondI32 => return RetentionCheck::NothingToPropagate,
            }
        }

        RetentionCheck::NothingToPropagate
    }
}

/// How the upper bound of a term relates to its lower bound plus the slack of the linear
/// inequality.
enum UpperBound {
    /// The upper bound is at most the lower bound plus the slack.
    Tight,
    /// The upper bound can be lowered to the lower bound plus the slack.
    Tightened,
    /// The lower bound plus the slack exceeds i32::MAX, so it cannot lower the upper bound.
    BeyondI32,
}

fn tightened_upper_bound<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    term: &Var,
    slack: IntExt<i64>,
    state: &VariableState<Atomic>,
) -> UpperBound {
    let new_upper_bound = slack + IntExt::<i64>::from(term.induced_lower_bound(state));

    if new_upper_bound > i64::from(i32::MAX) {
        UpperBound::BeyondI32
    } else if IntExt::<i64>::from(term.induced_upper_bound(state)) > new_upper_bound {
        log::error!("The upper bound of {term:?} could be lowered to {new_upper_bound:?}");
        UpperBound::Tightened
    } else {
        UpperBound::Tight
    }
}
