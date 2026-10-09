use super::LinearLessOrEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::IntExt;
use crate::RetentionCheck;
use crate::RetentionChecker;

impl<Var, Atomic> RetentionChecker<Atomic> for LinearLessOrEqualChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        // 1. Check if the constraint is conflicting
        let bound = i64::from(self.bound);
        let lower_bound_sum = self
            .terms
            .iter()
            .map(|term| IntExt::<i64>::from(term.induced_lower_bound(state)))
            .sum::<IntExt<i64>>();

        if lower_bound_sum > bound {
            log::error!(
                "The lower bounds of {:?} exceed the bound {} of the linear inequality",
                self.terms,
                self.bound
            );
            return RetentionCheck::PropagationMissed;
        }

        // 2. Assert that each variable can take its upper bound while the other variables are at
        //    their lower bounds.
        let all_tight = self.terms.iter().all(|term| {
            let greatest = bound
                - (lower_bound_sum - IntExt::<i64>::from(term.induced_lower_bound(state)));
            let is_tight = IntExt::<i64>::from(term.induced_upper_bound(state)) <= greatest;

            if !is_tight {
                log::error!(
                    "The upper bound of {term:?} could be lowered to {greatest:?} by the linear inequality {:?} <= {}",
                    self.terms,
                    self.bound
                );
            }

            is_tight
        });

        if all_tight {
            RetentionCheck::NothingToPropagate
        } else {
            RetentionCheck::PropagationMissed
        }
    }
}
