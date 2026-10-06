use super::LinearNotEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::RetentionCheck;
use crate::RetentionChecker;

impl<Var, Atomic> RetentionChecker<Atomic> for LinearNotEqualChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        let unfixed_terms = self
            .terms
            .iter()
            .filter(|&term| term.induced_fixed_value(state).is_none())
            .collect::<Vec<_>>();

        let fixed_sum = self
            .terms
            .iter()
            .filter_map(|term| term.induced_fixed_value(state))
            .map(i64::from)
            .sum::<i64>();

        // 1. Check if the constraint is conflicting, which is the case if all terms are fixed and
        //    sum to the bound
        if unfixed_terms.is_empty() {
            if fixed_sum == i64::from(self.bound) {
                log::error!(
                    "The fixed terms {:?} sum to the forbidden value {} of the linear disequality",
                    self.terms,
                    self.bound
                );
                return RetentionCheck::PropagationMissed;
            }

            return RetentionCheck::NothingToPropagate;
        }

        if unfixed_terms.len() >= 2 {
            return RetentionCheck::NothingToPropagate;
        }

        // 2. Assert that the single unfixed term cannot take the value which completes the sum to
        //    the bound
        let unfixed_term = unfixed_terms[0];
        let forbidden = i64::from(self.bound) - fixed_sum;
        let is_removed = match i32::try_from(forbidden) {
            Ok(forbidden) => !unfixed_term.induced_domain_contains(state, forbidden),
            Err(_) => true,
        };

        if is_removed {
            RetentionCheck::NothingToPropagate
        } else {
            log::error!(
                "The value {forbidden} could be removed from {unfixed_term:?} by the linear disequality {:?} != {}",
                self.terms,
                self.bound
            );
            RetentionCheck::PropagationMissed
        }
    }
}
