use super::MaximumChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::RetentionCheck;
use crate::RetentionChecker;

impl<ElementVar, Rhs, Atomic> RetentionChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        if self.array.is_empty() {
            return RetentionCheck::PropagationMissed;
        }

        let rhs_lower = self.rhs.induced_lower_bound(state);
        let rhs_upper = self.rhs.induced_upper_bound(state);
        let greatest_lower = self
            .array
            .iter()
            .map(|element| element.induced_lower_bound(state))
            .max()
            .expect("the array is not empty");
        let greatest_upper = self
            .array
            .iter()
            .map(|element| element.induced_upper_bound(state))
            .max()
            .expect("the array is not empty");

        // 1. Assert that the bounds of the maximum match the bounds of the array: its lower bound
        //    is at least the greatest lower bound and its upper bound is at most the greatest upper
        //    bound
        if rhs_lower < greatest_lower {
            log::error!(
                "The lower bound of {:?} could be raised to {greatest_lower:?} by the maximum of {:?}",
                self.rhs,
                self.array
            );
            return RetentionCheck::PropagationMissed;
        }

        if rhs_upper > greatest_upper {
            log::error!(
                "The upper bound of {:?} could be lowered to {greatest_upper:?} by the maximum of {:?}",
                self.rhs,
                self.array
            );
            return RetentionCheck::PropagationMissed;
        }

        // 2. Assert that no element exceeds the upper bound of the maximum
        for element in self.array.iter() {
            if element.induced_upper_bound(state) > rhs_upper {
                log::error!(
                    "The upper bound of {element:?} could be lowered to {rhs_upper:?}, the upper bound of the maximum {:?}",
                    self.rhs
                );
                return RetentionCheck::PropagationMissed;
            }
        }

        // 3. If only one element can reach the lower bound of the maximum, it is the maximum:
        //    assert that its lower bound is at least that of the maximum. Steps 1 and 2 cover its
        //    upper bound.
        //  Elements are counted by position, as the propagator does.
        let candidates = self
            .array
            .iter()
            .filter(|&element| element.induced_upper_bound(state) >= rhs_lower)
            .collect::<Vec<_>>();
        if candidates.len() == 1 && candidates[0].induced_lower_bound(state) < rhs_lower {
            log::error!(
                "The lower bound of {:?} could be raised to {rhs_lower:?}: it is the only element that can attain the maximum {:?}",
                candidates[0],
                self.rhs
            );
            return RetentionCheck::PropagationMissed;
        }

        RetentionCheck::NothingToPropagate
    }
}
