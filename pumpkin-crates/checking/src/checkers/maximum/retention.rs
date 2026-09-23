use super::MaximumChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionChecker;
use crate::VariableState;
use crate::checkers::retention_checker::scope_lower_bound;
use crate::checkers::retention_checker::scope_upper_bound;

impl<ElementVar, Rhs, Atomic> RetentionChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        if self.array.is_empty() {
            return false;
        }

        let rhs_lower = scope_lower_bound(&self.rhs, state);
        let rhs_upper = scope_upper_bound(&self.rhs, state);
        let mut greatest_lower = i32::MIN;
        let mut greatest_upper = i32::MIN;
        for element in self.array.iter() {
            greatest_lower = greatest_lower.max(scope_lower_bound(element, state));
            greatest_upper = greatest_upper.max(scope_upper_bound(element, state));
        }

        // 1. Assert that the bounds of the maximum match the bounds of the array: its lower bound
        //    is at least the greatest lower bound and its upper bound is at most the greatest upper
        //    bound
        if rhs_lower < greatest_lower {
            log::error!(
                "The lower bound of {:?} could be raised to {greatest_lower} by the maximum of {:?}",
                self.rhs,
                self.array
            );
            return false;
        }

        if rhs_upper > greatest_upper {
            log::error!(
                "The upper bound of {:?} could be lowered to {greatest_upper} by the maximum of {:?}",
                self.rhs,
                self.array
            );
            return false;
        }

        // 2. Assert that no element exceeds the upper bound of the maximum
        for element in self.array.iter() {
            if scope_upper_bound(element, state) > rhs_upper {
                log::error!(
                    "The upper bound of {element:?} could be lowered to {rhs_upper}, the upper bound of the maximum {:?}",
                    self.rhs
                );
                return false;
            }
        }

        // 3. If only one element can be at least the lower bound of the maximum then it is the
        //    maximum; assert that its lower bound is at least the lower bound of the maximum, the
        //    upper bound is already equal by 1 and 2
        //  Elements are counted by position, as the propagator does.
        let candidates = self
            .array
            .iter()
            .filter(|&element| scope_upper_bound(element, state) >= rhs_lower)
            .collect::<Vec<_>>();
        if candidates.len() == 1 && scope_lower_bound(candidates[0], state) < rhs_lower {
            log::error!(
                "The lower bound of {:?} could be raised to {rhs_lower}: it is the only element that can attain the maximum {:?}",
                candidates[0],
                self.rhs
            );
            return false;
        }

        true
    }
}
