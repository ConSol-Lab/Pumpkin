use super::BinaryEqualsChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::ConflictChecker;
use crate::IntExt;

impl<Lhs, Rhs, Atomic> ConflictChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        mut state: crate::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // We apply the domain of variable 2 to variable 1. If the state remains consistent, then
        // the step is unsound!
        let mut consistent = true;

        if let IntExt::Int(value) = self.rhs.induced_upper_bound(&state) {
            let atomic = self.lhs.atomic_less_than(value);
            consistent &= state.apply(&atomic);
        }

        if let IntExt::Int(value) = self.rhs.induced_lower_bound(&state) {
            let atomic = self.lhs.atomic_greater_than(value);
            consistent &= state.apply(&atomic);
        }

        for value in self.rhs.induced_holes(&state).collect::<Vec<_>>() {
            let atomic = self.lhs.atomic_not_equal(value);
            consistent &= state.apply(&atomic);
        }

        !consistent
    }
}
