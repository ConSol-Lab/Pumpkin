use super::MaximumChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::ConflictChecker;
use crate::IntExt;

impl<ElementVar, Rhs, Atomic> ConflictChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(&self, state: crate::VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        let lowest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_lower_bound(&state))
            .max()
            .unwrap_or(IntExt::NegativeInf);
        let highest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_upper_bound(&state))
            .max()
            .unwrap_or(IntExt::PositiveInf);

        // If the intersection between the domain of `rhs` and `[lowest_maximum,
        // highest_maximum]` is empty, there is a conflict.

        lowest_maximum > self.rhs.induced_upper_bound(&state)
            || highest_maximum < self.rhs.induced_lower_bound(&state)
    }
}
