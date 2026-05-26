use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;

#[derive(Clone, Debug)]
pub struct MaximumChecker<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar, Rhs, Atomic> InferenceChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
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
