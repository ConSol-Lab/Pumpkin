use super::BinaryNotEqualsChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::VariableState;

impl<Lhs, Rhs, Atomic> InferenceChecker<Atomic> for BinaryNotEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        // There is a conflict if both variables are fixed to the same values.

        self.lhs.induced_fixed_value(&state) == self.rhs.induced_fixed_value(&state)
    }
}
