use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::ConflictChecker;

#[derive(Clone, Debug)]
pub struct BinaryNotEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}

impl<Lhs, Rhs, Atomic> ConflictChecker<Atomic> for BinaryNotEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // There is a conflict if both variables are fixed to the same values.

        self.lhs.induced_fixed_value(&state) == self.rhs.induced_fixed_value(&state)
    }
}
