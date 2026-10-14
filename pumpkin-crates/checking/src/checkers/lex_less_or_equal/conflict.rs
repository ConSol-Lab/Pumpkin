use super::LexLessOrEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::ConflictCheck;
use crate::ConflictChecker;
use crate::VariableState;

/// The domains are a conflict when, within the bounds, the smallest possible `x` is
/// lexicographically greater than the largest possible `y`: at some position the lower bound of
/// `x` exceeds the upper bound of `y`, and at every earlier position the two bounds are equal.
impl<Var, Atomic> ConflictChecker<Atomic> for LexLessOrEqualChecker<Var>
where
    Atomic: AtomicConstraint,
    Var: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: &VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> ConflictCheck {
        for (x, y) in self.x.iter().zip(self.y.iter()) {
            let lower = x.induced_lower_bound(state);
            let upper = y.induced_upper_bound(state);
            if lower < upper {
                return ConflictCheck::NoConflictDetected;
            }
            if lower > upper {
                return ConflictCheck::ConflictDetected;
            }
        }

        // Every position can be equal, which satisfies the constraint.
        ConflictCheck::NoConflictDetected
    }
}
