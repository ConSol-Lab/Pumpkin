use super::LinearLessOrEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

impl<Var, Atomic> InferenceChecker<Atomic> for LinearLessOrEqualChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        variable_state: VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // Next, we evaluate the linear inequality. The lower bound of the
        // left-hand side must exceed the bound in the constraint. Note that the accumulator is an
        // IntExt, and if the lower bound of one of the terms is -infty, then the left-hand side
        // will be -infty regardless of the other terms.
        let left_hand_side: IntExt<i64> = self
            .terms
            .iter()
            .map(|variable| variable.induced_lower_bound(&variable_state).into())
            .sum();

        left_hand_side > i64::from(self.bound)
    }
}
