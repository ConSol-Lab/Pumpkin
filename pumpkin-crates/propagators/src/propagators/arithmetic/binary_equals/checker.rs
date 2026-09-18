use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;

#[derive(Clone, Debug)]
pub struct BinaryEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}

impl<Lhs, Rhs, Atomic> InferenceChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        mut state: pumpkin_checking::VariableState<Atomic>,
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
