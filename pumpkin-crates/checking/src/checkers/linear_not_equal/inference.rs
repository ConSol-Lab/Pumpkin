use super::LinearNotEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

impl<Var, Atomic> InferenceChecker<Atomic> for LinearNotEqualChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        // We evaluate the linear sum. It should be fixed to the bound for a conflict to
        // exist.
        let mut left_hand_side = IntExt::Int(0);

        for term in self.terms.iter() {
            let Some(value) = term.induced_fixed_value(&state) else {
                return false;
            };

            left_hand_side += i64::from(value);
        }

        left_hand_side == i64::from(self.bound)
    }
}
