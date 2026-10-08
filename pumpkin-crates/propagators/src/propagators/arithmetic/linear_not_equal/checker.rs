use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

#[derive(Debug, Clone)]
pub struct LinearNotEqualChecker<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}

impl<Var, Atomic> ConflictChecker<Atomic> for LinearNotEqualChecker<Var>
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
