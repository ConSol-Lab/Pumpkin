use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

#[derive(Debug, Clone)]
pub struct LinearLessOrEqualConflictChecker<Var> {
    terms: Box<[Var]>,
    bound: i32,
}

impl<Var> LinearLessOrEqualConflictChecker<Var> {
    pub fn new(terms: Box<[Var]>, bound: i32) -> Self {
        LinearLessOrEqualConflictChecker { terms, bound }
    }
}

impl<Var, Atomic> ConflictChecker<Atomic> for LinearLessOrEqualConflictChecker<Var>
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
