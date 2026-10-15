use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `a * b = c`.
#[derive(Clone, Debug, ScopeItem)]
pub struct IntegerMultiplicationDescription<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

impl<VA, VB, VC> ConstraintDescription for IntegerMultiplicationDescription<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let a = i64::from(solution.integer_value(&self.a));
        let b = i64::from(solution.integer_value(&self.b));
        let c = i64::from(solution.integer_value(&self.c));

        if c == a * b {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
