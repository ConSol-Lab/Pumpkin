use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `numerator / denominator = rhs`.
#[derive(Clone, Debug, ScopeItem)]
pub struct DivisionDescription<VA, VB, VC> {
    pub numerator: VA,
    pub denominator: VB,
    pub rhs: VC,
}

impl<VA, VB, VC> ConstraintDescription for DivisionDescription<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let numerator = i64::from(solution.integer_value(&self.numerator));
        let denominator = i64::from(solution.integer_value(&self.denominator));
        let rhs = i64::from(solution.integer_value(&self.rhs));

        // The division truncates towards zero.
        if denominator != 0 && rhs == numerator / denominator {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
