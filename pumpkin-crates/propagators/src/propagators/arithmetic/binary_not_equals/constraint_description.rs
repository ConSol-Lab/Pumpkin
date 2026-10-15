use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `a != b`.
#[derive(Clone, Debug, ScopeItem)]
pub struct BinaryNotEqualsDescription<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}

impl<AVar: IntegerVariable, BVar: IntegerVariable> ConstraintDescription
    for BinaryNotEqualsDescription<AVar, BVar>
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        if solution.integer_value(&self.a) != solution.integer_value(&self.b) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
