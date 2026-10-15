use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `rhs = max(array)`.
#[derive(Clone, Debug, ScopeItem)]
pub struct MaximumDescription<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> ConstraintDescription
    for MaximumDescription<ElementVar, Rhs>
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let maximum = self
            .array
            .iter()
            .map(|element| solution.integer_value(element))
            .max();

        if maximum == Some(solution.integer_value(&self.rhs)) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
