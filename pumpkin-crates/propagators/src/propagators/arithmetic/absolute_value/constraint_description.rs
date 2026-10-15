use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `absolute = |signed|`.
#[derive(Clone, Debug, ScopeItem)]
pub struct AbsoluteValueDescription<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
}

impl<VA: IntegerVariable, VB: IntegerVariable> ConstraintDescription
    for AbsoluteValueDescription<VA, VB>
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let signed = i64::from(solution.integer_value(&self.signed));
        let absolute = i64::from(solution.integer_value(&self.absolute));

        if absolute == signed.abs() {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
