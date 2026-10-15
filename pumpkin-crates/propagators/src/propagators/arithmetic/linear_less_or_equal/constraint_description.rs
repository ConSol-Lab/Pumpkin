use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the linear inequality `∑ terms_i <= bound`.
#[derive(Clone, Debug, ScopeItem)]
pub struct LinearLessOrEqualDescription<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}

impl<Var: IntegerVariable> ConstraintDescription for LinearLessOrEqualDescription<Var> {
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let sum = self
            .terms
            .iter()
            .map(|term| i64::from(solution.integer_value(term)))
            .sum::<i64>();

        if sum <= i64::from(self.bound) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
