use std::rc::Rc;

use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the linear disequality `∑ terms_i != rhs`.
#[derive(Clone, Debug, ScopeItem)]
pub struct LinearNotEqualDescription<Var> {
    /// The terms of the sum
    pub terms: Rc<[Var]>,
    /// The right-hand side of the sum
    pub rhs: i32,
}

impl<Var: IntegerVariable> ConstraintDescription for LinearNotEqualDescription<Var> {
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let sum = self
            .terms
            .iter()
            .map(|term| i64::from(solution.integer_value(term)))
            .sum::<i64>();

        if sum != i64::from(self.rhs) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
