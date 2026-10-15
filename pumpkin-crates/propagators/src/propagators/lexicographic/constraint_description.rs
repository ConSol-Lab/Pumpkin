use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint that `x` is lexicographically at most `y`.
#[derive(Clone, Debug, ScopeItem)]
pub struct LexLessOrEqualDescription<Var> {
    pub x: Box<[Var]>,
    pub y: Box<[Var]>,
}

impl<Var: IntegerVariable> ConstraintDescription for LexLessOrEqualDescription<Var> {
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        for (x, y) in self.x.iter().zip(self.y.iter()) {
            let (x, y) = (solution.integer_value(x), solution.integer_value(y));
            if x < y {
                return SolutionCheck::ConstraintSatisfied;
            }
            if x > y {
                return SolutionCheck::ConstraintViolated;
            }
        }
        SolutionCheck::ConstraintSatisfied
    }
}
