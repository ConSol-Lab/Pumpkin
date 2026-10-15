use crate::checkers::CompleteSolution;
use crate::checkers::ScopeItem;
use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;

/// The description of a nogood: the atomic constraints that cannot all hold at once.
#[derive(Clone, Debug, ScopeItem)]
pub struct NogoodDescription {
    pub nogood: Box<[Predicate]>,
}

impl ConstraintDescription for NogoodDescription {
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        if !self
            .nogood
            .iter()
            .all(|&predicate| solution.predicate_holds(predicate))
        {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
