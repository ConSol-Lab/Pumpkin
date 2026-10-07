use pumpkin_checking::DomainView;

use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;

crate::scoped_struct! {
/// The description of a nogood: the atomic constraints that cannot all hold at once.
#[derive(Clone, Debug)]
pub struct NogoodDescription {
    pub nogood: Box<[Predicate]>,
}
}

impl ConstraintDescription for NogoodDescription {
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        if self
            .nogood
            .iter()
            .any(|&predicate| domains.is_true(&!predicate))
        {
            SolutionCheck::ConstraintSatisfied
        } else if self
            .nogood
            .iter()
            .all(|predicate| domains.is_true(predicate))
        {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::Unknown
        }
    }
}
