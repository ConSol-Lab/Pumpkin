use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

use super::ID_DENOMINATOR;
use super::ID_NUMERATOR;
use super::ID_RHS;

/// The description of the constraint `numerator / denominator = rhs`.
#[derive(Clone, Debug)]
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
    fn scope(&self) -> Scope {
        Scope::from((
            (ID_NUMERATOR, &self.numerator),
            (ID_DENOMINATOR, &self.denominator),
            (ID_RHS, &self.rhs),
        ))
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(numerator), Some(denominator), Some(rhs)) = (
            self.numerator.induced_fixed_value(domains),
            self.denominator.induced_fixed_value(domains),
            self.rhs.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::Unknown;
        };

        // The division truncates towards zero.
        if denominator != 0 && i64::from(rhs) == i64::from(numerator) / i64::from(denominator) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
