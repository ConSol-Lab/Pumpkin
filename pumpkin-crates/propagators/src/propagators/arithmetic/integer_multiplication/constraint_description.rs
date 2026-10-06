use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

use super::constructor::ID_A;
use super::constructor::ID_B;
use super::constructor::ID_C;

/// The description of the constraint `a * b = c`.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationDescription<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

impl<VA, VB, VC> ConstraintDescription for IntegerMultiplicationDescription<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn scope(&self) -> Scope {
        Scope::from(((ID_A, &self.a), (ID_B, &self.b), (ID_C, &self.c)))
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(a), Some(b), Some(c)) = (
            self.a.induced_fixed_value(domains),
            self.b.induced_fixed_value(domains),
            self.c.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::UnfixedVariable;
        };

        if i64::from(c) == i64::from(a) * i64::from(b) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
