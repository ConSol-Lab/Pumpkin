use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `a != b`.
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsDescription<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}

impl<AVar: IntegerVariable, BVar: IntegerVariable> ConstraintDescription
    for BinaryNotEqualsDescription<AVar, BVar>
{
    fn scope(&self) -> Scope {
        Scope::from(((LocalId::from(0), &self.a), (LocalId::from(1), &self.b)))
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(a), Some(b)) = (
            self.a.induced_fixed_value(domains),
            self.b.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::Unknown;
        };

        if a == b {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::ConstraintSatisfied
        }
    }
}
