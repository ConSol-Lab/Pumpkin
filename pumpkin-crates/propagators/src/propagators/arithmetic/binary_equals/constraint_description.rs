use pumpkin_checking::DomainView;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the constraint `a = b`.
#[derive(Clone, Debug)]
pub struct BinaryEqualsDescription<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}
}

impl<AVar: IntegerVariable, BVar: IntegerVariable> ConstraintDescription
    for BinaryEqualsDescription<AVar, BVar>
{
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(a), Some(b)) = (
            self.a.induced_fixed_value(domains),
            self.b.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::Unknown;
        };

        if a == b {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
