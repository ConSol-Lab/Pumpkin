use pumpkin_checking::DomainView;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the constraint `absolute = |signed|`.
#[derive(Clone, Debug)]
pub struct AbsoluteValueDescription<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
}
}

impl<VA: IntegerVariable, VB: IntegerVariable> ConstraintDescription
    for AbsoluteValueDescription<VA, VB>
{
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(signed), Some(absolute)) = (
            self.signed.induced_fixed_value(domains),
            self.absolute.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::Unknown;
        };

        if i64::from(absolute) == i64::from(signed).abs() {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
