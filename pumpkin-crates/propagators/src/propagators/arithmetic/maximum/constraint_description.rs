use pumpkin_checking::DomainView;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the constraint `rhs = max(array)`.
#[derive(Clone, Debug)]
pub struct MaximumDescription<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> ConstraintDescription
    for MaximumDescription<ElementVar, Rhs>
{
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(elements), Some(rhs)) = (
            self.array
                .iter()
                .map(|element| element.induced_fixed_value(domains))
                .collect::<Option<Vec<_>>>(),
            self.rhs.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::Unknown;
        };

        if elements.iter().max() == Some(&rhs) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
