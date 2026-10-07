use pumpkin_checking::DomainView;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the constraint `array[index] = rhs`.
#[derive(Clone, Debug)]
pub struct ElementDescription<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
}
}

impl<VX, VI, VE> ConstraintDescription for ElementDescription<VX, VI, VE>
where
    VX: IntegerVariable,
    VI: IntegerVariable,
    VE: IntegerVariable,
{
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let (Some(index), Some(rhs)) = (
            self.index.induced_fixed_value(domains),
            self.rhs.induced_fixed_value(domains),
        ) else {
            return SolutionCheck::Unknown;
        };

        // The index starts at zero.
        let Some(selected) = usize::try_from(index)
            .ok()
            .and_then(|index| self.array.get(index))
        else {
            return SolutionCheck::ConstraintViolated;
        };

        match selected.induced_fixed_value(domains) {
            None => SolutionCheck::Unknown,
            Some(value) if value == rhs => SolutionCheck::ConstraintSatisfied,
            Some(_) => SolutionCheck::ConstraintViolated,
        }
    }
}
