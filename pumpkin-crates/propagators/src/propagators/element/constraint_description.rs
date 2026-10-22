use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `array[index] = rhs`.
#[derive(Clone, Debug, ScopeItem)]
pub struct ElementDescription<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
}

impl<VX, VI, VE> ConstraintDescription for ElementDescription<VX, VI, VE>
where
    VX: IntegerVariable,
    VI: IntegerVariable,
    VE: IntegerVariable,
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        // The index starts at zero; an index outside the array violates the constraint.
        let selected = usize::try_from(solution.integer_value(&self.index))
            .ok()
            .and_then(|index| self.array.get(index));

        if selected.is_some_and(|element| {
            solution.integer_value(element) == solution.integer_value(&self.rhs)
        }) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
