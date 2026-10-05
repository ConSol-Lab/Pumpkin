use super::ElementChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::Domain;
use crate::InferenceChecker;
use crate::VariableState;

impl<VX, VI, VE, Atomic> InferenceChecker<Atomic> for ElementChecker<VX, VI, VE>
where
    Atomic: AtomicConstraint,
    VX: CheckerVariable<Atomic>,
    VI: CheckerVariable<Atomic>,
    VE: CheckerVariable<Atomic>,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        self.union.borrow_mut().reset();

        // A domain consistent checker for element does the following:
        // 1. Determine the elements in the array whose index is in the domain of the index
        //    variable.
        // 2. Take the union of the domains of those elements.
        // 3. Intersect that union with the domain on the right-hand side.
        //
        // The intersection should be empty for a conflict to exist.
        let supported_elements: Vec<_> = self
            .array
            .iter()
            .enumerate()
            .filter(|(idx, _)| self.index.induced_domain_contains(&state, *idx as i32))
            .map(|(_, element)| element)
            .collect();

        for element in supported_elements {
            self.union.borrow_mut().add(&state, element);
        }

        assert!(
            self.union.borrow().is_consistent(),
            "at least one element has a non-empty domain or else variable state would be inconsistent"
        );

        // Compute `|union cap rhs| == 0`.
        let intersection_lower_bound = self
            .union
            .borrow()
            .lower_bound()
            .max(self.rhs.induced_lower_bound(&state));
        let intersection_upper_bound = self
            .union
            .borrow()
            .upper_bound()
            .min(self.rhs.induced_upper_bound(&state));
        let holes = self
            .union
            .borrow()
            .holes()
            .chain(self.rhs.induced_holes(&state))
            .collect();

        let intersected_domain =
            Domain::new(intersection_lower_bound, intersection_upper_bound, holes);

        !intersected_domain.is_consistent()
    }
}
