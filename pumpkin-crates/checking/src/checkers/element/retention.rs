use super::ElementChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::IntExt;
use crate::RetentionCheck;
use crate::RetentionChecker;

/// Mirrors one pass of the propagation of the element propagator, which works on the bounds of
/// the elements and the right-hand side and on the domain of the index.
impl<VX, VI, VE, Atomic> RetentionChecker<Atomic> for ElementChecker<VX, VI, VE>
where
    Atomic: AtomicConstraint,
    VX: CheckerVariable<Atomic>,
    VI: CheckerVariable<Atomic>,
    VE: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        // 1. The index selects an element of the array.
        let last_index = self.array.len() as i32 - 1;
        if self.index.induced_lower_bound(state) < 0
            || self.index.induced_upper_bound(state) > last_index
        {
            log::error!(
                "The bounds of the index {:?} could be restricted to [0, {last_index}]",
                self.index
            );
            return RetentionCheck::PropagationMissed;
        }

        let selectable = || {
            self.index
                .iter_induced_domain(state)
                .expect("the index is bounded")
                .map(|index| &self.array[index as usize])
        };

        // 2. The bounds of the right-hand side lie within the bounds of the selectable elements.
        let rhs_lower = self.rhs.induced_lower_bound(state);
        let rhs_upper = self.rhs.induced_upper_bound(state);
        let lowest = selectable()
            .map(|element| element.induced_lower_bound(state))
            .min()
            .unwrap_or(IntExt::PositiveInf);
        let highest = selectable()
            .map(|element| element.induced_upper_bound(state))
            .max()
            .unwrap_or(IntExt::NegativeInf);
        if rhs_lower < lowest || rhs_upper > highest {
            log::error!(
                "The bounds of {:?} could be tightened to [{lowest:?}, {highest:?}] by the elements selectable by {:?}",
                self.rhs,
                self.index
            );
            return RetentionCheck::PropagationMissed;
        }

        // 3. Every selectable element can take a value within the bounds of the right-hand side.
        for index in self
            .index
            .iter_induced_domain(state)
            .expect("the index is bounded")
        {
            let element = &self.array[index as usize];
            if rhs_lower > element.induced_upper_bound(state)
                || rhs_upper < element.induced_lower_bound(state)
            {
                log::error!(
                    "The value {index} could be removed from {:?}: the bounds of {element:?} do not meet those of {:?}",
                    self.index,
                    self.rhs
                );
                return RetentionCheck::PropagationMissed;
            }
        }

        // 4. When the index is fixed, the selected element lies within the bounds of the right-hand
        //    side.
        if let Some(index) = self.index.induced_fixed_value(state) {
            let element = &self.array[index as usize];
            if element.induced_lower_bound(state) < rhs_lower
                || element.induced_upper_bound(state) > rhs_upper
            {
                log::error!(
                    "The bounds of {element:?}, selected by {:?}, could be tightened to those of {:?}",
                    self.index,
                    self.rhs
                );
                return RetentionCheck::PropagationMissed;
            }
        }

        RetentionCheck::NothingToPropagate
    }
}
