use super::BinaryNotEqualsChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionChecker;
use crate::VariableState;

impl<Lhs, Rhs, Atomic> RetentionChecker<Atomic> for BinaryNotEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        match (
            self.lhs.induced_fixed_value(state),
            self.rhs.induced_fixed_value(state),
        ) {
            (Some(lhs), Some(rhs)) => {
                // 1. Both sides are fixed: check that the constraint is not conflicting
                if lhs == rhs {
                    log::error!(
                        "{:?} and {:?} are both fixed to {lhs}; the disequality is violated",
                        self.lhs,
                        self.rhs
                    );
                }
                lhs != rhs
            }
            (Some(value), None) => {
                // 2. One side is fixed: assert that its value is removed from the other side
                let is_removed = !self.rhs.induced_domain_contains(state, value);
                if !is_removed {
                    log::error!(
                        "The value {value} could be removed from {:?} since {:?} is fixed to it",
                        self.rhs,
                        self.lhs
                    );
                }
                is_removed
            }
            (None, Some(value)) => {
                // 2. One side is fixed: assert that its value is removed from the other side
                let is_removed = !self.lhs.induced_domain_contains(state, value);
                if !is_removed {
                    log::error!(
                        "The value {value} could be removed from {:?} since {:?} is fixed to it",
                        self.lhs,
                        self.rhs
                    );
                }
                is_removed
            }
            // 3. Neither side is fixed: nothing can be propagated
            (None, None) => true,
        }
    }
}
