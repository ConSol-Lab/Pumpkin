use std::collections::BTreeSet;

use super::BinaryEqualsChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::IntExt;
use crate::RetentionCheck;
use crate::RetentionChecker;

impl<Lhs, Rhs, Atomic> RetentionChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        // 1. Assert that the bounds are equal
        let lower = self.lhs.induced_lower_bound(state);
        let upper = self.lhs.induced_upper_bound(state);
        let same_bounds = lower == self.rhs.induced_lower_bound(state)
            && upper == self.rhs.induced_upper_bound(state);
        // 2. Assert that the holes within the bounds are equal
        let are_equal = same_bounds
            && holes_within(state, &self.lhs, lower, upper)
                == holes_within(state, &self.rhs, lower, upper);

        if are_equal {
            RetentionCheck::NothingToPropagate
        } else {
            log::error!(
                "The domains of {:?} and {:?} differ although the two are equal: {:?} and {:?}",
                self.lhs,
                self.rhs,
                self.lhs
                    .iter_induced_domain(state)
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>(),
                self.rhs
                    .iter_induced_domain(state)
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>()
            );
            RetentionCheck::PropagationMissed
        }
    }
}

fn holes_within<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    state: &dyn DomainView<Atomic>,
    variable: &Var,
    lower: IntExt,
    upper: IntExt,
) -> BTreeSet<i32> {
    variable
        .induced_holes(state)
        .filter(|&value| lower <= value && value <= upper)
        .collect()
}
