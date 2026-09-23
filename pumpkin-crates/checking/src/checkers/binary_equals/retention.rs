use std::collections::BTreeSet;

use super::BinaryEqualsChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionChecker;
use crate::VariableState;
use crate::checkers::retention_checker::scope_lower_bound;
use crate::checkers::retention_checker::scope_upper_bound;

impl<Lhs, Rhs, Atomic> RetentionChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        // 1. Assert that the bounds are equal
        let lower = scope_lower_bound(&self.lhs, state);
        let upper = scope_upper_bound(&self.lhs, state);
        let same_bounds = lower == scope_lower_bound(&self.rhs, state)
            && upper == scope_upper_bound(&self.rhs, state);
        // 2. Assert that the holes within the bounds are equal
        let are_equal = same_bounds
            && holes_within(state, &self.lhs, lower, upper)
                == holes_within(state, &self.rhs, lower, upper);

        if !are_equal {
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
        }

        are_equal
    }
}

fn holes_within<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    state: &VariableState<Atomic>,
    variable: &Var,
    lower: i32,
    upper: i32,
) -> BTreeSet<i32> {
    variable
        .induced_holes(state)
        .filter(|&value| lower <= value && value <= upper)
        .collect()
}
