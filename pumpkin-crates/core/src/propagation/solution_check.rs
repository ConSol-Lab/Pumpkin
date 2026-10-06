#[cfg(doc)]
use crate::propagation::ConstraintDescription;

/// The outcome of [`ConstraintDescription::check_solution`].
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolutionCheck {
    /// Every variable of the constraint is fixed, and the values satisfy the constraint.
    ConstraintSatisfied,
    /// Every variable of the constraint is fixed, and the values violate the constraint.
    ConstraintViolated,
    /// A variable of the constraint is not fixed, so the domains are not a solution.
    UnfixedVariable,
}
