#[cfg(doc)]
use crate::propagation::ConstraintDescription;

/// The outcome of [`ConstraintDescription::check_solution`].
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolutionCheck {
    /// The constraint holds, whatever values the variables that are not fixed take.
    ConstraintSatisfied,
    /// The constraint is violated, whatever values the variables that are not fixed take.
    ConstraintViolated,
    /// Whether the constraint holds depends on variables that are not fixed.
    Unknown,
}
