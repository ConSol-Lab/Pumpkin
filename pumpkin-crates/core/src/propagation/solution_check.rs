#[cfg(doc)]
use crate::propagation::ConstraintDescription;

/// The outcome of [`ConstraintDescription::check_solution`].
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolutionCheck {
    ConstraintSatisfied,
    ConstraintViolated,
}
