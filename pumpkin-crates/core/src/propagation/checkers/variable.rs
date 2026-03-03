use crate::propagation::checkers::SingleVariableAssignment;
#[cfg(doc)]
use crate::propagation::checkers::Witness;

/// A variable that can be part of a [`Witness`].
pub trait WitnessedVariable {
    /// Create a [`SingleVariableAssignment`] representing `self = value`.
    fn assign(&self, value: i32) -> SingleVariableAssignment;
}
