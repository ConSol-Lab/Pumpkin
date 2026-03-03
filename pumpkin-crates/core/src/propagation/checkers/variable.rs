use crate::propagation::LocalId;
use crate::propagation::checkers::ScopeBuilder;
use crate::propagation::checkers::SingleVariableAssignment;
#[cfg(doc)]
use crate::propagation::checkers::Witness;

/// A variable that can be part of a [`Witness`].
pub trait WitnessedVariable {
    /// Add self to the scope.
    fn add_to_scope(&self, scope: &mut ScopeBuilder, local_id: LocalId);

    /// Create a [`SingleVariableAssignment`] representing `self = value`.
    fn assign(&self, value: i32) -> SingleVariableAssignment;
}
