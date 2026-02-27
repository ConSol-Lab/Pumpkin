use crate::propagation::checkers::SingleVariableAssignment;
#[cfg(doc)]
use crate::propagation::checkers::Witness;

/// A variable that can be part of a [`Witness`].
pub trait WitnessedVariable {
    /// Convert a [`ValueToWitness`] to an integer.
    ///
    /// See the documentation for [`ValueToWitness`] for why this abstraction is necessary.
    fn unpack_value(&self, value: ValueToWitness) -> i32;

    /// Create a [`SingleVariableAssignment`] representing `self = value`.
    fn assign(&self, value: i32) -> SingleVariableAssignment;
}

/// Models a value that needs to be unpacked by a [`WitnessedVariable`].
///
/// Since propagators (and by extension, propagation checkers) will often use views to abstract
/// over the variable type, it is difficult to relate the domain_id to the logical variable in the
/// propagator or checker. This is why the [`ValueToWitness`] exists, as it allows the view to map
/// the value according to the view implementation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueToWitness(i32);

impl From<i32> for ValueToWitness {
    fn from(value: i32) -> Self {
        ValueToWitness(value)
    }
}

impl ValueToWitness {
    pub(crate) fn unpack(&self) -> i32 {
        self.0
    }
}
