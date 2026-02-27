use crate::propagation::Domains;
use crate::propagation::LocalId;
use crate::propagation::checkers::ValueToWitness;
use crate::propagation::checkers::Witness;
#[cfg(doc)]
use crate::propagation::checkers::WitnessedVariable;

/// A witness generator is responsible for constructing witness that a variable can be assigned a
/// value. The created witnesses should be solutions to a single constraint. The solutions are then
/// used by consistency checkers to determine whether propagators are at their claimed level of
/// consistency.
pub trait WitnessGenerator {
    /// Create a [`Witness`] such that the variable identified by `local_id` is assigned to `value`.
    ///
    /// Use [`WitnessedVariable::unpack_value`] to convert the [`ValueToWitness`] to an `i32`.
    fn support(&self, domains: &Domains<'_>, local_id: LocalId, value: ValueToWitness) -> Witness;
}
