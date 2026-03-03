use crate::propagation::Domains;
use crate::propagation::checkers::Witness;
#[cfg(doc)]
use crate::propagation::checkers::WitnessedVariable;

/// A witness generator is responsible for constructing witness that a variable can be assigned a
/// value. The created witnesses should be solutions to a single constraint. The solutions are then
/// used by consistency checkers to determine whether propagators are at their claimed level of
/// consistency.
pub trait WitnessGenerator {
    /// Create a [`Witness`] such that the domain of every variable in the scope of the propagator is appropriately supported.
    ///
    /// Depending on whether a propagator is bounds or domain consistent this implementation will
    /// change.
    fn support(&self, domains: Domains<'_>) -> Vec<Witness>;
}
