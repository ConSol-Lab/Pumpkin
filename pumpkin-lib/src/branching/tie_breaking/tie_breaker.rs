#[cfg(doc)]
use crate::branching::VariableSelector;

/// The interface for a tie-breaker which considers additional elements with values; depending on
/// the [`Direction`] it should only consider variables with the "best" value for selection.
pub trait TieBreaker<Var, Value> {
    /// Consider the next additional element with corresponding value
    fn consider(&mut self, variable: Var, value: Value);

    /// Get the final variable which was selected. After this method is called it resets the stored
    /// values such that it can be used again. This resetting is done to prevent the tie-breaker
    /// from returning a variable which has a value which is out-of-date.
    fn select(&mut self) -> Option<Var>;

    /// Returns whether the tie-breaker is attempting to find the minimum ([`Direction::Minimum`])
    /// or maximum ([`Direction::Maximum`]) element.
    fn get_direction(&self) -> Direction;
}

/// Whether the value comparison should find the maximum [`Direction::Maximum`] variable or the
/// [`Direction::Minimum`] variable.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Direction {
    Maximum,
    Minimum,
}
