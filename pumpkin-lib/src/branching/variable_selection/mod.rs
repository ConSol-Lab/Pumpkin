//! Provides the [`VariableSelector`] trait which is required
//! for variable selectors to implement; the main method in this trait relies on
//! [`VariableSelector::select_variable`].
//!
//! Furthermore, it defines several implementations of the [`VariableSelector`] trait such as
//! [`Vsids`]. Any [`VariableSelector`] should only select variables which have a domain of size 2
//! or larger.

mod variable_selector;
mod vsids;

pub use variable_selector::VariableSelector;
pub use vsids::*;
