//! Contains the branchers of the Pumpkin constraint programming solver.
//!
//! In general, it provides implementations of 3 traits (defined in `pumpkin-core`):
//! - [`Brancher`] implementations (see [`branchers`]) which define how a branching procedure (which
//!   selects an unfixed variable and splits the domain in some way) should operate; an example
//!   implementation is the [`IndependentVariableValueBrancher`].
//! - [`VariableSelector`] implementations (see [`variable_selection`]) which define how a variable
//!   is selected; an example implementation is [`AntiFirstFail`].
//! - [`ValueSelector`] implementations (see [`value_selection`]) which define how a value is
//!   selected for the variable chosen by a [`VariableSelector`].
//!
//! It also provides [`tie_breaking`] strategies which can be used by [`VariableSelector`]s.

pub mod branchers;
pub mod tie_breaking;
pub mod value_selection;
pub mod variable_selection;

mod default_brancher;
#[cfg(test)]
pub(crate) mod testing;

pub use default_brancher::DefaultBrancher;
#[cfg(doc)]
use pumpkin_core::branching::Brancher;

#[cfg(doc)]
use crate::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
#[cfg(doc)]
use crate::value_selection::ValueSelector;
#[cfg(doc)]
use crate::variable_selection::AntiFirstFail;
#[cfg(doc)]
use crate::variable_selection::VariableSelector;
