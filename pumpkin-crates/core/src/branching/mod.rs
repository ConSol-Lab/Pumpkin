//! Contains structures and traits to define the decision making procedure of the [`Solver`].
//!
//! In general, it provides 3 traits:
//! - The [`Brancher`] which defines how a branching procedure (which selects an unfixed variable and splits the domain in some way, see [Section 4.3.1 of \[1\]](http://www.cse.unsw.com.au/~tw/brwhkr08.pdf)
//!   for more information) should operate; the main method of this trait is the [`Brancher::next_decision`] method. An example implementation of this trait is the [`IndependentVariableValueBrancher`].
//! - The [`VariableSelector`] which defines the method required of a variable selector (including
//!   the hooks into the solver); the main method of this trait is the
//!   [`VariableSelector::select_variable`] method. An example implementation of this trait is the
//!   [`AntiFirstFail`] strategy.
//! - The [`ValueSelector`] which defines the method required of a value selector (including the
//!   hooks into the solver); the main method of this trait is the [`ValueSelector::select_value`]
//!   method.
//!
//! A [`Brancher`] is expected to be passed to [`Solver::satisfy`], and [`Solver::optimise`]:
//!
//! \[1\] F. Rossi, P. Van Beek, and T. Walsh, Handbook of constraint programming. Elsevier, 2006.

mod brancher;
pub mod branchers;
mod selection_context;
pub mod tie_breaking;
pub mod value_selection;
pub mod variable_selection;

pub use brancher::*;
pub use selection_context::SelectionContext;

#[cfg(doc)]
use crate::Solver;
#[cfg(doc)]
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
#[cfg(doc)]
use crate::branching::value_selection::ValueSelector;
#[cfg(doc)]
use crate::branching::variable_selection::AntiFirstFail;
#[cfg(doc)]
use crate::branching::variable_selection::VariableSelector;
