//! Contains structures and traits to define the decision making procedure of the [`Solver`].
//!
//! In general, it provides 3 traits:
//! - The [`Brancher`] which defines how a branching procedure (which selects an unfixed variable and splits the domain in some way, see [Section 4.3.1 of \[1\]](http://www.cse.unsw.com.au/~tw/brwhkr08.pdf)
//!   for more information) should operate; the main method of this trait is the [`Brancher::next_decision`] method. An example implementation of this trait is the [`IndependentVariableValueBrancher`].
//! - The [`VariableSelector`] which defines the method required of a variable selector (including
//!   the hooks into the solver); the main method of this trait is the
//!   [`VariableSelector::select_variable`] method. An example implementation of this trait is the
//!   [`Vsids`] strategy.
//! - The [`ValueSelector`] which defines the method required of a value selector (including the
//!   hooks into the solver); the main method of this trait is the [`ValueSelector::select_value`]
//!   method.
//!
//! A [`Brancher`] is expected to be passed to [`Solver::satisfy`], [`Solver::maximise`], and
//! [`Solver::minimise`]:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::variables::PropositionalVariable;
//! # use pumpkin_lib::branching::variable_selection::Vsids;
//! # use pumpkin_lib::branching::value_selection::PhaseSaving;
//! # use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
//! # use pumpkin_lib::variables::Literal;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::SatisfactionResult;
//! # use crate::pumpkin_lib::results::ProblemSolution;
//! let mut solver = Solver::default();
//!
//! let variables = vec![solver.new_literal().get_propositional_variable()];
//!
//! let mut termination = Indefinite;
//! let mut brancher = IndependentVariableValueBrancher::new(
//!     Vsids::new(&variables),
//!     PhaseSaving::new(&variables),
//! );
//! let result = solver.satisfy(&mut brancher, &mut termination);
//! if let SatisfactionResult::Satisfiable(solution) = result {
//!     // Getting the value of the literal in the solution should not panic
//!     variables.into_iter().for_each(|variable| {
//!         solver.get_literal_value(Literal::new(variable, true));
//!     });
//! } else {
//!     panic!("Solving should have returned satsifiable")
//! }
//! ```
//!
//!
//! A default implementation of a [`Brancher`]
//! is provided using the method
//! [`Solver::default_brancher_over_all_propositional_variables`].
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::variables::PropositionalVariable;
//! # use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
//! # use pumpkin_lib::variables::Literal;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::SatisfactionResult;
//! # use crate::pumpkin_lib::results::ProblemSolution;
//! let mut solver = Solver::default();
//!
//! let literals = vec![solver.new_literal()];
//!
//! let mut termination = Indefinite;
//! let mut brancher = solver.default_brancher_over_all_propositional_variables();
//! let result = solver.satisfy(&mut brancher, &mut termination);
//! if let SatisfactionResult::Satisfiable(solution) = result {
//!     // Getting the value of the literal in the solution should not panic
//!     literals.into_iter().for_each(|literal| {
//!         solution.get_literal_value(literal);
//!     })
//! } else {
//!     panic!("Solving should have returned satsifiable")
//! }
//! ```
//!
//! \[1\] F. Rossi, P. Van Beek, and T. Walsh, Handbook of constraint programming. Elsevier, 2006.

mod brancher;
pub mod branchers;
mod selection_context;
pub mod tie_breaking;
pub mod value_selection;
pub mod variable_selection;

pub use brancher::Brancher;
pub use selection_context::SelectionContext;
pub use tie_breaking::*;
pub use value_selection::*;
pub use variable_selection::*;

#[cfg(doc)]
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
#[cfg(doc)]
use crate::branching::value_selection::ValueSelector;
#[cfg(doc)]
use crate::branching::variable_selection::VariableSelector;
#[cfg(doc)]
use crate::Solver;
