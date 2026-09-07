//! Contains the core structures used to define the decision making procedure of the [`Solver`].
//!
//! The [`Brancher`] trait defines how a branching procedure (which selects an unfixed variable and
//! splits the domain in some way, see [Section 4.3.1 of \[1\]](http://www.cse.unsw.com.au/~tw/brwhkr08.pdf)
//! for more information) should operate; the main method of this trait is the
//! [`Brancher::next_decision`] method. The [`SelectionContext`] is passed to a [`Brancher`] and
//! allows it to inspect the domains of the variables and to make use of a random number generator.
//!
//! Implementations of [`Brancher`] (e.g. based on variable/value selection heuristics) are
//! provided by the `pumpkin-branchers` crate.
//!
//! A [`Brancher`] is expected to be passed to [`Solver::satisfy`], and [`Solver::optimise`]:
//!
//! \[1\] F. Rossi, P. Van Beek, and T. Walsh, Handbook of constraint programming. Elsevier, 2006.

mod brancher;
mod selection_context;
pub mod testing;

pub use brancher::*;
pub use selection_context::SelectionContext;

#[cfg(doc)]
use crate::Solver;
