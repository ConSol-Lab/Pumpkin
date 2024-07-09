//! Provides structures and traits to define the decision making procedure of the solver.
//!
//! The example in the docs has been temporary removed due to the new version.

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
