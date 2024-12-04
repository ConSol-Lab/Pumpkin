//! Provides the [`VariableSelector`] trait which is required
//! for variable selectors to implement; the main method in this trait relies on
//! [`VariableSelector::select_variable`].
//!
//! Furthermore, it defines several implementations of the [`VariableSelector`] trait. Any
//! [`VariableSelector`] should only select variables which have a domain of size 2 or larger.

mod anti_first_fail;
mod dynamic_variable_selector;
mod first_fail;
mod input_order;
mod largest;
mod max_regret;
mod most_constrained;
mod occurrence;
mod proportional_domain_size;
mod smallest;
mod variable_selector;

pub use anti_first_fail::*;
pub use dynamic_variable_selector::*;
pub use first_fail::*;
pub use input_order::*;
pub use largest::*;
pub use max_regret::*;
pub use most_constrained::*;
pub use occurrence::*;
pub use proportional_domain_size::*;
pub use smallest::*;
pub use variable_selector::VariableSelector;
