//! Provides several implementations of [`Brancher`]s.

pub mod alternating;
pub mod autonomous_search;
pub mod dynamic_brancher;
pub mod independent_variable_value_brancher;
pub mod warm_start;
#[cfg(doc)]
use super::Brancher;
