//! Utilities for propagators of the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html)
//! constraint which are generalisable enough to be useful for different types of cumulative
//! propagators

mod cumulative_structs;
pub use cumulative_structs::*;

pub mod util;
pub use util::*;

mod sparse_set;
pub use sparse_set::*;
