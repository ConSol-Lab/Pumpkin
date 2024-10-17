//! Utilities for propagators of the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html)
//! constraint which are generalisable enough to be useful for different types of cumulative
//! propagators

mod structs;
pub(crate) use structs::*;

pub(crate) mod util;

mod sparse_set;
pub(crate) use sparse_set::*;
