//! This crate contains abstractions for dealing with the Deletion Reverse Constraint Propagation
//! (DRCP) proof format. The format can be used by Constraint Programming solvers to provide a
//! certifyable proof of unsatisfiability or optimality.
//!
//! To write DRCP proof files, look at the [`ProofWriter`] documentation. Literal definition files
//! can be written using [`LiteralDefinitions`].

mod atomic;
mod encountered_literals;
mod format;
mod literal_code_provider;
mod writer;

pub mod steps;

pub use atomic::*;
pub use encountered_literals::*;
pub use format::*;
pub use literal_code_provider::*;
pub use writer::*;
