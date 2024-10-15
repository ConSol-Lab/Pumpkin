//! This crate contains abstractions for dealing with the Deletion Reverse Constraint Propagation
//! (DRCP) proof format. The format can be used by Constraint Programming solvers to provide a
//! certifyable proof of unsatisfiability or optimality.
//!
//! To write DRCP proof files, look at the [`writer::ProofWriter`] documentation. Literal definition
//! files can be written using [`writer::LiteralDefinitions`].

mod format;
pub mod reader;
pub mod writer;

pub mod steps;

pub use format::*;
