//! This crate contains abstractions for dealing with the Deletion Reverse Constraint Propagation
//! (DRCP) proof format. The format can be used by Constraint Programming solvers to provide a
//! certifyable proof of unsatisfiability or optimality.
//!
//! To read DRCP files see [`reader::ProofReader`], and to write DRCP files see
//! [`writer::ProofWriter`]. Literal definitions (`.lits`) files can be read and parsed with
//! [`LiteralDefinitions`].

mod atomic;
mod format;
mod literal_definitions;

pub mod reader;
pub mod steps;
pub mod writer;

pub use atomic::*;
pub use format::*;
pub use literal_definitions::*;
