//! Defines the constraints that Pumpkin provides out of the box which can be added to the
//! [`Solver`].
//!
//! A constraint is a relation over variables. In the solver, constraints are enforced through
//! propagators, and therefore constraints can be viewed as a collection of propagators.
//!
//! # Example
//! ```
//! # use pumpkin_core::constraints;
//! # use pumpkin_core::Solver;
//! let mut solver = Solver::default();
//!
//! let a = solver.new_bounded_integer(0, 3);
//! let b = solver.new_bounded_integer(0, 3);
//!
//! // All constraints require a constraint tag.
//! let constraint_tag = solver.new_constraint_tag();
//!
//! solver
//!     .add_constraint(pumpkin_constraints::equals([a, b], 0, constraint_tag))
//!     .post();
//! ```
//!
//! # Note
//! At the moment, the API for posting propagators is not yet publicly accessible as it is
//! unstable. Consumers of the Pumpkin library can therefore only define constraints by
//! decomposing them into the constraints that are predefined in the library. Once the
//! propagator API is stabilized, it will become part of the public API.

mod all_different;
mod arithmetic;
mod boolean;
mod clause;
mod cumulative;
mod disjunctive_strict;
mod element;
mod table;

pub use all_different::*;
pub use arithmetic::*;
pub use boolean::*;
pub use clause::*;
pub use cumulative::*;
pub use disjunctive_strict::*;
pub use element::*;
pub use table::*;
