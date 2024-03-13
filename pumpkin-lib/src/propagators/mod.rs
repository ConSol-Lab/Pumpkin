//! Contains the propagators which are used by the Pumpkin solver
//!
//! # Theoretical
//!
//! A propagator takes as input a set of variables (`x_i ∈ X`) and for each variable a corresponding
//! domain (`D_i ∈ D`); it can then be seen as a function which maps `D ↦ D'` such that `D'_i ⊆ D_i`
//! for all variables (i.e. the domain of a variable either remains the same after applying the
//! propagator or it becomes a subset of the domain before applying the propagator).
//!
//! An example of a propagator can be the simple [not equal][NotEq] (`!=`) propagator, suppose that
//! we have two variables `x ∈ {0}` and `y ∈ {0, 1}` and the constraint `x != y`. The not equal
//! propagator will then take as input the variables `x` and `y` and their respective domains
//! `D = {D_x = {0}, D_y = {0, 1}` and produce a new domain `D' = {D'_x = {0}, D'_y = {1}}` for
//! which we can see that `D_x = D'_x` and `D'_y ⊆ D_y`.
//!
//! A propagator is said to be at fix-point if `D_x = D'_x` meaning that no further propagations
//! can take place when applying the propagator. A propagator is said to be "idempotent" if a single
//! call to it will result in it being at fix-point.
//!
//! For more information about the construction of these types of propagation-based solvers, we
//! refer to [\[1\]](https://dl.acm.org/doi/pdf/10.1145/1452044.1452046).
//!
//! # Practical
//!
//! For every propagator, it is expected that it implements both [`PropagatorConstructor`] which
//! is used for creating the propagator and [`Propagator`] which defines the
//! methods for interaction between the solver and the propagator.
//!
//! We do not require *any* propagator to be idempotent (see the previous section for a definition)
//! and it can be assumed that if a propagator is not at fix-point after propagating that it will be
//! called again by the solver until no further propagations happen.
//!
//! # Bibliography
//!
//! \[1\] C. Schulte and P. J. Stuckey, ‘Efficient constraint propagation engines’, ACM Transactions
//! on Programming Languages and Systems (TOPLAS), vol. 31, no. 1, pp. 1–43, 2008.

pub mod arithmetic;
pub mod clausal;
mod cumulative;
mod difference_logic;
pub(crate) mod element;

pub use arithmetic::*;
pub use cumulative::*;

#[cfg(doc)]
use crate::engine::propagation::Propagator;
#[cfg(doc)]
use crate::engine::propagation::PropagatorConstructor;
