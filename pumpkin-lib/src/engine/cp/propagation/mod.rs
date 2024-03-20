//! Contains the main building blocks for propagators.
//!
//! # Theoretical
//!
//! A propagator takes as input a set of variables (`x_i ∈ X`) and for each variable a corresponding
//! domain (`D_i ∈ D`); it can then be seen as a function which maps `D ↦ D'` such that `D'_i ⊆ D_i`
//! for all variables (i.e. the domain of a variable either remains the same after applying the
//! propagator or it becomes a subset of the domain before applying the propagator).
//!
//! An example of a propagator can be the simple not equal (`!=`) propagator, suppose that
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
//! Each concrete propagator is associated with two structs, which implement traits with
//! corresponding names:
//! - [`Propagator`]: contains most of the propagator logic.
//! - [`PropagatorConstructor`]: propagators do not directly communicate with the solver, but rather
//!   use this struct as a communication point during creation to get access to variables.
//!
//! Propagators uses [`PropagatorVariable`]s when performing domain operations over variables,
//! which is an abstraction over the underlying variable representation in the solver.
//! For each such variable, the propagator is responsible for tracking its [`LocalId`] within the
//! propagator, i.e., each [`PropagatorVariable`] must be assigned a unique [`LocalId`]. When domain
//! changes happen for a variable outside the propagator, the propagator will receive information
//! that its variable with a specific local_id has changed (see
//! [`Propagator::notify`]). The idea behind using the structs apart
//! from Propagator is to support 'views' on variables.
//!
//! We do not require *any* propagator to be idempotent (see the previous section for a
//! definition) and it can be assumed that if a propagator is not at fix-point after propagating
//! that it will be called again by the solver until no further propagations happen.
//!
//! See the [`propagators`] folder for concrete propagator implementations.
//!
//! # How to implement a new propagator?
//!
//! We recommend the following workflow:
//! - Implement a propagator struct that implements the [`Propagator`] trait. For now only implement
//!   the required functions, i.e., [`Propagator::debug_propagate_from_scratch`] and
//!   [`Propagator::name`].
//!     * Note that [`PropagatorVariable`]s are to be used as the variables within the propagator,
//!       so all domain changes go through [`PropagatorVariable`]s.
//! - Implement a struct that implements the [`PropagatorConstructor`] trait, which contains
//!   necessary information to create the propagator.
//!     * For example, it may contain an array of IntegerVariables and some constants that will be
//!       used by the propagator.
//!     * Note that [`PropagatorConstructor`] receives [`PropagatorConstructorContext`], which is
//!       used to retrieve [`PropagatorVariable`]s.
//! - Following the procedure above gives a simple propagator version that is likely not efficient,
//!   but has an important role for testing. Now is a good time to write tests which use the a test
//!   solver. This will greatly help with testing the propagator.
//!     * For example, see the tests in the
//!       [`crate::propagators::arithmetic::absolute_value::absolute_value.rs`].
//! - The next step towards a more efficient propagator is to implement [`Propagator::notify`] and
//!   [`Propagator::notify_literal`]. Depending on the concrete propagator, this may only make sense
//!   when done in together with the next step.
//! - Afterwards step is to implement the remaining functions, i.e., [`Propagator::propagate`],
//!   [`Propagator::synchronise`], and [`Propagator::initialise_at_root`]. These are all
//!   interdependent.
//! - Decide on the priortiy of the propagator, i.e., implement [`Propagator::priority`].
//! - Make sure to write new tests and run all tests throughout the process.
//! - The propagator implementation is now done!
//!
//! The propagator is added to the solver through [`ConstraintSatisfactionSolver::add_propagator`]
//! by passing the [`PropagatorConstructor`] as an argument.
//!
//! //! # Bibliography
//!
//! \[1\] C. Schulte and P. J. Stuckey, ‘Efficient constraint propagation engines’, ACM Transactions
//! on Programming Languages and Systems (TOPLAS), vol. 31, no. 1, pp. 1–43, 2008.

pub(crate) mod local_id;
pub(crate) mod propagation_context;
pub(crate) mod propagator;
pub(crate) mod propagator_constructor;
pub(crate) mod propagator_constructor_context;
pub(crate) mod propagator_id;
pub(crate) mod propagator_var_id;
pub(crate) mod propagator_variable;

pub use local_id::LocalId;
pub use propagation_context::PropagationContext;
pub(crate) use propagation_context::PropagationContextMut;
pub(crate) use propagation_context::ReadDomains;
pub(crate) use propagator::EnqueueDecision;
pub use propagator::Propagator;
pub use propagator_constructor::PropagatorConstructor;
pub use propagator_constructor_context::PropagatorConstructorContext;
pub(crate) use propagator_id::PropagatorId;
pub(crate) use propagator_var_id::PropagatorVarId;
pub use propagator_variable::PropagatorVariable;

#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::propagators;
