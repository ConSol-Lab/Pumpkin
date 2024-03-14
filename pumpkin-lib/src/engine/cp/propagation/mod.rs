//! Contains the main building blocks for propagators.
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
//! See the [`propagators`] folder for concrete propagator implementations.
//!
//! # How to implement a new propagator?
//! The typical workflow for implementing a new propagator comes down to implementing the following:
//! - A propagator struct that implements the [`Propagator`] trait. Within the propagator, domain
//!   changes on variables are done using [`PropagatorVariable`]s.
//! - A constructor struct that implements [`PropagatorConstructor`], which contains necessary
//!   information to create the propagator. For example, it may contain an array of IntVars and some
//!   constants that will be used by the propagator.
//! - The [`PropagatorConstructor`] contains the logic needed to create the propagator. Note that
//!   [`PropagatorConstructor`] receives [`PropagatorConstructorContext`], which is used to retrieve
//!   [`PropagatorVariable`]s.
//! The propagator is added to the solver through [`ConstraintSatisfactionSolver::add_propagator`]
//! by passing the [`PropagatorConstructor`] as an argument.

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
