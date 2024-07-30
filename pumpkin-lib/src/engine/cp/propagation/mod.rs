//! Contains the main building blocks for propagators.
//!
//! # Background
//!
//! A propagator takes as input a set of variables (<code>x<sub>i</sub> ∈ X</code>) and for each
//! variable a corresponding domain (<code>D<sub>i</sub> ∈ D</code>); it can then be seen as a
//! function which maps `D ↦ D'` such that <code>D'<sub>i</sub> ⊆ D<sub>i</sub></code> for all
//! variables (i.e. the domain of a variable either remains the same after applying the propagator
//! or it becomes a subset of the domain before applying the propagator).
//!
//! An example of a propagator can be the simple not equal (`!=`) propagator, suppose that
//! we have two variables `x ∈ {0}` and `y ∈ {0, 1}` and the constraint `x != y`. The not equal
//! propagator will then take as input the variables `x` and `y` and their respective domains
//! <code>D = {D<sub>x</sub> = {0}, D<sub>y</sub> = {0, 1}</code> and produce a new domain <code>D'
//! = {D'<sub>x</sub> = {0}, D'<sub>y</sub> = {1}}</code> for which we can see that <code>D_x =
//! D'<sub>x</sub></code> and <code>D'<sub>y</sub> ⊆ D<sub>y</sub></code>.
//!
//! A propagator is said to be at fix-point if <code>D<sub>x</sub> = D'<sub>x</sub></code> meaning
//! that no further propagations can take place when applying the propagator. A propagator is said
//! to be "idempotent" if a single call to it will result in it being at fix-point.
//!
//! For more information about the construction of these types of propagation-based solvers, we
//! refer to [\[1\]](https://dl.acm.org/doi/pdf/10.1145/1452044.1452046).
//!
//! # Practical
//!
//! Each concrete propagator is associated with two traits:
//! - [`Propagator`]: contains the propagator logic.
//! - [`PropagatorConstructor`]: provides the functionality to register variables for their
//!   corresponding [`DomainEvents`] with the solver. Note that structs implementing this trait do
//!   not directly communicate with the solver, but rather use the [`PropagatorConstructorContext`]
//!   as a communication point during creation to get access to variables. *Note that this trait is
//!   **not** implemented on the propagator struct itself but rather on another struct holding the
//!   arguments necessary for creating the propagator; for an example, see
//!   [`LinearLessOrEqualConstructor`] and [`LinearLessOrEqualPropagator`]!*
//!
//! When domain changes happen for a variable outside the propagator, the propagator will receive
//! information that its variable with a specific [`LocalId`] has changed (see
//! [`Propagator::notify`]). The idea behind using the structs apart from Propagator is to support
//! views \[2\] on variables.
//!
//! We do not require propagators to be idempotent (see the previous section for a
//! definition) and it can be assumed that if a propagator is not at fix-point after propagating
//! that it will be called again by the solver until no further propagations happen.
//!
//! See the [`propagators`] folder for concrete propagator implementations.
//!
//! # How to implement a new propagator?
//!
//! We recommend the following workflow:
//! 1. Implement a propagator struct that implements the [`Propagator`] trait. For now only
//!    implement the required functions, i.e., [`Propagator::debug_propagate_from_scratch`] and
//!    [`Propagator::name`].
//! 2. Implement a struct that implements the [`PropagatorConstructor`] trait, which contains
//!    necessary information to create the propagator. It is also responsible for registering the
//!    variables and corresponding [`DomainEvents`] with the solver, so that the solver can notify
//!    the propagator once an event happens that relates to one of the variables of the propagator.
//!     * For example, the struct implementing [`PropagatorConstructor`] may contain an array of raw
//!       [`IntegerVariable`]s and parameters that will be used to initialise the propagator.
//! 3. Following the procedure above gives an initial version of the propagator that is likely not
//!    efficient, but has an important role for testing. Now is a good time to write tests which use
//!    the [`TestSolver`]. **We strongly discourage skipping this step**.
//!     * For example, see the tests in [`crate::propagators::arithmetic::absolute_value`].
//! 4. Implement [`Propagator::notify`]. Depending on the concrete propagator, this may only make
//!    sense when done together with the next step.
//! 5. Implement the remaining functions, i.e., [`Propagator::propagate`],
//!    [`Propagator::synchronise`], and [`Propagator::initialise_at_root`]. These are all
//!    interdependent.
//! 6. Decide on the priortiy of the propagator, i.e., implement [`Propagator::priority`].
//! 7. Make sure to write new tests and run all tests throughout the process.
//! 8. The propagator implementation is now done!
//!
//! The propagator is added to the solver through [`ConstraintSatisfactionSolver::add_propagator`]
//! by passing the [`PropagatorConstructor`] as an argument.
//!
//! # Bibliography
//!
//! \[1\] C. Schulte and P. J. Stuckey, ‘Efficient constraint propagation engines’, ACM Transactions
//! on Programming Languages and Systems (TOPLAS), vol. 31, no. 1, pp. 1–43, 2008.
//!
//! \[2\] C. Schulte and G. Tack, ‘Views and iterators for generic constraint implementations’, in
//! International Workshop on Constraint Solving and Constraint Logic Programming, 2005, pp.
//! 118–132.

pub(crate) mod local_id;
pub(crate) mod propagation_context;
pub(crate) mod propagator;
pub(crate) mod propagator_constructor;
pub(crate) mod propagator_constructor_context;
pub(crate) mod propagator_id;
pub(crate) mod propagator_var_id;

pub(crate) use local_id::LocalId;
pub(crate) use propagation_context::PropagationContext;
pub(crate) use propagation_context::PropagationContextMut;
pub(crate) use propagation_context::ReadDomains;
pub(crate) use propagator::EnqueueDecision;
pub(crate) use propagator::Propagator;
pub(crate) use propagator_constructor::PropagatorConstructor;
pub(crate) use propagator_constructor_context::PropagatorConstructorContext;
pub(crate) use propagator_id::PropagatorId;
pub(crate) use propagator_var_id::PropagatorVarId;

#[cfg(doc)]
use crate::engine::test_solver::TestSolver;
#[cfg(doc)]
use crate::engine::variables::IntegerVariable;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::DomainEvents;
#[cfg(doc)]
use crate::propagators;
#[cfg(doc)]
use crate::propagators::linear_less_or_equal::LinearLessOrEqualConstructor;
#[cfg(doc)]
use crate::propagators::linear_less_or_equal::LinearLessOrEqualPropagator;
