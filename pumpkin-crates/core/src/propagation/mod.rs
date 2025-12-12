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
//! Each concrete propagator is associated with one trait: [`Propagator`]. The main function to
//! implement for this trait is [`Propagator::propagate`], which performs the domain reduction.
//!
//! A propagator is created by a [`PropagatorConstructor`]. The constructor is responsible for
//! registering to domain events, and setting up the state of the propagator. The constructor is
//! provided a [`PropagatorConstructorContext`], which has all the available functions allowing the
//! propagator to hook into the solver state.
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
//! 2. Create an implementation of the [`PropagatorConstructor`] trait, to register for domain
//!    events and set up the propagator state.
//! 3. Following the procedure above gives an initial version of the propagator that is likely not
//!    efficient, but has an important role for testing. Now is a good time to write tests using the
//!    [`State`] API. **We strongly discourage skipping this step**.
//! 4. Implement [`Propagator::notify`] and/or [`Propagator::notify_predicate_id_satisfied`] for
//!    more control on when the propagator is enqueued. Depending on the concrete propagator, this
//!    may only make sense when done together with the next step.
//! 5. Implement the remaining hooks, i.e., [`Propagator::propagate`], and
//!    [`Propagator::synchronise`] to exploit incrementality. These are all interdependent.
//! 6. Decide on the priortiy of the propagator, i.e., implement [`Propagator::priority`].
//! 7. Make sure to write new tests and run all tests throughout the process.
//! 8. The propagator implementation is now done!
//!
//! The propagator is added to the solver through [`Solver::add_propagator`].
//!
//! # Bibliography
//!
//! \[1\] C. Schulte and P. J. Stuckey, ‘Efficient constraint propagation engines’, ACM Transactions
//! on Programming Languages and Systems (TOPLAS), vol. 31, no. 1, pp. 1–43, 2008.
//!
//! \[2\] C. Schulte and G. Tack, ‘Views and iterators for generic constraint implementations’, in
//! International Workshop on Constraint Solving and Constraint Logic Programming, 2005, pp.
//! 118–132.

mod constructor;
mod contexts;
mod domains;
mod local_id;
mod propagator;

pub(crate) mod propagator_id;
pub(crate) mod propagator_var_id;
pub(crate) mod store;

// BEGIN: Re-exports of types not in this module according to the file tree.
// These will probably be be moved at some point, but for now they are simply re-exported here to
// make the propagator API public.
// END
pub use constructor::*;
pub use contexts::*;
pub use domains::*;
pub use local_id::*;
pub use propagator::*;
pub use propagator_id::PropagatorId;
pub(crate) use propagator_var_id::PropagatorVarId;

#[cfg(doc)]
use crate::Solver;
pub use crate::basic_types::PredicateId;
pub use crate::engine::cp::TrailedInteger;
pub use crate::engine::notifications::DomainEvent;
pub use crate::engine::notifications::DomainEvents;
#[cfg(doc)]
use crate::engine::test_solver::TestSolver;
#[cfg(doc)]
use crate::engine::variables::IntegerVariable;
#[cfg(doc)]
use crate::propagators;
#[cfg(doc)]
use crate::propagators::linear_less_or_equal::LinearLessOrEqualPropagator;
#[cfg(doc)]
use crate::state::State;
