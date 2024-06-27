//! Propagators for clauses based on unit propagation with the two-watch literal scheme,
//! i.e., once all but one literal in the clause are falsified, the remaining literal is
//! propagated to true. There are two versions of the propagators:
//! - The basic version does propagation as usual.
//! - The inline binary version has special treatment for binary clauses, i.e., binary clauses are
//!   inlined in the watch list. This allows more efficient propagation since all information needed
//!   to propagate is stored in the watch list, there is no need to access a clause in memory,
//!   avoiding cache misses.
//! The clausal_interface trait is used to make it easier to swap different versions of the
//! propagator in the code. As such, its usage is mainly for testing purposes.

mod basic_clausal;
mod binary_inline_clausal;
mod clausal_propagator;

pub(crate) use basic_clausal::BasicClausalPropagator;
pub(crate) use clausal_propagator::is_clause_propagating;
pub(crate) use clausal_propagator::ClausalPropagator;
