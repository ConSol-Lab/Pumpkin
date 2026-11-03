//! Contains the [`OptimisationProcedure`] trait which defines how an optimisation procedure
//! should operate, and the [`SolutionCallback`] which is called with every encountered solution.
//!
//! Implementations of [`OptimisationProcedure`] (e.g. linear SAT-UNSAT search) are provided by the
//! `pumpkin-optimisation` crate.
//!
//! An [`OptimisationProcedure`] is expected to be passed to [`Solver::optimise`].

use solution_callback::SolutionCallback;

use crate::Solver;
use crate::branching::Brancher;
use crate::conflict_resolving::ConflictResolver;
use crate::results::OptimisationResult;
use crate::termination::TerminationCondition;

pub mod solution_callback;

/// A procedure which solves the model in a [`Solver`] to optimality.
pub trait OptimisationProcedure<B: Brancher, R: ConflictResolver, Callback: SolutionCallback<B, R>>
{
    /// Solves the model currently in the [`Solver`] to optimality (or until the provided
    /// [`TerminationCondition`] triggers).
    ///
    /// It returns an [`OptimisationResult`] which can be used to retrieve the optimal solution if
    /// it exists.
    fn optimise(
        &mut self,
        brancher: &mut B,
        termination: &mut impl TerminationCondition,
        resolver: &mut R,
        solver: &mut Solver,
    ) -> OptimisationResult<Callback::Stop>;
}
