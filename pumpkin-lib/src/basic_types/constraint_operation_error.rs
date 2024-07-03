use thiserror::Error;

#[cfg(doc)]
use crate::Solver;

/// Errors related to adding constraints to the [`Solver`].
#[derive(Error, Debug, Copy, Clone)]
pub enum ConstraintOperationError {
    /// Error which indicate that adding a clause led to infeasibility at the root.
    #[error("Adding the clause failed because it is infeasible at the root")]
    InfeasibleClause,
    /// Error which indicates that a constraint was attempted to be added while the [`Solver`] was
    /// in an infeasible state.
    #[error("Adding constraint failed because the solver is in an infeasible state")]
    InfeasibleState,
    /// Error which indicate that adding a propagator led to infeasibility at the root.
    #[error("Adding the constraint failed because it is infeasible at the root")]
    InfeasiblePropagator,
}
