use thiserror::Error;

#[derive(Error, Debug, Copy, Clone)]
pub enum ConstraintOperationError {
    #[error("Adding the predicate failed because it is infeasible at the root")]
    InfeasiblePredicate,
    #[error("Adding constraint failed because the solver is in an infeasible state")]
    InfeasibleState,
}
