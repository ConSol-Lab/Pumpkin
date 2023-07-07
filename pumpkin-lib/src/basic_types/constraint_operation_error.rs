use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConstraintOperationError {
    #[error("Adding the clause failed because it is infeasible at the root")]
    InfeasibleClause,
    #[error("Adding constraint failed because the solver is in an infeasible state")]
    InfeasibleState,
}
