use thiserror::Error;

#[derive(Error, Debug, Copy, Clone)]
pub enum ConstraintOperationError {
    #[error("Adding the nogood failed because it is infeasible at the root")]
    InfeasibleNogood,
    #[error("Adding constraint failed because the solver is in an infeasible state")]
    InfeasibleState,
}
