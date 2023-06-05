#[derive(PartialEq, Eq, Debug)]
pub enum ClauseAdditionOutcome {
    Infeasible,
    NoConflictDetected,
}
