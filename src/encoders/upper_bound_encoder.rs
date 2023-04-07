use crate::engine::ConstraintSatisfactionSolver;

use super::GeneralisedTotaliserEncoder;

pub trait UpperBoundEncoder {
    /// Set the upper bound to the objective function to k. This should re-use
    /// encodings as much as possible, instead of creating a new encoding at
    /// every invokation.
    fn constrain_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> EncodingStatus;
}

#[derive(Debug, PartialEq, Eq)]
pub enum EncodingStatus {
    NoConflictDetected,
    ConflictDetected,
}

/// Note: Implemented here to not touch the original GTE implementation. In
/// future, if this is an architecture to persue, this should probably be moved
/// to the gte file.
impl UpperBoundEncoder for GeneralisedTotaliserEncoder {
    fn constrain_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> EncodingStatus {
        self.constrain_at_most_k(k, csp_solver)
    }
}
