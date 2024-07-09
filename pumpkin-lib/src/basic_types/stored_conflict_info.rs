use super::PropositionalConjunction;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;

/// A conflict info which can be stored in the solver. It exists to annotate the
/// conflict nogood with the propagator which caused the conflict.
#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(variant_size_differences)]
pub struct StoredConflictInfo {
    pub conflict_nogood: PropositionalConjunction,
    pub propagator: PropagatorId,
}
