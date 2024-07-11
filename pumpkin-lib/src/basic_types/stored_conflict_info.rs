use super::PropositionalConjunction;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;

/// A conflict info which can be stored in the solver.
/// Two (related) conflicts can happen:
/// 1) A propagator explicitly detects a conflict.
/// 2) A propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StoredConflictInfo {
    Propagator {
        conflict_nogood: PropositionalConjunction,
        propagator_id: PropagatorId,
    },
    EmptyDomain {
        conflict_nogood: PropositionalConjunction,
    },
}
