use super::PropositionalConjunction;
use super::propagation_status_cp::PropagatorConflict;
use crate::ConstraintOperationError;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::propagation::Propagator;

/// A conflict info which can be stored in the solver.
/// Two (related) conflicts can happen:
/// 1) A propagator explicitly detects a conflict.
/// 2) A propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum StoredConflictInfo {
    Propagator(PropagatorConflict),
    EmptyDomain {
        conflict_nogood: PropositionalConjunction,
    },
    RootLevelConflict(ConstraintOperationError),
}
