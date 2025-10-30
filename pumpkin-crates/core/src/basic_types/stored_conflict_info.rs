use super::PropositionalConjunction;
use super::propagation_status_cp::PropagatorConflict;
use crate::ConstraintOperationError;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::reason::ReasonRef;
use crate::predicates::Predicate;

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
    EmptyDomainTwo {
        /// The predicate that triggered the conflict and is no longer on the trail for that
        /// reason.
        conflict_trigger: Predicate,
        /// The reason for the conflict trigger to be propagated.
        conflict_trigger_reason: ReasonRef,
    },
    RootLevelConflict(ConstraintOperationError),
}
