use super::propagation_status_cp::PropagatorConflict;
use crate::ConstraintOperationError;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::EmptyDomainConflict;
use crate::engine::state::Conflict;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::Propagator;

/// a conflict info which can be stored in the solver.
/// two (related) conflicts can happen:
/// 1) a propagator explicitly detects a conflict.
/// 2) a propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum StoredConflictInfo {
    Propagator(PropagatorConflict),
    EmptyDomain(EmptyDomainConflict),
    /// The conflict is due to inconsistent assumptions.
    ///
    /// The provided predicate and its negation are both assumptions.
    InconsistentAssumptions(Predicate),
    RootLevelConflict(ConstraintOperationError),
}

impl From<Conflict> for StoredConflictInfo {
    fn from(value: Conflict) -> Self {
        match value {
            Conflict::Propagator(propagator_conflict) => {
                StoredConflictInfo::Propagator(propagator_conflict)
            }
            Conflict::EmptyDomain(empty_domain_conflict) => {
                StoredConflictInfo::EmptyDomain(empty_domain_conflict)
            }
        }
    }
}
