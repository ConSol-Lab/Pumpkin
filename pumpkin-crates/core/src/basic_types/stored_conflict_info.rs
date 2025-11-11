use super::propagation_status_cp::PropagatorConflict;
use crate::ConstraintOperationError;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::reason::ReasonRef;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// A conflict info which can be stored in the solver.
/// Two (related) conflicts can happen:
/// 1) A propagator explicitly detects a conflict.
/// 2) A propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum StoredConflictInfo {
    Propagator(PropagatorConflict),
    EmptyDomain {
        trigger_predicate: Predicate,
        trigger_reason: ReasonRef,
        trigger_inference_code: InferenceCode,
    },
    InfeasibleAssumptions(Predicate),
    RootLevelConflict(ConstraintOperationError),
}
