use super::propagation_status_cp::PropagatorConflict;
use super::PropositionalConjunction;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::ConstraintOperationError;

/// A conflict info which can be stored in the solver.
/// Two (related) conflicts can happen:
/// 1) A propagator explicitly detects a conflict.
/// 2) A propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum StoredConflictInfo {
    Propagator(PropagatorConflict),
    EmptyDomain {
        inference_code: Option<InferenceCode>,
        conflict_nogood: PropositionalConjunction,
        /// The last inference that cause the empty domain. This is encoded in `conflict_nogood`
        /// but is needed more explicitly in the proof processor.
        last_inference: (PropositionalConjunction, Predicate),
    },
    RootLevelConflict(ConstraintOperationError),
}
