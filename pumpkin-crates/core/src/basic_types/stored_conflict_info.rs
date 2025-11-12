use super::propagation_status_cp::PropagatorConflict;
use crate::ConstraintOperationError;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::reason::ReasonRef;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::variables::DomainId;

/// A conflict info which can be stored in the solver.
/// Two (related) conflicts can happen:
/// 1) A propagator explicitly detects a conflict.
/// 2) A propagator post a domain change that results in a variable having an empty domain.
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

/// A conflict because a domain became empty.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct EmptyDomainConflict {
    /// The predicate that caused a domain to become empty.
    pub(crate) trigger_predicate: Predicate,
    /// The reason for [`EmptyDomainConflict::trigger_predicate`] to be true.
    pub(crate) trigger_reason: ReasonRef,
    /// The [`InferenceCode`] that accompanies [`EmptyDomainConflict::trigger_reason`].
    pub(crate) trigger_inference_code: InferenceCode,
}

impl EmptyDomainConflict {
    /// The domain that became empty.
    pub(crate) fn domain(&self) -> DomainId {
        self.trigger_predicate.get_domain()
    }
}
