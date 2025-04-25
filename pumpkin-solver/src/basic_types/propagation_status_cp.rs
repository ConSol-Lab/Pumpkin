use crate::basic_types::PropositionalConjunction;
use crate::containers::StorageKey;
use crate::engine::EmptyDomain;
use crate::proof::InferenceCode;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub(crate) type PropagationStatusCP = Result<(), Inconsistency>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Inconsistency {
    EmptyDomain,
    Conflict(PropagatorConflict),
}

impl From<EmptyDomain> for Inconsistency {
    fn from(_: EmptyDomain) -> Self {
        Inconsistency::EmptyDomain
    }
}

// TODO: Remove this after finalizing refactoring.
impl From<PropositionalConjunction> for Inconsistency {
    fn from(conjunction: PropositionalConjunction) -> Self {
        Inconsistency::Conflict(PropagatorConflict {
            conjunction,
            inference_code: InferenceCode::create_from_index(0),
        })
    }
}

impl From<PropagatorConflict> for Inconsistency {
    fn from(conflict: PropagatorConflict) -> Self {
        Inconsistency::Conflict(conflict)
    }
}

/// A conflict stated by a propagator. It's inference code identifies how how the conflict was
/// determined.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PropagatorConflict {
    pub(crate) conjunction: PropositionalConjunction,
    pub(crate) inference_code: InferenceCode,
}
