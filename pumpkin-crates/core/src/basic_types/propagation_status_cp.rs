use super::PropositionalConjunction;
#[cfg(test)]
use crate::basic_types::StoredConflictInfo;
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

impl From<PropagatorConflict> for Inconsistency {
    fn from(conflict: PropagatorConflict) -> Self {
        Inconsistency::Conflict(conflict)
    }
}

#[cfg(test)]
impl TryFrom<StoredConflictInfo> for Inconsistency {
    type Error = &'static str;

    fn try_from(value: StoredConflictInfo) -> Result<Self, Self::Error> {
        match value {
            StoredConflictInfo::Propagator(propagator_conflict) => Ok(propagator_conflict.into()),
            StoredConflictInfo::EmptyDomain(_) => Ok(Inconsistency::EmptyDomain),
            _ => Err("Could not convert"),
        }
    }
}

/// A conflict stated by a propagator. A propagator that identifies a conflict that is _not_ an
/// empty domain, describes that conflict with this type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PropagatorConflict {
    /// The conjunction that describes the infeasible partial assignment.
    pub(crate) conjunction: PropositionalConjunction,
    /// The inference code that identified the conflict.
    pub(crate) inference_code: InferenceCode,
}
