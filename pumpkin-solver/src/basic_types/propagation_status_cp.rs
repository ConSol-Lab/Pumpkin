use crate::basic_types::PropositionalConjunction;
use crate::engine::EmptyDomain;
use crate::predicates::Predicate;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub(crate) type PropagationStatusCP = Result<(), Inconsistency>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Inconsistency {
    EmptyDomain,
    Conflict(PropositionalConjunction),
}

impl From<EmptyDomain> for Inconsistency {
    fn from(_: EmptyDomain) -> Self {
        Inconsistency::EmptyDomain
    }
}

impl From<PropositionalConjunction> for Inconsistency {
    fn from(conflict_nogood: PropositionalConjunction) -> Self {
        Inconsistency::Conflict(conflict_nogood)
    }
}

impl<Slice> From<Slice> for Inconsistency
where
    Slice: AsRef<[Predicate]>,
{
    fn from(value: Slice) -> Self {
        let conflict_nogood: PropositionalConjunction = value.as_ref().to_vec().into();
        Inconsistency::Conflict(conflict_nogood)
    }
}
