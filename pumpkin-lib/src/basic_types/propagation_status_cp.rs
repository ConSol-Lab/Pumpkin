use crate::basic_types::ConflictInfo;
use crate::basic_types::PropositionalConjunction;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::EmptyDomain;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub(crate) type PropagationStatusCP = Result<(), Inconsistency>;

#[derive(Debug, PartialEq, Eq)]
pub enum Inconsistency {
    EmptyDomain,
    Other(ConflictInfo),
}

impl From<EmptyDomain> for Inconsistency {
    fn from(_: EmptyDomain) -> Self {
        Inconsistency::EmptyDomain
    }
}

impl From<PropositionalConjunction> for Inconsistency {
    fn from(value: PropositionalConjunction) -> Self {
        Inconsistency::Other(ConflictInfo::Explanation(value))
    }
}

impl<Slice> From<Slice> for Inconsistency
where
    Slice: AsRef<[Predicate]>,
{
    fn from(value: Slice) -> Self {
        let conjunction: PropositionalConjunction = value.as_ref().to_vec().into();
        conjunction.into()
    }
}
