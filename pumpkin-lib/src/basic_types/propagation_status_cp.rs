use crate::engine::EmptyDomain;

use super::{Predicate, PropositionalConjunction};

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub type PropagationStatusCP = Result<(), Inconsistency>;

#[derive(Debug, PartialEq)]
pub enum Inconsistency {
    EmptyDomain,
    Other(PropositionalConjunction),
}

impl From<EmptyDomain> for Inconsistency {
    fn from(_: EmptyDomain) -> Self {
        Inconsistency::EmptyDomain
    }
}

impl From<PropositionalConjunction> for Inconsistency {
    fn from(value: PropositionalConjunction) -> Self {
        Inconsistency::Other(value)
    }
}

impl<Slice> From<Slice> for Inconsistency
where
    Slice: AsRef<[Predicate]>,
{
    fn from(value: Slice) -> Self {
        Inconsistency::Other(value.as_ref().to_vec().into())
    }
}
