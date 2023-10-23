use crate::engine::EmptyDomain;

use super::PropositionalConjunction;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub type PropagationStatusCP = Result<(), Inconsistency>;

#[derive(Debug)]
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
