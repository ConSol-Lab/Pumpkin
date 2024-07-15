use crate::basic_types::PropositionalConjunction;
use crate::engine::EmptyDomain;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the
/// conflict nogood.

#[derive(Debug, PartialEq, Eq)]
pub enum Inconsistency {
    EmptyDomain,
    Conflict {
        conflict_nogood: PropositionalConjunction,
    },
}

impl From<EmptyDomain> for Inconsistency {
    fn from(_: EmptyDomain) -> Self {
        Inconsistency::EmptyDomain
    }
}

impl From<PropositionalConjunction> for Inconsistency {
    fn from(conflict_nogood: PropositionalConjunction) -> Self {
        Inconsistency::Conflict { conflict_nogood }
    }
}

// Todo? Uncomment?
// impl<Slice> From<Slice> for Inconsistency
// where
// Slice: AsRef<[Predicate]>,
// {
// fn from(value: Slice) -> Self {
// let conflict_nogood: PropositionalConjunction = value.as_ref().to_vec().into();
// Inconsistency::Conflict { conflict_nogood }
// }
// }
