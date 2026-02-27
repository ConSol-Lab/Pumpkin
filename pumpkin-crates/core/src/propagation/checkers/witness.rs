use crate::containers::HashMap;
use crate::variables::DomainId;

/// A light-weight variable assignment used during propagation checking.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Witness(HashMap<DomainId, i32>);

impl Witness {
    /// Create a new [`Witness`] from the given `assignments`.
    pub fn new(assignments: impl IntoIterator<Item = SingleVariableAssignment>) -> Witness {
        Witness(
            assignments
                .into_iter()
                .map(|assignment| (assignment.domain_id, assignment.value))
                .collect(),
        )
    }

    /// Get the value for the given [`DomainId`] in the witness.
    ///
    /// If the domain is not part of the witness, [`None`] is returned.
    pub fn value_for(&self, domain_id: DomainId) -> Option<i32> {
        self.0.get(&domain_id).copied()
    }

    /// Iterate over all assigned domains.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (DomainId, i32)> {
        self.0.iter().map(|(key, value)| (*key, *value))
    }
}

/// Models an assignment of `variable = value`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SingleVariableAssignment {
    domain_id: DomainId,
    value: i32,
}

impl SingleVariableAssignment {
    /// Create a new assignment.
    pub fn new(domain_id: DomainId, value: i32) -> SingleVariableAssignment {
        SingleVariableAssignment { domain_id, value }
    }
}
