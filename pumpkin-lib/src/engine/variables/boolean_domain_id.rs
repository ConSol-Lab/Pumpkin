use super::DomainId;
use crate::engine::predicates::integer_predicate::IntegerPredicate;

/// A wrapper around ['DomainId'], used for clarity.
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct BooleanDomainId {
    domain_id: DomainId,
}

impl BooleanDomainId {
    pub fn new(domain_id: DomainId) -> BooleanDomainId {
        BooleanDomainId { domain_id }
    }
}

impl From<BooleanDomainId> for IntegerPredicate {
    fn from(boolean_domain_id: BooleanDomainId) -> IntegerPredicate {
        IntegerPredicate::Equal {
            domain_id: boolean_domain_id.domain_id,
            equality_constant: 1,
        }
    }
}

impl From<BooleanDomainId> for DomainId {
    fn from(boolean_domain_id: BooleanDomainId) -> Self {
        boolean_domain_id.domain_id
    }
}
