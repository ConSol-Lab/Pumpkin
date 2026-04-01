use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::DomainId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BoundPredicate {
    pub domain: DomainId,
    pub comparator: BoundComparator,
    pub value: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoundComparator {
    LowerBound,
    UpperBound,
}

impl BoundPredicate {
    pub fn new(predicate: Predicate) -> Option<Self> {
        let comparator = match predicate.get_predicate_type() {
            crate::predicates::PredicateType::LowerBound => BoundComparator::LowerBound,
            crate::predicates::PredicateType::UpperBound => BoundComparator::UpperBound,
            crate::predicates::PredicateType::NotEqual => return None,
            crate::predicates::PredicateType::Equal => return None,
        };

        Some(BoundPredicate {
            domain: predicate.get_domain(),
            comparator,
            value: predicate.get_right_hand_side(),
        })
    }
}

impl Into<Predicate> for BoundPredicate {
    fn into(self) -> Predicate {
        match self.comparator {
            BoundComparator::LowerBound => predicate![self.domain >= self.value],
            BoundComparator::UpperBound => predicate![self.domain <= self.value],
        }
    }
}
