use std::ops::Not;

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

impl From<BoundPredicate> for Predicate {
    fn from(value: BoundPredicate) -> Self {
        match value.comparator {
            BoundComparator::LowerBound => predicate![value.domain >= value.value],
            BoundComparator::UpperBound => predicate![value.domain <= value.value],
        }
    }
}

impl Not for BoundPredicate {
    type Output = BoundPredicate;

    fn not(self) -> Self::Output {
        BoundPredicate {
            domain: self.domain,
            comparator: match self.comparator {
                BoundComparator::LowerBound => BoundComparator::UpperBound,
                BoundComparator::UpperBound => BoundComparator::LowerBound,
            },
            value: match self.comparator {
                BoundComparator::LowerBound => self.value - 1,
                BoundComparator::UpperBound => self.value + 1,
            },
        }
    }
}
