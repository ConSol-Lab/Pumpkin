use super::StorageKey;
use crate::engine::predicates::integer_predicate::IntegerPredicate;

#[derive(Debug)]
pub struct PredicateIdGenerator {}

impl PredicateIdGenerator {
    pub fn get_id(&mut self, _predicate: IntegerPredicate) -> PredicateId {
        todo!();
    }

    pub fn get_predicate(&self, _id: PredicateId) -> IntegerPredicate {
        todo!();
    }

    pub fn delete_id(&mut self, _id: PredicateId) {
        todo!();
    }

    /// Returns an iterator over all active predicate ids.
    /// Note that constructing the iterator is not constant time,
    /// since the function internally sorted the inactive predicate ids.
    pub fn iter(&self) -> PredicateIdIterator {
        todo!();
    }
}

#[derive(Debug)]
pub struct PredicateIdIterator {}

impl Iterator for PredicateIdIterator {
    type Item = PredicateId;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

impl Default for PredicateIdGenerator {
    fn default() -> Self {
        todo!();
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PredicateId {
    id: u32,
}

impl StorageKey for PredicateId {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        PredicateId { id: index as u32 }
    }
}
