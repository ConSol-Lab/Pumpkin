use super::HashMap;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::predicates::predicate::Predicate;
use crate::pumpkin_assert_moderate;

#[derive(Debug, Default, Clone)]
pub(crate) struct PredicateIdGenerator {
    id_to_predicate: KeyedVec<PredicateId, Predicate>,
    predicate_to_id: HashMap<Predicate, PredicateId>,
}

impl PredicateIdGenerator {
    pub(crate) fn has_id_for_predicate(&self, predicate: Predicate) -> bool {
        self.predicate_to_id.contains_key(&predicate)
    }

    /// Returns an id for the predicate. If the predicate already has an id, its id is returned.
    /// Otherwise, a new id is create and returned.
    pub(crate) fn get_id(&mut self, predicate: Predicate) -> PredicateId {
        if let Some(id) = self.predicate_to_id.get(&predicate) {
            *id
        } else {
            let id = self.id_to_predicate.push(predicate);
            let _ = self.predicate_to_id.insert(predicate, id);
            id
        }
    }

    pub(crate) fn get_predicate(&self, id: PredicateId) -> Predicate {
        self.id_to_predicate[id]
    }

    pub(crate) fn clear(&mut self) {
        self.id_to_predicate.clear();
        self.predicate_to_id.clear();
    }

    pub(crate) fn replace_predicate(&mut self, predicate: Predicate, replacement: Predicate) {
        pumpkin_assert_moderate!(self.has_id_for_predicate(predicate));
        let predicate_id = self.get_id(predicate);
        self.id_to_predicate[predicate_id] = replacement
    }

    pub(crate) fn num_predicate_ids(&self) -> usize {
        self.id_to_predicate.len()
    }
}

#[cfg(test)]
#[derive(Debug)]
pub(crate) struct PredicateIdIterator {
    sorted_deleted_ids: Vec<PredicateId>,
    current_id: u32,
    next_deleted: u32,
}

#[cfg(test)]
impl PredicateIdIterator {
    fn new(end_id: u32, mut deleted_ids: Vec<PredicateId>) -> PredicateIdIterator {
        deleted_ids.sort();
        deleted_ids.push(PredicateId { id: end_id });

        let mut iterator = PredicateIdIterator {
            sorted_deleted_ids: deleted_ids,
            current_id: 0,
            next_deleted: 0,
        };

        // If the initial value is not present, increment.
        if iterator.current_id == iterator.sorted_deleted_ids.first().unwrap().id {
            iterator.increment();
        }

        iterator
    }

    fn increment(&mut self) {
        if self.next_deleted == self.sorted_deleted_ids.len() as u32 {
            // Do nothing, iterator has been exhausted.
            return;
        }

        // Recall that deleted ids are kept sorted.
        // The worst case currently is if the deleted ids are all consecutive,
        // this could be handled better.
        if self.current_id == self.sorted_deleted_ids[self.next_deleted as usize].id {
            self.next_deleted += 1;
            self.increment();
        } else {
            assert!(self.current_id < self.sorted_deleted_ids[self.next_deleted as usize].id);
            self.current_id += 1;

            if self.current_id == self.sorted_deleted_ids[self.next_deleted as usize].id {
                self.increment();
            }
        }
    }
}

#[cfg(test)]
impl Iterator for PredicateIdIterator {
    type Item = PredicateId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_deleted == self.sorted_deleted_ids.len() as u32 {
            None
        } else {
            let id = PredicateId {
                id: self.current_id,
            };
            self.increment();
            Some(id)
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub(crate) struct PredicateId {
    pub(crate) id: u32,
}

impl StorageKey for PredicateId {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        PredicateId { id: index as u32 }
    }
}

#[cfg(test)]
mod tests {
    use super::PredicateId;
    use super::PredicateIdIterator;

    fn new_predicate_ids(ids: Vec<u32>) -> Vec<PredicateId> {
        ids.iter().map(|id| PredicateId { id: *id }).collect()
    }

    #[test]
    fn initial_values_removed1() {
        let deleted_ids = new_predicate_ids(vec![0, 1, 2, 3, 5]);
        let mut iterator = PredicateIdIterator::new(6, deleted_ids);
        assert_eq!(iterator.next().unwrap().id, 4);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn initial_values_removed2() {
        let deleted_ids = new_predicate_ids(vec![0, 1, 2, 3, 5]);
        let mut iterator = PredicateIdIterator::new(8, deleted_ids);
        assert_eq!(iterator.next().unwrap().id, 4);
        assert_eq!(iterator.next().unwrap().id, 6);
        assert_eq!(iterator.next().unwrap().id, 7);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn last_values_removed() {
        let deleted_ids = new_predicate_ids(vec![3, 4, 5]);
        let mut iterator = PredicateIdIterator::new(6, deleted_ids);
        assert_eq!(iterator.next().unwrap().id, 0);
        assert_eq!(iterator.next().unwrap().id, 1);
        assert_eq!(iterator.next().unwrap().id, 2);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn initial_and_last_values_removed() {
        let deleted_ids = new_predicate_ids(vec![0, 1, 2, 4, 5]);
        let mut iterator = PredicateIdIterator::new(6, deleted_ids);
        assert_eq!(iterator.next().unwrap().id, 3);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn no_removed_values() {
        let deleted_ids = new_predicate_ids(vec![]);
        let mut iterator = PredicateIdIterator::new(6, deleted_ids);
        assert_eq!(iterator.next().unwrap().id, 0);
        assert_eq!(iterator.next().unwrap().id, 1);
        assert_eq!(iterator.next().unwrap().id, 2);
        assert_eq!(iterator.next().unwrap().id, 3);
        assert_eq!(iterator.next().unwrap().id, 4);
        assert_eq!(iterator.next().unwrap().id, 5);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn all_values_removed() {
        let deleted_ids = new_predicate_ids(vec![0, 1, 2]);
        let mut iterator = PredicateIdIterator::new(3, deleted_ids);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn holes_in_removed_values() {
        let deleted_ids = new_predicate_ids(vec![0, 2, 4, 7, 9]);
        let mut iterator = PredicateIdIterator::new(11, deleted_ids);
        assert_eq!(iterator.next().unwrap().id, 1);
        assert_eq!(iterator.next().unwrap().id, 3);
        assert_eq!(iterator.next().unwrap().id, 5);
        assert_eq!(iterator.next().unwrap().id, 6);
        assert_eq!(iterator.next().unwrap().id, 8);
        assert_eq!(iterator.next().unwrap().id, 10);
        assert_eq!(iterator.next(), None);
    }
}
