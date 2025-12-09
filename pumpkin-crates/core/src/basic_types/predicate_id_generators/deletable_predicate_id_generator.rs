#[cfg(doc)]
use super::PredicateIdGenerator;
use crate::basic_types::predicate_id_generators::PredicateId;
use crate::containers::HashMap;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::predicates::predicate::Predicate;
use crate::pumpkin_assert_moderate;
use crate::variables::DomainId;

/// A [`PredicateIdGenerator`] which allows the deletion of stored [`PredicateId`]s.
///
/// This can be useful when numerous [`PredicateId`]s are defined, not all of which are required to
/// be defined at the moment.
#[derive(Debug, Default, Clone)]
pub(crate) struct DeletablePredicateIdGenerator {
    /// The value of the next id, provided there are no delete_ids that can be reused.
    next_id: u32,
    /// When an id is deleted, it gets stored here, so that the id can be reused in the future.
    deleted_ids: Vec<PredicateId>,
    /// Active predicates are stored here.
    /// todo: consider direct hashing.
    id_to_predicate: HashMap<PredicateId, Predicate>,
    predicate_to_id: KeyedVec<DomainId, HashMap<i32, PredicateId>>,
}

impl DeletablePredicateIdGenerator {
    pub(crate) fn len(&self) -> usize {
        self.id_to_predicate.len()
    }

    fn get_new_predicate_id(&mut self) -> PredicateId {
        // We either reuse a previously deleted id, or create a new one.
        if let Some(recycled_id) = self.deleted_ids.pop() {
            recycled_id
        } else {
            let new_id = self.next_id;
            self.next_id += 1;
            PredicateId { id: new_id }
        }
    }

    /// Returns an id for the predicate. If the predicate already has an id, its id is returned.
    /// Otherwise, a new id is create and returned.
    pub(crate) fn get_id(&mut self, predicate: Predicate) -> PredicateId {
        let domain = predicate.get_domain();
        let rhs = predicate.get_right_hand_side();

        if domain.index() < self.predicate_to_id.len()
            && let Some(id) = self.predicate_to_id[domain].get(&rhs)
        {
            *id
        } else {
            let id = self.get_new_predicate_id();
            let a = self.id_to_predicate.insert(id, predicate);

            if domain.index() >= self.predicate_to_id.len() {
                self.predicate_to_id
                    .resize(domain.index() + 1, Default::default());
            }
            let b = self.predicate_to_id[domain].insert(rhs, id);
            assert!(a.is_none() && b.is_none());
            id
        }
    }

    pub(crate) fn get_predicate(&self, id: PredicateId) -> Option<Predicate> {
        self.id_to_predicate.get(&id).copied()
    }

    pub(crate) fn delete_id(&mut self, id: PredicateId) {
        pumpkin_assert_moderate!(!self.deleted_ids.contains(&id));
        // Add the deleted id for future reuse.
        self.deleted_ids.push(id);
        // Remove the mapping id->predicate.
        let predicate = self
            .id_to_predicate
            .remove(&id)
            .expect("Id must be present.");
        // Remove the mapping predicate->id.
        let domain = predicate.get_domain();
        let rhs = predicate.get_right_hand_side();
        let removed_id = self.predicate_to_id[domain]
            .remove(&rhs)
            .expect("Predicate must be present");
        pumpkin_assert_moderate!(removed_id == id);
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::PredicateId;
    use crate::basic_types::predicate_id_generators::predicate_id_generator::PredicateIdIterator;

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
