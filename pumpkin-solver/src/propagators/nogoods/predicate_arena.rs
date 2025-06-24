use std::ops::Index;
use std::ops::IndexMut;

use crate::containers::StorageKey;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::propagators::nogoods::NogoodId;
use crate::variables::DomainId;

#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateArena {
    predicates: Vec<Predicate>,
}

impl PredicateArena {
    /// Stores the predicates into the arena
    /// and returns the nogood id associated with these predicates.
    pub(crate) fn store_predicates(&mut self, mut nogood_predicates: Vec<Predicate>) -> NogoodId {
        // New nogoods are always stored at the end.
        let nogood_id = NogoodId::create_from_index(self.predicates.len());
        // The first predicate stored in the arena is not a real predicate. Instead, the right-hand
        // side of that predicate stores the length of the nogood. of the nogood.
        let dummy_domain_id = DomainId::new(0);
        let predicate_storing_the_length = Predicate::new(
            dummy_domain_id,
            PredicateType::Equal,
            nogood_predicates.len() as i32,
        );
        self.predicates.push(predicate_storing_the_length);
        // The predicates from the nogood are placed after the length-indicating predicate
        self.predicates.append(&mut nogood_predicates);
        nogood_id
    }

    /// Returns the number of predicates in the nogood.
    fn get_len(&self, nogood_id: NogoodId) -> usize {
        // Recall that the length of nogood is stored in the right-hand side of the predicate
        // located at the position given by the nogood id.
        let size_storing_predicate = self.predicates[nogood_id.index()];
        size_storing_predicate.get_right_hand_side() as usize
    }
}

impl Index<NogoodId> for PredicateArena {
    type Output = [Predicate];

    fn index(&self, index: NogoodId) -> &Self::Output {
        let len = self.get_len(index);
        // Recall that the length of nogood is stored in the right-hand side of the predicate
        // located at the position given by the nogood id.
        // This means that the predicates start one position after.
        let start_index = index.index() + 1;
        &self.predicates[start_index..start_index + len]
    }
}

impl Index<&NogoodId> for PredicateArena {
    type Output = [Predicate];

    fn index(&self, index: &NogoodId) -> &Self::Output {
        let len = self.get_len(*index);
        // Recall that the length of nogood is stored in the right-hand side of the predicate
        // located at the position given by the nogood id.
        // This means that the predicates start one position after.
        let start_index = index.index() + 1;
        &self.predicates[start_index..start_index + len]
    }
}

impl IndexMut<NogoodId> for PredicateArena {
    fn index_mut(&mut self, index: NogoodId) -> &mut Self::Output {
        let len = self.get_len(index);
        // Recall that the length of nogood is stored in the right-hand side of the predicate
        // located at the position given by the nogood id.
        // This means that the predicates start one position after.
        let start_index = index.index() + 1;
        &mut self.predicates[start_index..start_index + len]
    }
}
