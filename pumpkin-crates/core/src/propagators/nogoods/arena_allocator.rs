use std::ops::Index;
use std::ops::IndexMut;

use crate::basic_types::PredicateId;
use crate::containers::HashMap;
use crate::containers::StorageKey;
use crate::propagators::nogoods::NogoodId;

#[derive(Clone, Default, Debug)]
pub(crate) struct ArenaAllocator {
    nogoods: Vec<PredicateId>,
    nogood_id_to_index: HashMap<NogoodId, u32>,
    current_index: u32,
}

impl ArenaAllocator {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            nogoods: Vec::with_capacity(capacity),
            nogood_id_to_index: HashMap::default(),
            current_index: 0,
        }
    }

    pub(crate) fn insert(&mut self, nogood: Vec<PredicateId>) -> NogoodId {
        let nogood_id = NogoodId::create_from_index(self.nogoods.len());

        let _ = self
            .nogood_id_to_index
            .insert(nogood_id, self.current_index);
        self.current_index += 1;

        self.nogoods
            .push(PredicateId::create_from_index(nogood.len()));
        self.nogoods.extend(nogood);

        nogood_id
    }

    pub(crate) fn get_index_of_nogood(&self, nogood_id: &NogoodId) -> u32 {
        *self
            .nogood_id_to_index
            .get(nogood_id)
            .expect("Expected nogood predicate to exist")
    }

    pub(crate) fn nogoods_ids(&self) -> impl Iterator<Item = NogoodId> + '_ {
        NogoodIdIterator {
            nogoods: &self.nogoods,
            current_index: 0,
        }
    }

    fn len_of_nogood(&self, nogood_id: NogoodId) -> usize {
        self.nogoods[nogood_id.index()].index()
    }
}

impl Index<NogoodId> for ArenaAllocator {
    type Output = [PredicateId];

    fn index(&self, index: NogoodId) -> &Self::Output {
        let len = self.len_of_nogood(index);

        &self.nogoods[index.index() + 1..index.index() + 1 + len]
    }
}

impl IndexMut<NogoodId> for ArenaAllocator {
    fn index_mut(&mut self, index: NogoodId) -> &mut Self::Output {
        let len = self.len_of_nogood(index);

        &mut self.nogoods[index.index() + 1..index.index() + 1 + len]
    }
}

pub(crate) struct NogoodIdIterator<'a> {
    nogoods: &'a Vec<PredicateId>,
    current_index: usize,
}

impl Iterator for NogoodIdIterator<'_> {
    type Item = NogoodId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index >= self.nogoods.len() {
            return None;
        }
        let id = NogoodId::create_from_index(self.current_index);
        self.current_index += self.nogoods[self.current_index].id as usize + 1;

        Some(id)
    }
}
