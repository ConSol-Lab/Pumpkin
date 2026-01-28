use std::ops::Index;
use std::ops::IndexMut;
use std::ops::Range;

use crate::basic_types::PredicateId;
use crate::containers::HashMap;
use crate::containers::StorageKey;
use crate::propagators::nogoods::NogoodId;

/// An arena allocator for storing nogoods.
///
/// The idea is to avoid double indirection by storing one large structure with [`PredicateId`]s.
///
/// Currently, deleting nogoods is not supported.
#[derive(Clone, Default, Debug)]
pub(crate) struct ArenaAllocator {
    /// A list of [`PredicateId`]s representing the nogoods.
    ///
    /// If there is a [`NogoodId`] with value `i`, then the [`PredicateId`] at position `i` will
    /// contain the length `x` of the nogood. The next `i + 1 + x` elements are then the nogood
    /// pointed to by the [`NogoodId`] with value `i`.
    nogoods: Vec<PredicateId>,
    /// Maps each [`NogoodId`] to an index; this is to prevent unnecessary allocations for other
    /// structures such as the [`NogoodInfo`] which use direct hashing for storing information
    /// about nogoods.
    nogood_id_to_index: HashMap<NogoodId, NogoodIndex>,
    /// The current index for the next [`NogoodId`] which is entered; see
    /// [`ArenaAllocator::nogood_id_to_index`].
    current_index: u32,
    capacity: usize,
}

#[derive(Clone, Copy, Debug, Hash)]
pub(crate) struct NogoodIndex(u32);

impl StorageKey for NogoodIndex {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        NogoodIndex(index as u32)
    }
}

impl ArenaAllocator {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            nogoods: Vec::default(),
            nogood_id_to_index: HashMap::default(),
            current_index: 0,
            capacity
        }
    }

    /// Inserts the nogood consisting of [`PredicateId`]s and returns its corresponding
    /// [`NogoodId`].
    pub(crate) fn insert(&mut self, nogood: Vec<PredicateId>) -> NogoodId {
        if self.nogoods.is_empty() {
            self.nogoods.reserve(self.capacity);
        }
        let nogood_id = NogoodId::create_from_index(self.nogoods.len());

        // We store the NogoodId with its index.
        let _ = self
            .nogood_id_to_index
            .insert(nogood_id, NogoodIndex(self.current_index));
        self.current_index += 1;

        // We push a PredicateId which stores the length of the nogood
        self.nogoods
            .push(PredicateId::create_from_index(nogood.len()));
        self.nogoods.extend(nogood);

        nogood_id
    }

    /// Returns the index of the provided [`NogoodId`].
    ///
    /// In other words, if the nogood with ID [`NogoodId`] was the `n`th nogood to be inserted then
    /// this method will return `n`.
    pub(crate) fn get_nogood_index(&self, nogood_id: &NogoodId) -> NogoodIndex {
        *self
            .nogood_id_to_index
            .get(nogood_id)
            .expect("Expected nogood predicate to exist")
    }

    /// Returns a list of all the present [`NogoodId`]s.
    pub(crate) fn nogoods_ids(&self) -> impl Iterator<Item = NogoodId> + '_ {
        NogoodIdIterator {
            nogoods: &self.nogoods,
            current_index: 0,
        }
    }

    /// Returns the length of the nogood corresponding to the provided [`NogoodId`].
    fn len_of_nogood(&self, nogood_id: NogoodId) -> usize {
        self.nogoods[nogood_id.index()].index()
    }

    /// Calculates the range of the nogood spanned by the nogood with ID [`NogoodId`].
    fn calculate_range_of_nogood(&self, nogood_id: NogoodId) -> Range<usize> {
        let len = self.len_of_nogood(nogood_id);
        nogood_id.index() + 1..nogood_id.index() + 1 + len
    }
}

impl Index<NogoodId> for ArenaAllocator {
    type Output = [PredicateId];

    fn index(&self, index: NogoodId) -> &Self::Output {
        let nogood_range = self.calculate_range_of_nogood(index);

        &self.nogoods[nogood_range]
    }
}

impl IndexMut<NogoodId> for ArenaAllocator {
    fn index_mut(&mut self, index: NogoodId) -> &mut Self::Output {
        let nogood_range = self.calculate_range_of_nogood(index);

        &mut self.nogoods[nogood_range]
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
