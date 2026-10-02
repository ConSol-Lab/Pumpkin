use std::ops::Range;

use crate::basic_types::PredicateId;
use crate::containers::HashMap;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::propagators::nogoods::NogoodId;
use crate::pumpkin_assert_eq_simple;

/// An arena allocator for storing nogoods.
///
/// The idea is to avoid double indirection by storing one large structure with [`PredicateId`]s.
///
/// The slot of a nogood which has been freed (see [`ArenaAllocator::free`]) is reused by the first
/// inserted nogood which fits in it, together with its [`NogoodId`] and [`NogoodIndex`].
#[derive(Clone, Default, Debug)]
pub(crate) struct ArenaAllocator {
    /// A list of [`PredicateId`]s representing the nogoods.
    ///
    /// If there is a [`NogoodId`] with value `i`, then the [`PredicateId`] at position `i` will
    /// contain the length `x` of the nogood, the [`PredicateId`] at position `i + 1` will contain
    /// the capacity `c >= x` of its slot, and the [`PredicateId`] at position `i + 2` will contain
    /// the last-traversed watcher index. The next `x` elements are then the nogood pointed to by
    /// the [`NogoodId`] with value `i`, and the slot ends after the next `c` elements.
    ///
    /// The capacity is larger than the length if the nogood was stored in the slot of a larger
    /// freed nogood; the remaining elements of the slot are then unused.
    pub(crate) nogoods: Vec<PredicateId>,
    /// Maps each [`NogoodId`] to an index; this is to prevent unnecessary allocations for other
    /// structures such as the [`NogoodInfo`] which use direct hashing for storing information
    /// about nogoods.
    pub(crate) nogood_id_to_index: HashMap<NogoodId, NogoodIndex>,
    /// The current index for the next [`NogoodId`] which is entered; see
    /// [`ArenaAllocator::nogood_id_to_index`].
    current_index: u32,
    /// The slots of the freed nogoods which can be reused by newly inserted nogoods.
    free_slots: Vec<NogoodId>,
    /// The number of elements (i.e., [`PredicateId`]s), that are created when the arena is
    /// initialised.
    ///
    /// Note that it is lazily initialised, so that this memory is only allocated the first time
    /// that a nogood is added to the arena.
    initial_capacity: usize,
}

/// The index offset which determines how many elements to skip before the actual nogood predicates
/// begin.
///
/// See [`ArenaAllocator::nogoods`] for more information.
const OFFSET: usize = 3;

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
            free_slots: Vec::default(),
            initial_capacity: capacity,
        }
    }

    /// Inserts the nogood consisting of [`PredicateId`]s and returns its corresponding
    /// [`NogoodId`] and [`NogoodIndex`].
    ///
    /// If the nogood fits in the slot of a freed nogood, then that slot is reused (including its
    /// [`NogoodId`] and [`NogoodIndex`]); see [`store_at_nogood_index`] for storing information
    /// about the nogood.
    pub(crate) fn insert(&mut self, nogood: Vec<PredicateId>) -> (NogoodId, NogoodIndex) {
        if self.nogoods.is_empty() {
            self.nogoods.reserve_exact(self.initial_capacity);
        }

        if let Some(position) = self
            .free_slots
            .iter()
            .position(|&nogood_id| self.capacity_of_slot(nogood_id) >= nogood.len())
        {
            let nogood_id = self.free_slots.swap_remove(position);

            self.nogoods[nogood_id.index()] = PredicateId::create_from_index(nogood.len());
            self.nogoods[nogood_id.index() + OFFSET - 1] = PredicateId::create_from_index(2);

            let start = nogood_id.index() + OFFSET;
            self.nogoods[start..start + nogood.len()].copy_from_slice(&nogood);

            return (nogood_id, self.get_nogood_index(&nogood_id));
        }

        let nogood_id = NogoodId::create_from_index(self.nogoods.len());

        // We store the NogoodId with its index.
        let nogood_index = NogoodIndex(self.current_index);
        let _ = self.nogood_id_to_index.insert(nogood_id, nogood_index);
        self.current_index += 1;

        // We push a PredicateId which stores the length of the nogood
        self.nogoods
            .push(PredicateId::create_from_index(nogood.len()));
        // We push a PredicateId which stores the capacity of the slot (equal to the length since
        // the slot is new)
        self.nogoods
            .push(PredicateId::create_from_index(nogood.len()));
        // We also push a PredicateId which stores the last-traversed watcher (defaults to the
        // first non-watcher element)
        self.nogoods.push(PredicateId::create_from_index(2));
        self.nogoods.extend(nogood);

        (nogood_id, nogood_index)
    }

    /// Frees the slot of the nogood with the provided [`NogoodId`] so that it can be reused by a
    /// newly inserted nogood.
    ///
    /// The nogood should not be used anymore after this call; i.e., it should not be watched, and
    /// it should not be the reason for any propagation.
    pub(crate) fn free(&mut self, nogood_id: NogoodId) {
        self.free_slots.push(nogood_id);
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

    /// Returns the capacity of the slot of the nogood corresponding to the provided [`NogoodId`].
    fn capacity_of_slot(&self, nogood_id: NogoodId) -> usize {
        self.nogoods[nogood_id.index() + 1].index()
    }

    /// Calculates the range of the nogood spanned by the nogood with ID [`NogoodId`].
    ///
    /// Does not include any of the information [`PredicateId`]s.
    fn calculate_range_of_nogood(&self, nogood_id: NogoodId) -> Range<usize> {
        let len = self.len_of_nogood(nogood_id);
        nogood_id.index() + OFFSET..nogood_id.index() + OFFSET + len
    }

    /// Calculates the range of the nogood spanned by the nogood with ID [`NogoodId`].
    ///
    /// Includes the [`PredicateId`] storing the last-traversed watcher index as the first element.
    #[allow(unused, reason = "Currently inlined due to borrow issues")]
    pub(crate) fn calculate_range_of_nogood_including_last_traversed(
        &self,
        nogood_id: NogoodId,
    ) -> Range<usize> {
        let len = self.len_of_nogood(nogood_id);
        nogood_id.index() + OFFSET - 1..nogood_id.index() + OFFSET + len
    }

    /// Returns the nogood pointed to by [`NogoodId`].
    pub(crate) fn get_nogood(&self, nogood_id: NogoodId) -> &[PredicateId] {
        let nogood_range = self.calculate_range_of_nogood(nogood_id);

        &self.nogoods[nogood_range]
    }

    /// Returns a mutable reference to the nogood pointed to by [`NogoodId`].
    #[allow(unused, reason = "Standard API")]
    pub(crate) fn get_nogood_mut(&mut self, nogood_id: NogoodId) -> &mut [PredicateId] {
        let nogood_range = self.calculate_range_of_nogood(nogood_id);

        &mut self.nogoods[nogood_range]
    }

    /// Returns a tuple consisting of a mutable reference to the index of the last-traversed watcher
    /// and a mutable reference to the nogood pointed to by [`NogoodId`].
    #[allow(unused, reason = "Currently inlined due to borrow issues")]
    pub(crate) fn get_nogood_mut_with_last_traversed(
        &mut self,
        nogood_id: NogoodId,
    ) -> (&mut u32, &mut [PredicateId]) {
        let nogood_range = self.calculate_range_of_nogood_including_last_traversed(nogood_id);

        self.nogoods[nogood_range]
            .split_first_mut()
            .map(|(last_traversed, nogood)| (&mut last_traversed.id, nogood))
            .expect("Expected nogood to be at least of length two")
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
        // We skip the whole slot, which is given by its capacity.
        self.current_index += self.nogoods[self.current_index + 1].id as usize + OFFSET;

        Some(id)
    }
}

/// Stores the `value` corresponding to the nogood with the provided [`NogoodIndex`].
///
/// If the [`NogoodIndex`] was reused from a freed nogood (see [`ArenaAllocator::insert`]), then the
/// value of the freed nogood is overwritten.
pub(crate) fn store_at_nogood_index<Value>(
    values: &mut KeyedVec<NogoodIndex, Value>,
    nogood_index: NogoodIndex,
    value: Value,
) {
    if nogood_index.index() < values.len() {
        values[nogood_index] = value;
    } else {
        let pushed_index = values.push(value);
        pumpkin_assert_eq_simple!(pushed_index.index(), nogood_index.index());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn predicate_ids(ids: &[u32]) -> Vec<PredicateId> {
        ids.iter().map(|&id| PredicateId { id }).collect()
    }

    #[test]
    fn freed_slot_of_same_length_is_reused() {
        let mut arena = ArenaAllocator::new(0);
        let (first_id, first_index) = arena.insert(predicate_ids(&[1, 2, 3]));
        let _ = arena.insert(predicate_ids(&[4, 5]));

        arena.free(first_id);
        let (id, index) = arena.insert(predicate_ids(&[6, 7, 8]));

        assert_eq!(id, first_id);
        assert_eq!(index.index(), first_index.index());
        assert_eq!(arena.get_nogood(id), predicate_ids(&[6, 7, 8]));
        assert_eq!(arena.nogoods_ids().count(), 2);
    }

    #[test]
    fn smaller_nogood_in_larger_freed_slot_is_skipped_over() {
        let mut arena = ArenaAllocator::new(0);
        let (first_id, _) = arena.insert(predicate_ids(&[1, 2, 3, 4]));
        let (second_id, _) = arena.insert(predicate_ids(&[5, 6]));

        arena.free(first_id);
        let (id, _) = arena.insert(predicate_ids(&[7, 8]));

        assert_eq!(id, first_id);
        assert_eq!(arena.get_nogood(id), predicate_ids(&[7, 8]));
        assert_eq!(arena.get_nogood(second_id), predicate_ids(&[5, 6]));
        assert_eq!(
            arena.nogoods_ids().collect::<Vec<_>>(),
            vec![first_id, second_id]
        );
    }

    #[test]
    fn freed_slots_which_are_too_small_are_skipped() {
        let mut arena = ArenaAllocator::new(0);
        let (small_id, _) = arena.insert(predicate_ids(&[1, 2]));
        let (large_id, _) = arena.insert(predicate_ids(&[3, 4, 5, 6]));

        arena.free(small_id);
        arena.free(large_id);
        let (id, _) = arena.insert(predicate_ids(&[7, 8, 9]));

        assert_eq!(id, large_id);
        assert_eq!(arena.get_nogood(id), predicate_ids(&[7, 8, 9]));
    }

    #[test]
    fn freeing_slot_with_smaller_nogood_keeps_its_capacity() {
        let mut arena = ArenaAllocator::new(0);
        let (first_id, _) = arena.insert(predicate_ids(&[1, 2, 3, 4]));
        let _ = arena.insert(predicate_ids(&[5, 6]));

        arena.free(first_id);
        let _ = arena.insert(predicate_ids(&[7, 8]));
        arena.free(first_id);

        assert_eq!(arena.capacity_of_slot(first_id), 4);
        let (id, _) = arena.insert(predicate_ids(&[9, 10, 11, 12]));
        assert_eq!(id, first_id);
        assert_eq!(arena.get_nogood(id), predicate_ids(&[9, 10, 11, 12]));
    }

    #[test]
    fn store_at_nogood_index_overwrites_reused_index() {
        let mut arena = ArenaAllocator::new(0);
        let mut values: KeyedVec<NogoodIndex, u32> = KeyedVec::default();

        let (first_id, first_index) = arena.insert(predicate_ids(&[1, 2]));
        store_at_nogood_index(&mut values, first_index, 10);
        let (_, second_index) = arena.insert(predicate_ids(&[3, 4]));
        store_at_nogood_index(&mut values, second_index, 20);

        arena.free(first_id);
        let (_, index) = arena.insert(predicate_ids(&[5, 6]));
        store_at_nogood_index(&mut values, index, 30);

        assert_eq!(values.len(), 2);
        assert_eq!(values[first_index], 30);
        assert_eq!(values[second_index], 20);
    }
}
