use std::cmp::max;
use std::ops::Range;

use crate::basic_types::PredicateId;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::propagators::nogoods::NogoodId;
use crate::pumpkin_assert_eq_simple;

/// An arena allocator for storing nogoods.
///
/// The idea is to avoid double indirection by storing one large structure with [`PredicateId`]s.
///
/// Nogoods are always appended to the end of the arena. The space of removed nogoods is reclaimed
/// by compacting the arena (see [`ArenaAllocator::compact`]), which moves the remaining nogoods
/// to the front of the arena.
///
/// The nogoods are stored in the arena in the order of their [`NogoodIndex`]; this order is
/// maintained when compacting the arena.
#[derive(Clone, Default, Debug)]
pub(crate) struct ArenaAllocator {
    /// A list of [`PredicateId`]s representing the nogoods.
    ///
    /// If there is a [`NogoodId`] with value `i`, then the [`PredicateId`] at position `i` will
    /// contain the length `x` of the nogood, the [`PredicateId`] at position `i + 1` will contain
    /// its [`NogoodIndex`], and the [`PredicateId`] at position `i + 2` will contain the
    /// last-traversed watcher index. The next `x` elements are then the nogood pointed to by the
    /// [`NogoodId`] with value `i`.
    pub(crate) nogoods: Vec<PredicateId>,
    /// The [`NogoodIndex`] of the next nogood which is inserted; see
    /// [`ArenaAllocator::get_nogood_index`].
    current_index: u32,
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
            current_index: 0,
            initial_capacity: capacity,
        }
    }

    /// Inserts the nogood consisting of [`PredicateId`]s and returns its corresponding
    /// [`NogoodId`].
    pub(crate) fn insert(&mut self, nogood: Vec<PredicateId>) -> NogoodId {
        if self.nogoods.is_empty() {
            self.nogoods.reserve_exact(self.initial_capacity);
        }

        let nogood_id = NogoodId::create_from_index(self.nogoods.len());

        // We push a PredicateId which stores the length of the nogood
        self.nogoods
            .push(PredicateId::create_from_index(nogood.len()));
        // We push a PredicateId which stores the index of the nogood
        self.nogoods.push(PredicateId {
            id: self.current_index,
        });
        self.current_index += 1;
        // We also push a PredicateId which stores the last-traversed watcher (defaults to the
        // first non-watcher element)
        self.nogoods.push(PredicateId::create_from_index(2));
        self.nogoods.extend(nogood);

        nogood_id
    }

    /// Returns the index of the provided [`NogoodId`].
    ///
    /// In other words, if the nogood with ID [`NogoodId`] was the `n`th nogood to be inserted then
    /// this method will return `n`.
    pub(crate) fn get_nogood_index(&self, nogood_id: &NogoodId) -> NogoodIndex {
        NogoodIndex(self.nogoods[nogood_id.index() + 1].id)
    }

    /// Returns the [`NogoodId`] which each nogood will have after compacting the arena (see
    /// [`ArenaAllocator::compact`]), indexed by its current [`NogoodIndex`]. A nogood is only kept
    /// if `keep` is true for its [`NogoodIndex`]; otherwise, its new [`NogoodId`] is [`None`].
    ///
    /// Note that this does not change the arena; this allows the references to the nogoods to be
    /// updated before the nogoods are moved.
    pub(crate) fn compute_compacted_ids(
        &self,
        keep: &KeyedVec<NogoodIndex, bool>,
    ) -> KeyedVec<NogoodIndex, Option<NogoodId>> {
        let mut compacted_ids = KeyedVec::default();
        let mut compacted_len = 0;

        for nogood_id in self.nogoods_ids() {
            let nogood_index = self.get_nogood_index(&nogood_id);
            pumpkin_assert_eq_simple!(nogood_index.index(), compacted_ids.len());

            let compacted_id = keep[nogood_index].then(|| {
                let compacted_id = NogoodId::create_from_index(compacted_len);
                compacted_len += OFFSET + self.len_of_nogood(nogood_id);
                compacted_id
            });
            let _ = compacted_ids.push(compacted_id);
        }

        compacted_ids
    }

    /// Compacts the arena by removing the nogoods which do not have a new [`NogoodId`] in
    /// `compacted_ids` (see [`ArenaAllocator::compute_compacted_ids`]), and moving the remaining
    /// nogoods to their new [`NogoodId`].
    ///
    /// The remaining nogoods are given consecutive [`NogoodIndex`]s in the same order; i.e., a
    /// nogood with [`NogoodIndex`] `i` gets the [`NogoodIndex`] `j` if `j` of the nogoods with a
    /// [`NogoodIndex`] smaller than `i` are kept.
    pub(crate) fn compact(&mut self, compacted_ids: &KeyedVec<NogoodIndex, Option<NogoodId>>) {
        let mut compacted_len = 0;
        let mut compacted_index = 0;

        let mut start = 0;
        while start < self.nogoods.len() {
            let nogood_id = NogoodId::create_from_index(start);
            let end = start + OFFSET + self.len_of_nogood(nogood_id);

            // Note that the nogood is only moved towards the front, so the nogoods after it have
            // not been overwritten yet.
            if let Some(compacted_id) = compacted_ids[self.get_nogood_index(&nogood_id)] {
                pumpkin_assert_eq_simple!(compacted_id.index(), compacted_len);

                self.nogoods.copy_within(start..end, compacted_len);
                self.nogoods[compacted_len + 1] = PredicateId {
                    id: compacted_index,
                };

                compacted_len += end - start;
                compacted_index += 1;
            }

            start = end;
        }

        self.nogoods.truncate(compacted_len);
        self.current_index = compacted_index;

        // Similar to kissat, we release memory if the arena is mostly unused.
        if self.nogoods.len() < self.nogoods.capacity() / 4 {
            self.nogoods
                .shrink_to(max(2 * self.nogoods.len(), self.initial_capacity));
        }
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
        self.current_index += self.nogoods[self.current_index].id as usize + OFFSET;

        Some(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nogood_index_is_stored_in_header() {
        let mut arena = ArenaAllocator::new(10);
        let first = arena.insert(vec![PredicateId { id: 1 }, PredicateId { id: 2 }]);
        let second = arena.insert(vec![PredicateId { id: 3 }, PredicateId { id: 4 }]);

        assert_eq!(arena.get_nogood_index(&first).index(), 0);
        assert_eq!(arena.get_nogood_index(&second).index(), 1);
        assert_eq!(
            arena.get_nogood(second),
            [PredicateId { id: 3 }, PredicateId { id: 4 }]
        );
        assert_eq!(arena.nogoods_ids().collect::<Vec<_>>(), [first, second]);
    }

    #[test]
    fn compacting_moves_kept_nogoods_to_the_front_with_consecutive_indices() {
        let mut arena = ArenaAllocator::new(20);
        let _ = arena.insert(vec![PredicateId { id: 1 }, PredicateId { id: 2 }]);
        let second = arena.insert(vec![PredicateId { id: 3 }, PredicateId { id: 4 }]);
        let _ = arena.insert(vec![PredicateId { id: 5 }, PredicateId { id: 6 }]);
        let fourth = arena.insert(vec![
            PredicateId { id: 7 },
            PredicateId { id: 8 },
            PredicateId { id: 9 },
        ]);
        let mut keep = KeyedVec::default();
        for is_kept in [false, true, false, true] {
            let _ = keep.push(is_kept);
        }

        let compacted_ids = arena.compute_compacted_ids(&keep);
        let new_second = compacted_ids[arena.get_nogood_index(&second)].unwrap();
        let new_fourth = compacted_ids[arena.get_nogood_index(&fourth)].unwrap();
        arena.compact(&compacted_ids);

        assert_eq!(
            arena.nogoods_ids().collect::<Vec<_>>(),
            [new_second, new_fourth]
        );
        assert_eq!(arena.get_nogood_index(&new_second).index(), 0);
        assert_eq!(arena.get_nogood_index(&new_fourth).index(), 1);
        assert_eq!(
            arena.get_nogood(new_fourth),
            [
                PredicateId { id: 7 },
                PredicateId { id: 8 },
                PredicateId { id: 9 }
            ]
        );

        let fifth = arena.insert(vec![PredicateId { id: 10 }, PredicateId { id: 11 }]);
        assert_eq!(arena.get_nogood_index(&fifth).index(), 2);
    }
}
