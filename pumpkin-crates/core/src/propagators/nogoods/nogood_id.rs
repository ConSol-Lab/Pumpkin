use bitfield_struct::bitfield;

use crate::containers::StorageKey;
use crate::propagators::nogoods::arena_allocator::NogoodIndex;
use crate::pumpkin_assert_simple;

/// The identifier of a nogood in the [`NogoodPropagator`].
///
/// There are two kinds of nogoods:
/// - Regular nogoods, which are stored in the [`ArenaAllocator`]; the `id` is then the offset of
///   the nogood in the arena.
/// - Binary nogoods, which are not stored in the arena; instead, they are stored inline in their
///   two watchers. The `id` is then the [`NogoodIndex`] of the binary nogood.
///
/// [`NogoodPropagator`]: crate::propagators::nogoods::NogoodPropagator
/// [`ArenaAllocator`]: crate::propagators::nogoods::arena_allocator::ArenaAllocator
#[bitfield(u32)]
#[derive(PartialEq, Eq, Hash)]
pub(crate) struct NogoodId {
    /// The offset in the arena for regular nogoods, or the [`NogoodIndex`] for binary nogoods.
    #[bits(31)]
    id: u32,
    /// Whether the nogood is a binary nogood (which is not stored in the arena).
    pub(crate) is_binary: bool,
}

impl StorageKey for NogoodId {
    fn index(&self) -> usize {
        pumpkin_assert_simple!(!self.is_binary());
        self.id() as usize
    }

    fn create_from_index(index: usize) -> Self {
        NogoodId::new().with_id(index as u32)
    }
}

impl NogoodId {
    /// Creates the [`NogoodId`] of the binary nogood with the provided [`NogoodIndex`].
    pub(crate) fn binary(index: NogoodIndex) -> Self {
        NogoodId::new()
            .with_id(index.index() as u32)
            .with_is_binary(true)
    }

    /// Returns the [`NogoodIndex`] of a binary nogood.
    pub(crate) fn binary_index(self) -> NogoodIndex {
        pumpkin_assert_simple!(self.is_binary());
        NogoodIndex::create_from_index(self.id() as usize)
    }
}
