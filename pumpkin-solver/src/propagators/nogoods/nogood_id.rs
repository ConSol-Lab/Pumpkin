use crate::containers::StorageKey;

#[derive(Default, Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub(crate) struct NogoodId {
    pub(crate) id: u32,
}

impl StorageKey for NogoodId {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        NogoodId { id: index as u32 }
    }
}
