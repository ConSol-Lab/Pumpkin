use crate::containers::StorageKey;

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

impl NogoodId {
    pub(crate) const fn into_bits(self) -> u32 {
        self.id
    }

    pub(crate) const fn from_bits(value: u32) -> Self {
        Self { id: value }
    }
}
