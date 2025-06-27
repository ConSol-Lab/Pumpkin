use crate::containers::StorageKey;

#[derive(Debug, Clone, Copy)]
pub(crate) struct TrailedInteger {
    id: u32,
}

impl StorageKey for TrailedInteger {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        Self { id: index as u32 }
    }
}
