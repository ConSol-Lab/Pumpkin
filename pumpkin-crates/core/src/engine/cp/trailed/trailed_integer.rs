use crate::containers::StorageKey;

/// An integer whose value is automatically restored upon backtracking to its previous value at the
/// checkpoint to which backtracking occurred.
#[derive(Debug, Clone, Copy)]
pub struct TrailedInteger {
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
