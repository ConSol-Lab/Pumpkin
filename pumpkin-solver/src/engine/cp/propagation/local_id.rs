use crate::containers::StorageKey;

/// A local id uniquely identifies a variable within a specific propagator. A local id can be
/// thought of as the index of the variable in the propagator.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct LocalId(u32);

impl LocalId {
    pub(crate) const fn from(value: u32) -> Self {
        LocalId(value)
    }

    pub(crate) fn unpack(self) -> u32 {
        self.0
    }
}

impl StorageKey for LocalId {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        LocalId(index as u32)
    }
}

impl std::fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
