use crate::containers::StorageKey;

/// An identifier to a propagator instance within the solver.
/// Each propagator is assigned a unique identifier at runtime.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct PropagatorId(pub(crate) u32);

impl std::fmt::Display for PropagatorId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PropagatorId({})", self.0)
    }
}

impl StorageKey for PropagatorId {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        PropagatorId(index as u32)
    }
}
