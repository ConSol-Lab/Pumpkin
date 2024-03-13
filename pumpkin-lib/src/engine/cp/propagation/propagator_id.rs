use std::ops::Index;
use std::ops::IndexMut;

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

impl<T> Index<PropagatorId> for Vec<T> {
    type Output = T;

    fn index(&self, index: PropagatorId) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T> IndexMut<PropagatorId> for Vec<T> {
    fn index_mut(&mut self, index: PropagatorId) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}
