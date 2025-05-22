use std::fmt::Debug;
use std::ops::Index;
use std::ops::IndexMut;

use super::Propagator;
use super::PropagatorId;
use crate::containers::KeyedVec;
use crate::containers::Slot;
use crate::engine::DebugDyn;

/// A central store for propagators.
#[derive(Default)]
pub(crate) struct PropagatorStore {
    propagators: KeyedVec<PropagatorId, Box<dyn Propagator>>,
}

impl PropagatorStore {
    pub(crate) fn iter_propagators(&self) -> impl Iterator<Item = &dyn Propagator> + '_ {
        self.propagators.iter().map(|b| b.as_ref())
    }

    pub(crate) fn iter_propagators_mut(
        &mut self,
    ) -> impl Iterator<Item = &mut Box<dyn Propagator>> + '_ {
        self.propagators.iter_mut()
    }

    pub(crate) fn new_propagator(&mut self) -> Slot<'_, PropagatorId, Box<dyn Propagator>> {
        self.propagators.new_slot()
    }
}

impl Index<PropagatorId> for PropagatorStore {
    type Output = dyn Propagator;

    fn index(&self, index: PropagatorId) -> &Self::Output {
        self.propagators[index].as_ref()
    }
}

impl IndexMut<PropagatorId> for PropagatorStore {
    fn index_mut(&mut self, index: PropagatorId) -> &mut Self::Output {
        self.propagators[index].as_mut()
    }
}

impl Debug for PropagatorStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let propagators: Vec<_> = self
            .propagators
            .iter()
            .map(|_| DebugDyn::from("Propagator"))
            .collect();

        write!(f, "{propagators:?}")
    }
}
