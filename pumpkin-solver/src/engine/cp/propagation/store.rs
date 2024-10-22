use std::fmt::Debug;
use std::num::NonZero;
use std::ops::Index;
use std::ops::IndexMut;

use super::Propagator;
use super::PropagatorId;
use crate::basic_types::KeyedVec;
use crate::engine::DebugDyn;

/// A central store for propagators.
///
/// The propagator store associates tags with propagators, whenever a tag is provided for a
/// propagator.
#[derive(Default)]
pub(crate) struct PropagatorStore {
    propagators: KeyedVec<PropagatorId, Box<dyn Propagator>>,
    tags: KeyedVec<PropagatorId, Option<NonZero<u32>>>,
}

impl PropagatorStore {
    pub(crate) fn alloc(
        &mut self,
        propagator: Box<dyn Propagator>,
        tag: Option<NonZero<u32>>,
    ) -> PropagatorId {
        let id = self.propagators.push(propagator);
        let _ = self.tags.push(tag);

        id
    }

    pub(crate) fn get_tag(&self, propagator_id: PropagatorId) -> Option<NonZero<u32>> {
        self.tags[propagator_id]
    }

    pub(crate) fn iter_propagators(&self) -> impl Iterator<Item = &dyn Propagator> + '_ {
        self.propagators.iter().map(|b| b.as_ref())
    }

    pub(crate) fn iter_propagators_mut(
        &mut self,
    ) -> impl Iterator<Item = &mut Box<dyn Propagator>> + '_ {
        self.propagators.iter_mut()
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
        let cp_propagators: Vec<_> = self
            .propagators
            .iter()
            .map(|_| DebugDyn::from("Propagator"))
            .collect();

        write!(f, "{cp_propagators:?}")
    }
}
