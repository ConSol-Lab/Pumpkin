use std::fmt::Debug;
use std::marker::PhantomData;
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

/// A typed wrapper around a [`PropagatorId`] that allows retrieving concrete propagators from the
/// [`PropagatorStore`].
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PropagatorHandle<P> {
    id: PropagatorId,
    propagator: PhantomData<P>,
}

impl<P> PropagatorHandle<P> {
    /// Get a type-erased handle to the propagator.
    pub(crate) fn untyped(self) -> PropagatorId {
        self.id
    }
}

impl<P> Clone for PropagatorHandle<P> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<P> Copy for PropagatorHandle<P> {}

impl PropagatorStore {
    pub(crate) fn num_propagators(&self) -> usize {
        self.propagators.len()
    }

    pub(crate) fn iter_propagators(&self) -> impl Iterator<Item = &dyn Propagator> + '_ {
        self.propagators.iter().map(|b| b.as_ref())
    }

    pub(crate) fn iter_propagators_mut(
        &mut self,
    ) -> impl Iterator<Item = &mut Box<dyn Propagator>> + '_ {
        self.propagators.iter_mut()
    }

    pub(crate) fn new_propagator<P>(&mut self) -> NewPropagator<'_, P> {
        NewPropagator {
            underlying: self.propagators.new_slot(),
            propagator: PhantomData,
        }
    }

    /// Get a reference to the propagator identified by the given handle.
    ///
    /// To prevent downcasting, [`PropagatorStore`] implements [`Index`] and [`IndexMut`] with
    /// [`PropagatorId`] as an index.
    pub(crate) fn get_propagator<P: Propagator>(&self, handle: PropagatorHandle<P>) -> Option<&P> {
        self[handle.id].downcast_ref()
    }

    /// Get an exclusive reference to the propagator identified by the given handle.
    ///
    /// For more info, see [`Self::get_propagator`].
    pub(crate) fn get_propagator_mut<P: Propagator>(
        &mut self,
        handle: PropagatorHandle<P>,
    ) -> Option<&mut P> {
        self[handle.id].downcast_mut()
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

pub(crate) struct NewPropagator<'a, P> {
    underlying: Slot<'a, PropagatorId, Box<dyn Propagator>>,
    propagator: PhantomData<P>,
}

impl<P> NewPropagator<'_, P> {
    /// The handle corresponding to this slot.
    pub(crate) fn key(&self) -> PropagatorHandle<P> {
        PropagatorHandle {
            id: self.underlying.key(),
            propagator: PhantomData,
        }
    }

    /// Put a propagator into the slot.
    pub(crate) fn populate(self, propagator: Box<dyn Propagator>) -> PropagatorHandle<P> {
        PropagatorHandle {
            id: self.underlying.populate(propagator),
            propagator: PhantomData,
        }
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
