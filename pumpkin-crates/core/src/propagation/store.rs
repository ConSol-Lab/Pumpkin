use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;

use super::Priority;
use super::Propagator;
use super::PropagatorId;
use crate::containers::KeyedVec;
use crate::containers::Slot;
use crate::engine::DebugDyn;
use crate::pumpkin_assert_simple;

/// A central store for propagators.
#[derive(Default, Clone)]
pub(crate) struct PropagatorStore {
    propagators: KeyedVec<PropagatorId, Box<dyn Propagator>>,
    /// Information about each propagator which is used when notifying propagators, stored
    /// separately to avoid dynamic dispatch.
    infos: KeyedVec<PropagatorId, PropagatorInfo>,
}

/// Information about a propagator which does not change after it has been added.
#[derive(Clone, Copy, Debug)]
struct PropagatorInfo {
    /// The [`Priority`] of the propagator (see [`Propagator::priority`]).
    priority: Priority,
    /// Whether [`Propagator::notify`] should be called (see
    /// [`crate::propagation::PropagatorSpec::requires_notify`]).
    requires_notify: bool,
}

/// A typed wrapper around a propagator id that allows retrieving concrete propagators instead of
/// type-erased instances `Box<dyn Propagator>`.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PropagatorHandle<P> {
    id: PropagatorId,
    propagator: PhantomData<P>,
}

impl<P> PropagatorHandle<P> {
    pub(crate) fn new(propagator_id: PropagatorId) -> PropagatorHandle<P> {
        Self {
            id: propagator_id,
            propagator: PhantomData,
        }
    }

    /// Get the type-erased [`PropagatorId`] of the propagator.
    pub(crate) fn propagator_id(self) -> PropagatorId {
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

    pub(crate) fn new_propagator<P>(&mut self) -> NewPropagatorSlot<'_, P> {
        NewPropagatorSlot {
            underlying_slot: self.propagators.new_slot(),
            infos: &mut self.infos,
            propagator_type: PhantomData,
        }
    }

    /// Returns the [`Priority`] of the propagator with the given id.
    pub(crate) fn priority(&self, propagator_id: PropagatorId) -> Priority {
        self.infos[propagator_id].priority
    }

    /// Returns whether [`Propagator::notify`] should be called for the propagator with the given
    /// id.
    pub(crate) fn requires_notify(&self, propagator_id: PropagatorId) -> bool {
        self.infos[propagator_id].requires_notify
    }

    /// Get an exclusive reference to the propagator identified by the given handle.
    ///
    /// For more info, see [`Self::get_propagator`].
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

    /// Get the given [`PropagatorId`] as a handle if the ID points to a propagator of type `P`.
    pub(crate) fn as_propagator_handle<P: Propagator>(
        &self,
        propagator_id: PropagatorId,
    ) -> Option<PropagatorHandle<P>> {
        if self[propagator_id].is::<P>() {
            Some(PropagatorHandle {
                id: propagator_id,
                propagator: PhantomData,
            })
        } else {
            None
        }
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

/// Wrapper around a [`Slot`] that provides a strongly typed [`PropagatorHandle`] instead of a
/// type-erased [`PropagatorId`].
pub(crate) struct NewPropagatorSlot<'a, P> {
    underlying_slot: Slot<'a, PropagatorId, Box<dyn Propagator>>,
    infos: &'a mut KeyedVec<PropagatorId, PropagatorInfo>,
    propagator_type: PhantomData<P>,
}

impl<P: Propagator + 'static> NewPropagatorSlot<'_, P> {
    /// The handle corresponding to this slot.
    pub(crate) fn key(&self) -> PropagatorHandle<P> {
        PropagatorHandle {
            id: self.underlying_slot.key(),
            propagator: PhantomData,
        }
    }

    /// Put a propagator into the slot.
    ///
    /// See [`crate::propagation::PropagatorSpec::requires_notify`] for `requires_notify`.
    pub(crate) fn populate(self, propagator: P, requires_notify: bool) -> PropagatorHandle<P> {
        let info_id = self.infos.push(PropagatorInfo {
            priority: propagator.priority(),
            requires_notify,
        });
        let id = self.underlying_slot.populate(Box::new(propagator));
        pumpkin_assert_simple!(info_id == id);

        PropagatorHandle {
            id,
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
