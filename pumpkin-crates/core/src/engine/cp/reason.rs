use std::fmt::Debug;

use super::propagation::ExplanationContext;
use super::propagation::PropagatorId;
use super::propagation::store::PropagatorStore;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Trail;
#[cfg(doc)]
use crate::containers::KeyedVec;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;

/// The reason store holds a reason for each change made by a CP propagator on a trail.
#[derive(Default, Debug)]
pub(crate) struct ReasonStore {
    trail: Trail<(PropagatorId, StoredReason)>,
}

impl ReasonStore {
    pub(crate) fn push(&mut self, propagator: PropagatorId, reason: StoredReason) -> ReasonRef {
        let index = self.trail.len();
        self.trail.push((propagator, reason));
        pumpkin_assert_simple!(
            index < (1 << 30),
            "ReasonRef in reason store should fit in ContraintReference, \
             which has 30 bits available at most"
        );
        ReasonRef(index as u32)
    }

    /// Similar to [`KeyedVec::new_slot`].
    pub(crate) fn new_slot(&mut self) -> Slot<'_> {
        Slot { store: self }
    }

    /// Evaluate the reason with the given reference, and write the predicates to
    /// `destination_buffer`.
    pub(crate) fn get_or_compute(
        &self,
        reference: ReasonRef,
        context: ExplanationContext<'_>,
        propagators: &mut PropagatorStore,
        destination_buffer: &mut impl Extend<Predicate>,
    ) -> bool {
        let Some(reason) = self.trail.get(reference.0 as usize) else {
            return false;
        };

        reason
            .1
            .compute(context, reason.0, propagators, destination_buffer);

        true
    }

    pub(crate) fn get_lazy_code(&self, reference: ReasonRef) -> Option<&u64> {
        match self.trail.get(reference.0 as usize) {
            Some(reason) => match &reason.1 {
                StoredReason::Eager(_) => None,
                StoredReason::DynamicLazy(code) => Some(code),
            },
            None => None,
        }
    }

    pub(crate) fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub(crate) fn synchronise(&mut self, level: usize) {
        let _ = self.trail.synchronise(level);
    }

    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.trail.len()
    }

    /// Get the propagator which generated the given reason.
    pub(crate) fn get_propagator(&self, reason_ref: ReasonRef) -> PropagatorId {
        self.trail.get(reason_ref.0 as usize).unwrap().0
    }
}

/// A reference to a reason
#[derive(Default, Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub(crate) struct ReasonRef(pub(crate) u32);

/// A reason for CP propagator to make a change
#[derive(Debug)]
pub(crate) enum Reason {
    /// An eager reason contains the propositional conjunction with the reason, without the
    ///   propagated predicate.
    Eager(PropositionalConjunction),
    /// A lazy reason, which is computed on-demand rather than up-front. This is also referred to
    /// as a 'backward' reason.
    ///
    /// A lazy reason contains a payload that propagators can use to identify what type of
    /// propagation the reason is for. The payload should be enough for the propagator to construct
    /// an explanation based on its internal state.
    DynamicLazy(u64),
}

/// A reason for CP propagator to make a change
#[derive(Debug)]
pub(crate) enum StoredReason {
    /// An eager reason contains the propositional conjunction with the reason, without the
    ///   propagated predicate.
    Eager(PropositionalConjunction),
    /// A lazy reason, which is computed on-demand rather than up-front. This is also referred to
    /// as a 'backward' reason.
    ///
    /// A lazy reason contains a payload that propagators can use to identify what type of
    /// propagation the reason is for. The payload should be enough for the propagator to construct
    /// an explanation based on its internal state.
    DynamicLazy(u64),
}

impl StoredReason {
    /// Evaluate the reason, and write the predicates to the `destination_buffer`.
    pub(crate) fn compute(
        &self,
        context: ExplanationContext<'_>,
        propagator_id: PropagatorId,
        propagators: &mut PropagatorStore,
        destination_buffer: &mut impl Extend<Predicate>,
    ) {
        match self {
            // We do not replace the reason with an eager explanation for dynamic lazy explanations.
            //
            // Benchmarking will have to show whether this should change or not.
            StoredReason::DynamicLazy(code) => destination_buffer.extend(
                propagators[propagator_id]
                    .lazy_explanation(*code, context)
                    .iter()
                    .copied(),
            ),
            StoredReason::Eager(result) => destination_buffer.extend(result.iter().copied()),
        }
    }
}

impl From<PropositionalConjunction> for Reason {
    fn from(value: PropositionalConjunction) -> Self {
        Reason::Eager(value)
    }
}

impl From<u64> for Reason {
    fn from(value: u64) -> Self {
        Reason::DynamicLazy(value)
    }
}

impl From<usize> for Reason {
    fn from(value: usize) -> Self {
        Reason::DynamicLazy(value as u64)
    }
}

/// A reserved slot for a new reason in the [`ReasonStore`].
#[derive(Debug)]
pub(crate) struct Slot<'a> {
    store: &'a mut ReasonStore,
}

impl Slot<'_> {
    /// The reference for this slot.
    pub(crate) fn reason_ref(&self) -> ReasonRef {
        ReasonRef(self.store.trail.len() as u32)
    }

    /// Populate the slot with a [`Reason`].
    pub(crate) fn populate(self, propagator: PropagatorId, reason: StoredReason) -> ReasonRef {
        self.store.push(propagator, reason)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::Assignments;
    use crate::engine::notifications::NotificationEngine;
    use crate::engine::variables::DomainId;

    #[test]
    fn computing_an_eager_reason_returns_a_reference_to_the_conjunction() {
        let integers = Assignments::default();
        let mut notification_engine = NotificationEngine::default();

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason = StoredReason::Eager(conjunction.clone());

        let mut out_reason = vec![];
        reason.compute(
            ExplanationContext::test_new(&integers, &mut notification_engine),
            PropagatorId(0),
            &mut PropagatorStore::default(),
            &mut out_reason,
        );

        assert_eq!(conjunction.as_slice(), &out_reason);
    }

    #[test]
    fn pushing_a_reason_gives_a_reason_ref_that_can_be_computed() {
        let mut reason_store = ReasonStore::default();
        let integers = Assignments::default();
        let mut notification_engine = NotificationEngine::default();

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason_ref =
            reason_store.push(PropagatorId(0), StoredReason::Eager(conjunction.clone()));

        assert_eq!(ReasonRef(0), reason_ref);

        let mut out_reason = vec![];
        let _ = reason_store.get_or_compute(
            reason_ref,
            ExplanationContext::test_new(&integers, &mut notification_engine),
            &mut PropagatorStore::default(),
            &mut out_reason,
        );

        assert_eq!(conjunction.as_slice(), &out_reason);
    }
}
