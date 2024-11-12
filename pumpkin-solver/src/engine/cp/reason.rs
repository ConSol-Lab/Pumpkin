use std::fmt::Debug;

use super::propagation::store::PropagatorStore;
use super::propagation::PropagatorId;
use super::Assignments;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Trail;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;

/// The reason store holds a reason for each change made by a CP propagator on a trail.
///   This trail makes is easy to garbage collect reasons by simply synchronising whenever
///   the `Assignments` and `AssignmentsPropositional` are synchronised.
#[derive(Default, Debug)]
pub struct ReasonStore {
    trail: Trail<(PropagatorId, Reason)>,
    pub helper: PropositionalConjunction,
}

impl ReasonStore {
    pub fn push(&mut self, propagator: PropagatorId, reason: Reason) -> ReasonRef {
        let index = self.trail.len();
        self.trail.push((propagator, reason));
        pumpkin_assert_simple!(
            index < (1 << 30),
            "ReasonRef in reason store should fit in ContraintReference, \
             which has 30 bits available at most"
        );
        ReasonRef(index as u32)
    }

    pub fn get_or_compute<'this>(
        &'this self,
        reference: ReasonRef,
        assignments: &Assignments,
        propagators: &'this mut PropagatorStore,
    ) -> Option<&'this [Predicate]> {
        self.trail
            .get(reference.0 as usize)
            .map(|reason| reason.1.compute(assignments, reason.0, propagators))
    }

    pub fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub fn synchronise(&mut self, level: usize) {
        let _ = self.trail.synchronise(level);
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.trail.len()
    }

    /// Get the propagator which generated the given reason.
    pub fn get_propagator(&self, reason_ref: ReasonRef) -> PropagatorId {
        self.trail.get(reason_ref.0 as usize).unwrap().0
    }
}

/// A reference to a reason
#[derive(Default, Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct ReasonRef(pub(crate) u32);

/// A reason for CP propagator to make a change
#[derive(Debug)]
pub enum Reason {
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

impl Reason {
    pub fn compute<'a>(
        &'a self,
        assignments: &Assignments,
        propagator_id: PropagatorId,
        propagators: &'a mut PropagatorStore,
    ) -> &'a [Predicate] {
        match self {
            // We do not replace the reason with an eager explanation for dynamic lazy explanations.
            //
            // Benchmarking will have to show whether this should change or not.
            Reason::DynamicLazy(code) => {
                propagators[propagator_id].lazy_explanation(*code, assignments)
            }
            Reason::Eager(result) => result.as_slice(),
        }
    }
}

impl From<PropositionalConjunction> for Reason {
    fn from(value: PropositionalConjunction) -> Self {
        Reason::Eager(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::variables::DomainId;
    use crate::engine::Assignments;

    #[test]
    fn computing_an_eager_reason_returns_a_reference_to_the_conjunction() {
        let integers = Assignments::default();

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason = Reason::Eager(conjunction.clone());

        assert_eq!(
            conjunction.as_slice(),
            reason.compute(&integers, PropagatorId(0), &mut PropagatorStore::default())
        );
    }

    #[test]
    fn pushing_a_reason_gives_a_reason_ref_that_can_be_computed() {
        let mut reason_store = ReasonStore::default();
        let integers = Assignments::default();

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason_ref = reason_store.push(PropagatorId(0), Reason::Eager(conjunction.clone()));

        assert_eq!(ReasonRef(0), reason_ref);

        assert_eq!(
            Some(conjunction.as_slice()),
            reason_store.get_or_compute(reason_ref, &integers, &mut PropagatorStore::default())
        );
    }
}
