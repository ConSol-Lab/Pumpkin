use std::fmt::Debug;

use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Trail;
#[cfg(doc)]
use crate::containers::KeyedVec;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::propagation::ExplanationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorId;
use crate::propagation::ReadDomains;
use crate::propagation::store::PropagatorStore;
use crate::pumpkin_assert_simple;

/// The reason store holds a reason for each change made by a CP propagator on a trail.
#[derive(Default, Debug, Clone)]
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

    /// Evaluate the reason with the given reference, write the predicates to `destination_buffer`,
    /// and return the [`InferenceCode`] associated with the reason.
    ///
    /// # Panics
    /// Panics if `reference` does not exist in the store.
    pub(crate) fn get_or_compute(
        &self,
        reference: ReasonRef,
        context: ExplanationContext<'_>,
        propagators: &mut PropagatorStore,
        destination_buffer: &mut impl Extend<Predicate>,
        predicate_to_explain: Predicate,
    ) -> InferenceCode {
        let reason = self
            .trail
            .get(reference.0 as usize)
            .expect("cannot get reason for predicate");

        reason.1.compute(
            context,
            reason.0,
            propagators,
            destination_buffer,
            predicate_to_explain,
        )
    }

    pub(crate) fn get_lazy_code(&self, reference: ReasonRef) -> Option<u64> {
        match self.trail.get(reference.0 as usize) {
            Some(reason) => match &reason.1 {
                StoredReason::Eager(_, _) => None,
                StoredReason::DynamicLazy(code) => Some(*code),
            },
            None => None,
        }
    }

    pub(crate) fn new_checkpoint(&mut self) {
        self.trail.new_checkpoint()
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
pub enum Reason {
    /// An eager reason contains the propositional conjunction with the reason, without the
    ///   propagated predicate, and the [`InferenceCode`] identifying the explanation algorithm.
    Eager(PropositionalConjunction, InferenceCode),
    /// A lazy reason, which is computed on-demand rather than up-front. This is also referred to
    /// as a 'backward' reason.
    ///
    /// A lazy reason contains a payload that propagators can use to identify what type of
    /// propagation the reason is for. The payload should be enough for the propagator to construct
    /// an explanation based on its internal state. The [`InferenceCode`] is returned by
    /// [`crate::propagation::Propagator::lazy_explanation`] on demand.
    DynamicLazy(u64),
}

/// A reason for CP propagator to make a change
#[derive(Debug, Clone)]
pub(crate) enum StoredReason {
    /// An eager reason contains the propositional conjunction with the reason, without the
    ///   propagated predicate, and the [`InferenceCode`] identifying the explanation algorithm.
    Eager(PropositionalConjunction, InferenceCode),
    /// A lazy reason, which is computed on-demand rather than up-front. This is also referred to
    /// as a 'backward' reason.
    ///
    /// A lazy reason contains a payload that propagators can use to identify what type of
    /// propagation the reason is for. The payload should be enough for the propagator to construct
    /// an explanation based on its internal state. The [`InferenceCode`] is returned by
    /// [`crate::propagation::Propagator::lazy_explanation`] on demand.
    DynamicLazy(u64),
}

impl StoredReason {
    /// Evaluate the reason, write the predicates to `destination_buffer`, and return the
    /// [`InferenceCode`] associated with the reason.
    pub(crate) fn compute(
        &self,
        context: ExplanationContext<'_>,
        propagator_id: PropagatorId,
        propagators: &mut PropagatorStore,
        destination_buffer: &mut impl Extend<Predicate>,
        predicate_to_explain: Predicate,
    ) -> InferenceCode {
        match self {
            // We do not replace the reason with an eager explanation for dynamic lazy explanations.
            //
            // Benchmarking will have to show whether this should change or not.
            StoredReason::DynamicLazy(code) => self.compute_lazy_explanation(
                context,
                *code,
                &mut propagators[propagator_id],
                destination_buffer,
                predicate_to_explain,
            ),

            StoredReason::Eager(result, inference_code) => {
                destination_buffer.extend(result.iter().copied());
                inference_code.clone()
            }
        }
    }

    fn compute_lazy_explanation(
        &self,
        mut context: ExplanationContext<'_>,
        code: u64,
        propagator: &mut dyn Propagator,
        destination_buffer: &mut impl Extend<Predicate>,
        predicate_to_explain: Predicate,
    ) -> InferenceCode {
        if let Some((hypercube, linear, inference_code)) =
            propagator.explain_as_hypercube_linear(code, context.reborrow())
        {
            convert_hl_to_clause(
                context,
                hypercube,
                linear,
                destination_buffer,
                predicate_to_explain,
            );
            inference_code
        } else {
            let explanation = propagator.lazy_explanation(code, context);
            destination_buffer.extend(explanation.predicates.iter().copied());
            explanation.inference_code
        }
    }
}

fn convert_hl_to_clause(
    context: ExplanationContext<'_>,
    hypercube: crate::hypercube_linear::Hypercube,
    linear: crate::hypercube_linear::LinearInequality,
    destination_buffer: &mut impl Extend<Predicate>,
    predicate_to_explain: Predicate,
) {
    let unsatisfied_hypercube_predicates = hypercube
        .iter_predicates()
        .filter(|&p| {
            context.evaluate_predicate_at_trail_position(p, context.get_trail_position())
                != Some(true)
        })
        .collect::<Vec<_>>();

    if unsatisfied_hypercube_predicates.is_empty() {
        // The propagation is part of the linear. The hypercube is entirely part of the
        // reason.
        destination_buffer.extend(hypercube.iter_predicates());

        // Add all lower bounds, except for the domain that was propagated. The iterator is
        // guaranteed to yield every domain at most once.
        destination_buffer.extend(linear.terms().filter_map(|term| {
            if term.inner == predicate_to_explain.get_domain() {
                None
            } else {
                let lb = context.lower_bound_at_trail_position(&term, context.get_trail_position());
                Some(predicate![term >= lb])
            }
        }));
    } else {
        assert_eq!(
            unsatisfied_hypercube_predicates.len(),
            1,
            "cannot have more than one unassigned predicate when a hypercube linear propagates"
        );

        let unsatisfied_predicate = unsatisfied_hypercube_predicates[0];

        // Add all true predicates in the hypercube.
        destination_buffer.extend(
            hypercube
                .iter_predicates()
                .filter(|&p| p != unsatisfied_predicate),
        );

        // Add all lower bounds that are true.
        destination_buffer.extend(
            linear
                .terms()
                .filter_map(|term| {
                    if term.inner == predicate_to_explain.get_domain() {
                        None
                    } else {
                        let lb = context
                            .lower_bound_at_trail_position(&term, context.get_trail_position());
                        Some(predicate![term >= lb])
                    }
                })
                .filter(|&p| {
                    context.evaluate_predicate_at_trail_position(p, context.get_trail_position())
                        == Some(true)
                }),
        );
    }
}

impl From<(PropositionalConjunction, &InferenceCode)> for Reason {
    fn from((conj, code): (PropositionalConjunction, &InferenceCode)) -> Self {
        Reason::Eager(conj, code.clone())
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
    use std::num::NonZero;

    use super::*;
    use crate::conjunction;
    use crate::engine::Assignments;
    use crate::engine::notifications::NotificationEngine;
    use crate::engine::variables::DomainId;
    use crate::proof::ConstraintTag;

    fn dummy_inference_code() -> InferenceCode {
        InferenceCode::unknown_label(ConstraintTag::from_non_zero(NonZero::new(1).unwrap()))
    }

    #[test]
    fn computing_an_eager_reason_returns_a_reference_to_the_conjunction() {
        let integers = Assignments::default();
        let mut notification_engine = NotificationEngine::default();

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason = StoredReason::Eager(conjunction.clone(), dummy_inference_code());

        let mut out_reason = vec![];
        let _ = reason.compute(
            ExplanationContext::test_new(&integers, &mut notification_engine),
            PropagatorId(0),
            &mut PropagatorStore::default(),
            &mut out_reason,
            predicate![x == 5],
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
        let reason_ref = reason_store.push(
            PropagatorId(0),
            StoredReason::Eager(conjunction.clone(), dummy_inference_code()),
        );

        assert_eq!(ReasonRef(0), reason_ref);

        let mut out_reason = vec![];
        let _ = reason_store.get_or_compute(
            reason_ref,
            ExplanationContext::test_new(&integers, &mut notification_engine),
            &mut PropagatorStore::default(),
            &mut out_reason,
            predicate![x == 5],
        );

        assert_eq!(conjunction.as_slice(), &out_reason);
    }
}
