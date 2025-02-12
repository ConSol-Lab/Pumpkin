use std::fmt::Debug;

use super::propagation::store::PropagatorStore;
use super::propagation::ExplanationContext;
use super::propagation::PropagatorId;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Trail;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;
use crate::variables::Literal;

/// The reason store holds a reason for each change made by a CP propagator on a trail.
#[derive(Default, Debug)]
pub(crate) struct ReasonStore {
    trail: Trail<(PropagatorId, Reason)>,
}

impl ReasonStore {
    pub(crate) fn push(&mut self, propagator: PropagatorId, reason: Reason) -> ReasonRef {
        let index = self.trail.len();
        self.trail.push((propagator, reason));
        pumpkin_assert_simple!(
            index < (1 << 30),
            "ReasonRef in reason store should fit in ContraintReference, \
             which has 30 bits available at most"
        );
        ReasonRef(index as u32)
    }

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
                Reason::Eager(_) => None,
                Reason::DynamicLazy(code) => Some(code),
                Reason::ReifiedLazy(_, _) => {
                    unimplemented!("Getting the code of a reified lazy reason is unsupported.")
                }
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
pub struct ReasonRef(pub(crate) u32);

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
    /// A lazy explanation that has reified.
    ReifiedLazy(Literal, u64),
}

impl Reason {
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
            Reason::DynamicLazy(code) => destination_buffer.extend(
                propagators[propagator_id]
                    .lazy_explanation(*code, context)
                    .iter()
                    .copied(),
            ),
            Reason::Eager(result) => destination_buffer.extend(result.iter().copied()),
            Reason::ReifiedLazy(literal, code) => {
                destination_buffer.extend(
                    propagators[propagator_id]
                        .lazy_explanation(*code, context)
                        .iter()
                        .copied(),
                );
                destination_buffer.extend(std::iter::once(literal.get_true_predicate()));
            }
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
    use crate::engine::propagation::Propagator;
    use crate::engine::variables::DomainId;
    use crate::engine::Assignments;
    use crate::predicate;

    #[test]
    fn computing_an_eager_reason_returns_a_reference_to_the_conjunction() {
        let integers = Assignments::default();

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason = Reason::Eager(conjunction.clone());

        let mut out_reason = vec![];
        reason.compute(
            ExplanationContext::from(&integers),
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

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason_ref = reason_store.push(PropagatorId(0), Reason::Eager(conjunction.clone()));

        assert_eq!(ReasonRef(0), reason_ref);

        let mut out_reason = vec![];
        let _ = reason_store.get_or_compute(
            reason_ref,
            ExplanationContext::from(&integers),
            &mut PropagatorStore::default(),
            &mut out_reason,
        );

        assert_eq!(conjunction.as_slice(), &out_reason);
    }

    #[test]
    fn reified_lazy_explanation_has_reification_added_after_compute() {
        let mut reason_store = ReasonStore::default();
        let mut integers = Assignments::default();

        let x = integers.grow(1, 5);
        let reif = Literal::new(integers.grow(0, 1));

        struct TestPropagator(Vec<Predicate>);

        impl Propagator for TestPropagator {
            fn name(&self) -> &str {
                todo!()
            }

            fn debug_propagate_from_scratch(
                &self,
                _: crate::engine::propagation::PropagationContextMut,
            ) -> crate::basic_types::PropagationStatusCP {
                todo!()
            }

            fn initialise_at_root(
                &mut self,
                _: &mut crate::engine::propagation::PropagatorInitialisationContext,
            ) -> Result<(), PropositionalConjunction> {
                todo!()
            }

            fn lazy_explanation(&mut self, code: u64, _: ExplanationContext) -> &[Predicate] {
                assert_eq!(0, code);

                &self.0
            }
        }

        let mut propagator_store = PropagatorStore::default();
        let propagator_id =
            propagator_store.alloc(Box::new(TestPropagator(vec![predicate![x >= 2]])), None);
        let reason_ref = reason_store.push(propagator_id, Reason::ReifiedLazy(reif, 0));

        assert_eq!(ReasonRef(0), reason_ref);

        let mut reason = vec![];
        let _ = reason_store.get_or_compute(
            reason_ref,
            ExplanationContext::from(&integers),
            &mut propagator_store,
            &mut reason,
        );

        assert_eq!(vec![predicate![x >= 2], reif.get_true_predicate()], reason);
    }
}
