use crate::engine::PropagationStatusCP;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::EnqueueDecision;
use crate::propagation::ExplanationContext;
use crate::propagation::LazyExplanation;
use crate::propagation::LocalId;
use crate::propagation::NotificationContext;
use crate::propagation::Priority;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_simple;
use crate::state::Conflict;
use crate::variables::Literal;

/// Propagator for the constraint `r -> p`, where `r` is a Boolean literal and `p` is an arbitrary
/// propagator.
///
/// When a propagator is reified, it will only propagate whenever `r` is set to true. However, if
/// the propagator implements [`Propagator::detect_inconsistency`], the result of that method may
/// be used to propagate `r` to false. If that method is not implemented, `r` will never be
/// propagated to false.
#[derive(Clone, Debug)]
pub struct ReifiedPropagator<WrappedPropagator> {
    pub(super) propagator: WrappedPropagator,
    pub(super) reification_literal: Literal,
    /// The formatted name of the propagator.
    pub(super) name: String,
    /// The `LocalId` of the reification literal. Is guaranteed to be a larger ID than any of the
    /// registered ids of the wrapped propagator.
    pub(super) reification_literal_id: LocalId,

    /// Holds the lazy explanations.
    pub(super) reason_buffer: Vec<Predicate>,
}

impl<WrappedPropagator: Propagator + Clone> Propagator for ReifiedPropagator<WrappedPropagator> {
    fn notify(
        &mut self,
        mut context: NotificationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if local_id < self.reification_literal_id {
            let decision = self.propagator.notify(context.reborrow(), local_id, event);
            self.filter_enqueue_decision(context, decision)
        } else {
            pumpkin_assert_simple!(local_id == self.reification_literal_id);
            EnqueueDecision::Enqueue
        }
    }

    fn notify_backtrack(&mut self, context: Domains, local_id: LocalId, event: OpaqueDomainEvent) {
        if local_id < self.reification_literal_id {
            self.propagator.notify_backtrack(context, local_id, event)
        } else {
            pumpkin_assert_simple!(local_id == self.reification_literal_id);
        }
    }

    fn priority(&self) -> Priority {
        self.propagator.priority()
    }

    fn synchronise(&mut self, context: NotificationContext<'_>) {
        self.propagator.synchronise(context);
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        self.propagate_reification(&mut context)?;

        if context.evaluate_literal(self.reification_literal) == Some(true) {
            context.with_reification(self.reification_literal);

            let result = self.propagator.propagate(context);

            self.map_propagation_status(result)?;
        }

        Ok(())
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        self.propagate_reification(&mut context)?;

        if context.evaluate_literal(self.reification_literal) == Some(true) {
            context.with_reification(self.reification_literal);

            let result = self.propagator.propagate_from_scratch(context);

            self.map_propagation_status(result)?;
        }

        Ok(())
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> LazyExplanation<'_> {
        let inner = self.propagator.lazy_explanation(code, context);
        let inference_code = inner.inference_code;

        self.reason_buffer.clear();
        self.reason_buffer
            .push(self.reification_literal.get_true_predicate());
        self.reason_buffer.extend(inner.predicates);

        LazyExplanation {
            predicates: self.reason_buffer.as_slice(),
            inference_code,
        }
    }
}

impl<Prop: Propagator + Clone> ReifiedPropagator<Prop> {
    fn map_propagation_status(&self, mut status: PropagationStatusCP) -> PropagationStatusCP {
        if let Err(Conflict::Propagator(ref mut conflict)) = status {
            conflict
                .conjunction
                .push(self.reification_literal.get_true_predicate());
        }
        status
    }

    fn propagate_reification(&self, context: &mut PropagationContext<'_>) -> PropagationStatusCP
    where
        Prop: Propagator,
    {
        if context.evaluate_literal(self.reification_literal) == Some(true) {
            return Ok(());
        }

        if let Some(conflict) = self.propagator.detect_inconsistency(context.domains()) {
            context.post(
                self.reification_literal.get_false_predicate(),
                (conflict.conjunction, &conflict.inference_code),
            )?;
        }

        Ok(())
    }

    fn filter_enqueue_decision(
        &self,
        mut context: NotificationContext<'_>,
        decision: EnqueueDecision,
    ) -> EnqueueDecision {
        if decision == EnqueueDecision::Skip {
            // If the original propagator skips then we always skip
            return EnqueueDecision::Skip;
        }

        if context.evaluate_literal(self.reification_literal) == Some(true) {
            // If the propagator would have enqueued and the literal is true then the reified
            // propagator is also enqueued
            return EnqueueDecision::Enqueue;
        }

        if context.evaluate_literal(self.reification_literal) != Some(false)
            && self
                .propagator
                .detect_inconsistency(context.domains())
                .is_some()
        {
            // Or the literal is not false already and there the propagator has found an
            // inconsistency (i.e. we should and can propagate the reification variable)
            return EnqueueDecision::Enqueue;
        }

        EnqueueDecision::Skip
    }
}
