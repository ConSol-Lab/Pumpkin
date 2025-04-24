use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::DomainEvents;
use crate::pumpkin_assert_simple;
use crate::variables::Literal;

/// A [`PropagatorConstructor`] for the [`ReifiedPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct ReifiedPropagatorArgs<WrappedArgs> {
    pub(crate) propagator: WrappedArgs,
    pub(crate) reification_literal: Literal,
}

impl<WrappedArgs, WrappedPropagator> PropagatorConstructor for ReifiedPropagatorArgs<WrappedArgs>
where
    WrappedArgs: PropagatorConstructor<PropagatorImpl = WrappedPropagator>,
    WrappedPropagator: Propagator,
{
    type PropagatorImpl = ReifiedPropagator<WrappedPropagator>;

    fn create(self, context: &mut PropagatorConstructorContext) -> Self::PropagatorImpl {
        let ReifiedPropagatorArgs {
            propagator,
            reification_literal,
        } = self;

        let propagator = propagator.create(context);
        let reification_literal_id = context.get_next_local_id();

        context.register(
            self.reification_literal,
            DomainEvents::BOUNDS,
            reification_literal_id,
        );

        let name = format!("Reified({})", propagator.name());

        ReifiedPropagator {
            propagator,
            reification_literal,
            reification_literal_id,
            name,
        }
    }
}

/// Propagator for the constraint `r -> p`, where `r` is a Boolean literal and `p` is an arbitrary
/// propagator.
///
/// By default, the propagator 'p' only propagates whenever `r` is set to true.
/// However, if the propagator 'p' implements [`Propagator::detect_inconsistency`], the result of
/// that method may be used to propagate `r` to false. If that method is not implemented, `r` will
/// never be propagated to false.
#[derive(Clone, Debug)]
pub(crate) struct ReifiedPropagator<WrappedPropagator> {
    reification_literal: Literal,
    /// The `LocalId` of the reification literal. Is guaranteed to be a larger ID than any of the
    /// registered ids of the wrapped propagator.
    reification_literal_id: LocalId,
    propagator: WrappedPropagator,
    /// An inconsistency that is identified by `propagator`.
    inconsistency: Option<PropositionalConjunction>,
    /// The formatted name of the propagator.
    name: String,
}

impl<WrappedPropagator: Propagator> Propagator for ReifiedPropagator<WrappedPropagator> {
    fn notify(
        &mut self,
        context: PropagationContextWithTrailedValues,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if local_id < self.reification_literal_id {
            let decision = self.propagator.notify(
                PropagationContextWithTrailedValues::new(
                    context.trailed_values,
                    context.assignments,
                ),
                local_id,
                event,
            );
            self.filter_enqueue_decision(context, decision)
        } else {
            pumpkin_assert_simple!(local_id == self.reification_literal_id);
            EnqueueDecision::Enqueue
        }
    }

    fn notify_backtrack(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) {
        if local_id < self.reification_literal_id {
            self.propagator.notify_backtrack(context, local_id, event)
        } else {
            pumpkin_assert_simple!(local_id == self.reification_literal_id);
        }
    }

    fn priority(&self) -> u32 {
        self.propagator.priority()
    }

    fn synchronise(&mut self, context: PropagationContext) {
        self.propagator.synchronise(context);
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        self.propagate_reification(&mut context)?;

        if context.is_literal_true(&self.reification_literal) {
            context.with_reification(self.reification_literal);

            let result = self.propagator.propagate(context);

            self.map_propagation_status(result)?;
        }

        Ok(())
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        self.propagate_reification(&mut context)?;

        if context.is_literal_true(&self.reification_literal) {
            context.with_reification(self.reification_literal);

            let result = self.propagator.debug_propagate_from_scratch(context);

            self.map_propagation_status(result)?;
        }

        Ok(())
    }
}

impl<Prop: Propagator> ReifiedPropagator<Prop> {
    fn map_propagation_status(&self, mut status: PropagationStatusCP) -> PropagationStatusCP {
        if let Err(Inconsistency::Conflict(ref mut conflict_nogood)) = status {
            conflict_nogood.add(self.reification_literal.get_true_predicate());
        }
        status
    }

    fn propagate_reification(&self, context: &mut PropagationContextMut<'_>) -> PropagationStatusCP
    where
        Prop: Propagator,
    {
        if !context.is_literal_fixed(&self.reification_literal) {
            if let Some(conjunction) = self
                .propagator
                .detect_inconsistency(context.as_trailed_readonly())
            {
                context.assign_literal(&self.reification_literal, false, conjunction)?;
            }
        }

        Ok(())
    }

    fn filter_enqueue_decision(
        &mut self,
        context: PropagationContextWithTrailedValues<'_>,
        decision: EnqueueDecision,
    ) -> EnqueueDecision {
        if decision == EnqueueDecision::Skip {
            // If the original propagator skips then we always skip
            return EnqueueDecision::Skip;
        }

        if context.is_literal_true(&self.reification_literal) {
            // If the propagator would have enqueued and the literal is true then the reified
            // propagator is also enqueued
            return EnqueueDecision::Enqueue;
        }

        if !context.is_literal_false(&self.reification_literal)
            && self.propagator.detect_inconsistency(context).is_some()
        {
            // Or the literal is not false already and there the propagator has found an
            // inconsistency (i.e. we should and can propagate the reification variable)
            return EnqueueDecision::Enqueue;
        }

        EnqueueDecision::Skip
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::predicates::PropositionalConjunction;
    use crate::variables::DomainId;

    #[test]
    fn a_detected_inconsistency_is_given_as_reason_for_propagating_reification_literal_to_false() {
        let mut solver = TestSolver::default();

        let reification_literal = solver.new_literal();
        let a = solver.new_variable(1, 1);
        let b = solver.new_variable(2, 2);

        let triggered_conflict = conjunction!([a == 1] & [b == 2]);
        let t1 = triggered_conflict.clone();
        let t2 = triggered_conflict.clone();

        let _ = solver
            .new_propagator(ReifiedPropagatorArgs {
                propagator: GenericPropagator::new(
                    move |_: PropagationContextMut| Err(t1.clone().into()),
                    move |_: PropagationContextWithTrailedValues| Some(t2.clone()),
                ),
                reification_literal,
            })
            .expect("no conflict");

        assert!(solver.is_literal_false(reification_literal));

        let reason = solver.get_reason_bool(reification_literal, false);
        assert_eq!(reason, triggered_conflict);
    }

    #[test]
    fn a_true_literal_is_added_to_reason_for_propagation() {
        let mut solver = TestSolver::default();

        let reification_literal = solver.new_literal();
        let var = solver.new_variable(1, 5);

        let propagator = solver
            .new_propagator(ReifiedPropagatorArgs {
                propagator: GenericPropagator::new(
                    move |mut ctx: PropagationContextMut| {
                        ctx.post(predicate![var >= 3], conjunction!())?;
                        Ok(())
                    },
                    |_: PropagationContextWithTrailedValues| None,
                ),
                reification_literal,
            })
            .expect("no conflict");

        solver.assert_bounds(var, 1, 5);

        let _ = solver.set_literal(reification_literal, true);
        solver.propagate(propagator).expect("no conflict");

        solver.assert_bounds(var, 3, 5);
        let reason = solver.get_reason_int(predicate![var >= 3]);
        assert_eq!(
            reason,
            PropositionalConjunction::from(reification_literal.get_true_predicate())
        );
    }

    #[test]
    fn a_true_literal_is_added_to_a_conflict_conjunction() {
        let mut solver = TestSolver::default();

        let reification_literal = solver.new_literal();
        let _ = solver.set_literal(reification_literal, true);

        let var = solver.new_variable(1, 1);

        let inconsistency = solver
            .new_propagator(ReifiedPropagatorArgs {
                propagator: GenericPropagator::new(
                    move |_: PropagationContextMut| Err(conjunction!([var >= 1]).into()),
                    |_: PropagationContextWithTrailedValues| None,
                ),
                reification_literal,
            })
            .expect_err("eagerly triggered the conflict");

        match inconsistency {
            Inconsistency::Conflict(conflict_nogood) => {
                assert_eq!(
                    conflict_nogood,
                    PropositionalConjunction::from(vec![
                        reification_literal.get_true_predicate(),
                        predicate![var >= 1]
                    ])
                )
            }

            other => panic!("Inconsistency {other:?} is not expected."),
        }
    }

    #[test]
    fn notify_propagator_is_enqueued_if_inconsistency_can_be_detected() {
        let mut solver = TestSolver::default();

        let reification_literal = solver.new_literal();
        let var = solver.new_variable(1, 5);

        let propagator = solver
            .new_propagator(ReifiedPropagatorArgs {
                propagator: GenericPropagator::new(
                    |_: PropagationContextMut| Ok(()),
                    move |context: PropagationContextWithTrailedValues| {
                        if context.is_fixed(&var) {
                            Some(conjunction!([var == 5]))
                        } else {
                            None
                        }
                    },
                )
                .with_variables(&[var]),
                reification_literal,
            })
            .expect("No conflict expected");

        let enqueue = solver.increase_lower_bound_and_notify(propagator, 0, var, 5);
        assert!(matches!(enqueue, EnqueueDecision::Enqueue))
    }

    struct GenericPropagator<Propagation, ConsistencyCheck> {
        propagation: Propagation,
        consistency_check: ConsistencyCheck,
        variables_to_register: Vec<DomainId>,
    }

    impl<Propagation, ConsistencyCheck> PropagatorConstructor
        for GenericPropagator<Propagation, ConsistencyCheck>
    where
        Propagation: Fn(PropagationContextMut) -> PropagationStatusCP + 'static,
        ConsistencyCheck:
            Fn(StatefulPropagationContext) -> Option<PropositionalConjunction> + 'static,
    {
        type PropagatorImpl = Self;

        fn create(self, context: &mut PropagatorConstructorContext) -> Self::PropagatorImpl {
            self.variables_to_register
                .iter()
                .enumerate()
                .for_each(|(index, variable)| {
                    let _ = context.register(
                        *variable,
                        DomainEvents::ANY_INT,
                        LocalId::from(index as u32),
                    );
                });
            (self.init)(context);
            self
        }
    }

    impl<Propagation, ConsistencyCheck> Propagator for GenericPropagator<Propagation, ConsistencyCheck>
    where
        Propagation: Fn(PropagationContextMut) -> PropagationStatusCP + 'static,
        ConsistencyCheck:
            Fn(PropagationContextWithTrailedValues) -> Option<PropositionalConjunction> + 'static,
    {
        fn name(&self) -> &str {
            "Generic Propagator"
        }

        fn debug_propagate_from_scratch(
            &self,
            context: PropagationContextMut,
        ) -> PropagationStatusCP {
            (self.propagation)(context)
        }

        fn detect_inconsistency(
            &self,
            context: PropagationContextWithTrailedValues,
        ) -> Option<PropositionalConjunction> {
            (self.consistency_check)(context)
        }
    }

    impl<Propagation, ConsistencyCheck> GenericPropagator<Propagation, ConsistencyCheck>
    where
        Propagation: Fn(PropagationContextMut) -> PropagationStatusCP,
        ConsistencyCheck:
            Fn(PropagationContextWithTrailedValues) -> Option<PropositionalConjunction>,
    {
        pub(crate) fn new(propagation: Propagation, consistency_check: ConsistencyCheck) -> Self {
            GenericPropagator {
                propagation,
                consistency_check,
                variables_to_register: vec![],
            }
        }

        pub(crate) fn with_variables(mut self, variables: &[DomainId]) -> Self {
            // Necessary for ensuring that the local IDs are correct when notifying
            self.variables_to_register = variables.into();
            self
        }
    }
}
