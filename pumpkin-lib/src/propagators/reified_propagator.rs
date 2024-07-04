use crate::basic_types::ConflictInfo;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::PropagatorVariable;
use crate::engine::propagation::ReadDomains;
use crate::engine::BooleanDomainEvent;
use crate::engine::DomainEvents;
use crate::variables::Literal;

pub(crate) struct ReifiedPropagatorArgs<PropArgs> {
    pub(crate) propagator: PropArgs,
    pub(crate) reification_literal: Literal,
}

impl<PropArgs: PropagatorConstructor> PropagatorConstructor for ReifiedPropagatorArgs<PropArgs> {
    type Propagator = ReifiedPropagator<PropArgs::Propagator>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let propagator = self.propagator.create(context);

        let reification_literal_id = context.get_next_local_id();

        let reification_literal = context.register_literal(
            self.reification_literal,
            DomainEvents::create_with_bool_events(BooleanDomainEvent::AssignedTrue.into()),
            reification_literal_id,
        );

        ReifiedPropagator {
            reification_literal,
            propagator,
        }
    }
}

pub(crate) struct ReifiedPropagator<Prop> {
    propagator: Prop,
    reification_literal: PropagatorVariable<Literal>,
}

impl<Prop: Propagator> Propagator for ReifiedPropagator<Prop> {
    fn name(&self) -> &str {
        todo!("Cannot format name")
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        if context.is_literal_true(&self.reification_literal) {
            context.with_reification(&self.reification_literal);

            let mut result = self.propagator.debug_propagate_from_scratch(context);

            if let Err(Inconsistency::Other(ConflictInfo::Explanation(ref mut conjunction))) =
                result
            {
                conjunction.add(self.reification_literal.get_literal().into());
            }

            return result;
        } else if !context.is_literal_fixed(&self.reification_literal) {
            if let Some(conjunction) = self.propagator.detect_inconsistency(context) {
                context.assign_literal(&self.reification_literal, false, conjunction)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::ConflictInfo;
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::predicates::PropositionalConjunction;

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
                reification_literal,
                propagator: GenericArgs {
                    propagation: move |_: &mut PropagationContextMut| Err(t1.clone().into()),
                    consistency_check: move |_: &PropagationContextMut| Some(t2.clone()),
                },
            })
            .expect("no conflict");

        assert!(solver.is_literal_false(reification_literal));

        let reason = solver.get_reason_bool(reification_literal, false);
        assert_eq!(reason, &triggered_conflict);
    }

    #[test]
    fn a_true_literal_is_added_to_reason_for_propagation() {
        let mut solver = TestSolver::default();

        let reification_literal = solver.new_literal();
        let var = solver.new_variable(1, 5);

        let mut propagator = solver
            .new_propagator(ReifiedPropagatorArgs {
                reification_literal,
                propagator: GenericArgs {
                    propagation: move |ctx: &mut PropagationContextMut| {
                        ctx.set_lower_bound(&PropagatorVariable { inner: var }, 3, conjunction!())?;
                        Ok(())
                    },
                    consistency_check: |_: &PropagationContextMut| None,
                },
            })
            .expect("no conflict");

        solver.assert_bounds(var, 1, 5);

        solver.set_literal(reification_literal, true);
        solver.propagate(&mut propagator).expect("no conflict");

        solver.assert_bounds(var, 3, 5);
        let reason = solver.get_reason_int(predicate![var >= 3].try_into().unwrap());
        assert_eq!(
            reason,
            &PropositionalConjunction::from(Predicate::from(reification_literal))
        );
    }

    #[test]
    fn a_true_literal_is_added_to_a_conflict_conjunction() {
        let mut solver = TestSolver::default();

        let reification_literal = solver.new_literal();
        solver.set_literal(reification_literal, true);

        let var = solver.new_variable(1, 1);

        let inconsistency = solver
            .new_propagator(ReifiedPropagatorArgs {
                reification_literal,
                propagator: GenericArgs {
                    propagation: move |_: &mut PropagationContextMut| {
                        Err(conjunction!([var >= 1]).into())
                    },
                    consistency_check: |_: &PropagationContextMut| None,
                },
            })
            .expect_err("eagerly triggered the conflict");

        match inconsistency {
            Inconsistency::Other(ConflictInfo::Explanation(conjunction)) => {
                assert_eq!(
                    conjunction,
                    PropositionalConjunction::from(vec![
                        reification_literal.into(),
                        predicate![var >= 1]
                    ])
                )
            }

            other => panic!("Inconsistency {other:?} is not expected."),
        }
    }

    struct GenericPropagator<Propagation, ConsistencyCheck> {
        propagation: Propagation,
        consistency_check: ConsistencyCheck,
    }

    impl<Propagation, ConsistencyCheck> Propagator for GenericPropagator<Propagation, ConsistencyCheck>
    where
        Propagation: Fn(&mut PropagationContextMut) -> PropagationStatusCP,
        ConsistencyCheck: Fn(&PropagationContextMut) -> Option<PropositionalConjunction>,
    {
        fn name(&self) -> &str {
            "Failing Propagator"
        }

        fn debug_propagate_from_scratch(
            &self,
            context: &mut PropagationContextMut,
        ) -> PropagationStatusCP {
            (self.propagation)(context)
        }

        fn detect_inconsistency(
            &self,
            context: &PropagationContextMut,
        ) -> Option<PropositionalConjunction> {
            (self.consistency_check)(context)
        }
    }

    struct GenericArgs<Propagation, ConsistencyCheck> {
        propagation: Propagation,
        consistency_check: ConsistencyCheck,
    }

    impl<Propagation, ConsistencyCheck> PropagatorConstructor
        for GenericArgs<Propagation, ConsistencyCheck>
    where
        Propagation: Fn(&mut PropagationContextMut) -> PropagationStatusCP,
        ConsistencyCheck: Fn(&PropagationContextMut) -> Option<PropositionalConjunction>,
    {
        type Propagator = GenericPropagator<Propagation, ConsistencyCheck>;

        fn create(self, _: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
            GenericPropagator {
                propagation: self.propagation,
                consistency_check: self.consistency_check,
            }
        }
    }
}
