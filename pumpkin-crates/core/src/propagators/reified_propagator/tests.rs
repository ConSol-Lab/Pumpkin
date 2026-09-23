use crate::conjunction;
use crate::containers::StorageKey;
use crate::engine::PropagationStatusCP;
use crate::engine::PropagatorConflict;
use crate::engine::test_solver::TestSolver;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::proof::Unknown;
use crate::propagation::DomainEvents;
use crate::propagation::Domains;
use crate::propagation::EnqueueDecision;
use crate::propagation::EventsToRegister;
use crate::propagation::LocalId;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorSpec;
use crate::propagation::ReadDomains;
use crate::propagation::RuntimeCheckers;
use crate::propagators::ReifiedPropagatorArgs;
use crate::state::Conflict;
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

    let inference_code = solver.accept_inferences_by(ConstraintTag::create_from_index(0), Unknown);
    let i1 = inference_code.clone();
    let i2 = inference_code.clone();

    let _ = solver
        .new_propagator(ReifiedPropagatorArgs {
            propagator: GenericPropagator::new(
                vec![a, b],
                move |_: PropagationContext| {
                    Err(PropagatorConflict {
                        conjunction: t1.clone(),
                        inference_code: i1.clone(),
                    }
                    .into())
                },
                move |_: Domains| {
                    Some(PropagatorConflict {
                        conjunction: t2.clone(),
                        inference_code: i2.clone(),
                    })
                },
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
                vec![var],
                move |mut ctx: PropagationContext| {
                    ctx.post(
                        predicate![var >= 3],
                        (
                            conjunction!(),
                            &InferenceCode::unknown_label(ConstraintTag::create_from_index(0)),
                        ),
                    )?;
                    Ok(())
                },
                |_: Domains| None,
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
    let inference_code = solver.accept_inferences_by(ConstraintTag::create_from_index(0), Unknown);

    let inconsistency = solver
        .new_propagator(ReifiedPropagatorArgs {
            propagator: GenericPropagator::new(
                vec![var],
                move |_: PropagationContext| {
                    Err(PropagatorConflict {
                        conjunction: conjunction!([var >= 1]),
                        inference_code: inference_code.clone(),
                    }
                    .into())
                },
                |_: Domains| None,
            ),
            reification_literal,
        })
        .expect_err("eagerly triggered the conflict");

    match inconsistency {
        Conflict::Propagator(conflict_nogood) => {
            assert_eq!(
                conflict_nogood.conjunction,
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

    let inference_code = solver.accept_inferences_by(ConstraintTag::create_from_index(0), Unknown);

    let propagator = solver
        .new_propagator(ReifiedPropagatorArgs {
            propagator: GenericPropagator::new(
                vec![var],
                |_: PropagationContext| Ok(()),
                move |context: Domains| {
                    if context.is_fixed(&var) {
                        Some(PropagatorConflict {
                            conjunction: conjunction!([var == 5]),
                            inference_code: inference_code.clone(),
                        })
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

#[derive(Clone)]
struct GenericPropagator<Propagation, ConsistencyCheck> {
    propagation: Propagation,
    consistency_check: ConsistencyCheck,
    variables_to_register: Vec<DomainId>,
}

impl<Propagation, ConsistencyCheck> PropagatorConstructor
    for GenericPropagator<Propagation, ConsistencyCheck>
where
    Propagation: Fn(PropagationContext) -> PropagationStatusCP + 'static + Clone,
    ConsistencyCheck: Fn(Domains) -> Option<PropagatorConflict> + 'static + Clone,
{
    type PropagatorImpl = Self;

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let mut registration = EventsToRegister::empty();

        for (index, variable) in self.variables_to_register.iter().enumerate() {
            registration.add(variable, DomainEvents::ANY_INT, LocalId::from(index as u32));
        }

        PropagatorSpec {
            registration,
            checkers: RuntimeCheckers::empty(),
            propagator: self,
        }
    }
}

impl<Propagation, ConsistencyCheck> Propagator for GenericPropagator<Propagation, ConsistencyCheck>
where
    Propagation: Fn(PropagationContext) -> PropagationStatusCP + 'static + Clone,
    ConsistencyCheck: Fn(Domains) -> Option<PropagatorConflict> + 'static + Clone,
{
    fn name(&self) -> &str {
        "Generic Propagator"
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        (self.propagation)(context)
    }

    fn detect_inconsistency(&self, domains: Domains) -> Option<PropagatorConflict> {
        (self.consistency_check)(domains)
    }
}

impl<Propagation, ConsistencyCheck> GenericPropagator<Propagation, ConsistencyCheck>
where
    Propagation: Fn(PropagationContext) -> PropagationStatusCP,
    ConsistencyCheck: Fn(Domains) -> Option<PropagatorConflict>,
{
    pub(crate) fn new(
        variables_to_register: Vec<DomainId>,
        propagation: Propagation,
        consistency_check: ConsistencyCheck,
    ) -> Self {
        GenericPropagator {
            propagation,
            consistency_check,
            variables_to_register,
        }
    }

    pub(crate) fn with_variables(mut self, variables: &[DomainId]) -> Self {
        // Necessary for ensuring that the local IDs are correct when notifying
        self.variables_to_register = variables.into();
        self
    }
}
