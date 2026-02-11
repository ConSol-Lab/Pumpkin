//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use std::fmt::Debug;

use pumpkin_checking::InferenceChecker;

use super::PropagatorQueue;
use crate::containers::KeyGenerator;
use crate::engine::EmptyDomain;
use crate::engine::State;
use crate::engine::constraint_satisfaction_solver::AnalysisMode;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::options::LearningOptions;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::EnqueueDecision;
use crate::propagation::ExplanationContext;
use crate::propagation::NotificationContext;
use crate::propagation::PropagationContext;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorId;
use crate::propagators::nogoods::NogoodPropagator;
use crate::propagators::nogoods::NogoodPropagatorConstructor;
use crate::state::Conflict;
use crate::state::PropagatorHandle;

/// A container for CP variables, which can be used to test propagators.
#[derive(Debug)]
pub struct TestSolver {
    pub state: State,
    constraint_tags: KeyGenerator<ConstraintTag>,
    pub nogood_handle: PropagatorHandle<NogoodPropagator>,
}

impl Default for TestSolver {
    fn default() -> Self {
        let mut state = State::default();
        let handle = state.add_propagator(NogoodPropagatorConstructor::new(
            0,
            LearningOptions::default(),
            AnalysisMode::OneUIP,
        ));
        let mut solver = Self {
            state,
            constraint_tags: Default::default(),
            nogood_handle: handle,
        };
        // We allocate space for the zero-th dummy variable at the root level of the assignments.
        solver.state.notification_engine.grow();
        solver
    }
}

#[deprecated = "Will be replaced by the state API"]
impl TestSolver {
    pub fn accept_inferences_by(&mut self, inference_code: InferenceCode) {
        #[derive(Debug, Clone, Copy)]
        struct Checker;

        impl InferenceChecker<Predicate> for Checker {
            fn check(
                &self,
                _: pumpkin_checking::VariableState<Predicate>,
                _: &[Predicate],
                _: Option<&Predicate>,
            ) -> bool {
                true
            }
        }

        self.state
            .add_inference_checker(inference_code, Box::new(Checker));
    }

    pub fn new_variable(&mut self, lb: i32, ub: i32) -> DomainId {
        self.state.new_interval_variable(lb, ub, None)
    }

    pub fn new_sparse_variable(&mut self, values: Vec<i32>) -> DomainId {
        self.state.new_sparse_variable(values, None)
    }

    pub fn new_literal(&mut self) -> Literal {
        let domain_id = self.new_variable(0, 1);
        Literal::new(domain_id)
    }

    pub fn new_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> Result<PropagatorId, Conflict>
    where
        Constructor: PropagatorConstructor,
        Constructor::PropagatorImpl: 'static,
    {
        let handle = self.state.add_propagator(constructor);
        self.state
            .propagate_to_fixed_point()
            .map(|_| handle.propagator_id())
    }

    pub fn contains<Var: IntegerVariable>(&self, var: Var, value: i32) -> bool {
        var.contains(&self.state.assignments, value)
    }

    pub fn lower_bound(&self, var: DomainId) -> i32 {
        self.state.assignments.get_lower_bound(var)
    }

    pub fn remove_and_notify(
        &mut self,
        propagator: PropagatorId,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let result = self.state.post(predicate!(var != value));
        assert!(
            result.is_ok(),
            "The provided value to `increase_lower_bound` caused an empty domain, generally the propagator should not be notified of this change!"
        );
        let mut propagator_queue = PropagatorQueue::new(4);
        #[allow(deprecated, reason = "Will be refactored in the future")]
        self.state
            .notification_engine
            .notify_propagators_about_domain_events_test(
                &mut self.state.assignments,
                &mut self.state.trailed_values,
                &mut self.state.propagators,
                &mut propagator_queue,
            );
        if propagator_queue.is_propagator_enqueued(propagator) {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    pub fn increase_lower_bound_and_notify(
        &mut self,
        propagator: PropagatorId,
        _local_id: u32,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let result = self.state.post(predicate!(var >= value));
        assert!(
            result.is_ok(),
            "The provided value to `increase_lower_bound` caused an empty domain, generally the propagator should not be notified of this change!"
        );
        let mut propagator_queue = PropagatorQueue::new(4);
        #[allow(deprecated, reason = "Will be refactored in the future")]
        self.state
            .notification_engine
            .notify_propagators_about_domain_events_test(
                &mut self.state.assignments,
                &mut self.state.trailed_values,
                &mut self.state.propagators,
                &mut propagator_queue,
            );
        if propagator_queue.is_propagator_enqueued(propagator) {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    pub fn decrease_upper_bound_and_notify(
        &mut self,
        propagator: PropagatorId,
        _local_id: u32,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let result = self.state.post(predicate!(var <= value));
        assert!(
            result.is_ok(),
            "The provided value to `increase_lower_bound` caused an empty domain, generally the propagator should not be notified of this change!"
        );
        let mut propagator_queue = PropagatorQueue::new(4);
        #[allow(deprecated, reason = "Will be refactored in the future")]
        self.state
            .notification_engine
            .notify_propagators_about_domain_events_test(
                &mut self.state.assignments,
                &mut self.state.trailed_values,
                &mut self.state.propagators,
                &mut propagator_queue,
            );
        if propagator_queue.is_propagator_enqueued(propagator) {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    pub fn is_literal_false(&self, literal: Literal) -> bool {
        self.state
            .assignments
            .evaluate_predicate(literal.get_true_predicate())
            .is_some_and(|truth_value| !truth_value)
    }

    pub fn upper_bound(&self, var: DomainId) -> i32 {
        self.state.assignments.get_upper_bound(var)
    }

    pub fn remove(&mut self, var: DomainId, value: i32) -> Result<(), EmptyDomain> {
        let _ = self.state.post(predicate!(var != value))?;

        Ok(())
    }

    pub fn set_literal(&mut self, literal: Literal, truth_value: bool) -> Result<(), EmptyDomain> {
        let _ = match truth_value {
            true => self.state.assignments.post_predicate(
                literal.get_true_predicate(),
                None,
                &mut self.state.notification_engine,
            )?,
            false => self.state.assignments.post_predicate(
                (!literal).get_true_predicate(),
                None,
                &mut self.state.notification_engine,
            )?,
        };

        Ok(())
    }

    pub fn propagate(&mut self, propagator: PropagatorId) -> Result<(), Conflict> {
        let context = PropagationContext::new(
            &mut self.state.trailed_values,
            &mut self.state.assignments,
            &mut self.state.reason_store,
            &mut self.state.notification_engine,
            propagator,
        );
        self.state.propagators[propagator].propagate(context)
    }

    pub fn propagate_until_fixed_point(
        &mut self,
        propagator: PropagatorId,
    ) -> Result<(), Conflict> {
        let mut num_trail_entries = self.state.assignments.num_trail_entries();
        self.notify_propagator(propagator);
        loop {
            {
                // Specify the life-times to be able to retrieve the trail entries
                let context = PropagationContext::new(
                    &mut self.state.trailed_values,
                    &mut self.state.assignments,
                    &mut self.state.reason_store,
                    &mut self.state.notification_engine,
                    propagator,
                );
                self.state.propagators[propagator].propagate(context)?;
                self.notify_propagator(propagator);
            }
            if self.state.assignments.num_trail_entries() == num_trail_entries {
                break;
            }
            num_trail_entries = self.state.assignments.num_trail_entries();
        }
        Ok(())
    }

    pub fn notify_propagator(&mut self, _propagator: PropagatorId) {
        #[allow(deprecated, reason = "Will be refactored in the future")]
        self.state
            .notification_engine
            .notify_propagators_about_domain_events_test(
                &mut self.state.assignments,
                &mut self.state.trailed_values,
                &mut self.state.propagators,
                &mut PropagatorQueue::new(4),
            );
    }

    pub fn get_reason_int(&mut self, predicate: Predicate) -> PropositionalConjunction {
        #[allow(deprecated, reason = "Will be refactored in the future")]
        let reason_ref = self
            .state
            .assignments
            .get_reason_for_predicate_brute_force(predicate);
        let mut predicates = vec![];
        let _ = self.state.reason_store.get_or_compute(
            reason_ref,
            ExplanationContext::without_working_nogood(
                &self.state.assignments,
                self.state
                    .assignments
                    .get_trail_position(&predicate)
                    .unwrap(),
                &mut self.state.notification_engine,
            ),
            &mut self.state.propagators,
            &mut predicates,
        );

        PropositionalConjunction::from(predicates)
    }

    pub fn get_reason_bool(
        &mut self,
        literal: Literal,
        truth_value: bool,
    ) -> PropositionalConjunction {
        let predicate = match truth_value {
            true => literal.get_true_predicate(),
            false => (!literal).get_true_predicate(),
        };
        self.get_reason_int(predicate)
    }

    pub fn assert_bounds(&self, var: DomainId, lb: i32, ub: i32) {
        let actual_lb = self.lower_bound(var);
        let actual_ub = self.upper_bound(var);

        assert_eq!(
            (lb, ub),
            (actual_lb, actual_ub),
            "The expected bounds [{lb}..{ub}] did not match the actual bounds [{actual_lb}..{actual_ub}]"
        );
    }

    pub fn new_constraint_tag(&mut self) -> ConstraintTag {
        self.constraint_tags.next_key()
    }

    pub fn new_checkpoint(&mut self) {
        self.state.new_checkpoint();
    }

    pub fn synchronise(&mut self, level: usize) {
        let _ = self
            .state
            .assignments
            .synchronise(level, &mut self.state.notification_engine);
        self.state.notification_engine.synchronise(
            level,
            &self.state.assignments,
            &mut self.state.trailed_values,
        );
        self.state.trailed_values.synchronise(level);

        for propagator in self.state.propagators.iter_propagators_mut() {
            let mut context =
                NotificationContext::new(&mut self.state.trailed_values, &self.state.assignments);

            propagator.synchronise(context.reborrow());
        }
    }
}
