use std::sync::Arc;

use crate::PropagatorHandle;
use crate::basic_types::EmptyDomainConflict;
use crate::basic_types::Inconsistency;
use crate::basic_types::StoredConflictInfo;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::create_statistics_struct;
use crate::engine::AssignmentReason;
use crate::engine::Assignments;
use crate::engine::ConstraintProgrammingTrailEntry;
use crate::engine::DebugHelper;
use crate::engine::EmptyDomain;
use crate::engine::PropagatorQueue;
use crate::engine::TrailedValues;
use crate::engine::VariableNames;
use crate::engine::notifications::NotificationEngine;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonStore;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::proof::InferenceLabel;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::results::SolutionReference;
use crate::statistics::StatisticLogger;
use crate::statistics::log_statistic;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

#[derive(Debug)]
pub struct State {
    /// The list of propagators. Propagators live here and are queried when events (domain changes)
    /// happen. The list is only traversed during synchronisation for now.
    pub(crate) propagators: PropagatorStore,
    /// Tracks information related to the assignments of integer variables.
    pub(crate) assignments: Assignments,
    /// Keep track of trailed values (i.e. values which automatically backtrack)
    pub(crate) trailed_values: TrailedValues,
    /// The names of the variables in the solver.
    pub(crate) variable_names: VariableNames,
    /// Dictates the order in which propagators will be called to propagate.
    propagator_queue: PropagatorQueue,
    /// Handles storing information about propagation reasons, which are used later to construct
    /// explanations during conflict analysis
    pub(crate) reason_store: ReasonStore,
    /// Component responsible for providing notifications for changes to the domains of variables
    /// and/or the polarity [Predicate]s
    pub(crate) notification_engine: NotificationEngine,
    pub(crate) inference_codes: Option<KeyedVec<InferenceCode, (ConstraintTag, Arc<str>)>>,

    statistics: StateStatistics,
}

create_statistics_struct!(StateStatistics {
    num_propagators_called: usize,
    num_propagations: usize,
    num_conflicts: usize,
});

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub(crate) fn new() -> Self {
        let mut result = Self {
            assignments: Default::default(),
            trailed_values: TrailedValues::default(),
            variable_names: VariableNames::default(),
            propagator_queue: PropagatorQueue::default(),
            propagators: PropagatorStore::default(),
            reason_store: ReasonStore::default(),
            notification_engine: NotificationEngine::default(),
            inference_codes: Some(KeyedVec::default()),
            statistics: StateStatistics::default(),
        };
        // As a convention, the assignments contain a dummy domain_id=0, which represents a 0-1
        // variable that is assigned to one. We use it to represent predicates that are
        // trivially true. We need to adjust other data structures to take this into account.
        let dummy_id = Predicate::trivially_true().get_domain();

        result
            .variable_names
            .add_integer(dummy_id, "Dummy".to_owned());
        assert!(dummy_id.id() == 0);
        assert!(result.assignments.get_lower_bound(dummy_id) == 1);
        assert!(result.assignments.get_upper_bound(dummy_id) == 1);

        result
    }

    pub(crate) fn as_readonly(&self) -> PropagationContext<'_> {
        PropagationContext::new(&self.assignments)
    }

    pub(crate) fn log_statistics(&self, verbose: bool) {
        log_statistic("variables", self.assignments.num_domains());
        log_statistic("propagators", self.propagators.num_propagators());
        log_statistic("failures", self.statistics.num_conflicts);
        log_statistic("propagations", self.statistics.num_propagators_called);
        log_statistic("nogoods", self.statistics.num_conflicts);
        if verbose {
            log_statistic(
                "numAtomicConstraintsPropagated",
                self.statistics.num_propagations,
            );
            for (index, propagator) in self.propagators.iter_propagators().enumerate() {
                propagator.log_statistics(StatisticLogger::new([
                    propagator.name(),
                    "number",
                    index.to_string().as_str(),
                ]));
            }
        }
    }
}

/// Operations to create .
impl State {
    /// Create a new [`InferenceCode`] for a [`ConstraintTag`] and [`InferenceLabel`] combination.
    /// The inference codes are required to log inferences with [`Self::log_inference`].
    pub(crate) fn create_inference_code(
        &mut self,
        constraint_tag: ConstraintTag,
        inference_label: impl InferenceLabel,
    ) -> InferenceCode {
        if let Some(inference_codes) = &mut self.inference_codes {
            inference_codes.push((constraint_tag, inference_label.to_str()))
        } else {
            InferenceCode::create_from_index(0)
        }
    }

    pub(crate) fn create_new_literal(&mut self, name: Option<String>) -> Literal {
        let domain_id = self.create_new_integer_variable(0, 1, name);
        Literal::new(domain_id)
    }

    /// Create a new integer variable. Its domain will have the given lower and upper bounds.
    pub(crate) fn create_new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: Option<String>,
    ) -> DomainId {
        let domain_id = self.assignments.grow(lower_bound, upper_bound);

        if let Some(name) = name {
            self.variable_names.add_integer(domain_id, name);
        }

        self.notification_engine.grow();

        domain_id
    }

    /// Creates an integer variable with a domain containing only the values in `values`
    pub(crate) fn create_new_integer_variable_sparse(
        &mut self,
        values: Vec<i32>,
        name: Option<String>,
    ) -> DomainId {
        let domain_id = self.assignments.create_new_integer_variable_sparse(values);

        if let Some(name) = name {
            self.variable_names.add_integer(domain_id, name);
        }

        self.notification_engine.grow();

        domain_id
    }
}

/// Operations to retrieve information about values
impl State {
    pub(crate) fn lower_bound<Var: IntegerVariable>(&self, variable: Var) -> i32 {
        variable.lower_bound(&self.assignments)
    }

    pub(crate) fn upper_bound<Var: IntegerVariable>(&self, variable: Var) -> i32 {
        variable.upper_bound(&self.assignments)
    }

    pub(crate) fn contains<Var: IntegerVariable>(&self, variable: Var, value: i32) -> bool {
        variable.contains(&self.assignments, value)
    }
    pub(crate) fn is_fixed<Var: IntegerVariable>(&self, variable: Var) -> bool {
        self.lower_bound(variable.clone()) == self.upper_bound(variable)
    }

    pub(crate) fn fixed_value<Var: IntegerVariable>(&self, variable: Var) -> Option<i32> {
        self.is_fixed(variable.clone())
            .then(|| self.lower_bound(variable))
    }

    pub(crate) fn truth_value(&self, predicate: Predicate) -> Option<bool> {
        self.assignments.evaluate_predicate(predicate)
    }

    pub(crate) fn is_predicate_satisfied(&self, predicate: Predicate) -> bool {
        self.assignments.is_predicate_satisfied(predicate)
    }

    pub(crate) fn is_predicate_falsified(&self, predicate: Predicate) -> bool {
        self.assignments.is_predicate_falsified(predicate)
    }

    pub(crate) fn get_decision_level_for_predicate(&self, predicate: Predicate) -> Option<usize> {
        self.assignments
            .get_decision_level_for_predicate(&predicate)
    }

    pub(crate) fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        let literal_is_true = self
            .assignments
            .is_predicate_satisfied(literal.get_true_predicate());
        let opposite_literal_is_true = self
            .assignments
            .is_predicate_satisfied((!literal).get_true_predicate());

        pumpkin_assert_moderate!(!(literal_is_true && opposite_literal_is_true));

        // If both the literal is not true and its negation is not true then the literal is
        // unassigned
        if !literal_is_true && !opposite_literal_is_true {
            None
        } else {
            Some(literal_is_true)
        }
    }

    pub(crate) fn get_assignment_level(&self) -> usize {
        self.assignments.get_assignment_level()
    }
}

/// Operations for retrieving information about trail
impl State {
    pub(crate) fn trail_len(&self) -> usize {
        self.assignments.num_trail_entries()
    }

    pub(crate) fn trail_entry(&self, trail_index: usize) -> ConstraintProgrammingTrailEntry {
        self.assignments.get_trail_entry(trail_index)
    }
}

/// Operations for adding constraints.
impl State {
    pub(crate) fn enqueue_propagator(&mut self, propagator_id: PropagatorId) {
        let priority = self.propagators[propagator_id].priority();
        self.propagator_queue
            .enqueue_propagator(propagator_id, priority);
    }

    pub(crate) fn new_propagator_handle<P: Propagator>(&mut self) -> PropagatorHandle<P> {
        self.propagators.new_propagator().key()
    }

    pub(crate) fn add_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> PropagatorHandle<Constructor::PropagatorImpl>
    where
        Constructor: PropagatorConstructor,
        Constructor::PropagatorImpl: 'static,
    {
        let constructor_context = PropagatorConstructorContext::new(
            self.new_propagator_handle::<Constructor::PropagatorImpl>()
                .propagator_id(),
            self,
        );
        let propagator = constructor.create(constructor_context);

        pumpkin_assert_simple!(
            propagator.priority() <= 3,
            "The propagator priority exceeds 3.
             Currently we only support values up to 3,
             but this can easily be changed if there is a good reason."
        );

        let slot = self.propagators.new_propagator();
        let handle = slot.populate(propagator);

        self.enqueue_propagator(handle.propagator_id());

        handle
    }
}

/// Operations for retrieving propagators.
impl State {
    /// Get a reference to the propagator identified by the given handle.
    ///
    /// For an exclusive reference, use [`State::get_propagator_mut`].
    #[allow(
        private_bounds,
        reason = "Propagator will be part of public interface in the future"
    )]
    pub fn get_propagator<P: Propagator>(&self, handle: PropagatorHandle<P>) -> Option<&P> {
        self.propagators.get_propagator(handle)
    }

    /// Get an exclusive reference to the propagator identified by the given handle.
    #[allow(
        private_bounds,
        reason = "Propagator will be part of public interface in the future"
    )]
    pub fn get_propagator_mut<P: Propagator>(
        &mut self,
        handle: PropagatorHandle<P>,
    ) -> Option<&mut P> {
        self.propagators.get_propagator_mut(handle)
    }

    /// Get an exclusive reference to the propagator identified by the given handle and a context.
    #[allow(
        private_bounds,
        reason = "Propagator will be part of public interface in the future"
    )]
    pub fn get_propagator_mut_with_context<P: Propagator>(
        &mut self,
        handle: PropagatorHandle<P>,
    ) -> (Option<&mut P>, PropagationContextMut<'_>) {
        (
            self.propagators.get_propagator_mut(handle),
            PropagationContextMut::new(
                &mut self.trailed_values,
                &mut self.assignments,
                &mut self.reason_store,
                &mut self.notification_engine,
                handle.propagator_id(),
            ),
        )
    }
}

/// Operations for modifying the state.
impl State {
    pub(crate) fn post(
        &mut self,
        predicate: Predicate,
        reason: Option<AssignmentReason>,
    ) -> Result<bool, EmptyDomain> {
        self.assignments
            .post_predicate(predicate, reason, &mut self.notification_engine)
    }

    pub(crate) fn new_checkpoint(&mut self) {
        self.assignments.increase_decision_level();
        self.notification_engine.increase_decision_level();
        self.trailed_values.increase_decision_level();
        self.reason_store.increase_decision_level();
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "This method requires this many arguments, though a backtracking context could be considered; for now this function needs to be used by conflict analysis"
    )]
    pub(crate) fn restore_to(&mut self, backtrack_level: usize) -> Vec<(DomainId, i32)> {
        pumpkin_assert_simple!(backtrack_level < self.get_assignment_level());

        let unfixed_after_backtracking = self
            .assignments
            .synchronise(backtrack_level, &mut self.notification_engine);
        self.trailed_values.synchronise(backtrack_level);
        self.reason_store.synchronise(backtrack_level);

        self.propagator_queue.clear();
        // For now all propagators are called to synchronise, in the future this will be improved in
        // two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator in self.propagators.iter_propagators_mut() {
            let context = PropagationContext::new(&self.assignments);
            propagator.synchronise(context);
        }

        let _ = self
            .notification_engine
            .process_backtrack_events(&mut self.assignments, &mut self.propagators);
        self.notification_engine.clear_event_drain();

        self.notification_engine
            .update_last_notified_index(&mut self.assignments);
        // Should be done after the assignments and trailed values have been synchronised
        self.notification_engine.synchronise(
            backtrack_level,
            &self.assignments,
            &mut self.trailed_values,
        );

        unfixed_after_backtracking
    }

    pub(crate) fn prepare_for_conflict_resolution(&mut self) -> StoredConflictInfo {
        // TODO: As a temporary solution, we remove the last trail element.
        // This way we guarantee that the assignment is consistent, which is needed
        // for the conflict analysis data structures. The proper alternative would
        // be to forbid the assignments from getting into an inconsistent state.
        let (trigger_predicate, trigger_reason, trigger_inference_code) =
            self.assignments.remove_last_trail_element();

        StoredConflictInfo::EmptyDomain(EmptyDomainConflict {
            trigger_predicate,
            trigger_reason,
            trigger_inference_code,
        })
    }

    pub(crate) fn propagate(
        &mut self,
        propagator_id: PropagatorId,
    ) -> Result<(), StoredConflictInfo> {
        self.statistics.num_propagators_called += 1;

        let num_trail_entries_before = self.assignments.num_trail_entries();

        let propagation_status = {
            let propagator = &mut self.propagators[propagator_id];
            let context = PropagationContextMut::new(
                &mut self.trailed_values,
                &mut self.assignments,
                &mut self.reason_store,
                &mut self.notification_engine,
                propagator_id,
            );
            propagator.propagate(context)
        };

        match propagation_status {
            Ok(_) => {
                // Notify other propagators of the propagations and continue.
                self.notification_engine
                    .notify_propagators_about_domain_events(
                        &mut self.assignments,
                        &mut self.trailed_values,
                        &mut self.propagators,
                        PropagatorHandle {
                            id: PropagatorId(0),
                            propagator: std::marker::PhantomData,
                        },
                        &mut self.propagator_queue,
                    );
            }
            Err(inconsistency) => {
                self.statistics.num_conflicts += 1;
                match inconsistency {
                    // A propagator did a change that resulted in an empty domain.
                    Inconsistency::EmptyDomain => {
                        let info = self.prepare_for_conflict_resolution();
                        return Err(info);
                    }
                    // A propagator-specific reason for the current conflict.
                    Inconsistency::Conflict(conflict) => {
                        pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                            &self.trailed_values,
                            &self.assignments,
                            &conflict.conjunction,
                            &self.propagators[propagator_id],
                            propagator_id,
                            &self.notification_engine
                        ));

                        let stored_conflict_info = StoredConflictInfo::Propagator(conflict);
                        return Err(stored_conflict_info);
                    }
                }
            }
        }
        pumpkin_assert_extreme!(
            DebugHelper::debug_check_propagations(
                num_trail_entries_before,
                propagator_id,
                &self.trailed_values,
                &self.assignments,
                &mut self.reason_store,
                &mut self.propagators,
                &self.notification_engine
            ),
            "Checking the propagations performed by the propagator led to inconsistencies!"
        );
        Ok(())
    }

    pub(crate) fn fixed_point_propagate(&mut self) -> Result<(), StoredConflictInfo> {
        // The initial domain events are due to the decision predicate.
        self.notification_engine
            .notify_propagators_about_domain_events(
                &mut self.assignments,
                &mut self.trailed_values,
                &mut self.propagators,
                PropagatorHandle {
                    id: PropagatorId(0),
                    propagator: std::marker::PhantomData,
                },
                &mut self.propagator_queue,
            );
        // Keep propagating until there are unprocessed propagators, or a conflict is detected.
        let mut result = Ok(());
        while let Some(propagator_id) = self.propagator_queue.pop() {
            result = self.propagate(propagator_id);
            if result.is_err() {
                break;
            }
        }
        // Only check fixed point propagation if there was no reported conflict,
        // since otherwise the state may be inconsistent.
        pumpkin_assert_extreme!(
            result.is_err()
                || DebugHelper::debug_fixed_point_propagation(
                    &self.trailed_values,
                    &self.assignments,
                    &self.propagators,
                    &self.notification_engine
                )
        );

        result
    }
}

impl State {
    /// This is a temporary accessor to help refactoring.
    pub(crate) fn get_solution_reference(&self) -> SolutionReference<'_> {
        SolutionReference::new(&self.assignments)
    }

    pub(crate) fn variable_names(&self) -> &VariableNames {
        &self.variable_names
    }

    pub(crate) fn get_propagation_reason_trail_entry(
        &mut self,
        trail_position: usize,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
    ) {
        let entry = self.trail_entry(trail_position);
        let (reason_ref, _) = entry
            .reason
            .expect("Added by a propagator and must therefore have a reason");
        let _ = self.reason_store.get_or_compute(
            reason_ref,
            ExplanationContext::without_working_nogood(
                &self.assignments,
                trail_position,
                &mut self.notification_engine,
            ),
            &mut self.propagators,
            reason_buffer,
        );
    }

    /// Compute the reason for `predicate` being true. The reason will be stored in
    /// `reason_buffer`.
    ///
    /// If `predicate` is not true, or it is a decision, then this function will panic.
    #[allow(unused, reason = "Will be part of public API")]
    pub(crate) fn get_propagation_reason(
        &mut self,
        predicate: Predicate,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
    ) {
        // TODO: this function could be put into the reason store

        // Note that this function can only be called with propagations, and never decision
        // predicates. Furthermore only predicate from the current decision level will be
        // considered. This is due to how the 1uip conflict analysis works: it scans the
        // predicates in reverse order of assignment, and stops as soon as there is only one
        // predicate from the current decision level in the learned nogood.

        // This means that the procedure would never ask for the reason of the decision predicate
        // from the current decision level, because that would mean that all other predicates from
        // the current decision level have been removed from the nogood, and the decision
        // predicate is the only one left, but in that case, the 1uip would terminate since
        // there would be only one predicate from the current decision level. For this
        // reason, it is safe to assume that in the following, that any input predicate is
        // indeed a propagated predicate.
        if self.assignments.is_initial_bound(predicate) {
            return;
        }

        let trail_position = self
            .assignments
            .get_trail_position(&predicate)
            .unwrap_or_else(|| panic!("The predicate {predicate:?} must be true during conflict analysis. Bounds were {},{}", self.lower_bound(predicate.get_domain()), self.upper_bound(predicate.get_domain())));

        let trail_entry = self.assignments.get_trail_entry(trail_position);

        // We distinguish between three cases:
        // 1) The predicate is explicitly present on the trail.
        if trail_entry.predicate == predicate {
            let (reason_ref, _) = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            let explanation_context = ExplanationContext::new(
                &self.assignments,
                CurrentNogood::empty(),
                trail_position,
                &mut self.notification_engine,
            );

            let reason_exists = self.reason_store.get_or_compute(
                reason_ref,
                explanation_context,
                &mut self.propagators,
                reason_buffer,
            );

            assert!(reason_exists, "reason reference should not be stale");
        }
        // 2) The predicate is true due to a propagation, and not explicitly on the trail.
        // It is necessary to further analyse what was the reason for setting the predicate true.
        else {
            // The reason for propagation depends on:
            // 1) The predicate on the trail at the moment the input predicate became true, and
            // 2) The input predicate.
            match (
                trail_entry.predicate.get_predicate_type(),
                predicate.get_predicate_type(),
            ) {
                (PredicateType::LowerBound, PredicateType::LowerBound) => {
                    let trail_lower_bound = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_lower_bound = predicate.get_right_hand_side();
                    // Both the input predicate and the trail predicate are lower bound
                    // literals. Two cases to consider:
                    // 1) The trail predicate has a greater right-hand side, meaning
                    //  the reason for the input predicate is true is because a stronger
                    //  right-hand side predicate was posted. We can reuse the same
                    //  reason as for the trail bound.
                    //  todo: could consider lifting here, since the trail bound
                    //  might be too strong.
                    if trail_lower_bound > input_lower_bound {
                        reason_buffer.extend(std::iter::once(trail_entry.predicate));
                    }
                    // Otherwise, the input bound is strictly greater than the trailed
                    // bound. This means the reason is due to holes in the domain.
                    else {
                        // Note that the bounds cannot be equal.
                        // If the bound were equal, the predicate would be explicitly on the
                        // trail, so we would have detected this case earlier.
                        pumpkin_assert_simple!(trail_lower_bound < input_lower_bound);

                        // The reason for the propagation of the input predicate [x >= a] is
                        // because [x >= a-1] & [x != a]. Conflict analysis will then
                        // recursively decompose these further.

                        // Note that we do not need to worry about decreasing the lower
                        // bounds so much so that it reaches its root lower bound, for which
                        // there is no reason since it is given as input to the problem.
                        // We cannot reach the original lower bound since in the 1uip, we
                        // only look for reasons for predicates from the current decision
                        // level, and we never look for reasons at the root level.

                        let one_less_bound_predicate =
                            predicate!(domain_id >= input_lower_bound - 1);

                        let not_equals_predicate = predicate!(domain_id != input_lower_bound - 1);
                        reason_buffer.extend(std::iter::once(one_less_bound_predicate));
                        reason_buffer.extend(std::iter::once(not_equals_predicate));
                    }
                }
                (PredicateType::LowerBound, PredicateType::NotEqual) => {
                    let trail_lower_bound = trail_entry.predicate.get_right_hand_side();
                    let not_equal_constant = predicate.get_right_hand_side();
                    // The trail entry is a lower bound literal,
                    // and the input predicate is a not equals.
                    // Only one case to consider:
                    // The trail lower bound is greater than the not_equals_constant,
                    // so it safe to take the reason from the trail.
                    // todo: lifting could be used here
                    pumpkin_assert_simple!(trail_lower_bound > not_equal_constant);
                    reason_buffer.extend(std::iter::once(trail_entry.predicate));
                }
                (PredicateType::LowerBound, PredicateType::Equal) => {
                    let domain_id = predicate.get_domain();
                    let equality_constant = predicate.get_right_hand_side();
                    // The input predicate is an equality predicate, and the trail predicate
                    // is a lower bound predicate. This means that the time of posting the
                    // trail predicate is when the input predicate became true.

                    // Note that the input equality constant does _not_ necessarily equal
                    // the trail lower bound. This would be the
                    // case when the the trail lower bound is lower than the input equality
                    // constant, but due to holes in the domain, the lower bound got raised
                    // to just the value of the equality constant.
                    // For example, {1, 2, 3, 10}, then posting [x >= 5] will raise the
                    // lower bound to x >= 10.

                    let predicate_lb = predicate!(domain_id >= equality_constant);
                    let predicate_ub = predicate!(domain_id <= equality_constant);
                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (PredicateType::UpperBound, PredicateType::UpperBound) => {
                    let trail_upper_bound = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_upper_bound = predicate.get_right_hand_side();
                    // Both the input and trail predicates are upper bound predicates.
                    // There are two scenarios to consider:
                    // 1) The input upper bound is greater than the trail upper bound, meaning that
                    //    the reason for the input predicate is the propagation of a stronger upper
                    //    bound. We can safely use the reason for of the trail predicate as the
                    //    reason for the input predicate.
                    // todo: lifting could be applied here.
                    if trail_upper_bound < input_upper_bound {
                        reason_buffer.extend(std::iter::once(trail_entry.predicate));
                    } else {
                        // I think it cannot be that the bounds are equal, since otherwise we
                        // would have found the predicate explicitly on the trail.
                        pumpkin_assert_simple!(trail_upper_bound > input_upper_bound);

                        // The input upper bound is greater than the trail predicate, meaning
                        // that holes in the domain also played a rule in lowering the upper
                        // bound.

                        // The reason of the input predicate [x <= a] is computed recursively as
                        // the reason for [x <= a + 1] & [x != a + 1].

                        let new_ub_predicate = predicate!(domain_id <= input_upper_bound + 1);
                        let not_equal_predicate = predicate!(domain_id != input_upper_bound + 1);
                        reason_buffer.extend(std::iter::once(new_ub_predicate));
                        reason_buffer.extend(std::iter::once(not_equal_predicate));
                    }
                }
                (PredicateType::UpperBound, PredicateType::NotEqual) => {
                    let trail_upper_bound = trail_entry.predicate.get_right_hand_side();
                    let not_equal_constant = predicate.get_right_hand_side();
                    // The input predicate is a not equal predicate, and the trail predicate is
                    // an upper bound predicate. This is only possible when the upper bound was
                    // pushed below the not equals value. Otherwise the hole would have been
                    // explicitly placed on the trail and we would have found it earlier.
                    pumpkin_assert_simple!(not_equal_constant > trail_upper_bound);

                    // The bound was set past the not equals, so we can safely returns the trail
                    // reason. todo: can do lifting here.
                    reason_buffer.extend(std::iter::once(trail_entry.predicate));
                }
                (PredicateType::UpperBound, PredicateType::Equal) => {
                    let domain_id = predicate.get_domain();
                    let equality_constant = predicate.get_right_hand_side();
                    // The input predicate is an equality predicate, and the trail predicate
                    // is an upper bound predicate. This means that the time of posting the
                    // trail predicate is when the input predicate became true.

                    // Note that the input equality constant does _not_ necessarily equal
                    // the trail upper bound. This would be the
                    // case when the the trail upper bound is greater than the input equality
                    // constant, but due to holes in the domain, the upper bound got lowered
                    // to just the value of the equality constant.
                    // For example, x = {1, 2, 3, 8, 15}, setting [x <= 12] would lower the
                    // upper bound to x <= 8.

                    // Note that it could be that one of the two predicates are decision
                    // predicates, so we need to use the substitute functions.

                    let predicate_lb = predicate!(domain_id >= equality_constant);
                    let predicate_ub = predicate!(domain_id <= equality_constant);
                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (PredicateType::NotEqual, PredicateType::LowerBound) => {
                    let not_equal_constant = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_lower_bound = predicate.get_right_hand_side();
                    // The trail predicate is not equals, but the input predicate is a lower
                    // bound predicate. This means that creating the hole in the domain resulted
                    // in raising the lower bound.

                    // I think this holds. The not_equals_constant cannot be greater, since that
                    // would not impact the lower bound. It can also not be the same, since
                    // creating a hole cannot result in the lower bound being raised to the
                    // hole, there must be some other reason for that to happen, which we would
                    // find earlier.
                    pumpkin_assert_simple!(input_lower_bound > not_equal_constant);

                    // The reason for the input predicate [x >= a] is computed recursively as
                    // the reason for [x >= a - 1] & [x != a - 1].
                    let new_lb_predicate = predicate!(domain_id >= input_lower_bound - 1);
                    let new_not_equals_predicate = predicate!(domain_id != input_lower_bound - 1);

                    reason_buffer.extend(std::iter::once(new_lb_predicate));
                    reason_buffer.extend(std::iter::once(new_not_equals_predicate));
                }
                (PredicateType::NotEqual, PredicateType::UpperBound) => {
                    let not_equal_constant = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_upper_bound = predicate.get_right_hand_side();
                    // The trail predicate is not equals, but the input predicate is an upper
                    // bound predicate. This means that creating the hole in the domain resulted
                    // in lower the upper bound.

                    // I think this holds. The not_equals_constant cannot be smaller, since that
                    // would not impact the upper bound. It can also not be the same, since
                    // creating a hole cannot result in the upper bound being lower to the
                    // hole, there must be some other reason for that to happen, which we would
                    // find earlier.
                    pumpkin_assert_simple!(input_upper_bound < not_equal_constant);

                    // The reason for the input predicate [x <= a] is computed recursively as
                    // the reason for [x <= a + 1] & [x != a + 1].
                    let new_ub_predicate = predicate!(domain_id <= input_upper_bound + 1);
                    let new_not_equals_predicate = predicate!(domain_id != input_upper_bound + 1);

                    reason_buffer.extend(std::iter::once(new_ub_predicate));
                    reason_buffer.extend(std::iter::once(new_not_equals_predicate));
                }
                (PredicateType::NotEqual, PredicateType::Equal) => {
                    let domain_id = predicate.get_domain();
                    let equality_constant = predicate.get_right_hand_side();
                    // The trail predicate is not equals, but the input predicate is
                    // equals. The only time this could is when the not equals forces the
                    // lower/upper bounds to meet. So we simply look for the reasons for those
                    // bounds recursively.

                    // Note that it could be that one of the two predicates are decision
                    // predicates, so we need to use the substitute functions.

                    let predicate_lb = predicate!(domain_id >= equality_constant);
                    let predicate_ub = predicate!(domain_id <= equality_constant);

                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (
                    PredicateType::Equal,
                    PredicateType::LowerBound | PredicateType::UpperBound | PredicateType::NotEqual,
                ) => {
                    // The trail predicate is equality, but the input predicate is either a
                    // lower-bound, upper-bound, or not equals.
                    //
                    // TODO: could consider lifting here
                    reason_buffer.extend(std::iter::once(trail_entry.predicate))
                }
                _ => unreachable!(
                    "Unreachable combination of {} and {}",
                    trail_entry.predicate, predicate
                ),
            };
        }
    }
}
