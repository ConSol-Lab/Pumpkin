use std::sync::Arc;

use crate::basic_types::PropagatorConflict;
use crate::containers::KeyGenerator;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::ConstraintProgrammingTrailEntry;
use crate::engine::DebugHelper;
use crate::engine::EmptyDomain;
use crate::engine::PropagatorQueue;
use crate::engine::TrailedValues;
use crate::engine::VariableNames;
use crate::engine::notifications::NotificationEngine;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
#[cfg(doc)]
use crate::proof::ProofLog;
use crate::propagation::CurrentNogood;
use crate::propagation::Domains;
use crate::propagation::ExplanationContext;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorId;
use crate::propagation::store::PropagatorStore;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;
use crate::results::SolutionReference;
use crate::state::PropagatorHandle;
use crate::statistics::StatisticLogger;
use crate::statistics::log_statistic;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

/// The [`State`] is the container of variables and propagators.
///
/// [`State`] implements [`Clone`], and cloning the [`State`] will create a fresh copy of the
/// [`State`]. If the [`State`] is large, this may be extremely expensive.
#[derive(Debug, Clone)]
pub struct State {
    /// The list of propagators; propagators live here and are queried when events (domain changes)
    /// happen.
    pub(crate) propagators: PropagatorStore,
    /// Tracks information related to the assignments of integer variables.
    pub(crate) assignments: Assignments,
    /// Keep track of trailed values (i.e. values which automatically backtrack).
    pub(crate) trailed_values: TrailedValues,
    /// The names of the variables in the solver.
    pub(crate) variable_names: VariableNames,
    /// Dictates the order in which propagators will be called to propagate.
    pub(crate) propagator_queue: PropagatorQueue,
    /// Handles storing information about propagation reasons, which are used later to construct
    /// explanations during conflict analysis.
    pub(crate) reason_store: ReasonStore,
    /// Component responsible for providing notifications for changes to the domains of variables
    /// and/or the polarity [Predicate]s
    pub(crate) notification_engine: NotificationEngine,

    /// The [`ConstraintTag`]s generated for this proof.
    pub(crate) constraint_tags: KeyGenerator<ConstraintTag>,

    statistics: StateStatistics,
}

create_statistics_struct!(StateStatistics {
    num_propagators_called: usize,
    num_propagations: usize,
    num_conflicts: usize,
    /// The number of levels which were backjumped.
    ///
    /// For an individual backtrack due to a learned nogood, this is calculated according to the
    /// formula `CurrentDecisionLevel - 1 - BacktrackLevel` (i.e. how many levels (in total) has
    /// the solver backtracked and not backjumped)
    sum_of_backjumps: u64,
    /// The number of times a backjump (i.e. backtracking more than a single decision level due to
    /// a learned nogood) occurs.
    num_backjumps: u64,
});

/// Information concerning the conflict returned by [`State::propagate_to_fixed_point`].
///
/// Two (related) conflicts can happen:
/// 1) a propagator explicitly detects a conflict.
/// 2) a propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Conflict {
    /// A conflict raised explicitly by a propagator.
    Propagator(PropagatorConflict),
    /// A conflict caused by an empty domain for a variable occurring.
    EmptyDomain(EmptyDomainConflict),
}

impl From<EmptyDomainConflict> for Conflict {
    fn from(value: EmptyDomainConflict) -> Self {
        Conflict::EmptyDomain(value)
    }
}

impl From<PropagatorConflict> for Conflict {
    fn from(value: PropagatorConflict) -> Self {
        Conflict::Propagator(value)
    }
}

/// A conflict because a domain became empty.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmptyDomainConflict {
    /// The predicate that caused a domain to become empty.
    pub trigger_predicate: Predicate,
    /// The reason for [`EmptyDomainConflict::trigger_predicate`] to be true.
    pub(crate) trigger_reason: ReasonRef,
    /// The [`InferenceCode`] that accompanies [`EmptyDomainConflict::trigger_reason`].
    pub(crate) trigger_inference_code: InferenceCode,
}

impl EmptyDomainConflict {
    /// The domain that became empty.
    pub fn domain(&self) -> DomainId {
        self.trigger_predicate.get_domain()
    }

    /// Returns the reason for the [`EmptyDomainConflict::trigger_predicate`] being propagated to
    /// true while it is already false in the [`State`].
    pub fn get_reason(
        &self,
        state: &mut State,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        current_nogood: CurrentNogood,
    ) {
        let _ = state.reason_store.get_or_compute(
            self.trigger_reason,
            ExplanationContext::new(
                &state.assignments,
                current_nogood,
                state.trail_len(),
                &mut state.notification_engine,
            ),
            &mut state.propagators,
            reason_buffer,
        );
    }
}

impl Default for State {
    fn default() -> Self {
        let mut result = Self {
            assignments: Default::default(),
            trailed_values: TrailedValues::default(),
            variable_names: VariableNames::default(),
            propagator_queue: PropagatorQueue::default(),
            propagators: PropagatorStore::default(),
            reason_store: ReasonStore::default(),
            notification_engine: NotificationEngine::default(),
            statistics: StateStatistics::default(),
            constraint_tags: KeyGenerator::default(),
        };
        // As a convention, the assignments contain a dummy domain_id=0, which represents a 0-1
        // variable that is assigned to one. We use it to represent predicates that are
        // trivially true. We need to adjust other data structures to take this into account.
        let dummy_id = Predicate::trivially_true().get_domain();

        result.variable_names.add_integer(dummy_id, "Dummy".into());
        assert!(dummy_id.id() == 0);
        assert!(result.assignments.get_lower_bound(dummy_id) == 1);
        assert!(result.assignments.get_upper_bound(dummy_id) == 1);

        result
    }
}

impl State {
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
    /// Create a new [`ConstraintTag`].
    pub fn new_constraint_tag(&mut self) -> ConstraintTag {
        self.constraint_tags.next_key()
    }

    /// Creates a new Boolean (0-1) variable.
    ///
    /// The name is used in solver traces to identify individual domains. They are required to be
    /// unique. If the state already contains a domain with the given name, then this function
    /// will panic.
    ///
    /// Creation of new [`Literal`]s is not influenced by the current checkpoint of the state.
    /// If a [`Literal`] is created at a non-zero checkpoint, then it will _not_ 'disappear'
    /// when backtracking past the checkpoint where the domain was created.
    pub fn new_literal(&mut self, name: Option<Arc<str>>) -> Literal {
        let domain_id = self.new_interval_variable(0, 1, name);
        Literal::new(domain_id)
    }

    /// Creates a new interval variable with the given lower and upper bound.
    ///
    /// The name is used in solver traces to identify individual domains. They are required to be
    /// unique. If the state already contains a domain with the given name, then this function
    /// will panic.
    ///
    /// Creation of new domains is not influenced by the current checkpoint of the state. If
    /// a domain is created at a non-zero checkpoint, then it will _not_ 'disappear' when
    /// backtracking past the checkpoint where the domain was created.)
    pub fn new_interval_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: Option<Arc<str>>,
    ) -> DomainId {
        let domain_id = self.assignments.grow(lower_bound, upper_bound);

        if let Some(name) = name {
            self.variable_names.add_integer(domain_id, name);
        }

        self.notification_engine.grow();

        domain_id
    }

    /// Creates a new sparse domain with the given values.
    ///
    /// Note that this is implemented as an interval domain with explicit holes in the domain. For
    /// very sparse domains, this can result in a high memory overhead.
    ///
    /// For more information on creation of domains, see [`State::new_interval_variable`].
    pub fn new_sparse_variable(&mut self, values: Vec<i32>, name: Option<String>) -> DomainId {
        let domain_id = self.assignments.create_new_integer_variable_sparse(values);

        if let Some(name) = name {
            self.variable_names.add_integer(domain_id, name.into());
        }

        self.notification_engine.grow();

        domain_id
    }
}

/// Operations to retrieve information about values
impl State {
    /// Returns the lower-bound of the given `variable`.
    pub fn lower_bound<Var: IntegerVariable>(&self, variable: Var) -> i32 {
        variable.lower_bound(&self.assignments)
    }

    /// Returns the upper-bound of the given `variable`.
    pub fn upper_bound<Var: IntegerVariable>(&self, variable: Var) -> i32 {
        variable.upper_bound(&self.assignments)
    }

    /// Returns whether the given `variable` contains the provided `value`.
    pub fn contains<Var: IntegerVariable>(&self, variable: Var, value: i32) -> bool {
        variable.contains(&self.assignments, value)
    }

    /// If the given `variable` is fixed, then [`Some`] containing the assigned value is
    /// returned. Otherwise, [`None`] is returned.
    pub fn fixed_value<Var: IntegerVariable>(&self, variable: Var) -> Option<i32> {
        (self.lower_bound(variable.clone()) == self.upper_bound(variable.clone()))
            .then(|| self.lower_bound(variable))
    }

    /// Returns the truth value of the provided [`Predicate`].
    ///
    /// If the [`Predicate`] is assigned in the current [`State`] then [`Some`] containing whether
    /// the [`Predicate`] is satisfied or falsified is returned. Otherwise, [`None`] is returned.
    pub fn truth_value(&self, predicate: Predicate) -> Option<bool> {
        self.assignments.evaluate_predicate(predicate)
    }

    /// If the provided [`Predicate`] is satisfied then it returns [`Some`] containing the
    /// checkpoint at which the [`Predicate`] became satisfied. Otherwise, [`None`] is returned.
    pub fn get_checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize> {
        self.assignments.get_checkpoint_for_predicate(&predicate)
    }

    /// Returns the truth value of the provided [`Literal`].
    ///
    /// If the [`Literal`] is assigned in the current [`State`] then [`Some`] containing whether
    /// the [`Literal`] is satisfied or falsified is returned. Otherwise, [`None`] is returned.
    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        self.truth_value(literal.get_true_predicate())
    }

    /// Returns the number of created checkpoints.
    pub fn get_checkpoint(&self) -> usize {
        self.assignments.get_checkpoint()
    }
}

/// Operations for retrieving information about trail
impl State {
    /// Returns the length of the trail.
    pub(crate) fn trail_len(&self) -> usize {
        self.assignments.num_trail_entries()
    }

    /// Returns the [`Predicate`] at the provided `trail_index`.
    pub(crate) fn trail_entry(&self, trail_index: usize) -> ConstraintProgrammingTrailEntry {
        self.assignments.get_trail_entry(trail_index)
    }

    /// Returns whether the provided [`Predicate`] is explicitly on the trail.
    ///
    /// For example, if we post the [`Predicate`] [x >= v], then the predicate [x >= v - 1] is
    /// not explicity on the trail.
    pub fn is_on_trail(&self, predicate: Predicate) -> bool {
        let trail_position = self.trail_position(predicate);

        trail_position.is_some_and(|trail_position| {
            self.assignments.trail[trail_position].predicate == predicate
        })
    }

    /// Returns whether the trail position of the provided [`Predicate`].
    pub fn trail_position(&self, predicate: Predicate) -> Option<usize> {
        self.assignments.get_trail_position(&predicate)
    }
}

/// Operations for adding constraints.
impl State {
    /// Enqueues the propagator with [`PropagatorHandle`] `handle` for propagation.
    #[deprecated]
    pub(crate) fn enqueue_propagator<P: Propagator>(&mut self, handle: PropagatorHandle<P>) {
        let priority = self.propagators[handle.propagator_id()].priority();
        self.propagator_queue
            .enqueue_propagator(handle.propagator_id(), priority);
    }

    /// Add a new propagator to the [`State`]. The constructor for that propagator should
    /// subscribe to the appropriate domain events so that the propagator is called when
    /// necessary.
    ///
    /// While the propagator is added to the queue for propagation, this function does _not_
    /// trigger a round of propagation. An explicit call to [`State::propagate_to_fixed_point`] is
    /// necessary to run the new propagator for the first time.
    pub fn add_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> PropagatorHandle<Constructor::PropagatorImpl>
    where
        Constructor: PropagatorConstructor,
        Constructor::PropagatorImpl: 'static,
    {
        let original_handle: PropagatorHandle<Constructor::PropagatorImpl> =
            self.propagators.new_propagator().key();
        let constructor_context =
            PropagatorConstructorContext::new(original_handle.propagator_id(), self);
        let propagator = constructor.create(constructor_context);

        pumpkin_assert_simple!(
            propagator.priority() as u8 <= 3,
            "The propagator priority exceeds 3.
             Currently we only support values up to 3,
             but this can easily be changed if there is a good reason."
        );

        let slot = self.propagators.new_propagator();
        let handle = slot.populate(propagator);

        pumpkin_assert_eq_simple!(handle.propagator_id(), original_handle.propagator_id());

        #[allow(deprecated, reason = "Will be refactored")]
        self.enqueue_propagator(handle);

        handle
    }
}

/// Operations for retrieving propagators.
impl State {
    /// Get a reference to the propagator identified by the given handle.
    ///
    /// For an exclusive reference, use [`State::get_propagator_mut`].
    pub fn get_propagator<P: Propagator>(&self, handle: PropagatorHandle<P>) -> Option<&P> {
        self.propagators.get_propagator(handle)
    }

    /// Get an exclusive reference to the propagator identified by the given handle.
    pub fn get_propagator_mut<P: Propagator>(
        &mut self,
        handle: PropagatorHandle<P>,
    ) -> Option<&mut P> {
        self.propagators.get_propagator_mut(handle)
    }

    /// Get an exclusive reference to the propagator identified by the given handle and a context
    /// which can be used for propagation.
    pub(crate) fn get_propagator_mut_with_context<P: Propagator>(
        &mut self,
        handle: PropagatorHandle<P>,
    ) -> (Option<&mut P>, PropagationContext<'_>) {
        (
            self.propagators.get_propagator_mut(handle),
            PropagationContext::new(
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
    /// Apply a [`Predicate`] to the [`State`].
    ///
    /// Returns `true` if a change to a domain occured, and `false` if the given [`Predicate`] was
    /// already true.
    ///
    /// If a domain becomes empty due to this operation, an [`EmptyDomain`] error is returned.
    ///
    /// This method does _not_ perform any propagation. For that, an explicit call to
    /// [`State::propagate_to_fixed_point`] is required. This allows the
    /// posting of multiple predicates before the entire propagation engine is invoked.
    ///
    /// A call to [`State::restore_to`] that goes past the checkpoint at which a [`Predicate`]
    /// was posted will undo the effect of that [`Predicate`]. See the documentation of
    /// [`State::new_checkpoint`] and
    /// [`State::restore_to`] for more information.
    pub fn post(&mut self, predicate: Predicate) -> Result<bool, EmptyDomain> {
        self.assignments
            .post_predicate(predicate, None, &mut self.notification_engine)
    }

    /// Create a checkpoint of the current [`State`], that can be returned to with
    /// [`State::restore_to`].
    ///
    /// The current checkpoint can be retrieved using the method [`State::get_checkpoint`].
    ///
    /// If the state is not at fixed-point, then this method will panic.
    ///
    /// # Example
    /// ```
    /// use pumpkin_core::predicate;
    /// use pumpkin_core::state::State;
    ///
    /// let mut state = State::default();
    /// let variable = state.new_interval_variable(1, 10, Some("x1".into()));
    ///
    /// assert_eq!(state.get_checkpoint(), 0);
    ///
    /// state.new_checkpoint();
    ///
    /// assert_eq!(state.get_checkpoint(), 1);
    ///
    /// state
    ///     .post(predicate![variable <= 5])
    ///     .expect("The lower bound is 1 so no conflict");
    /// assert_eq!(state.upper_bound(variable), 5);
    ///
    /// state.restore_to(0);
    ///
    /// assert_eq!(state.get_checkpoint(), 0);
    /// assert_eq!(state.upper_bound(variable), 10);
    /// ```
    pub fn new_checkpoint(&mut self) {
        pumpkin_assert_simple!(
            self.propagator_queue.is_empty(),
            "Can only create a new checkpoint when all propagation has occurred"
        );
        self.assignments.new_checkpoint();
        self.notification_engine.new_checkpoint();
        self.trailed_values.new_checkpoint();
        self.reason_store.new_checkpoint();
    }

    /// Restore to the given checkpoint and return the [`DomainId`]s which were fixed before
    /// restoring, with their assigned values.
    ///
    /// If the provided checkpoint is equal to the current checkpoint, this is a no-op. If
    /// the provided checkpoint is larger than the current checkpoint, this method will
    /// panic.
    ///
    /// See [`State::new_checkpoint`] for an example.
    pub fn restore_to(&mut self, checkpoint: usize) -> Vec<(DomainId, i32)> {
        pumpkin_assert_simple!(checkpoint <= self.get_checkpoint());

        self.statistics.sum_of_backjumps += (self.get_checkpoint() - 1 - checkpoint) as u64;
        if self.get_checkpoint() - checkpoint > 1 {
            self.statistics.num_backjumps += 1;
        }

        if checkpoint == self.get_checkpoint() {
            return vec![];
        }

        let unfixed_after_backtracking = self
            .assignments
            .synchronise(checkpoint, &mut self.notification_engine);
        self.trailed_values.synchronise(checkpoint);
        self.reason_store.synchronise(checkpoint);

        self.propagator_queue.clear();
        // For now all propagators are called to synchronise, in the future this will be improved in
        // two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator in self.propagators.iter_propagators_mut() {
            let context = Domains::new(&self.assignments, &mut self.trailed_values);
            propagator.synchronise(context);
        }

        let _ = self.notification_engine.process_backtrack_events(
            &mut self.assignments,
            &mut self.trailed_values,
            &mut self.propagators,
        );
        self.notification_engine.clear_event_drain();

        self.notification_engine
            .update_last_notified_index(&mut self.assignments);
        // Should be done after the assignments and trailed values have been synchronised
        self.notification_engine.synchronise(
            checkpoint,
            &self.assignments,
            &mut self.trailed_values,
        );

        unfixed_after_backtracking
    }

    /// Performs a single call to [`Propagator::propagate`] for the propagator with the provided
    /// [`PropagatorId`].
    ///
    /// Other propagators could be enqueued as a result of the changes made by the propagated
    /// propagator but a call to [`State::propagate_to_fixed_point`] is
    /// required for further propagation to occur.
    ///
    /// It could be that the current [`State`] implies a conflict by propagation. In that case, an
    /// [`Err`] with [`Conflict`] is returned.
    ///
    /// Once the [`State`] is conflicting, then the only operation that is defined is
    /// [`State::restore_to`]. All other operations and queries on the state are undetermined.
    fn propagate(&mut self, propagator_id: PropagatorId) -> Result<(), Conflict> {
        self.statistics.num_propagators_called += 1;

        let num_trail_entries_before = self.assignments.num_trail_entries();

        let propagation_status = {
            let propagator = &mut self.propagators[propagator_id];
            let context = PropagationContext::new(
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
                        &mut self.propagator_queue,
                    );
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
            }
            Err(conflict) => {
                self.statistics.num_conflicts += 1;
                if let Conflict::Propagator(inner) = &conflict {
                    pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                        &self.trailed_values,
                        &self.assignments,
                        &inner.conjunction,
                        &self.propagators[propagator_id],
                        propagator_id,
                        &self.notification_engine
                    ));
                }

                return Err(conflict);
            }
        }
        Ok(())
    }

    /// Performs fixed-point propagation using the propagators defined in the [`State`].
    ///
    /// The posted [`Predicate`]s (using [`State::post`]) and added propagators (using
    /// [`State::add_propagator`]) cause propagators to be enqueued when the events that
    /// they have subscribed to are triggered. As propagation causes more changes to be made,
    /// more propagators are enqueued. This continues until applying all (enqueued)
    /// propagators leads to no more domain changes.
    ///
    /// It could be that the current [`State`] implies a conflict by propagation. In that case, an
    /// error with [`Conflict`] is returned.
    ///
    /// Once the [`State`] is conflicting, then the only operation that is defined is
    /// [`State::restore_to`]. All other operations and queries on the state are unspecified.
    pub fn propagate_to_fixed_point(&mut self) -> Result<(), Conflict> {
        // The initial domain events are due to the decision predicate.
        self.notification_engine
            .notify_propagators_about_domain_events(
                &mut self.assignments,
                &mut self.trailed_values,
                &mut self.propagators,
                &mut self.propagator_queue,
            );

        // Keep propagating until there are unprocessed propagators, or a conflict is detected.
        while let Some(propagator_id) = self.propagator_queue.pop() {
            self.propagate(propagator_id)?;
        }

        // Only check fixed point propagation if there was no reported conflict,
        // since otherwise the state may be inconsistent.
        pumpkin_assert_extreme!(DebugHelper::debug_fixed_point_propagation(
            &self.trailed_values,
            &self.assignments,
            &self.propagators,
            &self.notification_engine
        ));

        Ok(())
    }
}

impl State {
    /// This is a temporary accessor to help refactoring.
    pub(crate) fn get_solution_reference(&self) -> SolutionReference<'_> {
        SolutionReference::new(&self.assignments)
    }

    /// Returns a mapping of [`DomainId`] to variable name.
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
    /// Get the reason for a predicate being true and store it in `reason_buffer`; additionally, if
    /// the provided [`Predicate`] is explicitly on the trail, this method will return the
    /// corresponding trail index.
    ///
    /// The provided `current_nogood` can be used by the propagator to provide a different reason;
    /// use [`CurrentNogood::empty`] otherwise.
    ///
    /// All the predicates in the returned slice will evaluate to `true`.
    ///
    /// If the provided predicate is not true, then this method will panic.
    #[allow(unused, reason = "Will be part of public API")]
    pub fn get_propagation_reason(
        &mut self,
        predicate: Predicate,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        current_nogood: CurrentNogood<'_>,
    ) -> Option<usize> {
        // TODO: this function could be put into the reason store

        // Note that this function can only be called with propagations, and never decision
        // predicates. Furthermore only predicate from the current checkpoint will be
        // considered. This is due to how the 1uip conflict analysis works: it scans the
        // predicates in reverse order of assignment, and stops as soon as there is only one
        // predicate from the current checkpoint in the learned nogood.

        // This means that the procedure would never ask for the reason of the decision predicate
        // from the current checkpoint, because that would mean that all other predicates from
        // the current checkpoint have been removed from the nogood, and the decision
        // predicate is the only one left, but in that case, the 1uip would terminate since
        // there would be only one predicate from the current checkpoint. For this
        // reason, it is safe to assume that in the following, that any input predicate is
        // indeed a propagated predicate.
        if self.assignments.is_initial_bound(predicate) {
            return None;
        }

        let trail_position = self
            .assignments
            .get_trail_position(&predicate)
            .unwrap_or_else(|| panic!("The predicate {predicate:?} must be true during conflict analysis. Bounds were {},{}", self.lower_bound(predicate.get_domain()), self.upper_bound(predicate.get_domain())));

        let trail_entry = self.assignments.get_trail_entry(trail_position);

        // We distinguish between three cases:
        // 1) The predicate is explicitly present on the trail.
        if trail_entry.predicate == predicate {
            let (reason_ref, inference_code) = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            let explanation_context = ExplanationContext::new(
                &self.assignments,
                current_nogood,
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

            Some(trail_position)
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
            None
        }
    }
}

impl State {
    pub fn get_domains(&mut self) -> Domains<'_> {
        Domains::new(&self.assignments, &mut self.trailed_values)
    }

    pub fn get_propagation_context(&mut self) -> PropagationContext<'_> {
        PropagationContext::new(
            &mut self.trailed_values,
            &mut self.assignments,
            &mut self.reason_store,
            &mut self.notification_engine,
            PropagatorId(0),
        )
    }
}
