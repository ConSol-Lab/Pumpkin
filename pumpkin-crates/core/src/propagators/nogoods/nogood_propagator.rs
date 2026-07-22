use std::cmp::max;
use std::ops::Not;

use bitfield_struct::bitfield;
use log::warn;

use super::LearningOptions;
use super::NogoodId;
use super::NogoodInfo;
use crate::basic_types::PredicateId;
use crate::basic_types::PropositionalConjunction;
use crate::containers::HashSet;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::Lbd;
use crate::engine::PropagationStatusCP;
use crate::engine::PropagatorConflict;
use crate::engine::notifications::NotificationEngine;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::predicate;
use crate::predicates::PredicateType;
use crate::proof::InferenceCode;
use crate::propagation::EnqueueDecision;
use crate::propagation::EventsToRegister;
use crate::propagation::ExplanationContext;
use crate::propagation::HasAssignments;
use crate::propagation::LazyExplanation;
use crate::propagation::NotificationContext;
use crate::propagation::Priority;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorSpec;
use crate::propagation::ReadDomains;
use crate::propagation::RuntimeCheckers;
use crate::propagators::nogoods::PropagationMode;
use crate::propagators::nogoods::WatcherProcessingStatus;
use crate::propagators::nogoods::arena_allocator::ArenaAllocator;
use crate::propagators::nogoods::arena_allocator::NogoodIndex;
use crate::propagators::nogoods::semantic_minimiser::SemanticMinimiser;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::state::Conflict;
use crate::state::PropagatorHandle;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::statistics::moving_averages::CumulativeMovingAverage;
use crate::statistics::moving_averages::MovingAverage;
use crate::variables::DomainId;

/// A propagator which propagates nogoods (i.e. a list of [`Predicate`]s which cannot all be true
/// at the same time).
///
/// The propagation used is based on the provided [`PropagationMode`].
#[derive(Clone, Debug)]
pub struct NogoodPropagator {
    /// The [`PredicateId`]s of the nogoods.
    nogood_predicates: ArenaAllocator,
    /// The information corresponding to each nogood; including activity, and LBD.
    nogood_info: KeyedVec<NogoodIndex, NogoodInfo>,
    /// The inference codes for the nogoods.
    inference_codes: KeyedVec<NogoodIndex, InferenceCode>,
    /// Nogoods which are permanently present
    permanent_nogood_ids: Vec<NogoodId>,
    /// Stores all learned nogoods.
    learned_nogood_ids: LearnedNogoodIds,
    /// Watch lists for the nogood propagator.
    watch_lists: KeyedVec<PredicateId, Vec<Watcher>>,
    /// Keep track of the events which the propagator has been notified of.
    updated_predicate_ids: Vec<PredicateId>,
    /// A helper for calculating the LBD for the nogoods.
    lbd_helper: Lbd,
    /// The parameters which influence the learning of the propagator and aspects such as clause
    /// management
    parameters: LearningOptions,
    /// The nogoods which have been bumped.
    bumped_nogoods: Vec<NogoodId>,
    /// Used to return lazy reasons
    temp_nogood_reason: Vec<Predicate>,
    /// The handle of this instance inside the solver.
    ///
    /// Used during nogood cleanup. A nogood can only be removed if it is not propagated in the
    /// current subtree. To test for that, we compare this handle with the propagator ID of a
    /// proapgated literal to see if this propagator propagated a predicate.
    #[allow(unused, reason = "Will be reintroduced with database management")]
    handle: PropagatorHandle<NogoodPropagator>,
    /// What form of propagation is performed (e.g., unit propagation, or extended nogood
    /// propagation).
    ///
    /// This, among other components, influences how watchers are placed.
    propagation_mode: PropagationMode,
    /// The statistics kept by the [`NogoodPropagator`].
    statistics: NogoodPropagatorStatistics,
    /// A [`SemanticMinimiser`] used for preprocessing nogoods when added to the database.
    semantic_minimiser: SemanticMinimiser,
    /// The priority of the nogood propagator.
    priority: Priority,
}

create_statistics_struct!(NogoodPropagatorStatistics {
    /// Records the number of unit propagations.
    num_unit_propagations: usize,
    /// Records the number of calls to the extended nogood propagation algorithm.
    num_extended_propagation_calls: usize,
    /// Records the number of variables propagated by extended nogood propagation.
    num_variables_propagated: usize,
    /// Records the number of lower-bounds propagated by extended nogood propagation.
    num_extended_lower_bound_propagations: usize,
    /// Records the number of upper-bounds propagated by extended nogood propagation.
    num_extended_upper_bound_propagations: usize,
    /// Records the number of disequalities propagated by extended nogood propagation.
    num_extended_disequality_propagations: usize,
    /// The average number of [`Predicate`]s describing the propagated domain when performing
    /// extended nogood propagation.
    average_num_predicates_describing_domain_when_propagating_extended: CumulativeMovingAverage<usize>
});

/// The information necessary to calculate the explanation for a propagation.
#[bitfield(u64)]
struct LazyNogoodExplanation {
    /// The [`NogoodId`] of the nogood which caused the propagation.
    #[bits(32)]
    nogood_id: NogoodId,
    /// Whether extended nogood propagation takes place; if this is the case, then a different
    /// explanation than when using unit propagation is generated.
    #[bits(1)]
    explains_extended_propagation: bool,
    /// Whether the extended nogood propagation was a unit propagation.
    #[bits(1)]
    is_extended_unit_propagation: bool,
    /// When extended nogood propagation performs unit propagation, the lazy explanation expects
    /// the propagated atomic constraint at the 0-th index. However, this invariant is not
    /// maintained when performing extended nogood propagation, so this index is used to indicate
    /// the [`PredicateId`] of the propagating atomic constraint.
    #[bits(30)]
    unit_propagation_index: u32,
}

/// [`PropagatorConstructor`] for constructing a new instance of the [`NogoodPropagator`] with the
/// provided [`LearningOptions`] and `capacity`.
pub(crate) struct NogoodPropagatorConstructor {
    /// How many [`PredicateId`]s to preallocate to the [`ArenaAllocator`].
    capacity: usize,
    parameters: LearningOptions,
    propagation_mode: PropagationMode,
    priority: Priority,
}

impl NogoodPropagatorConstructor {
    pub(crate) fn new(
        capacity: usize,
        parameters: LearningOptions,
        propagation_mode: PropagationMode,
        priority: Priority,
    ) -> Self {
        Self {
            capacity,
            parameters,
            propagation_mode,
            priority,
        }
    }
}

impl PropagatorConstructor for NogoodPropagatorConstructor {
    type PropagatorImpl = NogoodPropagator;

    fn create(self, context: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let propagator = NogoodPropagator {
            statistics: NogoodPropagatorStatistics::default(),
            handle: PropagatorHandle::new(context.propagator_id),
            parameters: self.parameters,
            nogood_predicates: ArenaAllocator::new(self.capacity),
            nogood_info: Default::default(),
            inference_codes: Default::default(),
            permanent_nogood_ids: Default::default(),
            learned_nogood_ids: Default::default(),
            watch_lists: Default::default(),
            updated_predicate_ids: Default::default(),
            lbd_helper: Default::default(),
            bumped_nogoods: Default::default(),
            temp_nogood_reason: Default::default(),
            propagation_mode: self.propagation_mode,
            semantic_minimiser: Default::default(),
            priority: self.priority,
        };

        PropagatorSpec {
            registration: EventsToRegister::empty(),
            checkers: RuntimeCheckers::empty(),
            propagator,
        }
    }
}

/// Watcher for a single nogood.
///
/// A watcher is a combination of a nogood ID and a cached predicate. If the nogood has a predicate
/// that is observed to be `false`, it will be made the cached predicate. That way, whenever the
/// watcher is triggered, the propagator may be able to quickly determine if the nogood can be
/// skipped by looking at the cached predicate.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Watcher {
    pub(crate) nogood_id: NogoodId,
    pub(crate) cached_predicate: PredicateId,
}

/// Keeps track of three tiers of nogoods:
/// - "low" LBD nogoods
/// - "mid" LBD nogoods
/// - "high" LBD nogoods
///
/// In general, the lower the LBD the better the nogood.
///
/// See the [`LearningOptions`] for the paramters which determine what tier to assign each nogood
/// to.
#[derive(Default, Debug, Clone)]
struct LearnedNogoodIds {
    low_lbd: Vec<NogoodId>,
    mid_lbd: Vec<NogoodId>,
    high_lbd: Vec<NogoodId>,
}

impl NogoodPropagator {
    /// Replace the watcher at `watcher_to_replace` with `i`.
    ///
    /// Note that this method does not remove any watchers but only adds a watcher to
    /// `watcher_to_replace`.
    fn replace_watcher(
        context: &mut PropagationContext<'_>,
        watcher: Watcher,
        nogood_predicates: &mut [PredicateId],
        i: usize,
        watcher_to_replace: usize,
        watch_lists: &mut KeyedVec<PredicateId, Vec<Watcher>>,
    ) {
        // Replace the current watcher with the new predicate watcher.
        nogood_predicates.swap(watcher_to_replace, i);
        // Add this nogood to the watch list of the new watcher.
        Self::add_watcher(
            context,
            nogood_predicates[watcher_to_replace],
            watcher,
            watch_lists,
        );
    }

    /// Removes the provided `watcher` in the watchlist of `predicate_id`.
    ///
    /// Note that this method removes the watcher by iterating through the watchers of
    /// `predicate_id` and finding the one that matches it nogood id. If the index of the watcher
    /// in the watchlist of `predicate_id` is known, then this method should not be used.
    fn remove_watcher(
        context: &mut PropagationContext<'_>,
        watcher: Watcher,
        predicate_id: PredicateId,
        watch_lists: &mut KeyedVec<PredicateId, Vec<Watcher>>,
    ) {
        let index_in_zeroth_watchlist = watch_lists[predicate_id]
            .iter()
            .position(|other_watcher| other_watcher.nogood_id == watcher.nogood_id)
            .expect("Expected to be able to retrieve watcher");
        let _ = watch_lists[predicate_id].swap_remove(index_in_zeroth_watchlist);

        if watch_lists[predicate_id].is_empty() {
            context.unregister_predicate(predicate_id);
        }
    }
}

impl Propagator for NogoodPropagator {
    fn name(&self) -> &str {
        // It is important to keep this name exactly this.
        // In parts of code for debugging, it looks for this particular name.
        "NogoodPropagator"
    }

    fn priority(&self) -> Priority {
        self.priority
    }

    fn notify_predicate_id_satisfied(
        &mut self,
        _: NotificationContext,
        predicate_id: PredicateId,
    ) -> EnqueueDecision {
        self.updated_predicate_ids.push(predicate_id);
        EnqueueDecision::Enqueue
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    #[allow(
        clippy::filter_map_bool_then,
        reason = "Will run into borrow issues otherwise"
    )]
    fn propagate(&mut self, mut context: PropagationContext) -> Result<(), Conflict> {
        pumpkin_assert_moderate!(self.debug_is_properly_watched());

        // First we perform nogood management to ensure that the database does not grow excessively
        // large with "bad" nogoods
        self.clean_up_learned_nogoods_if_needed(
            context.assignments,
            context.reason_store,
            context.notification_engine,
        );

        if self.watch_lists.len() <= context.num_predicate_ids() {
            self.watch_lists
                .resize(context.num_predicate_ids() + 1, Vec::default());
        }
        for predicate_id in self.updated_predicate_ids.drain(..) {
            pumpkin_assert_moderate!(
                {
                    let predicate = context.get_predicate(predicate_id);
                    context.evaluate_predicate(predicate) == Some(true)
                },
                "The predicate {} with id {predicate_id:?} should be satisfied but was not",
                context.get_predicate(predicate_id),
            );
            let mut index = 0;
            while index < self.watch_lists[predicate_id].len() {
                let watcher = self.watch_lists[predicate_id][index];
                // We first check whether the cached predicate might already make the nogood
                // satisfied
                if context.is_predicate_id_falsified(watcher.cached_predicate) {
                    index += 1;
                    continue;
                }
                let calculate_range_of_nogood = self
                    .nogood_predicates
                    .calculate_range_of_nogood(watcher.nogood_id);
                let nogood_predicates =
                    &mut self.nogood_predicates.nogoods[calculate_range_of_nogood];

                // Place the watched predicate at position 1 for simplicity.
                if nogood_predicates[0] == predicate_id {
                    nogood_predicates.swap(0, 1);
                }

                pumpkin_assert_moderate!(context.is_predicate_id_satisfied(nogood_predicates[1]));

                // Check the other watched predicate is already falsified, in which case
                // no propagation can take place. Recall that the other watched
                // predicate is at position 0 due to previous code.
                if context.is_predicate_id_falsified(nogood_predicates[0]) {
                    self.watch_lists[predicate_id][index].cached_predicate = nogood_predicates[0];
                    index += 1;
                    continue;
                }

                // We start updating the watchers.
                //
                // Look for another nonsatisfied predicate to replace the watched predicate.
                let mut found_new_watch = false;

                // In the case of extended nogood propagation, we need to track more information.
                //
                // If there is a falsified predicate over the same variable as the 0th
                // predicate, then we need to replace it.
                //
                // Similarly, we need to keep the invariant for lazy unit propagation explanations
                // that the propagated predicate is at the 0th position. Hence, if we are looking
                // for a new predicate over a different variable but find an unassinged predicate
                // over the same variable, then we replace it (this Boolean ensures that we only do
                // that once).
                let mut falsified_zeroth = None;

                // Start from index 2 since we are skipping watched predicates.
                for i in 2..nogood_predicates.len() {
                    // We process the watcher based on the analysis mode that we are in
                    match self.propagation_mode.process_potential_watcher(
                        &mut context,
                        nogood_predicates,
                        i,
                    ) {
                        WatcherProcessingStatus::Continue => continue,
                        WatcherProcessingStatus::FoundNewWatch => {
                            // Found another predicate that can be the watcher.
                            found_new_watch = true;

                            Self::replace_watcher(
                                &mut context,
                                watcher,
                                nogood_predicates,
                                i,
                                1,
                                &mut self.watch_lists,
                            );

                            // No propagation is taking place, go to the next nogood.
                            break;
                        }
                        WatcherProcessingStatus::FalsifiedZeroth => {
                            // We have found a predicate which reasons over the same
                            // variable as the 0-th predicate *and* is falsified
                            //
                            // We swap the two predicates and mark that the current
                            // nogood id should be removed from the watchlist of the 0-th
                            // predicate (before swapping)
                            falsified_zeroth = Some(nogood_predicates[0]);

                            Self::replace_watcher(
                                &mut context,
                                watcher,
                                nogood_predicates,
                                i,
                                0,
                                &mut self.watch_lists,
                            );

                            // Note that we do not break, since we still want to find a new
                            // watcher for the other predicate
                        }
                    }
                }

                if found_new_watch {
                    // We remove the current watcher
                    let _ = self.watch_lists[predicate_id].swap_remove(index);

                    if self.watch_lists[predicate_id].is_empty() {
                        context.unregister_predicate(predicate_id);
                    }
                }
                if let Some(to_remove) = falsified_zeroth {
                    // We have replaced `to_remove` with a predicate that has been
                    // falsified; we now remove this nogood from the watchlist of
                    // `to_remove`
                    Self::remove_watcher(&mut context, watcher, to_remove, &mut self.watch_lists);
                }
                if found_new_watch || falsified_zeroth.is_some() {
                    // We have either found a new watcher, or we have found a falsified
                    // predicate; no propagation can take place in either case, so we
                    // continue
                    pumpkin_assert_moderate!(nogood_predicates.iter().skip(2).all(
                        |predicate_id| {
                            !self.watch_lists[predicate_id]
                                .iter()
                                .any(|other_watcher| watcher.nogood_id == other_watcher.nogood_id)
                        }
                    ),);
                    continue;
                }

                // Now we perform the propagation
                let nogood_index = self
                    .nogood_predicates
                    .nogood_id_to_index
                    .get(&watcher.nogood_id)
                    .expect("Expected nogood predicate to exist");
                self.propagation_mode.perform_propagation(
                    &mut context,
                    nogood_predicates,
                    &self.inference_codes[nogood_index],
                    watcher.nogood_id,
                    &mut self.statistics,
                )?;

                index += 1;
            }
        }

        pumpkin_assert_moderate!(self.debug_is_properly_watched());

        Ok(())
    }

    fn synchronise(&mut self, _context: NotificationContext<'_>) {
        self.updated_predicate_ids.clear()
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> Result<(), Conflict> {
        // Very inefficient version!

        // The algorithm goes through every nogood explicitly
        // and computes from scratch.
        for nogood_id in self.nogood_predicates.nogoods_ids() {
            self.debug_propagate_nogood_from_scratch(nogood_id, &mut context)?;
        }
        Ok(())
    }

    /// Returns the slice representing a conjunction of predicates that explain the propagation
    /// encoded by the code, which was given to the solver by the propagator at the time of
    /// propagation.
    ///
    /// In case of the noogood propagator, lazy explanations internally also update information
    /// about the LBD and activity of the nogood, which is used when cleaning up nogoods.
    fn lazy_explanation(
        &mut self,
        code: u64,
        mut context: ExplanationContext,
    ) -> LazyExplanation<'_> {
        let reason = LazyNogoodExplanation::from_bits(code);
        let id = reason.nogood_id();
        let result = if reason.explains_extended_propagation() {
            // The lazy explanations explains a propagation using extended nogood propagation.
            let nogood = &self.nogood_predicates[id];
            let info_id = self.nogood_predicates.get_nogood_index(&id);

            // We retrieve the predicate which is being explained.
            let predicate_to_be_explained = context.get_predicate_to_be_explained();
            let rhs = predicate_to_be_explained.get_right_hand_side();
            let propagated_domain = predicate_to_be_explained.get_domain();

            if reason.is_extended_unit_propagation() {
                let propagating_predicate_id =
                    PredicateId::create_from_index(reason.unit_propagation_index() as usize);

                self.temp_nogood_reason = self.nogood_predicates[id]
                    .iter()
                    .filter(|&&predicate_id| predicate_id != propagating_predicate_id)
                    .map(|&predicate_id| context.get_predicate(predicate_id))
                    .collect::<Vec<_>>();
            } else {
                match predicate_to_be_explained.get_predicate_type() {
                    PredicateType::UpperBound => {
                        self.temp_nogood_reason = nogood
                            .iter()
                            .filter_map(|predicate_id| {
                                let predicate = context.get_predicate(*predicate_id);

                                (context.evaluate_predicate_at_trail_position(
                                    predicate,
                                    context.get_trail_position(),
                                ) == Some(true)
                                    && (predicate.get_domain() != propagated_domain
                                        || predicate.is_upper_bound_predicate()
                                        || (predicate.is_not_equal_predicate()
                                            && predicate.get_right_hand_side() > rhs)))
                                    .then_some(predicate)
                            })
                            .collect();
                    }
                    PredicateType::LowerBound => {
                        self.temp_nogood_reason = nogood
                            .iter()
                            .filter_map(|predicate_id| {
                                let predicate = context.get_predicate(*predicate_id);

                                (context.evaluate_predicate_at_trail_position(
                                    predicate,
                                    context.get_trail_position(),
                                ) == Some(true)
                                    && (predicate.get_domain() != propagated_domain
                                        || predicate.is_lower_bound_predicate()
                                        || (predicate.is_not_equal_predicate()
                                            && predicate.get_right_hand_side() < rhs)))
                                    .then_some(predicate)
                            })
                            .collect();
                    }
                    PredicateType::NotEqual => {
                        self.temp_nogood_reason = nogood
                            .iter()
                            .filter_map(|predicate_id| {
                                let predicate = context.get_predicate(*predicate_id);

                                (predicate.get_domain() != propagated_domain).then_some(predicate)
                            })
                            .collect();
                    }
                    PredicateType::Equal => unreachable!(),
                }
            }

            LazyExplanation {
                predicates: &self.temp_nogood_reason,
                inference_code: self.inference_codes[info_id].clone(),
            }
        } else {
            self.temp_nogood_reason = self.nogood_predicates[id][1..]
                .iter()
                .map(|predicate_id| context.get_predicate(*predicate_id))
                .collect::<Vec<_>>();

            let info_id = self.nogood_predicates.get_nogood_index(&id);

            LazyExplanation {
                predicates: self.temp_nogood_reason.as_slice(),
                inference_code: self.inference_codes[info_id].clone(),
            }
        };

        let info_id = self.nogood_predicates.get_nogood_index(&id);

        // Update the LBD and activity of the nogood, if appropriate.
        //
        // Note that low lbd nogoods are kept permanently, so these are not updated.
        if !self.nogood_info[info_id].block_bumps
            && self.nogood_info[info_id].is_learned
            && self.nogood_info[info_id].lbd > self.parameters.lbd_threshold_low
        {
            self.nogood_info[info_id].block_bumps = true;
            self.bumped_nogoods.push(id);
            // Note that we do not need to take into account the propagated predicate (in
            // position zero), since it will share a decision level with one of
            // the other predicates (if it did not then it should have
            // propagated earlier).
            let current_lbd = self
                .lbd_helper
                .compute_lbd(&self.temp_nogood_reason, &context);

            // The nogood keeps track of the best lbd encountered.
            if current_lbd < self.nogood_info[info_id].lbd {
                self.nogood_info[info_id].lbd = current_lbd;
            }

            // Nogood activity update.
            //
            // Rescale the nogood activity if bumping would lead to a (too) large activity
            // value.
            if self.nogood_info[info_id].activity + self.parameters.activity_bump_increment
                > self.parameters.max_activity
            {
                // Rescale the activity of the "mid" and "high" LBD learned nogoods (recall that
                // "low" LBD nogoods do not have their LBD scaled).
                //
                // TODO: we could consider having separate activity bump values for each tier,
                // so that we can do rescaling only within the same tier.
                // This would lead to less rescaling, and anyway we are (probably) only
                // interested in the relative order of nogoods within a
                // tier.
                self.learned_nogood_ids
                    .high_lbd
                    .iter()
                    .chain(self.learned_nogood_ids.mid_lbd.iter())
                    .for_each(|i| {
                        let i = self.nogood_predicates.get_nogood_index(i);
                        self.nogood_info[i].activity /= self.parameters.max_activity;
                    });
                self.parameters.activity_bump_increment /= self.parameters.max_activity;
            }

            // At this point, it is safe to increase the activity value
            self.nogood_info[info_id].activity += self.parameters.activity_bump_increment;
        }

        result
    }
}

/// Functions for adding nogoods
impl NogoodPropagator {
    /// Propagates a nogood using "extended" reasoning.
    ///
    /// If the nogood only contains unassigned predicates over a single variable, then the nogood
    /// can be seen as a domain description of that variable.
    ///
    /// We assume that the nogood has been semantically minimised beforehand.
    ///
    /// For example, let's say that we have the nogood:
    /// `[x >= 6] /\ [x <= 15] /\ [x != 12] /\ ... -> false`
    /// For this nogood to be satisfied, we can see that [x <= 5] \/ [x >= 16] \/ [x == 12].
    /// Based on this reasoning, we can remove the values {6, 7, 8, 9, 10, 11, 13, 14, 15} from the
    /// domain of `x`.
    #[allow(
        clippy::filter_map_bool_then,
        reason = "Otherwise leads to borrow issues."
    )]
    pub(crate) fn extended_nogood_propagation(
        context: &mut PropagationContext,
        nogood: &[PredicateId],
        propagated_domain: DomainId,
        inference_code: &InferenceCode,
        statistics: &mut NogoodPropagatorStatistics,
        nogood_id: Option<NogoodId>,
    ) -> Result<(), Conflict> {
        statistics.num_extended_propagation_calls += 1;

        let (
            exceptions,
            lower_bound,
            upper_bound,
            num_describing_domain,
            last_describing_predicate_id,
            is_falsified,
        ) = get_domain_info(context, nogood, propagated_domain);

        if is_falsified {
            // The nogood is already falsified
            return Ok(());
        }

        if num_describing_domain == 0 {
            // We could not find another watcher (i.e., all predicats over other variables are
            // satisfied). However, we could not find a predicate in the nogood which is over
            // `propgating_domain` *and* unsatisfied.
            //
            // This means that there is a conflict which should be reported.
            let reason = nogood
                .iter()
                .filter_map(|predicate_id| {
                    (context.is_predicate_id_satisfied(*predicate_id))
                        .then(|| context.get_predicate(*predicate_id))
                })
                .collect::<PropositionalConjunction>();

            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction: reason,
                inference_code: inference_code.clone(),
            }));
        }

        statistics
            .average_num_predicates_describing_domain_when_propagating_extended
            .add_term(num_describing_domain);

        // We perform the standard unit propagation if possible
        //
        // TODO: Could use a lazy explanation here
        if num_describing_domain == 1 {
            statistics.num_unit_propagations += 1;

            let last_describing_predicate = context.get_predicate(last_describing_predicate_id);

            let reason = if let Some(nogood_id) = nogood_id {
                Reason::DynamicLazy(
                    LazyNogoodExplanation::new()
                        .with_nogood_id(nogood_id)
                        .with_explains_extended_propagation(true)
                        .with_is_extended_unit_propagation(true)
                        .with_unit_propagation_index(last_describing_predicate_id.id)
                        .into(),
                )
            } else {
                (
                    nogood
                        .iter()
                        .filter_map(|predicate_id| {
                            (context.is_predicate_id_satisfied(*predicate_id))
                                .then(|| context.get_predicate(*predicate_id))
                        })
                        .collect::<PropositionalConjunction>(),
                    inference_code,
                )
                    .into()
            };

            return context
                .post(!last_describing_predicate, reason)
                .map_err(|e| e.into());
        }

        // Now we get the minimum and maximum exception values for propagation
        let (min_exception, max_exception) =
            exceptions
                .iter()
                .fold((None, None), |(min_exception, max_exception), exception| {
                    (
                        min_exception.map_or_else(
                            || Some(*exception),
                            |min_exception: i32| Some(min_exception.min(*exception)),
                        ),
                        max_exception.map_or_else(
                            || Some(*exception),
                            |max_exception: i32| Some(max_exception.max(*exception)),
                        ),
                    )
                });

        // Now we propagate our nogood
        //
        // We store the bound of the lower-bound predicate in `lb` e.g., if we have the
        // predicate [x >= 5], then we store 5
        //
        // If there is no such predicate in the nogood then there must be one or more holes, and we
        // store the minimum value of this.
        let lb = lower_bound.map_or_else(
            || min_exception.unwrap(),
            |predicate| predicate.get_right_hand_side(),
        );

        // We store the bound of the upper-bound predicate in `ub`; e.g., if we have the
        // predicate [x <= 10], then we store 10
        //
        // If there is no such predicate in the nogood then there must be one or more holes, and we
        // store the maximum value of this.
        let ub = upper_bound.map_or_else(
            || max_exception.unwrap(),
            |predicate| predicate.get_right_hand_side(),
        );
        pumpkin_assert_simple!(lb <= ub);

        // We keep track of whether propagation took place.
        let mut propagated = false;

        // Now we check whether we can do any bound propagation
        if upper_bound.is_none() {
            pumpkin_assert_simple!(
                exceptions.len() > 1
                    || (!exceptions.is_empty()
                        && lower_bound.is_some()
                        && ub > lower_bound.unwrap().get_right_hand_side()),
            );
            // First, if there is no upper-bound predicate ([x <= v]), then we can propagate the
            // upper-bound based on the lower-bound predicate and/or the inequality predicates (note
            // that there must be one or more holes due to semantic minimisation and unit
            // propagation taking place previously).
            //
            // For example, if we have the predicates [x >= 5] /\ [x != 10], then we know that it
            // should either hold that [x <= 4] or [x = 10]. In either case, we know that [x <=
            // 10].
            //
            // Thus, we calculate the new maximum value as either the maximum value of the
            // inequality predicates (note that it can never be the case that there is an inequality
            // predicate with a value lower than the lower-bound predicate due to semantic
            // minimisation).

            if ub < context.upper_bound(&propagated_domain) {
                propagated = true;

                // The reason consists of:
                // 1) All predicates which reason over a different domain than the propagated
                //    predicate (which are all guaranteed to be satisfied)
                // 2) All predicates which reason over the same domain as the propagated predicate,
                //    but are *not* disequality predicates with a right-hand side larger than the
                //    max exception
                //
                //    For example, if we have the nogood [x >= 5] /\ [x != 10] /\ [x != 7] /\ [x !=
                // 15],    where the first two predicates are unassigned but the
                // last two are    satisfied; then the fact that [x != 7] is true,
                // does not matter for    the propagation of [x <= 10] to hold, but
                // [x != 15] is required in the    explanation
                statistics.num_extended_upper_bound_propagations += 1;
                let reason = if let Some(nogood_id) = nogood_id {
                    Reason::DynamicLazy(
                        LazyNogoodExplanation::new()
                            .with_nogood_id(nogood_id)
                            .with_explains_extended_propagation(true)
                            .into(),
                    )
                } else {
                    (
                        nogood
                            .iter()
                            .filter_map(|predicate_id| {
                                let predicate = context.get_predicate(*predicate_id);

                                (context.is_predicate_id_satisfied(*predicate_id)
                                    && (predicate.get_domain() != propagated_domain
                                        || predicate.is_upper_bound_predicate()
                                        || (predicate.is_not_equal_predicate()
                                            && predicate.get_right_hand_side() > ub)))
                                    .then_some(predicate)
                            })
                            .collect::<PropositionalConjunction>(),
                        inference_code,
                    )
                        .into()
                };

                let result = context.post(predicate!(propagated_domain <= ub), reason);

                if result.is_err() {
                    statistics.num_variables_propagated += 1;
                }
                result?
            }
        }

        if lower_bound.is_none() {
            // First, if there is no lower-bound predicate ([x >= v]), then we can propagate the
            // lower-bound based on the upper-bound predicate and/or the inequality predicates (note
            // that there must be one or more holes due to semantic minimisation and unit
            // propagation taking place previously).
            //
            // For example, if we have the predicates [x <= 15] /\ [x != 10], then we know that it
            // should either hold that [x >= 16] or [x = 10]. In either case, we know that [x >= 10]
            //
            // Thus, we calculate the new minimum value as the minimum value of the
            // inequality predicates (note that it can never be the case that there is an inequality
            // predicate with a value higher than the upper-bound predicate due to
            // semantic minimisation).
            if lb > context.lower_bound(&propagated_domain) {
                propagated = true;

                // The reason consists of:
                // 1) All predicates which reason over a different domain than the propagated
                //    predicate (which are all guaranteed to be satisfied)
                // 2) All predicates which reason over the same domain as the propagated predicate,
                //    but are *not* disequality predicates with a right-hand side smaller than the
                //    min exception
                //
                //    For example, if we have the nogood [x <= 15] /\ [x != 7] /\ [x != 10] /\ [x !=
                //    5], where the first two predicates are unassigned but the
                //    last two are satisfied; then the fact that [x != 10] is true,
                //    does not matter for the propagation of [x >= 7] to hold, but
                //    [x != 5] is required in the explanation
                statistics.num_extended_lower_bound_propagations += 1;
                let reason = if let Some(nogood_id) = nogood_id {
                    Reason::DynamicLazy(
                        LazyNogoodExplanation::new()
                            .with_nogood_id(nogood_id)
                            .with_explains_extended_propagation(true)
                            .into(),
                    )
                } else {
                    (
                        nogood
                            .iter()
                            .filter_map(|predicate_id| {
                                let predicate = context.get_predicate(*predicate_id);

                                (context.is_predicate_id_satisfied(*predicate_id)
                                    && (predicate.get_domain() != propagated_domain
                                        || predicate.is_lower_bound_predicate()
                                        || (predicate.is_not_equal_predicate()
                                            && predicate.get_right_hand_side() < lb)))
                                    .then_some(predicate)
                            })
                            .collect::<PropositionalConjunction>(),
                        inference_code,
                    )
                        .into()
                };

                let result = context.post(predicate!(propagated_domain >= lb), reason);

                if result.is_err() {
                    statistics.num_variables_propagated += 1;
                }
                result?
            }
        }

        // Now we still need to create the holes in the domain which are infeasible.
        //
        // Let's look at some scenarios:
        //
        // We have all three types of predicates; for example, [x >= 5] /\ [x <= 15] /\ [x !=
        // 10]. In this case we should remove all values between [5, 15] with the exception of 10.
        //
        // We have two types of predicates; for example, [x >= 5] /\ [x != 10]. In this case, we
        // have previously propagated [x <= 10] and we need to remove the range [5, 9].
        // Another example is the case where [x >= 5] /\ [x <= 15]. In this case, we have not
        // previously propagated anything and we need to remove the range [5, 15].
        //
        // We only have one type of predicate (necessarily inequalities due to semantic
        // minimisation); for example, [x != 10] /\ [x != 12]. In this case, we have previous
        // propagated [x >= 10] and [x <= 12] and we need to remove 11 from the domain.
        //
        //
        // In all of these scenarios, we need to determine the range over which to iterate by
        // looking at the values which should be removed after updating the bounds (where the new
        // bounds are stored in `min_range` and `max_range`).
        //
        // We do this by traversing the following range:
        // - If the lower-bound was propagated, then we use the new value as the lower-bound of our
        //   range.
        //
        //   If it was not (i.e. because [x <= v] is in the nogood), then we use either the
        //   right-hand side of the lower-bound predicate or the lower-bound of the variable as the
        //   lower-bound of the range to start iterating over.
        // - If the upper-bound was propagated, then we use the new value as the upper-bound of our
        //   range.
        //
        //   If it was not (i.e. because [x >= v] is in the nogood), then we use either the
        //   right-hand side of the upper-bound predicate or the upper-bound of the variable as the
        //   upper-bound of the range to start iterating over.
        //
        //
        // Hence, we iterate over the range that we have created, removing the values while *not*
        // removing the values for which there is an inequality predicate.

        // The reason consists of:
        // 1) All predicates which reason over a different domain than the propagated predicate
        //    (which are all guaranteed to be satisfied)
        for value_in_domain in lb..=ub {
            if !exceptions.contains(&value_in_domain)
                && context.contains(&propagated_domain, value_in_domain)
            {
                propagated = true;
                statistics.num_extended_disequality_propagations += 1;
                let reason = if let Some(nogood_id) = nogood_id {
                    Reason::DynamicLazy(
                        LazyNogoodExplanation::new()
                            .with_nogood_id(nogood_id)
                            .with_explains_extended_propagation(true)
                            .into(),
                    )
                } else {
                    (
                        nogood
                            .iter()
                            .filter_map(|predicate_id| {
                                let predicate = context.get_predicate(*predicate_id);

                                (predicate.get_domain() != propagated_domain).then_some(predicate)
                            })
                            .collect::<PropositionalConjunction>(),
                        inference_code,
                    )
                        .into()
                };
                let result = context.post(predicate!(propagated_domain != value_in_domain), reason);
                if result.is_err() {
                    statistics.num_variables_propagated += 1;
                }
                result?
            }
        }

        if propagated {
            statistics.num_variables_propagated += 1;
        }

        Ok(())
    }

    /// Adds a nogood which has been learned during search.
    ///
    /// The first predicate should be asserting and the second predicate should contain the
    /// predicate with the next highest decision level.
    pub(crate) fn add_asserting_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        inference_code: InferenceCode,
        context: &mut PropagationContext,
    ) {
        if self
            .propagation_mode
            .can_be_added_as_permanent(context, &nogood)
        {
            self.add_permanent_nogood(nogood, inference_code, context)
                .expect("Unit learned nogoods cannot fail.");
            return;
        }

        let lbd = self
            .propagation_mode
            .calculate_lbd(context, &nogood, &mut self.lbd_helper);

        let nogood = nogood
            .iter()
            .map(|predicate| context.get_id(*predicate))
            .collect::<Vec<_>>();

        // Add the nogood to the database.
        //
        // Currently we always allocate a fresh ID
        let nogood_id = self.nogood_predicates.insert(nogood);
        let _ = self
            .nogood_info
            .push(NogoodInfo::new_learned_nogood_info(lbd));
        let _ = self.inference_codes.push(inference_code);

        let watcher = Watcher {
            nogood_id,
            cached_predicate: self.nogood_predicates[nogood_id][0],
        };

        // Now we add two watchers to the first two predicates in the nogood; we are
        // guaranteed that these are different predicates
        NogoodPropagator::add_watcher(
            context,
            self.nogood_predicates[nogood_id][0],
            watcher,
            &mut self.watch_lists,
        );
        NogoodPropagator::add_watcher(
            context,
            self.nogood_predicates[nogood_id][1],
            watcher,
            &mut self.watch_lists,
        );

        let inference_code =
            &self.inference_codes[self.nogood_predicates.get_nogood_index(&nogood_id)];

        self.propagation_mode
            .perform_propagation(
                context,
                &self.nogood_predicates[nogood_id],
                inference_code,
                nogood_id,
                &mut self.statistics,
            )
            .expect("Asserting nogood cannot fail");

        // We then assign the nogood to the correct tier based on its LBD
        if lbd >= self.parameters.lbd_threshold_high {
            self.learned_nogood_ids.high_lbd.push(nogood_id);
        } else if lbd <= self.parameters.lbd_threshold_low {
            self.learned_nogood_ids.low_lbd.push(nogood_id);
        } else {
            self.learned_nogood_ids.mid_lbd.push(nogood_id);
        }
    }

    /// Adds a nogood to the propagator as a permanent nogood and sets the internal state to be
    /// infeasible if the nogood led to a conflict.
    pub(crate) fn add_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        inference_code: InferenceCode,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        self.add_permanent_nogood(nogood, inference_code, context)
    }

    /// Adds a nogood which cannot be deleted by clause management.
    fn add_permanent_nogood(
        &mut self,
        mut nogood: Vec<Predicate>,
        inference_code: InferenceCode,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        pumpkin_assert_simple!(
            context.get_checkpoint() == 0,
            "Only allowed to add nogoods permanently at the root for now."
        );

        // If the nogood is empty then it is automatically satisfied (though it is unusual!)
        if nogood.is_empty() {
            warn!("Adding empty nogood, unusual!");
            return Ok(());
        }

        // After preprocessing the nogood may propagate. If that happens, there is no reason for
        // the propagation which breaks the proof logging. Therefore, we keep the original nogood
        // here so we can construct a reason for the propagation later.
        let mut input_nogood = nogood.clone();

        // Then we pre-process the nogood such that (among others) it does not contain duplicates
        Self::preprocess_nogood(&mut nogood, context, &mut self.semantic_minimiser);

        // Unit nogoods are added as root assignments rather than as nogoods.
        if nogood.len() == 1 {
            // Get the reason for the propagation. Note that preprocessing removes literals from
            // `nogood` that are still present in `input_nogood`, so this does not necessarily
            // result in an empty reason.
            input_nogood = input_nogood
                .iter()
                .filter_map(|&p| {
                    // There is a special case we need to consider;
                    //
                    // Imagine we have the following input_nogood: [x = 1] /\ [y != 3] -> false
                    // Now we preprocess this to the nogood: [x <= 1] -> false (since [x >= 1] is
                    // a root-level bound)
                    //
                    // The reason for [x >= 2] should then be [x >= 1] /\ [y != 3] -> false
                    //
                    // Thus we check the following criteria before removal:
                    // 1. The current predicate and the propagating predicate are not the same
                    // 2. The current predicate is an equality predicate
                    // 3. The propagating predicate is either a lower-bound or and upper-bound
                    //    predicate
                    // 4. The right-hand side of the current predicate and the propagating predicate
                    //    are the same
                    // If all of these conditions hold then we need to replace the current
                    // predicate with:
                    // - If the propagating predicate is a lower-bound predicate then it is
                    // replaced with an upper-bound predicate
                    // - If the propagating predicate is an upper-bound predicate then it is
                    // replaced with a lower-bound predicate
                    if p != nogood[0]
                        && p.is_equality_predicate()
                        && p.get_domain() == nogood[0].get_domain()
                        && (nogood[0].is_lower_bound_predicate()
                            || nogood[0].is_upper_bound_predicate())
                        && p.get_right_hand_side() == nogood[0].get_right_hand_side()
                    {
                        let domain = p.get_domain();
                        let rhs = p.get_right_hand_side();
                        if nogood[0].is_lower_bound_predicate() {
                            Some(predicate!(domain <= rhs))
                        } else if nogood[0].is_upper_bound_predicate() {
                            Some(predicate!(domain >= rhs))
                        } else {
                            unreachable!()
                        }
                    } else {
                        // Otherwise, we just check whether they are not the same, if they are not
                        // then keep the predicate
                        (p != nogood[0]).then_some(p)
                    }
                })
                .collect::<Vec<_>>();

            pumpkin_assert_extreme!(
                nogood[0] == Predicate::trivially_false()
                    || input_nogood
                        .iter()
                        .all(|predicate| context.assignments.is_predicate_satisfied(*predicate)),
                "Expected every element in {input_nogood:?} to be satisfied when propagating {:?} with preprocessed nogood {nogood:?}",
                !nogood[0]
            );

            // Post the negated predicate at the root to respect the nogood.
            context.post(
                !nogood[0],
                (
                    PropositionalConjunction::from(input_nogood),
                    &inference_code,
                ),
            )?;
            Ok(())
        }
        // Standard case, nogood is of size at least two.
        //
        // The preprocessing ensures that all predicates are unassigned.
        else {
            self.propagation_mode.add_permanent_nogood_non_unit(
                nogood,
                &input_nogood,
                inference_code,
                context,
                &mut self.nogood_predicates,
                &mut self.nogood_info,
                &mut self.inference_codes,
                &mut self.watch_lists,
                &mut self.permanent_nogood_ids,
                &mut self.statistics,
            )
        }
    }
}

/// Returns the following information about the unsatisfied predicates concerning the provided
/// domain:
/// 1. The values disequality values which are present in the nogood.
/// 2. The lower-bound predicate, if present.
/// 3. The upper-bound predicate, if present
/// 4. The number of elements describing the domain.
/// 5. The index of the last processed predicate of the domain.
/// 6. Whether any predicates in the nogood are satisfied.
fn get_domain_info(
    context: &mut PropagationContext<'_>,
    nogood: &[PredicateId],
    propagated_domain: DomainId,
) -> (
    HashSet<i32>,
    Option<Predicate>,
    Option<Predicate>,
    usize,
    PredicateId,
    bool,
) {
    // We keep track of the holes in the domains which are posted; these can be seen as
    // "exceptions" to the removals of the domain
    let mut exceptions: HashSet<i32> = Default::default();

    // We need to keep track of whether there is a lower-bound and/or upper-bound predicate in
    // the nogood.
    let mut lower_bound = None;
    let mut upper_bound = None;

    let mut num_describing_domain = 0;
    let mut last_describing_predicate_id = PredicateId::create_from_index(u32::MAX as usize);
    let mut is_falsified = false;

    for predicate_id in nogood.iter().copied() {
        let predicate = context.get_predicate(predicate_id);

        is_falsified |= context.is_predicate_id_falsified(predicate_id);

        pumpkin_assert_moderate!(
            predicate.get_domain() == propagated_domain
                || context.is_predicate_id_satisfied(predicate_id),
            "Expected {predicate} to be unassigned with {propagated_domain:?}",
        );

        // First, we filter out all of the predicates which are currently satisfied and
        // which are not concerning the propagated domain id
        if context.is_predicate_id_satisfied(predicate_id)
            || predicate.get_domain() != propagated_domain
        {
            continue;
        }

        // Then we add the information of the predicates to our structures
        num_describing_domain += 1;
        last_describing_predicate_id = predicate_id;
        match predicate.get_predicate_type() {
            PredicateType::UpperBound => {
                pumpkin_assert_simple!(upper_bound.is_none());
                upper_bound = Some(predicate)
            }
            PredicateType::LowerBound => {
                pumpkin_assert_simple!(lower_bound.is_none());
                lower_bound = Some(predicate)
            }
            PredicateType::NotEqual => {
                let _ = exceptions.insert(predicate.get_right_hand_side());
            }
            PredicateType::Equal => {}
        }
    }
    (
        exceptions,
        lower_bound,
        upper_bound,
        num_describing_domain,
        last_describing_predicate_id,
        is_falsified,
    )
}

/// Methods concerning the watchers and watch lists
impl NogoodPropagator {
    /// Adds a watcher to the predicate.
    pub(crate) fn add_watcher(
        context: &mut PropagationContext,
        predicate: PredicateId,
        watcher: Watcher,
        watch_lists: &mut KeyedVec<PredicateId, Vec<Watcher>>,
    ) {
        // First we resize the watch list to accommodate the new nogood
        if predicate.id as usize >= watch_lists.len() {
            watch_lists.resize((predicate.id + 1) as usize, Vec::default());
        }

        if watch_lists[predicate].is_empty() {
            let actual_predicate = context.get_predicate(predicate);
            let other_id = context.register_predicate(actual_predicate);
            pumpkin_assert_eq_simple!(predicate, other_id);
        }

        watch_lists[predicate].push(watcher);
    }
}

/// Nogood management
impl NogoodPropagator {
    /// Removes learned nogoods if there are too many learned nogoods based on the three-tiered
    /// clause system from \[1\] and \[2\] (with a sligh change).
    ///
    /// The removal process is as follows:
    /// - Learned nogoods are partitioned into three categories: high-, mid-, and low-LBD nogoods
    ///   where each tier has a predefined limit on the number of nogoods it may store (see
    ///   [`LearningOptions`]).
    /// - If a tier has more nogoods than the limit prescribes, roughly half of the nogoods from the
    ///   tier are removed. High- and mid-tier nogoods remove the least active nogoods, whereas
    ///   low-tier nogoods are removed based on lbd and size.
    ///
    /// Nogoods can move from higher tiers to lower tiers if the LBD drops sufficiently low, but
    /// not the other way around. This is the main difference compared to \[2\].
    ///
    /// # Bibliography
    /// - \[1\] Oh, C. (2016). Improving SAT solvers by exploiting empirical characteristics of
    ///   CDCL. New York University.
    /// - \[2\] Kochemazov, S. (2020). Improving implementation of SAT competitions 2017--2019
    ///   winners. International Conference on Theory and Applications of Satisfiability Testing,
    ///   139–148. Springer.
    fn clean_up_learned_nogoods_if_needed(
        &mut self,
        assignments: &Assignments,
        reason_store: &mut ReasonStore,
        notification_engine: &mut NotificationEngine,
    ) {
        // The clean-up procedure is divided into four stages (for simplicity of implementation).
        //
        // For each tier, if the number of nogoods exceeds the predefined threshold for that tier:
        //  1. Promote nogoods in the "mid"- and "high" LBD tiers that have achieved a sufficiently
        //     low LBD to the next tier.
        //  2. Sort the nogoods according to a criteria.
        //    - Criteria for high- and mid-lbd nogoods: activity (higher activity better)
        //    - Criteria for low-lbd nogoods: LBD, and tie-break on size (lower LBD and size better)
        //  3. Remove the "worst" half of the nogoods, skipping nogoods which are currently
        //     propagating. This only removes the nogood IDs from their respective tier.
        //  4. Finally, after all of the previous stages, remove deleted nogoods from the watchers.
        //
        // Note: in other works, instead of deleting nogoods, they get demoted to the previous tier.
        // The rational for our choice is that demoting many nogoods will likely trigger clean up
        // of the previous tier, and demoted nogoods have low activities, so they would be targets
        // for deletion anyway.
        //
        // TODO: check whether this is the case.

        // We keep track of whether at least one of the nogoods has been removed in the third
        // stage
        //
        // If at least one nogood has been removed then we need to update the watchers as well
        let mut removed_at_least_one_nogood = false;

        // Process high-lbd nogoods.
        if self.learned_nogood_ids.high_lbd.len() > self.parameters.max_num_high_lbd_nogoods {
            self.promote_high_lbd_nogoods();

            // Sort the "high" LBD nogood by activity
            NogoodPropagator::sort_nogoods_by_decreasing_activity(
                &mut self.learned_nogood_ids.high_lbd,
                &self.nogood_info,
                &self.nogood_predicates,
            );

            // Then we remove roughly the worst half of the "high" LBD tier nogood IDs
            removed_at_least_one_nogood |= NogoodPropagator::remove_roughly_worst_half_nogood_ids(
                self.handle,
                &mut self.learned_nogood_ids.high_lbd,
                &mut self.nogood_info,
                &self.nogood_predicates,
                assignments,
                reason_store,
                notification_engine,
                self.propagation_mode,
            );
        }

        // Process mid-lbd nogoods.
        if self.learned_nogood_ids.mid_lbd.len() > self.parameters.max_num_mid_lbd_nogoods {
            self.promote_mid_lbd_nogoods();

            // Sort the "mid" LBD nogood by activity
            NogoodPropagator::sort_nogoods_by_decreasing_activity(
                &mut self.learned_nogood_ids.mid_lbd,
                &self.nogood_info,
                &self.nogood_predicates,
            );

            // Then we remove roughly the worst half of the "mid" LBD tier nogood IDs
            removed_at_least_one_nogood |= NogoodPropagator::remove_roughly_worst_half_nogood_ids(
                self.handle,
                &mut self.learned_nogood_ids.mid_lbd,
                &mut self.nogood_info,
                &self.nogood_predicates,
                assignments,
                reason_store,
                notification_engine,
                self.propagation_mode,
            );
        }

        // Process low-lbd nogoods.
        if self.learned_nogood_ids.low_lbd.len() > self.parameters.max_num_low_lbd_nogoods {
            // Sort the "low" LBD nogood by LBD while tie-breaking based on size
            NogoodPropagator::sort_nogoods_by_increasing_lbd_and_size(
                &mut self.learned_nogood_ids.low_lbd,
                &self.nogood_predicates,
                &self.nogood_info,
            );

            // Then we remove roughly the worst half of the "low" LBD tier nogood IDs
            removed_at_least_one_nogood |= NogoodPropagator::remove_roughly_worst_half_nogood_ids(
                self.handle,
                &mut self.learned_nogood_ids.low_lbd,
                &mut self.nogood_info,
                &self.nogood_predicates,
                assignments,
                reason_store,
                notification_engine,
                self.propagation_mode,
            );
        }

        if removed_at_least_one_nogood {
            self.remove_deleted_nogoods_from_watchers(assignments, notification_engine);
        }
    }

    fn has_a_watched_predicate_falsified_at_root_level(
        nogood: &[PredicateId],
        assignments: &Assignments,
        notification_engine: &mut NotificationEngine,
    ) -> bool {
        let watcher1 = notification_engine.get_predicate(nogood[0]);
        let watcher2 = notification_engine.get_predicate(nogood[1]);
        assignments.is_predicate_falsified(watcher1)
            && assignments
                .get_checkpoint_for_predicate(&!watcher1)
                .expect("Falsified predicates must have a decision level.")
                == 0
            || assignments.is_predicate_falsified(watcher2)
                && assignments
                    .get_checkpoint_for_predicate(&!watcher2)
                    .expect("Falsified predicates must have a decision level.")
                    == 0
    }

    /// Remove deleted nogoods from watchers.
    ///
    /// As an additional step, it attempts to determine whether certain nogoods are satisfied at
    /// the root level meaning that they can be ignored. Note that not all such cases are detected
    /// by this function.
    ///
    /// Note: the function is implemented as going through all watchers. If we only store few
    /// learned nogoods, but have many permanent nogoods, then this function will be called often
    /// and may be a bottleneck.
    fn remove_deleted_nogoods_from_watchers(
        &mut self,
        assignments: &Assignments,
        notification_engine: &mut NotificationEngine,
    ) {
        // The idea is to go through the watchers and remove watchers that contain deleted nogoods.
        //
        // On the way, if we detect that a nogood is trivially falsified at the root level, or if
        // its cached predicate is falsified at the root, then it is also deleted and removed from
        // the watchers.

        // While going through the watchers, we maintain a flag that signals if the process below
        // will require another pass. This can happen when the cached predicate is falsified
        // at the root level.
        //
        // Recall that each nogood has two watchers; if we encounter a watcher where the cached
        // predicate is falsified at the root, we mark it as deleted. However, we could have
        // encountered the other watcher (with a different cached predicate) attached to
        // this nogood prior to encountering this watcher with a falsified predicate; to
        // ensure that we delete the other watcher as well, we require another pass.
        //
        // We expect this situation to occur infrequently.
        let mut another_pass_needed = false;

        // We also track if we have deleted a trivial nogood, because afterwards we need to also
        // delete this nogood from its correpsonding tier.
        //
        // As above, we expect to rarely happen, so most of the time, this flag will stay false.
        let mut trivial_nogood_deleted = false;

        // We now go over all of the watchers
        for i in 0..self.watch_lists.len() {
            let index = PredicateId::create_from_index(i);
            self.watch_lists[index].retain(|watcher| {
                let info_index = self.nogood_predicates.get_nogood_index(&watcher.nogood_id);
                // If the nogood has been deleted, do not keep this watcher
                if self.nogood_info[info_index].is_deleted {
                    false
                } else if NogoodPropagator::has_a_watched_predicate_falsified_at_root_level(
                    &self.nogood_predicates[watcher.nogood_id],
                    assignments,
                    notification_engine,
                ) {
                    // If the nogood is falsified at the root level, mark the nogood
                    // for deletion, and do not keep this watcher.
                    self.nogood_info[info_index].is_deleted = true;
                    trivial_nogood_deleted = true;
                    false
                }
                // We check whether the cached predicate is falsified at the root level
                else if notification_engine
                    .is_predicate_id_falsified(watcher.cached_predicate, assignments)
                    && assignments
                        .get_checkpoint_for_predicate(
                            &!notification_engine.get_predicate(watcher.cached_predicate),
                        )
                        .expect("Falsified predicates must have a decision level.")
                        == 0
                {
                    // Mark that a nogood has been deleted due to the cached predicate.
                    another_pass_needed = true;
                    trivial_nogood_deleted = true;
                    self.nogood_info[info_index].is_deleted = true;
                    false
                } else {
                    true
                }
            });
        }

        // If another pass is needed, then we go over all of the watch lists and ensure that the
        // watchers which have been deleted in the previous step are also removed.
        if another_pass_needed {
            for i in 0..self.watch_lists.len() {
                let index = PredicateId::create_from_index(i);
                self.watch_lists[index].retain(|watcher| {
                    let info_index = self.nogood_predicates.get_nogood_index(&watcher.nogood_id);
                    // If the nogood has been deleted, do not keep this watcher
                    !self.nogood_info[info_index].is_deleted
                });
            }
        }

        // We have deleted a trivial nogood; we need to ensure that it is also removed from the
        // structures to ensure that it does not clutter up the tier.
        if trivial_nogood_deleted {
            self.learned_nogood_ids.high_lbd.retain(|nogood_id| {
                !self.nogood_info[self.nogood_predicates.get_nogood_index(nogood_id)].is_deleted
            });

            self.learned_nogood_ids.mid_lbd.retain(|nogood_id| {
                !self.nogood_info[self.nogood_predicates.get_nogood_index(nogood_id)].is_deleted
            });

            self.learned_nogood_ids.low_lbd.retain(|nogood_id| {
                !self.nogood_info[self.nogood_predicates.get_nogood_index(nogood_id)].is_deleted
            });
        }
    }

    // Attempts to remove the worst half of the provided `nogood_ids` and returns true if at least
    // one nogood has been removed.
    //
    // It is assumed that the provided `nogood_ids` is sorted based on the criterion where
    // `nogood_ids[0]` contains the nogood with the "best" value for the criterion.
    //
    // A nogood is not removed if it is currently propagating at a non-root level.
    // This means that the function may remove some nogoods from the first half if
    // some of the bottom nogoods are currently propagating.
    #[allow(
        clippy::too_many_arguments,
        reason = "Will run into borrow-issues when passing it by itself"
    )]
    fn remove_roughly_worst_half_nogood_ids(
        handle: PropagatorHandle<NogoodPropagator>,
        nogood_ids: &mut Vec<NogoodId>,
        nogood_info: &mut KeyedVec<NogoodIndex, NogoodInfo>,
        nogoods: &ArenaAllocator,
        assignments: &Assignments,
        reason_store: &mut ReasonStore,
        notification_engine: &mut NotificationEngine,
        propagation_mode: PropagationMode,
    ) -> bool {
        // The removal is done in two phases.
        // 1. Nogoods are deleted in the database, but the IDs are not removed from `nogood_ids`.
        // 2. The corresponding IDs are removed from the `nogood_ids`.
        //
        // Recall that these deleted nogoods are not yet removed from the watch lists.

        // First we calculate how many nogoods to remove (at least one)
        let mut num_nogoods_to_remove = max(nogood_ids.len() / 2, 1);

        // Then we go over all of the nogoods; recall that the "worst" nogood IDs are stored at the
        // end of `nogood_ids` (hence the rev).
        //
        // The aim is to remove half of the nogoods but fewer could be removed if many nogoods are
        // currently propagating.
        for &id in nogood_ids.iter().rev() {
            if num_nogoods_to_remove == 0 {
                // We are done removing nogoods.
                break;
            }

            // Skip nogoods which are propagating at a non-root level.
            if propagation_mode.is_nogood_propagating(
                handle,
                &nogoods[id],
                assignments,
                reason_store,
                id,
                notification_engine,
            ) && (matches!(propagation_mode, PropagationMode::ExtendedNogoodPropagation)
                || assignments
                    .get_checkpoint_for_predicate(
                        &!notification_engine.get_predicate(nogoods[id][0]),
                    )
                    .expect("A propagating predicate must have a decision level.")
                    > 0)
            {
                continue;
            }

            // We can now delete the nogood.
            //
            // It will be kept in the database for now but it will not be used for propagation
            // since its watchers will be removed in the next step.
            nogood_info[nogoods.get_nogood_index(&id)].is_deleted = true;

            num_nogoods_to_remove -= 1;
        }

        // We remove the nogood from the provided `nogood_ids`.
        //
        // Note that this does not remove it from either the nogood database or the watchers!
        let num_nogoods_before_removal = nogood_ids.len();
        nogood_ids.retain(|&id| !nogood_info[nogoods.get_nogood_index(&id)].is_deleted);
        let num_nogoods_after_removal = nogood_ids.len();

        num_nogoods_before_removal != num_nogoods_after_removal
    }

    /// Goes through all of the "high" LBD nogoods and promotes nogoods which have been updated to
    /// either the "low" LBD or "mid" LBD tier.
    fn promote_high_lbd_nogoods(&mut self) {
        self.learned_nogood_ids.high_lbd.retain(|id| {
            let info_index = self.nogood_predicates.get_nogood_index(id);
            if self.nogood_info[info_index].lbd >= self.parameters.lbd_threshold_high {
                // If the LBD is still high, the nogood stays in the high LBD category.
                true
            } else if self.nogood_info[info_index].lbd <= self.parameters.lbd_threshold_low {
                // If the LBD is low then the nogood is moved to the low LBD category
                self.learned_nogood_ids.low_lbd.push(*id);
                false
            } else {
                // Otherwise, if has neither high nor low LBD, the nogood is placed in the mid LBD
                // category
                self.learned_nogood_ids.mid_lbd.push(*id);
                false
            }
        })
    }

    /// Goes through all of the "high" LBD nogoods and promotes nogoods which have been updated to
    /// the "low" LBD tier.
    ///
    /// A "mid" LBD nogood cannot be moved to the high LBD tier.
    fn promote_mid_lbd_nogoods(&mut self) {
        self.learned_nogood_ids.mid_lbd.retain(|id| {
            let info_index = self.nogood_predicates.get_nogood_index(id);
            if self.nogood_info[info_index].lbd > self.parameters.lbd_threshold_low {
                // If the LBD is still mid, the nogood stays in the mid LBD category.
                //
                // Note that we do not move it to the high LBD tier.
                true
            } else {
                // If the LBD is low then the nogood is moved to the low LBD category
                self.learned_nogood_ids.low_lbd.push(*id);
                false
            }
        })
    }

    fn sort_nogoods_by_decreasing_activity(
        nogood_ids: &mut [NogoodId],
        nogood_info: &KeyedVec<NogoodIndex, NogoodInfo>,
        nogood_predicates: &ArenaAllocator,
    ) {
        nogood_ids.sort_unstable_by(|id1, id2| {
            let nogood1 = &nogood_info[nogood_predicates.get_nogood_index(id1)];
            let nogood2 = &nogood_info[nogood_predicates.get_nogood_index(id2)];
            // Notice that nogood2 goes first in the next line (and not nogood1) to ensure that it
            // is sorted decreasingly.
            nogood2.activity.partial_cmp(&nogood1.activity).unwrap()
        });
    }

    /// Sorts the provided nogoods non-decreasingly based on LBD and tie-breaks based on the size
    /// of the nogoods (giving preference to smaller nogoods).
    fn sort_nogoods_by_increasing_lbd_and_size(
        nogood_ids: &mut [NogoodId],
        nogoods: &ArenaAllocator,
        nogood_info: &KeyedVec<NogoodIndex, NogoodInfo>,
    ) {
        nogood_ids.sort_unstable_by(|&id1, &id2| {
            let lbd1 = nogood_info[nogoods.get_nogood_index(&id1)].lbd;
            let lbd2 = nogood_info[nogoods.get_nogood_index(&id2)].lbd;

            if lbd1 != lbd2 {
                // Recall that lower LBD is better.
                lbd1.cmp(&lbd2)
            } else {
                // As a tie-breaker, a smaller nogoods is better.
                //
                // TODO: currently we do not remove true predicates from
                // nogoods, so calling len() might not be accurate.
                let size1 = nogoods[id1].len();
                let size2 = nogoods[id2].len();
                size1.cmp(&size2)
            }
        });
    }

    /// Decays the activity bump increment by
    /// [`LearningOptions::self.parameters.activity_decay_factor`].
    pub(crate) fn decay_nogood_activities(&mut self) {
        self.parameters.activity_bump_increment /= self.parameters.activity_decay_factor;
        for &id in &self.bumped_nogoods {
            let info_id = self.nogood_predicates.get_nogood_index(&id);
            self.nogood_info[info_id].block_bumps = false;
        }
        self.bumped_nogoods.clear();
    }
}

impl NogoodPropagator {
    /// Does simple preprocessing, modifying the input nogood by:
    ///     1. Removing duplicate predicates.
    ///     2. Removing satisfied predicates at the root.
    ///     3. Detecting predicates falsified at the root. In that case, the nogood is preprocessed
    ///        to the empty nogood.
    ///     4. Conflicting predicates?
    fn preprocess_nogood(
        nogood: &mut Vec<Predicate>,
        context: &mut PropagationContext,
        semantic_minimiser: &mut SemanticMinimiser,
    ) {
        pumpkin_assert_simple!(context.get_checkpoint() == 0);
        // The code below is broken down into several parts

        // We opt for semantic minimisation upfront. This way we avoid the possibility of having
        // assigned predicates in the final nogood. This could happen since the root bound can
        // change since the initial time the semantic minimiser recorded it, so it would not know
        // that a previously nonroot bound is now actually a root bound.
        semantic_minimiser.minimise_internal(context.assignments(), nogood);

        // We assume that duplicate predicates have been removed

        // Check if the nogood cannot be violated, i.e., it has a falsified predicate.
        if nogood.is_empty()
            || nogood
                .iter()
                .any(|p| context.evaluate_predicate(*p) == Some(false))
        {
            *nogood = vec![Predicate::trivially_false()];
            return;
        }

        // Remove predicates that are satisfied at the root level.
        nogood.retain(|p| context.evaluate_predicate(*p) != Some(true));

        // If the nogood is violating at the root, the previous retain would leave an empty nogood.
        // Return a violating nogood.
        if nogood.is_empty() {
            *nogood = vec![Predicate::trivially_true()];
        }

        // Done with preprocessing, the result is stored in the input nogood.
    }
}

/// Debug methods
impl NogoodPropagator {
    fn debug_propagate_nogood_from_scratch(
        &self,
        nogood_id: NogoodId,
        context: &mut PropagationContext,
    ) -> Result<(), Conflict> {
        // This is an inefficient implementation for testing purposes
        let nogood = &self.nogood_predicates[nogood_id];
        let info_id = self.nogood_predicates.get_nogood_index(&nogood_id);
        let inference_code = &self.inference_codes[info_id];

        if self.nogood_info[info_id].is_deleted {
            // The nogood has already been deleted, meaning that it could be that the call to
            // `propagate` would not find any propagations using it due to the watchers being
            // deleted
            return Ok(());
        }

        // First we get the number of falsified predicates
        let has_falsified_predicate = nogood.iter().any(|predicate| {
            let predicate = context.get_predicate(*predicate);
            context.evaluate_predicate(predicate) == Some(false)
        });

        // If at least one predicate is false, then the nogood can be skipped
        if has_falsified_predicate {
            return Ok(());
        }

        match self.propagation_mode {
            PropagationMode::ExtendedNogoodPropagation => {
                // We find all of the unasssigned predicates and get their domains
                //
                // If there is a falsified predicate then we do not propagate; also, if
                // the nogood can be unit propagated, then
                // we do not propagate
                let mut is_falsified = false;
                let mut num_unassigned = 0;
                let unassigned_predicate_ids = nogood
                    .iter()
                    .filter_map(|predicate_id| {
                        if context.is_predicate_id_falsified(*predicate_id) {
                            is_falsified = true;
                            None
                        } else if context.is_predicate_id_satisfied(*predicate_id) {
                            None
                        } else {
                            num_unassigned += 1;
                            let predicate = context.get_predicate(*predicate_id);
                            Some(predicate.get_domain())
                        }
                    })
                    .collect::<HashSet<_>>();
                if num_unassigned > 1 && !is_falsified && unassigned_predicate_ids.len() == 1 {
                    NogoodPropagator::extended_nogood_propagation(
                        context,
                        nogood,
                        *unassigned_predicate_ids.iter().next().unwrap(),
                        inference_code,
                        &mut NogoodPropagatorStatistics::default(),
                        Some(nogood_id),
                    )?;
                }
            }
            PropagationMode::UnitPropagation => {}
        }

        let num_satisfied_predicates = nogood
            .iter()
            .filter(|predicate| {
                let predicate = context.get_predicate(**predicate);
                context.evaluate_predicate(predicate) == Some(true)
            })
            .count();

        let nogood_len = nogood.len();

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood_len {
            return Err(PropagatorConflict {
                conjunction: nogood
                    .iter()
                    .map(|predicate_id| context.get_predicate(*predicate_id))
                    .collect::<PropositionalConjunction>(),
                inference_code: inference_code.clone(),
            }
            .into());
        }
        // If all but one predicate are satisfied, then we can propagate.
        //
        // Note that this only makes sense since we know that there are no falsifying predicates at
        // this point.
        else if num_satisfied_predicates == nogood_len - 1 {
            // Note that we negate the remaining unassigned predicate!
            let propagated_predicate = nogood
                .iter()
                .find_map(|predicate_id| {
                    let predicate = context.get_predicate(*predicate_id);

                    context
                        .evaluate_predicate(predicate)
                        .is_none()
                        .then_some(predicate)
                })
                .unwrap()
                .not();

            assert!(
                nogood
                    .iter()
                    .any(|p| context.get_predicate(*p) == propagated_predicate.not())
            );

            // Cannot use lazy explanations when propagating from scratch
            // since the propagated predicate may not be at position zero.
            // but we cannot change the nogood since this function is with nonmutable self.
            //
            // So an eager reason is constructed
            let reason = nogood
                .iter()
                .map(|&p| context.get_predicate(p))
                .filter(|&p| p != !propagated_predicate)
                .collect::<PropositionalConjunction>();

            context.post(propagated_predicate, (reason, inference_code))?;
        }
        Ok(())
    }

    /// Checks for each nogood whether the first two predicates in the nogood are being watched
    fn debug_is_properly_watched(&self) -> bool {
        let is_watching = |predicate_id: PredicateId, nogood_id: NogoodId| -> bool {
            self.watch_lists[predicate_id]
                .iter()
                .copied()
                .any(|watcher| watcher.nogood_id == nogood_id)
        };

        for nogood_id in self.nogood_predicates.nogoods_ids() {
            let nogood_predicates = &self.nogood_predicates[nogood_id];

            if self.nogood_info[self.nogood_predicates.get_nogood_index(&nogood_id)].is_deleted {
                // If the clause is deleted then it will have no watchers
                assert!(
                    !is_watching(nogood_predicates[0], nogood_id)
                        && !is_watching(nogood_predicates[1], nogood_id)
                );
                continue;
            }

            if !(is_watching(nogood_predicates[0], nogood_id)
                && is_watching(nogood_predicates[1], nogood_id))
            {
                eprintln!("Nogood id: {}", nogood_id.id);
                eprintln!("Nogood: {nogood_predicates:?}");
                eprintln!(
                    "watching 0: {}",
                    is_watching(nogood_predicates[0], nogood_id)
                );
                eprintln!(
                    "watching 1: {}",
                    is_watching(nogood_predicates[1], nogood_id)
                );
            }

            assert!(
                is_watching(nogood_predicates[0], nogood_id)
                    && is_watching(nogood_predicates[1], nogood_id)
            );
        }
        true
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use super::NogoodPropagator;
    use crate::conjunction;
    use crate::containers::StorageKey;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::proof::ConstraintTag;
    use crate::proof::InferenceCode;

    #[test]
    fn ternary_nogood_propagate() {
        let mut solver = TestSolver::default();
        let inference_code = InferenceCode::unknown_label(ConstraintTag::create_from_index(0));
        let dummy = solver.new_variable(0, 1);
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let id = solver.nogood_handle.propagator_id();

        let _ = solver.increase_lower_bound_and_notify(id, dummy.id(), dummy, 1);

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let (nogood_propagator, mut context) = solver
                .state
                .get_propagator_mut_with_context(solver.nogood_handle);
            let nogood_propagator: &mut NogoodPropagator = nogood_propagator.unwrap();

            nogood_propagator
                .add_nogood(nogood.into(), inference_code, &mut context)
                .unwrap();
        }

        let _ = solver.increase_lower_bound_and_notify(id, a.id(), a, 3);
        let _ = solver.increase_lower_bound_and_notify(id, b.id(), b, 0);

        solver.propagate_until_fixed_point(id).expect("");

        let _ = solver.increase_lower_bound_and_notify(id, c.id(), c, 15);

        solver.propagate(id).expect("");

        assert_eq!(solver.upper_bound(b), 0);

        let reason_lb = solver.get_reason_int(predicate!(b <= 0));
        assert_eq!(conjunction!([a >= 2] & [c >= 10]), reason_lb);
    }

    #[test]
    fn unsat() {
        let mut solver = TestSolver::default();
        let inference_code = InferenceCode::unknown_label(ConstraintTag::create_from_index(0));
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let id = solver.nogood_handle.propagator_id();

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let (nogood_propagator, mut context) = solver
                .state
                .get_propagator_mut_with_context(solver.nogood_handle);
            let nogood_propagator: &mut NogoodPropagator = nogood_propagator.unwrap();

            nogood_propagator
                .add_nogood(nogood.into(), inference_code, &mut context)
                .unwrap();
        }

        let _ = solver.increase_lower_bound_and_notify(id, a.id(), a, 3);
        let _ = solver.increase_lower_bound_and_notify(id, b.id(), b, 1);
        let _ = solver.increase_lower_bound_and_notify(id, c.id(), c, 15);

        let result = solver.propagate_until_fixed_point(id);
        assert!(result.is_err());
    }
}
