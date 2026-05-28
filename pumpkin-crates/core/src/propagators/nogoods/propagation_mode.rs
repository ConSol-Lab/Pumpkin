use crate::basic_types::PredicateId;
use crate::containers::HashSet;
use crate::containers::KeyedVec;
use crate::engine::Assignments;
use crate::engine::Lbd;
use crate::engine::Reason;
use crate::engine::notifications::NotificationEngine;
use crate::engine::reason::ReasonStore;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::propagation::PropagationContext;
use crate::propagation::ReadDomains;
use crate::propagators::nogoods::NogoodId;
use crate::propagators::nogoods::NogoodInfo;
use crate::propagators::nogoods::NogoodPropagator;
use crate::propagators::nogoods::NogoodPropagatorStatistics;
use crate::propagators::nogoods::Watcher;
use crate::propagators::nogoods::arena_allocator::ArenaAllocator;
use crate::propagators::nogoods::arena_allocator::NogoodIndex;
use crate::pumpkin_assert_moderate;
use crate::state::Conflict;
use crate::state::PropagationStatusCP;
use crate::state::PropagatorHandle;
use crate::variables::DomainId;

#[derive(Clone, Copy, Debug, Default)]
pub enum PropagationMode {
    #[default]
    UnitPropagation,
    ExtendedNogoodPropagation,
}

impl PropagationMode {
    /// Returns a [`WatcherProcessingStatus`] based on the [`Predicate`] pointed to by `index` in
    /// `nogood_predicates`.
    pub(crate) fn process_potential_watcher(
        &self,
        context: &mut PropagationContext,
        nogood_predicates: &[PredicateId],
        index: usize,
    ) -> WatcherProcessingStatus {
        match self {
            PropagationMode::ExtendedNogoodPropagation => {
                // In the case of CPIP nogoods, we split into cases dependent on whether the
                // predicate which we are processing reasons over the same domain.
                //
                // First, we store whether the current predicate reasons over the same domain as
                // the predicate for which we are finding a new watcher.
                let reasons_over_same_domain =
                    context.get_predicate(nogood_predicates[index]).get_domain()
                        == context.get_predicate(nogood_predicates[0]).get_domain();

                // Next we split into several cases depending on the states of the to process
                // predicate.
                match context.evaluate_predicate_id(nogood_predicates[index]) {
                    None => {
                        if !reasons_over_same_domain {
                            // If the predicate is unassigned and does not reason over the same
                            // domain as the 0-th predicate, then we have found a new watch.
                            WatcherProcessingStatus::FoundNewWatch
                        } else {
                            // Otherwise, we have found a new watcher, but, since it reasons over
                            // the same variable as the 0-th predicate, we need to re-use it.
                            //
                            // Note that we replace the 0-th predicate with the predicate we are
                            // currently processing to ensure that during unit propagation the 0-th
                            // predicate is the one being propagated.
                            WatcherProcessingStatus::FoundNewWatchButContinue
                        }
                    }
                    Some(false) => {
                        if !reasons_over_same_domain {
                            // If the predicate is falsified and does not reason over the same
                            // domain as the 0-th predicate, then we have found a new watch.
                            WatcherProcessingStatus::FoundNewWatch
                        } else {
                            // Otherwise, we have found a falsified predicate, and we need to
                            // update the zero-th predicate with this predicate.
                            WatcherProcessingStatus::FalsifiedZeroth
                        }
                    }
                    _ => WatcherProcessingStatus::Continue,
                }
            }
            PropagationMode::UnitPropagation => {
                // Standard case, we check whether the atomic constraint is not satisfied and
                // replace it if we have found such a predicate.
                if !context.is_predicate_id_satisfied(nogood_predicates[index]) {
                    WatcherProcessingStatus::FoundNewWatch
                } else {
                    WatcherProcessingStatus::Continue
                }
            }
        }
    }

    /// Computes from scratch whether extended nogood propagation can take place.
    pub fn can_perform_extended_nogood_propagation(
        &self,
        context: &mut PropagationContext,
        nogood_predicates: &[PredicateId],
    ) -> Option<DomainId> {
        // We find all of the unasssigned predicates and get their domains
        //
        // If there is a falsified predicate then we do not propagate; also,
        // if the nogood can be unit
        // propagated, then
        // we do not propagate
        let mut is_falsified = false;
        let mut num_unassigned = 0;
        let mut unassigned_domains = HashSet::new();
        for predicate_id in nogood_predicates.iter() {
            if context.is_predicate_id_falsified(*predicate_id) {
                is_falsified = true;
                break;
            } else if context.is_predicate_id_satisfied(*predicate_id) {
                continue;
            } else {
                num_unassigned += 1;
                let predicate = context.get_predicate(*predicate_id);
                let _ = unassigned_domains.insert(predicate.get_domain());
            }
        }

        (num_unassigned > 1 && !is_falsified && unassigned_domains.len() == 1)
            .then(|| *unassigned_domains.iter().next().unwrap())
    }

    /// Performs unit propagation or extended nogood propagation depending on what types of nogoods
    /// are being learned.
    ///
    /// Note that this method does *not* check whether the propagation conditions have been met.
    pub(crate) fn perform_propagation(
        &self,
        context: &mut PropagationContext,
        nogood_predicates: &[PredicateId],
        inference_code: &InferenceCode,
        nogood_id: NogoodId,
        statistics: &mut NogoodPropagatorStatistics,
    ) -> PropagationStatusCP {
        match self {
            PropagationMode::ExtendedNogoodPropagation => {
                let propagated_domain = context.get_predicate(nogood_predicates[0]).get_domain();
                NogoodPropagator::propagate_extended_nogood(
                    context,
                    nogood_predicates,
                    propagated_domain,
                    inference_code,
                    statistics,
                    Some(nogood_id),
                )?;
            }
            PropagationMode::UnitPropagation => {
                statistics.num_unit_propagations += 1;

                // There are two scenarios:
                // nogood[0] is unassigned -> propagate the predicate to false
                // nogood[0] is assigned true -> conflict.
                let reason = Reason::DynamicLazy(nogood_id.id as u64);

                let predicate = !context.get_predicate(nogood_predicates[0]);
                let result = context.post(predicate, reason);
                // If the propagation lead to a conflict.
                if let Err(e) = result {
                    return Err(e.into());
                }
            }
        }

        Ok(())
    }

    /// Returns whether the provided `nogood` can be added as a permanent nogood (i.e., whether it
    /// would propagate at the root level).
    pub(crate) fn can_be_added_as_permanent(
        &self,
        context: &PropagationContext,
        nogood: &[Predicate],
    ) -> bool {
        // We treat unit nogoods in a special way by adding it as a permanent nogood at the
        // root-level; this is essentially the same as adding a predicate at the root level
        if nogood.len() == 1 {
            pumpkin_assert_moderate!(
                context.get_checkpoint() == 0,
                "A unit nogood should have backtracked to the root-level"
            );
            return true;
        }
        match self {
            PropagationMode::ExtendedNogoodPropagation => {
                // We maintain the invariant that the first two predicates in a learned clause
                // point to different variables; if this does not hold, then it is a "unit" nogood
                pumpkin_assert_moderate!(
                    context.get_checkpoint_for_predicate(nogood[1]).unwrap()
                        >= nogood
                            .iter()
                            .skip(2)
                            .filter(|predicate| predicate.get_domain() != nogood[0].get_domain())
                            .map(|predicate| context
                                .get_checkpoint_for_predicate(*predicate)
                                .unwrap())
                            .max()
                            .unwrap_or(0),
                );
                if nogood[0].get_domain() == nogood[1].get_domain() {
                    pumpkin_assert_moderate!(
                        context.get_checkpoint() == 0,
                        "A unit nogood should have backtracked to the root-level"
                    );
                    return true;
                }
            }
            PropagationMode::UnitPropagation => {}
        }

        false
    }

    /// Calculates the LBD.
    pub(crate) fn calculate_lbd(
        &self,
        context: &PropagationContext,
        nogood: &[Predicate],
        lbd_helper: &mut Lbd,
    ) -> u32 {
        match self {
            PropagationMode::ExtendedNogoodPropagation => lbd_helper.compute_lbd(
                &nogood
                    .iter()
                    .filter(|predicate| context.evaluate_predicate(**predicate).is_some())
                    .copied()
                    .collect::<Vec<_>>(),
                context,
            ),
            PropagationMode::UnitPropagation => {
                // Skip the zero-th predicate since it is unassigned,
                // but will be assigned at the level of the predicate at index one.
                lbd_helper.compute_lbd(&nogood[1..], context)
            }
        }
    }

    /// Determines whether the nogood (pointed to by `id`) is propagating using the following
    /// reasoning:
    ///
    /// - The predicate at position 0 is falsified; this is one of the conventions of the nogood
    ///   propagator
    /// - The reason for the predicate is the nogood propagator
    pub(crate) fn is_nogood_propagating(
        &self,
        handle: PropagatorHandle<NogoodPropagator>,
        nogood: &[PredicateId],
        assignments: &Assignments,
        reason_store: &ReasonStore,
        id: NogoodId,
        notification_engine: &mut NotificationEngine,
    ) -> bool {
        match self {
            PropagationMode::ExtendedNogoodPropagation => {
                let potential_domain = notification_engine.get_predicate(nogood[0]).get_domain();
                for predicate_id in nogood {
                    let predicate = notification_engine.get_predicate(*predicate_id);
                    if predicate.get_domain() == potential_domain {
                        continue;
                    }

                    if notification_engine.evaluate_predicate_id(*predicate_id, assignments)
                        != Some(true)
                    {
                        return false;
                    }
                }
                true
            }
            PropagationMode::UnitPropagation => {
                if notification_engine.is_predicate_id_falsified(nogood[0], assignments) {
                    let trail_position = assignments
                        .get_trail_position(&!notification_engine.get_predicate(nogood[0]))
                        .unwrap();
                    let trail_entry = assignments.get_trail_entry(trail_position);
                    if let Some(reason_ref) = trail_entry.reason {
                        let propagator_id = reason_store.get_propagator(reason_ref);
                        let code = reason_store.get_lazy_code(reason_ref);

                        // We check whether the predicate was propagated by the nogood propagator
                        // first
                        let propagated_by_nogood_propagator =
                            propagator_id == handle.propagator_id();
                        // Then we check whether the lazy reason for the propagation was this
                        // particular nogood
                        let code_matches_id = code.is_none() || *code.unwrap() == id.id as u64;
                        return propagated_by_nogood_propagator && code_matches_id;
                    }
                }
                false
            }
        }
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "Cannot take the nogood propagator; could be refactored in the future"
    )]
    pub(crate) fn add_permanent_nogood_non_unit(
        &mut self,
        nogood: Vec<Predicate>,
        inference_code: InferenceCode,
        context: &mut PropagationContext<'_>,
        nogood_predicates: &mut ArenaAllocator,
        nogood_info: &mut KeyedVec<NogoodIndex, NogoodInfo>,
        inference_codes: &mut KeyedVec<NogoodIndex, InferenceCode>,
        watch_lists: &mut KeyedVec<PredicateId, Vec<Watcher>>,
        permanent_nogood_ids: &mut Vec<NogoodId>,
        statistics: &mut NogoodPropagatorStatistics,
    ) -> Result<(), Conflict> {
        match self {
            PropagationMode::ExtendedNogoodPropagation => {
                // We try to find a predicate with a different domain than the 0-th predicate;
                // this is the invariant that we maintain for the watchers
                let other = nogood
                    .iter()
                    .position(|predicate| predicate.get_domain() != nogood[0].get_domain());

                let first_domain = nogood[0].get_domain();

                let mut nogood = nogood
                    .iter()
                    .map(|predicate| context.get_id(*predicate))
                    .collect::<Vec<_>>();

                if let Some(position) = other {
                    // If we can find predicate which reasons over a different domain than the
                    // 0th, then we proceed to add watchers
                    nogood.swap(1, position);

                    // Add the nogood to the database.
                    //
                    // Currently we always allocate a fresh ID
                    let nogood_id = nogood_predicates.insert(nogood);
                    let _ = nogood_info.push(NogoodInfo::new_permanent_nogood_info());
                    let _ = inference_codes.push(inference_code);

                    let watcher = Watcher {
                        nogood_id,
                        cached_predicate: nogood_predicates[nogood_id][0],
                    };

                    NogoodPropagator::add_watcher(
                        context,
                        nogood_predicates[nogood_id][0],
                        watcher,
                        watch_lists,
                    );

                    NogoodPropagator::add_watcher(
                        context,
                        nogood_predicates[nogood_id][1],
                        watcher,
                        watch_lists,
                    );

                    permanent_nogood_ids.push(nogood_id);

                    Ok(())
                } else {
                    // Otherwise, we treat it as a "unit" nogood and we perform propagation and
                    // then do not add the nogood to the database.

                    NogoodPropagator::propagate_extended_nogood(
                        context,
                        &nogood,
                        first_domain,
                        &inference_code,
                        statistics,
                        None,
                    )?;

                    Ok(())
                }
            }
            PropagationMode::UnitPropagation => {
                #[cfg(feature = "check-propagations")]
                let nogood = nogood
                    .iter()
                    .map(|predicate| context.get_id(*predicate))
                    .collect::<Vec<_>>();

                #[cfg(not(feature = "check-propagations"))]
                let nogood = nogood
                    .iter()
                    .map(|predicate| context.get_id(*predicate))
                    .collect::<Vec<_>>();

                // Add the nogood to the database.
                //
                // Currently we always allocate a fresh ID
                let nogood_id = nogood_predicates.insert(nogood);
                let _ = nogood_info.push(NogoodInfo::new_permanent_nogood_info());
                let _ = inference_codes.push(inference_code);

                permanent_nogood_ids.push(nogood_id);

                let watcher = Watcher {
                    nogood_id,
                    cached_predicate: nogood_predicates[nogood_id][0],
                };

                NogoodPropagator::add_watcher(
                    context,
                    nogood_predicates[nogood_id][0],
                    watcher,
                    watch_lists,
                );
                NogoodPropagator::add_watcher(
                    context,
                    nogood_predicates[nogood_id][1],
                    watcher,
                    watch_lists,
                );

                Ok(())
            }
        }
    }
}

/// The result of [`AnalysisMode::process_potential_watcher`] indicating what should happen to the
/// watchers of the nogood.
#[derive(Debug, Clone, Copy)]
pub(crate) enum WatcherProcessingStatus {
    /// No new watcher has been found, we should simply move to the next potential watcher.
    Continue,
    /// A new watcher has been found and it can replace the satisfied watcher.
    FoundNewWatch,
    /// **Only applicable when learning CPIP nogoods** - Indicates that a [`Predicate`] reasoning
    /// over the same variable as the other watcher (i.e., the watcher for which a new watcher
    /// is currently *not* being looked for) has been found which is falsified.
    ///
    /// This return value ensures that the watcher at index 0 is replaced with the currently
    /// processed predicate.
    FalsifiedZeroth,
    /// **Only applicable when learning CPIP nogoods** - Indicates that an unsatisfied [`Predicate`]
    /// has been found but that it reasons over the same variable as the other watcher.
    ///
    /// This return value ensures that the watcher at index 0 is replaced with the currently
    /// processed predicate to ensure that during unit propagation the propagating predicate is
    /// always placed at position 0.
    FoundNewWatchButContinue,
}
