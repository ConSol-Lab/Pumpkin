use std::ops::Not;

use log::warn;

use super::LearnedNogoodSortingStrategy;
use super::LearningOptions;
use super::NogoodId;
use super::NogoodWatchList;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::containers::KeyedVec;
use crate::engine::conflict_analysis::Mode;
use crate::engine::nogoods::Lbd;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::contexts::HasAssignments;
use crate::engine::propagation::contexts::StatefulPropagationContext;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::propagation::ReadDomains;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::EventSink;
use crate::engine::IntDomainEvent;
use crate::engine::SolverStatistics;
use crate::predicate;
use crate::propagators::nogoods::Nogood;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// A propagator which propagates nogoods (i.e. a list of [`Predicate`]s which cannot all be true
/// at the same time).
///
/// It should be noted that this propagator is notified about each event which occurs in the solver
/// (since the propagator does not know which IDs will be present in its learnt clauses).
///
/// The idea for propagation is the two-watcher scheme; this is achieved by internally keeping
/// track of watch lists.
#[derive(Clone, Debug, Default)]
pub(crate) struct NogoodPropagator {
    /// The list of currently stored nogoods
    nogoods: KeyedVec<NogoodId, Nogood>,
    /// Nogoods which are permanently present
    permanent_nogoods: Vec<NogoodId>,
    /// The ids of the nogoods sorted based on whether they have a "low" LBD score or a "high" LBD
    /// score.
    learned_nogood_ids: LearnedNogoodIds,
    /// Ids which have been deleted and can now be re-used
    delete_ids: Vec<NogoodId>,
    /// The trail index is used to determine the domains of the variables since last time.
    last_index_on_trail: usize,
    /// Watch lists for the nogood propagator.
    // TODO: could improve the data structure for watching.
    watch_lists: KeyedVec<DomainId, NogoodWatchList>,
    /// Keep track of the events which the propagator has been notified of.
    enqueued_updates: EventSink,
    /// A helper for calculating the LBD for the nogoods.
    lbd_helper: Lbd,
    /// The parameters which influence the learning of the propagator and aspects such as clause
    /// management
    parameters: LearningOptions,
    /// The nogoods which have been bumped.
    bumped_nogoods: Vec<NogoodId>,
}

/// A struct which keeps track of which nogoods are considered "high" LBD and which nogoods are
/// considered "low" LBD.
#[derive(Default, Debug, Clone)]
struct LearnedNogoodIds {
    low_lbd: Vec<NogoodId>,
    high_lbd: Vec<NogoodId>,
}

impl NogoodPropagator {
    pub(crate) fn with_options(parameters: LearningOptions) -> Self {
        Self {
            parameters,
            ..Default::default()
        }
    }

    /// Determines whether the nogood (pointed to by `id`) is propagating using the following
    /// reasoning:
    ///
    /// - The predicate at position 0 is falsified; this is one of the conventions of the nogood
    ///   propagator
    /// - The reason for the predicate is the nogood propagator
    fn is_nogood_propagating(
        &self,
        context: PropagationContext,
        reason_store: &ReasonStore,
        id: NogoodId,
    ) -> bool {
        if context.is_predicate_falsified(self.nogoods[id].predicates[0]) {
            let trail_position = context
                .assignments()
                .get_trail_position(&!self.nogoods[id].predicates[0])
                .unwrap();
            let trail_entry = context.assignments().get_trail_entry(trail_position);
            if let Some(reason_ref) = trail_entry.reason {
                let propagator_id = reason_store.get_propagator(reason_ref);
                let code = reason_store.get_lazy_code(reason_ref);

                // We check whether the predicate was propagated by the nogood propagator first
                let propagated_by_nogood_propagator =
                    propagator_id == ConstraintSatisfactionSolver::get_nogood_propagator_id();
                // Then we check whether the lazy reason for the propagation was this particular
                // nogood
                let code_matches_id = code.is_none() || *code.unwrap() == id.id as u64;
                return propagated_by_nogood_propagator && code_matches_id;
            }
        }
        false
    }
}

impl Propagator for NogoodPropagator {
    fn name(&self) -> &str {
        // It is important to keep this name exactly this.
        // In parts of code for debugging, it looks for this particular name.
        "NogoodPropagator"
    }

    fn priority(&self) -> u32 {
        0
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> Result<(), Inconsistency> {
        pumpkin_assert_advanced!(self.debug_is_properly_watched());

        // First we perform nogood management to ensure that the database does not grow excessively
        // large with "bad" nogoods
        self.clean_up_learned_nogoods_if_needed(context.as_readonly(), context.reason_store);

        if self.watch_lists.len() <= context.assignments().num_domains() as usize {
            self.watch_lists.resize(
                context.assignments().num_domains() as usize + 1,
                NogoodWatchList::default(),
            );
        }

        let old_trail_position = context.assignments.trail.len() - 1;

        for (domain_event, updated_domain_id) in self.enqueued_updates.drain() {
            let mut current_index = 0;
            let mut end_index = 0;

            match domain_event {
                IntDomainEvent::LowerBound => {
                    let old_lower_bound = context.lower_bound_at_trail_position(
                        &updated_domain_id,
                        self.last_index_on_trail,
                    );
                    let new_lower_bound = context.lower_bound(&updated_domain_id);

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let num_watchers =
                        self.watch_lists[updated_domain_id].num_lower_bound_watchers();
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self.watch_lists[updated_domain_id]
                            .get_lower_bound_watcher_at_index(current_index)
                            .right_hand_side;

                        if old_lower_bound < right_hand_side && right_hand_side <= new_lower_bound {
                            let nogood_id = self.watch_lists[updated_domain_id]
                                .get_lower_bound_watcher_at_index(current_index)
                                .nogood_id;

                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: Predicate| {
                                predicate.is_lower_bound_predicate()
                                    && predicate.get_domain() == updated_domain_id
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists[updated_domain_id]
                                    .set_lower_bound_watcher_to_other_watcher(
                                        end_index,
                                        current_index,
                                    );
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // todo: does it make sense to replace the cached predicate with
                                    // this new predicate?

                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == updated_domain_id
                                    );
                                    // Add this nogood to the watch list of the new watcher.
                                    Self::add_watcher(&mut self.watch_lists, nogood[1], nogood_id);

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.
                                current_index += 1;
                                continue;
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_lower_bound_watcher_to_other_watcher(end_index, current_index);
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy(nogood_id.id as u64);

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[updated_domain_id]
                                        .set_lower_bound_watcher_to_other_watcher(
                                            end_index,
                                            current_index,
                                        );

                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[updated_domain_id]
                                    .truncate_lower_bound_watchers(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_lower_bound_watcher_to_other_watcher(end_index, current_index);
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[updated_domain_id]
                            .truncate_lower_bound_watchers(end_index);
                    }
                }
                IntDomainEvent::UpperBound => {
                    let old_upper_bound = context.upper_bound_at_trail_position(
                        &updated_domain_id,
                        self.last_index_on_trail,
                    );
                    let new_upper_bound = context.upper_bound(&updated_domain_id);

                    // We are manually implementing a retain-like function from Vec.

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let num_watchers =
                        self.watch_lists[updated_domain_id].num_upper_bound_watchers();
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self.watch_lists[updated_domain_id]
                            .get_upper_bound_watcher_at_index(current_index)
                            .right_hand_side;

                        if old_upper_bound > right_hand_side && right_hand_side >= new_upper_bound {
                            let nogood_id = self.watch_lists[updated_domain_id]
                                .get_upper_bound_watcher_at_index(current_index)
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: Predicate| {
                                predicate.is_upper_bound_predicate()
                                    && predicate.get_domain() == updated_domain_id
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists[updated_domain_id]
                                    .set_upper_bound_watcher_to_other_watcher(
                                        end_index,
                                        current_index,
                                    );
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == updated_domain_id
                                    );
                                    // Add this nogood to the watch list of the new watcher.
                                    Self::add_watcher(&mut self.watch_lists, nogood[1], nogood_id);

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.

                                current_index += 1;
                                continue;
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_upper_bound_watcher_to_other_watcher(end_index, current_index);
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy(nogood_id.id as u64);

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[updated_domain_id]
                                        .set_upper_bound_watcher_to_other_watcher(
                                            end_index,
                                            current_index,
                                        );
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[updated_domain_id]
                                    .truncate_upper_bound_watchers(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_upper_bound_watcher_to_other_watcher(end_index, current_index);
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[updated_domain_id]
                            .truncate_upper_bound_watchers(end_index);
                    }
                }
                IntDomainEvent::Removal => {
                    let old_lower_bound = context.lower_bound_at_trail_position(
                        &updated_domain_id,
                        self.last_index_on_trail,
                    );
                    let new_lower_bound = context.lower_bound(&updated_domain_id);

                    let old_upper_bound = context.upper_bound_at_trail_position(
                        &updated_domain_id,
                        self.last_index_on_trail,
                    );
                    let new_upper_bound = context.upper_bound(&updated_domain_id);

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let num_watchers =
                        self.watch_lists[updated_domain_id].num_inequality_watchers();
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self.watch_lists[updated_domain_id]
                            .get_inequality_watcher_at_index(current_index)
                            .right_hand_side;

                        let update_domain = updated_domain_id;
                        // Only look at the watcher if:
                        // 1) The removed value was definitely removed due to bound changes, OR
                        // 2) The removed value is within the bounds, and was actually removed.
                        if old_upper_bound >= right_hand_side && right_hand_side > new_upper_bound
                            || old_lower_bound <= right_hand_side
                                && right_hand_side < new_lower_bound
                            || (new_lower_bound < right_hand_side
                                && right_hand_side < new_upper_bound
                                && context.is_predicate_satisfied(predicate!(
                                    update_domain != right_hand_side
                                )))
                        {
                            let nogood_id = self.watch_lists[updated_domain_id]
                                .get_inequality_watcher_at_index(current_index)
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: Predicate| {
                                predicate.is_not_equal_predicate()
                                    && predicate.get_domain() == updated_domain_id
                                    && predicate.get_right_hand_side() == right_hand_side
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists[updated_domain_id]
                                    .set_inequality_watcher_to_other_watcher(
                                        end_index,
                                        current_index,
                                    );
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // The watcher for holes has a special case. In case the watcher that is
                            // going to replace this one is 1) a predicate with the same
                            // domain_id and 2) is also a not equals predicate, then the watcher
                            // should not be moved from this list, but instead only its right hand
                            // side should be changed to reflect the new watcher. The variable
                            // 'kept_watcher_new_rhs' holds info about this new rhs if appropriate.
                            let mut kept_watcher_new_rhs: Option<i32> = None;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == updated_domain_id
                                    );

                                    // Add this nogood to the watch list of the new watcher. Note
                                    // that there
                                    if nogood[1].is_not_equal_predicate()
                                        && nogood[1].get_domain() == updated_domain_id
                                    {
                                        // The watcher should stay in this list, but change
                                        // its right hand side to reflect the new watching
                                        // predicate. Here we only note that the watcher
                                        // should stay, and later it actually gets copied.
                                        kept_watcher_new_rhs =
                                            Some(nogood[1].get_right_hand_side());
                                    } else {
                                        // Add this nogood to the watch list of the new watcher.
                                        Self::add_watcher(
                                            &mut self.watch_lists,
                                            nogood[1],
                                            nogood_id,
                                        );
                                    }

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                if let Some(new_rhs) = kept_watcher_new_rhs {
                                    // Keep the current watch for this predicate,
                                    // and update its right hand side.
                                    self.watch_lists[updated_domain_id]
                                        .set_inequality_watcher_to_other_watcher(
                                            end_index,
                                            current_index,
                                        );
                                    self.watch_lists[updated_domain_id]
                                        .set_right_hand_side_of_inequality_watcher_at_index(
                                            end_index, new_rhs,
                                        );

                                    end_index += 1;
                                    current_index += 1;

                                    continue;
                                } else {
                                    // Note this nogood is effectively removed from the watch list
                                    // of the the current predicate, since we
                                    // are only incrementing the current index, and not copying
                                    // anything to the end_index.
                                    current_index += 1;
                                    continue;
                                }
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_inequality_watcher_to_other_watcher(end_index, current_index);
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy(nogood_id.id as u64);

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[updated_domain_id]
                                        .set_inequality_watcher_to_other_watcher(
                                            end_index,
                                            current_index,
                                        );
                                    current_index += 1;
                                    end_index += 1;
                                }

                                self.watch_lists[updated_domain_id]
                                    .truncate_inequality_watchers(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_inequality_watcher_to_other_watcher(end_index, current_index);
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[updated_domain_id].truncate_inequality_watchers(end_index);
                    }
                }
                IntDomainEvent::Assign => {
                    let new_lower_bound = context.lower_bound(&updated_domain_id);
                    let new_upper_bound = context.upper_bound(&updated_domain_id);

                    assert!(new_lower_bound == new_upper_bound);
                    let assigned_value = new_lower_bound;

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let num_watchers = self.watch_lists[updated_domain_id].num_equality_watchers();
                    // Iterate through all watchers.

                    while current_index < num_watchers {
                        let right_hand_side = self.watch_lists[updated_domain_id]
                            .get_equality_watcher_at_index(current_index)
                            .right_hand_side;

                        if assigned_value == right_hand_side {
                            let nogood_id = self.watch_lists[updated_domain_id]
                                .get_equality_watcher_at_index(current_index)
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: Predicate| {
                                predicate.is_equality_predicate()
                                    && predicate.get_domain() == updated_domain_id
                                    && predicate.get_right_hand_side() == assigned_value
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists[updated_domain_id]
                                    .set_equality_watcher_to_other_watcher(
                                        end_index,
                                        current_index,
                                    );
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;

                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == updated_domain_id
                                    );
                                    // Add this nogood to the watch list of the new watcher.
                                    // Ensure there is an entry.
                                    // Add this nogood to the watch list of the new watcher.
                                    Self::add_watcher(&mut self.watch_lists, nogood[1], nogood_id);

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.

                                current_index += 1;
                                continue;
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_equality_watcher_to_other_watcher(end_index, current_index);

                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy(nogood_id.id as u64);

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[updated_domain_id]
                                        .set_equality_watcher_to_other_watcher(
                                            end_index,
                                            current_index,
                                        );

                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[updated_domain_id]
                                    .truncate_equality_watchers(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[updated_domain_id]
                                .set_equality_watcher_to_other_watcher(end_index, current_index);

                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[updated_domain_id].truncate_equality_watchers(end_index);
                    }
                }
            }
        }
        self.last_index_on_trail = old_trail_position;

        pumpkin_assert_advanced!(self.debug_is_properly_watched());

        Ok(())
    }

    fn synchronise(&mut self, context: PropagationContext) {
        self.last_index_on_trail = context.assignments().trail.len() - 1;
        let _ = self.enqueued_updates.drain();
    }

    fn notify(
        &mut self,
        _context: StatefulPropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        while local_id.unpack() as usize >= self.enqueued_updates.num_domains() {
            self.enqueued_updates.grow();
        }

        // Save the update, and also enqueue removal in case the lower or upper bound updates are
        // set.
        self.enqueued_updates.event_occurred(
            event.unwrap(),
            DomainId {
                id: local_id.unpack(),
            },
        );
        if let IntDomainEvent::LowerBound | IntDomainEvent::UpperBound = event.unwrap() {
            // If it is a lower-bound or upper-bound event then we also add a removal event
            self.enqueued_updates.event_occurred(
                IntDomainEvent::Removal,
                DomainId {
                    id: local_id.unpack(),
                },
            );
        }
        EnqueueDecision::Enqueue
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // Very inefficient version!

        // The algorithm goes through every nogood explicitly
        // and computes from scratch.
        for nogood_id in self.nogoods.keys() {
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
    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> &[Predicate] {
        let id = NogoodId { id: code as u32 };

        // Update the LBD and activity of the nogood, if appropriate.
        //
        // Note that low lbd nogoods are kept permanently, so these are not updated.
        if !self.nogoods[id].block_bumps
            && self.nogoods[id].is_learned
            && self.nogoods[id].lbd > self.parameters.lbd_threshold
        {
            self.nogoods[id].block_bumps = true;
            self.bumped_nogoods.push(id);
            // LBD update.
            // Note that we do not need to take into account the propagated predicate (in position
            // zero), since it will share a decision level with one of the other predicates.
            let current_lbd = self.lbd_helper.compute_lbd(
                &self.nogoods[id].predicates.as_slice()[1..],
                #[allow(deprecated, reason = "should be refactored later")]
                context.assignments(),
            );

            // The nogood keeps track of the best lbd encountered.
            if current_lbd < self.nogoods[id].lbd {
                self.nogoods[id].lbd = current_lbd;
                if current_lbd <= 30 {
                    self.nogoods[id].is_protected = true;
                }
            }

            // Nogood activity update.
            // Rescale the nogood activities,
            // in case bumping the activity now would lead to a large activity value.
            if self.nogoods[id].activity + self.parameters.activity_bump_increment
                > self.parameters.max_activity
            {
                self.learned_nogood_ids.high_lbd.iter().for_each(|i| {
                    self.nogoods[*i].activity /= self.parameters.max_activity;
                });
                self.parameters.activity_bump_increment /= self.parameters.max_activity;
            }

            // At this point, it is safe to increase the activity value
            self.nogoods[id].activity += self.parameters.activity_bump_increment;
        }
        // update LBD, so we need code plus assignments as input.
        &self.nogoods[id].predicates.as_slice()[1..]
    }

    fn initialise_at_root(
        &mut self,
        _context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        // There should be no nogoods yet
        pumpkin_assert_simple!(self.nogoods.len() == 0);
        Ok(())
    }
}

/// Functions for adding nogoods
impl NogoodPropagator {
    /// Adds a nogood which has been learned during search.
    ///
    /// The first predicate should be asserting and the second predicate should contain the
    /// predicte with the next highest decision level.
    pub(crate) fn add_asserting_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
        statistics: &mut SolverStatistics,
    ) {
        // We treat unit nogoods in a special way by adding it as a permanent nogood at the
        // root-level; this is essentially the same as adding a predicate at the root level
        if nogood.len() == 1 {
            pumpkin_assert_moderate!(
                context.get_decision_level() == 0,
                "A unit nogood should have backtracked to the root-level"
            );
            self.add_permanent_nogood(nogood, context)
                .expect("Unit learned nogoods cannot fail.");
            return;
        }

        // Skip the zero-th predicate since it is unassigned,
        // but will be assigned at the level of the predicate at index one.
        let lbd = self
            .lbd_helper
            .compute_lbd(&nogood.as_slice()[1..], context.assignments());

        statistics
            .learned_clause_statistics
            .average_lbd
            .add_term(lbd as u64);

        // Add the nogood to the database.
        //
        // If there is an available nogood id, use it, otherwise allocate a fresh id.
        let new_id = if let Some(reused_id) = self.delete_ids.pop() {
            self.nogoods[reused_id] = Nogood::new_learned_nogood(nogood.into(), lbd);
            reused_id
        } else {
            let new_nogood_id = NogoodId {
                id: self.nogoods.len() as u32,
            };
            let _ = self
                .nogoods
                .push(Nogood::new_learned_nogood(nogood.into(), lbd));
            new_nogood_id
        };

        // Now we add two watchers to the first two predicates in the nogood
        NogoodPropagator::add_watcher(
            &mut self.watch_lists,
            self.nogoods[new_id].predicates[0],
            new_id,
        );
        NogoodPropagator::add_watcher(
            &mut self.watch_lists,
            self.nogoods[new_id].predicates[1],
            new_id,
        );

        // Then we propagate the asserting predicate and as reason we give the index to the
        // asserting nogood such that we can re-create the reason when asked for it
        let reason = Reason::DynamicLazy(new_id.id as u64);
        context
            .post_predicate(!self.nogoods[new_id].predicates[0], reason)
            .expect("Cannot fail to add the asserting predicate.");

        // We then divide the new nogood based on the LBD level
        if lbd <= self.parameters.lbd_threshold {
            self.learned_nogood_ids.low_lbd.push(new_id);
        } else {
            self.learned_nogood_ids.high_lbd.push(new_id);
        }
    }

    /// Adds a nogood to the propagator as a permanent nogood and sets the internal state to be
    /// infeasible if the nogood led to a conflict.
    pub(crate) fn add_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        self.add_permanent_nogood(nogood, context)
    }

    /// Adds a nogood which cannot be deleted by clause management.
    fn add_permanent_nogood(
        &mut self,
        mut nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        pumpkin_assert_simple!(
            context.get_decision_level() == 0,
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
        Self::preprocess_nogood(&mut nogood, context);

        // Unit nogoods are added as root assignments rather than as nogoods.
        if nogood.len() == 1 {
            // Get the reason for the propagation. Note that preprocessing removes literals from
            // `nogood` that are still present in `input_nogood`, so this does not necessarily
            // result in an empty reason.
            input_nogood.retain(|&p| p != nogood[0]);

            // Post the negated predicate at the root to respect the nogood.
            context.post_predicate(!nogood[0], PropositionalConjunction::from(input_nogood))?;
        }
        // Standard case, nogood is of size at least two.
        //
        // The preprocessing ensures that all predicates are unassigned.
        else {
            // Add the nogood to the database.
            // If there is an available nogood id, use it, otherwise allocate a fresh id.
            let new_id = if let Some(reused_id) = self.delete_ids.pop() {
                self.nogoods[reused_id] = Nogood::new_permanent_nogood(nogood.into());
                reused_id
            } else {
                self.nogoods
                    .push(Nogood::new_permanent_nogood(nogood.into()))
            };

            self.permanent_nogoods.push(new_id);

            NogoodPropagator::add_watcher(
                &mut self.watch_lists,
                self.nogoods[new_id].predicates[0],
                new_id,
            );
            NogoodPropagator::add_watcher(
                &mut self.watch_lists,
                self.nogoods[new_id].predicates[1],
                new_id,
            );
        }

        Ok(())
    }
}

/// Methods concerning the watchers and watch lists
impl NogoodPropagator {
    /// Adds a watcher to the predicate in the provided nogood with the provided [`NogoodId`].
    fn add_watcher(
        watch_lists: &mut KeyedVec<DomainId, NogoodWatchList>,
        predicate: Predicate,
        nogood_id: NogoodId,
    ) {
        // First we resize the watch list to accomodate the new nogood
        if predicate.get_domain().id as usize >= watch_lists.len() {
            watch_lists.resize(
                (predicate.get_domain().id + 1) as usize,
                NogoodWatchList::default(),
            );
        }

        // Then we add this nogood to the watch list of the new watcher.
        match predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => watch_lists[domain_id].add_lower_bound_watcher(nogood_id, lower_bound),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => {
                watch_lists[domain_id].add_upper_bound_watcher(nogood_id, upper_bound);
            }
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => {
                watch_lists[domain_id].add_inequality_watcher(nogood_id, not_equal_constant);
            }
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => {
                watch_lists[domain_id].add_equality_watcher(nogood_id, equality_constant);
            }
        }
    }

    /// Removes the noogd from the watch list
    fn remove_nogood_from_watch_list(
        watch_lists: &mut KeyedVec<DomainId, NogoodWatchList>,
        watching_predicate: Predicate,
        id: NogoodId,
    ) {
        match watching_predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => watch_lists[domain_id].remove_lower_bound_watcher(id, lower_bound),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => watch_lists[domain_id].remove_upper_bound_watcher(id, upper_bound),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => watch_lists[domain_id].remove_inequality_watcher(id, not_equal_constant),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => watch_lists[domain_id].remove_equality_watcher(id, equality_constant),
        }
    }
}

/// Nogood management
impl NogoodPropagator {
    /// Removes nogoods if there are too many nogoods with a "high" LBD
    fn clean_up_learned_nogoods_if_needed(
        &mut self,
        context: PropagationContext,
        reason_store: &ReasonStore,
    ) {
        // Only remove learned nogoods if there are too many.
        if self.learned_nogood_ids.high_lbd.len() > self.parameters.limit_num_high_lbd_nogoods {
            // The procedure is divided into two parts (for simplicity of implementation).
            //  1. Promote nogoods that are in the high lbd group but got updated to a low lbd.
            //  2. Remove roughly half of the nogoods that have high lbd.
            self.promote_high_lbd_nogoods();
            self.remove_high_lbd_nogoods(context, reason_store);
        }
    }

    /// Goes through all of the "high" LBD nogoods and promotes nogoods which have been updated to
    /// a "low" LBD.
    fn promote_high_lbd_nogoods(&mut self) {
        self.learned_nogood_ids.high_lbd.retain(|id| {
            // If the LBD is still high, the nogood stays in the high LBD category.
            if self.nogoods[*id].lbd > self.parameters.lbd_threshold {
                true
            }
            // Otherwise the nogood is promoted to the low LBD group.
            else {
                self.learned_nogood_ids.low_lbd.push(*id);
                false
            }
        })
    }

    /// Removes high LBD nogoods from the internal structures.
    ///
    /// The idea is that these are likely poor quality nogoods and the overhead of propagating them
    /// is not worth it.
    fn remove_high_lbd_nogoods(&mut self, context: PropagationContext, reason_store: &ReasonStore) {
        // First we sort the high LBD nogoods based on non-increasing "quality"
        self.sort_high_lbd_nogoods_by_quality_better_first();

        // The removal is done in two phases.
        // 1) Nogoods are deleted but the ids are not removed from self.learned_nogoods_ids.
        // 2) The corresponding ids are removed from the self.learned_nogoods_ids.
        let mut num_clauses_to_remove =
            self.learned_nogood_ids.high_lbd.len() - self.parameters.limit_num_high_lbd_nogoods / 2;

        // Note the 'rev', since poor nogoods have priority for deletion.
        // The aim is to remove half of the nogoods, but less could be removed due to protection.
        for &id in self.learned_nogood_ids.high_lbd.iter().rev() {
            if num_clauses_to_remove == 0 {
                // We are not removing any clauses
                break;
            }

            // Protected clauses are skipped for one clean up iteration.
            if self.nogoods[id].is_protected {
                self.nogoods[id].is_protected = false;
                continue;
            }

            if self.is_nogood_propagating(context, reason_store, id) {
                continue;
            }

            // Remove the nogood from the watch list.
            Self::remove_nogood_from_watch_list(
                &mut self.watch_lists,
                self.nogoods[id].predicates[0],
                id,
            );
            Self::remove_nogood_from_watch_list(
                &mut self.watch_lists,
                self.nogoods[id].predicates[1],
                id,
            );

            // Delete the nogood.
            //
            // Note that the deleted nogood is still kept in the database but it will not be used
            // for propagation. A new nogood may take the place of a deleted nogood, this makes it
            // simpler, since other nogood ids remain unchanged.
            self.nogoods[id].is_deleted = true;
            self.delete_ids.push(id);

            num_clauses_to_remove -= 1;
        }

        // Now we remove all of the nogoods from the `high_lbd` nogoods; note that this does not
        // remove it from the database.
        self.learned_nogood_ids
            .high_lbd
            .retain(|&id| !self.nogoods[id].is_deleted);
    }

    /// Orders the `high_lbd` nogoods in such a way that the 'better' nogoods are in front.
    ///
    /// The sorting depends on the provided [`LearnedNogoodSortingStrategy`]
    fn sort_high_lbd_nogoods_by_quality_better_first(&mut self) {
        // Note that this is not the most efficient sorting comparison, but will do for now.
        self.learned_nogood_ids
            .high_lbd
            .sort_unstable_by(|&id1, &id2| {
                let nogood1 = &self.nogoods[id1];
                let nogood2 = &self.nogoods[id2];

                match self.parameters.nogood_sorting_strategy {
                    LearnedNogoodSortingStrategy::Activity => {
                        // Note that here we reverse nogood1 and nogood2,
                        // because a higher value for activity is better.
                        nogood2.activity.partial_cmp(&nogood1.activity).unwrap()
                    }
                    LearnedNogoodSortingStrategy::Lbd => {
                        if nogood1.lbd != nogood2.lbd {
                            // Recall that lower LBD is better.
                            nogood1.lbd.cmp(&nogood2.lbd)
                        } else {
                            // Note that here we reverse nogood1 and nogood2,
                            // because a higher value for activity is better.
                            nogood2.activity.partial_cmp(&nogood1.activity).unwrap()
                        }
                    }
                }
            });
    }

    /// Decays the activity bump increment by
    /// [`LearningOptions::self.parameters.activity_decay_factor`].
    pub(crate) fn decay_nogood_activities(&mut self) {
        self.parameters.activity_bump_increment /= self.parameters.activity_decay_factor;
        for &id in &self.bumped_nogoods {
            self.nogoods[id].block_bumps = false;
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
    fn preprocess_nogood(nogood: &mut Vec<Predicate>, context: &mut PropagationContextMut) {
        pumpkin_assert_simple!(context.get_decision_level() == 0);
        // The code below is broken down into several parts

        // We opt for semantic minimisation upfront. This way we avoid the possibility of having
        // assigned predicates in the final nogood. This could happen since the root bound can
        // change since the initial time the semantic minimiser recorded it, so it would not know
        // that a previously nonroot bound is now actually a root bound.

        // Semantic minimisation will take care of removing duplicate predicates, conflicting
        // nogoods, and may result in few predicates since it removes redundancies.
        *nogood = context.semantic_minimiser.minimise(
            nogood,
            context.assignments,
            Mode::EnableEqualityMerging,
        );

        // Check if the nogood cannot be violated, i.e., it has a falsified predicate.
        if nogood.is_empty() || nogood.iter().any(|p| context.is_predicate_falsified(*p)) {
            *nogood = vec![Predicate::trivially_false()];
            return;
        }

        // Remove predicates that are satisfied at the root level.
        nogood.retain(|p| !context.is_predicate_satisfied(*p));

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
        context: &mut PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // This is an inefficient implementation for testing purposes
        let nogood = &self.nogoods[nogood_id];

        if nogood.is_deleted {
            // The nogood has already been deleted, meaning that it could be that the call to
            // `propagate` would not find any propagations using it due to the watchers being
            // deleted
            return Ok(());
        }

        // First we get the number of falsified predicates
        let has_falsified_predicate = nogood
            .predicates
            .iter()
            .any(|predicate| context.evaluate_predicate(*predicate).is_some_and(|x| !x));

        // If at least one predicate is false, then the nogood can be skipped
        if has_falsified_predicate {
            return Ok(());
        }

        let num_satisfied_predicates = nogood
            .predicates
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| x))
            .count();

        let nogood_len = nogood.predicates.len();

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood_len {
            return Err(Inconsistency::Conflict(
                nogood.predicates.iter().copied().collect(),
            ));
        }
        // If all but one predicate are satisfied, then we can propagate.
        //
        // Note that this only makes sense since we know that there are no falsifying predicates at
        // this point.
        else if num_satisfied_predicates == nogood_len - 1 {
            // Note that we negate the remaining unassigned predicate!
            let propagated_predicate = nogood
                .predicates
                .iter()
                .find(|predicate| context.evaluate_predicate(**predicate).is_none())
                .unwrap()
                .not();

            assert!(nogood
                .predicates
                .iter()
                .any(|p| *p == propagated_predicate.not()));

            // Cannot use lazy explanations when propagating from scratch
            // since the propagated predicate may not be at position zero.
            // but we cannot change the nogood since this function is with nonmutable self.
            //
            // So an eager reason is constructed
            let reason: PropositionalConjunction = nogood
                .predicates
                .iter()
                .filter(|p| **p != !propagated_predicate)
                .copied()
                .collect();

            context.post_predicate(propagated_predicate, reason)?;
        }
        Ok(())
    }

    /// Checks for each nogood whether the first two predicates in the nogood are being watched
    fn debug_is_properly_watched(&self) -> bool {
        let is_watching = |predicate: Predicate, nogood_id: NogoodId| -> bool {
            match predicate {
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                } => self.watch_lists[domain_id]
                    .iter_lower_bound_watchers()
                    .any(|w| w.right_hand_side == lower_bound && w.nogood_id == nogood_id),
                Predicate::UpperBound {
                    domain_id,
                    upper_bound,
                } => self.watch_lists[domain_id]
                    .iter_upper_bound_watchers()
                    .any(|w| w.right_hand_side == upper_bound && w.nogood_id == nogood_id),
                Predicate::NotEqual {
                    domain_id,
                    not_equal_constant,
                } => self.watch_lists[domain_id]
                    .iter_inequality_watchers()
                    .any(|w| w.right_hand_side == not_equal_constant && w.nogood_id == nogood_id),
                Predicate::Equal {
                    domain_id,
                    equality_constant,
                } => self.watch_lists[domain_id]
                    .iter_equality_watchers()
                    .any(|w| w.right_hand_side == equality_constant && w.nogood_id == nogood_id),
            }
        };

        for nogood in self.nogoods.iter().enumerate() {
            let nogood_id = NogoodId {
                id: nogood.0 as u32,
            };

            if nogood.1.is_deleted {
                // If the clause is deleted then it will have no watchers
                assert!(
                    !is_watching(nogood.1.predicates[0], nogood_id)
                        && !is_watching(nogood.1.predicates[1], nogood_id)
                );
                continue;
            }

            if !(is_watching(nogood.1.predicates[0], nogood_id)
                && is_watching(nogood.1.predicates[1], nogood_id))
            {
                eprintln!("Nogood id: {}", nogood_id.id);
                eprintln!("Nogood: {:?}", nogood);
                eprintln!(
                    "watching 0: {}",
                    is_watching(nogood.1.predicates[0], nogood_id)
                );
                eprintln!(
                    "watching 1: {}",
                    is_watching(nogood.1.predicates[1], nogood_id)
                );
                eprintln!(
                    "watch list 0: {:?}",
                    self.watch_lists[nogood.1.predicates[0].get_domain()]
                );
                eprintln!(
                    "watch list 1: {:?}",
                    self.watch_lists[nogood.1.predicates[1].get_domain()]
                );
            }

            assert!(
                is_watching(nogood.1.predicates[0], nogood_id)
                    && is_watching(nogood.1.predicates[1], nogood_id)
            );
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::NogoodPropagator;
    use crate::conjunction;
    use crate::engine::propagation::store::PropagatorStore;
    use crate::engine::propagation::PropagationContextMut;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;

    fn downcast_to_nogood_propagator(
        nogood_propagator: PropagatorId,
        propagators: &mut PropagatorStore,
    ) -> &mut NogoodPropagator {
        match propagators[nogood_propagator].downcast_mut::<NogoodPropagator>() {
            Some(nogood_propagator) => nogood_propagator,
            None => panic!("Provided propagator should be the nogood propagator"),
        }
    }

    #[test]
    fn ternary_nogood_propagate() {
        let mut solver = TestSolver::default();
        let dummy = solver.new_variable(0, 1);
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let propagator = solver
            .new_propagator(NogoodPropagator::default())
            .expect("no empty domains");

        let _ = solver.increase_lower_bound_and_notify(propagator, dummy.id, dummy, 1);

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let mut context = PropagationContextMut::new(
                &mut solver.stateful_assignments,
                &mut solver.assignments,
                &mut solver.reason_store,
                &mut solver.semantic_minimiser,
                propagator,
            );

            downcast_to_nogood_propagator(propagator, &mut solver.propagator_store)
                .add_nogood(nogood.into(), &mut context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(propagator, a.id, a, 3);
        let _ = solver.increase_lower_bound_and_notify(propagator, b.id, b, 0);

        solver.propagate_until_fixed_point(propagator).expect("");

        let _ = solver.increase_lower_bound_and_notify(propagator, c.id, c, 15);

        solver.propagate(propagator).expect("");

        assert_eq!(solver.upper_bound(b), 0);

        let reason_lb = solver.get_reason_int(predicate!(b <= 0));
        assert_eq!(conjunction!([a >= 2] & [c >= 10]), reason_lb);
    }

    #[test]
    fn unsat() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let propagator = solver
            .new_propagator(NogoodPropagator::default())
            .expect("no empty domains");

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let mut context = PropagationContextMut::new(
                &mut solver.stateful_assignments,
                &mut solver.assignments,
                &mut solver.reason_store,
                &mut solver.semantic_minimiser,
                propagator,
            );

            downcast_to_nogood_propagator(propagator, &mut solver.propagator_store)
                .add_nogood(nogood.into(), &mut context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(propagator, a.id, a, 3);
        let _ = solver.increase_lower_bound_and_notify(propagator, b.id, b, 1);
        let _ = solver.increase_lower_bound_and_notify(propagator, c.id, c, 15);

        let result = solver.propagate_until_fixed_point(propagator);
        assert!(result.is_err());
    }
}
