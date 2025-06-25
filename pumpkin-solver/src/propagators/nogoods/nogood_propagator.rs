use std::cmp::max;
use std::ops::Not;

use log::warn;

use super::LearningOptions;
use super::NogoodId;
use super::NogoodInfo;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::Inconsistency;
use crate::basic_types::PredicateId;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::basic_types::PropositionalConjunction;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::conflict_analysis::Mode;
use crate::engine::notifications::NotificationEngine;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::HasAssignments;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::Assignments;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::Lbd;
use crate::engine::SolverStatistics;
use crate::engine::TrailedValues;
use crate::proof::InferenceCode;
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
    /// The predicates corresponding to each Nogood.
    nogood_predicates: KeyedVec<NogoodId, Vec<Predicate>>,
    /// The information corresponding to each nogood; including activity, and lbd.
    nogood_info: KeyedVec<NogoodId, NogoodInfo>,
    /// The inference codes for the nogoods.
    inference_codes: KeyedVec<NogoodId, InferenceCode>,
    /// Nogoods which are permanently present
    permanent_nogood_ids: Vec<NogoodId>,
    /// Stores all learned nogoods.
    learned_nogood_ids: LearnedNogoodIds,
    /// Ids which have been deleted and can now be re-used
    deleted_nogood_ids: Vec<NogoodId>,
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
}

/// Watcher for a single nogood.
///
/// A watcher is a combination of a nogood ID and a cached predicate. If the nogood has a predicate
/// that is observed to be `false`, it will be made the cached predicate. That way, whenever the
/// watcher is triggered, the propagator may be able to quickly determine if the nogood can be
/// skipped by looking at the cached predicate.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Watcher {
    nogood_id: NogoodId,
    cached_predicate: Predicate,
}

impl PropagatorConstructor for NogoodPropagator {
    type PropagatorImpl = Self;

    fn create(self, _: PropagatorConstructorContext) -> Self::PropagatorImpl {
        self
    }
}

/// A struct which keeps track of which nogoods are considered "high" LBD and which nogoods are
/// considered "low" LBD.
#[derive(Default, Debug, Clone)]
struct LearnedNogoodIds {
    low_lbd: Vec<NogoodId>,
    mid_lbd: Vec<NogoodId>,
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
        nogood: &[Predicate],
        context: PropagationContext,
        reason_store: &ReasonStore,
        id: NogoodId,
    ) -> bool {
        if context.is_predicate_falsified(nogood[0]) {
            let trail_position = context
                .assignments()
                .get_trail_position(&!nogood[0])
                .unwrap();
            let trail_entry = context.assignments().get_trail_entry(trail_position);
            if let Some((reason_ref, _)) = trail_entry.reason {
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

    fn is_watched_predicate(
        predicate: Predicate,
        predicate_id: &PredicateId,
        context: &mut PropagationContextMut,
    ) -> bool {
        context.notification_engine.has_id_for_predicate(predicate)
            && context.notification_engine.get_id(predicate) == *predicate_id
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

    fn notify_predicate_id_satisfied(&mut self, predicate_id: PredicateId) {
        self.updated_predicate_ids.push(predicate_id);
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> Result<(), Inconsistency> {
        pumpkin_assert_advanced!(self.debug_is_properly_watched(context.notification_engine));

        // First we perform nogood management to ensure that the database does not grow excessively
        // large with "bad" nogoods
        self.clean_up_learned_nogoods_if_needed(
            PropagationContext {
                assignments: context.assignments,
            },
            context.reason_store,
        );

        if self.watch_lists.len() <= context.assignments().num_domains() as usize {
            self.watch_lists.resize(
                context.assignments().num_domains() as usize + 1,
                Vec::default(),
            );
        }

        // TODO: should drop all elements afterwards
        for predicate_id in self.updated_predicate_ids.drain(..) {
            pumpkin_assert_moderate!(
                {
                    let predicate = context
                        .notification_engine
                        .get_predicate_for_id(predicate_id);
                    context.is_predicate_satisfied(predicate)
                },
                "The predicate {} should be satisfied but was not",
                context
                    .notification_engine
                    .get_predicate_for_id(predicate_id),
            );

            let mut index = 0;
            while index < self.watch_lists[predicate_id].len() {
                let watcher = self.watch_lists[predicate_id][index];

                // We first check whether the cached predicate might already make the nogood
                // satisfied
                if context.is_predicate_falsified(watcher.cached_predicate) {
                    index += 1;
                    continue;
                }

                let nogood_predicates = &mut self.nogood_predicates[watcher.nogood_id];

                // Place the watched predicate at position 1 for simplicity.
                if Self::is_watched_predicate(nogood_predicates[0], &predicate_id, &mut context) {
                    nogood_predicates.swap(0, 1);
                }

                pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood_predicates[1]));

                // Check the other watched predicate is already falsified, in which case
                // no propagation can take place. Recall that the other watched
                // predicate is at position 0 due to previous code.
                if context.is_predicate_falsified(nogood_predicates[0]) {
                    self.watch_lists[predicate_id][index].cached_predicate = nogood_predicates[0];
                    index += 1;
                    continue;
                }

                // Look for another nonsatisfied predicate
                // to replace the watched predicate.
                let mut found_new_watch = false;
                // Start from index 2 since we are skipping watched predicates.
                for i in 2..nogood_predicates.len() {
                    // Find a predicate that is either false or unassigned,
                    // i.e., not assigned true.
                    if !context.is_predicate_satisfied(nogood_predicates[i]) {
                        // Found another predicate that can be the watcher.
                        found_new_watch = true;
                        // todo: does it make sense to replace the cached predicate with
                        // this new predicate?

                        // Replace the current watcher with the new predicate watcher.
                        nogood_predicates.swap(1, i);
                        // Add this nogood to the watch list of the new watcher.
                        Self::add_watcher(
                            nogood_predicates[1],
                            watcher,
                            context.notification_engine,
                            context.trailed_values,
                            &mut self.watch_lists,
                            context.assignments,
                        );

                        // No propagation is taking place, go to the next nogood.
                        break;
                    }
                } // end iterating through the nogood

                if found_new_watch {
                    // We remove the current watcher
                    let _ = self.watch_lists[predicate_id].swap_remove(index);
                    continue;
                }

                // At this point, nonwatched predicates and nogood[1] are falsified.
                pumpkin_assert_advanced!(nogood_predicates
                    .iter()
                    .skip(1)
                    .all(|p| context.is_predicate_satisfied(*p)));

                // There are two scenarios:
                // nogood[0] is unassigned -> propagate the predicate to false
                // nogood[0] is assigned true -> conflict.
                let reason = Reason::DynamicLazy(watcher.nogood_id.id as u64);

                let result = context.post(
                    !nogood_predicates[0],
                    reason,
                    self.inference_codes[watcher.nogood_id],
                );
                // If the propagation lead to a conflict.
                if let Err(e) = result {
                    return Err(e.into());
                }
                index += 1;
            }
        }

        pumpkin_assert_advanced!(self.debug_is_properly_watched(context.notification_engine));

        Ok(())
    }

    fn synchronise(&mut self, _context: PropagationContext) {
        self.updated_predicate_ids.clear()
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // Very inefficient version!

        // The algorithm goes through every nogood explicitly
        // and computes from scratch.
        for nogood_id in self.nogood_predicates.keys() {
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

        // The LBD is only updated for high- and mid-tier LBD nogoods.
        // The low-lbd nogoods do not get their LBDs nor activities updated.
        if !self.nogood_info[id].block_bumps
            && self.nogood_info[id].is_learned
            && self.nogood_info[id].lbd > self.parameters.lbd_low
        {
            self.nogood_info[id].block_bumps = true;
            self.bumped_nogoods.push(id);
            // LBD update.
            // Note that we do not need to take into account the propagated predicate (in position
            // zero), since it will share a decision level with one of the other predicates.
            let current_lbd = self.lbd_helper.compute_lbd(
                &self.nogood_predicates[id].as_slice()[1..],
                #[allow(deprecated, reason = "should be refactored later")]
                context.assignments(),
            );

            // The nogood keeps track of the best lbd encountered.
            if current_lbd < self.nogood_info[id].lbd {
                self.nogood_info[id].lbd = current_lbd;
            }

            // Nogood activity update.
            // Rescale the nogood activity if bumping would lead to a large activity value.
            if self.nogood_info[id].activity + self.parameters.activity_bump_increment
                > self.parameters.max_activity
            {
                // Rescale the activity of all learned nogoods.

                // TODO: we could consider having separate activity bump values for each tier, so
                // that we can do rescaling only within the same tier.
                // This would lead to less rescaling, and anyway we are only interested in the
                // relative order of nogoods within a tier (probably).
                self.learned_nogood_ids
                    .high_lbd
                    .iter()
                    .chain(self.learned_nogood_ids.mid_lbd.iter())
                    .for_each(|i| {
                        self.nogood_info[*i].activity /= self.parameters.max_activity;
                    });
                self.parameters.activity_bump_increment /= self.parameters.max_activity;
            }

            // At this point, it is safe to increase the activity value
            self.nogood_info[id].activity += self.parameters.activity_bump_increment;
        }
        // update LBD, so we need code plus assignments as input.
        &self.nogood_predicates[id].as_slice()[1..]
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
        inference_code: InferenceCode,
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
            self.add_permanent_nogood(nogood, inference_code, context)
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
        let nogood_id = if let Some(reused_id) = self.deleted_nogood_ids.pop() {
            self.nogood_info[reused_id] = NogoodInfo::new_learned_nogood_info(lbd);
            self.nogood_predicates[reused_id] = nogood;
            self.inference_codes[reused_id] = inference_code;
            reused_id
        } else {
            let new_id = self
                .nogood_info
                .push(NogoodInfo::new_learned_nogood_info(lbd));
            let _ = self.inference_codes.push(inference_code);
            let _ = self.nogood_predicates.push(nogood);
            new_id
        };

        let watcher = Watcher {
            nogood_id,
            cached_predicate: self.nogood_predicates[nogood_id][0],
        };

        // Now we add two watchers to the first two predicates in the nogood
        NogoodPropagator::add_watcher(
            self.nogood_predicates[nogood_id][0],
            watcher,
            context.notification_engine,
            context.trailed_values,
            &mut self.watch_lists,
            context.assignments,
        );
        NogoodPropagator::add_watcher(
            self.nogood_predicates[nogood_id][1],
            watcher,
            context.notification_engine,
            context.trailed_values,
            &mut self.watch_lists,
            context.assignments,
        );

        // Then we propagate the asserting predicate and as the reason we give the index to the
        // asserting nogood such that we can re-create the reason when asked for it
        let reason = Reason::DynamicLazy(nogood_id.id as u64);
        let inference_code = self.inference_codes[nogood_id];

        context
            .post(
                !self.nogood_predicates[nogood_id][0],
                reason,
                inference_code,
            )
            .expect("Cannot fail to add the asserting predicate.");

        // We then divide the new nogood based on the LBD level
        if lbd >= self.parameters.lbd_high {
            self.learned_nogood_ids.high_lbd.push(nogood_id);
        } else if lbd <= self.parameters.lbd_low {
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
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        self.add_permanent_nogood(nogood, inference_code, context)
    }

    /// Adds a nogood which cannot be deleted by clause management.
    fn add_permanent_nogood(
        &mut self,
        mut nogood: Vec<Predicate>,
        inference_code: InferenceCode,
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
            context.post(
                !nogood[0],
                PropositionalConjunction::from(input_nogood),
                inference_code,
            )?;
            Ok(())
        }
        // Standard case, nogood is of size at least two.
        //
        // The preprocessing ensures that all predicates are unassigned.
        else {
            // Add the nogood to the database.
            // If there is an available nogood id, use it, otherwise allocate a fresh id.
            let nogood_id = if let Some(reused_id) = self.deleted_nogood_ids.pop() {
                self.nogood_info[reused_id] = NogoodInfo::new_permanent_nogood_info();
                self.nogood_predicates[reused_id] = nogood;
                self.inference_codes[reused_id] = inference_code;
                reused_id
            } else {
                let new_id = self
                    .nogood_info
                    .push(NogoodInfo::new_permanent_nogood_info());
                let _ = self.nogood_predicates.push(nogood);
                let _ = self.inference_codes.push(inference_code);
                new_id
            };

            self.permanent_nogood_ids.push(nogood_id);

            let watcher = Watcher {
                nogood_id,
                cached_predicate: self.nogood_predicates[nogood_id][0],
            };

            NogoodPropagator::add_watcher(
                self.nogood_predicates[nogood_id][0],
                watcher,
                context.notification_engine,
                context.trailed_values,
                &mut self.watch_lists,
                context.assignments,
            );
            NogoodPropagator::add_watcher(
                self.nogood_predicates[nogood_id][1],
                watcher,
                context.notification_engine,
                context.trailed_values,
                &mut self.watch_lists,
                context.assignments,
            );

            Ok(())
        }
    }
}

/// Methods concerning the watchers and watch lists
impl NogoodPropagator {
    /// Adds a watcher to the predicate.
    fn add_watcher(
        predicate: Predicate,
        watcher: Watcher,
        notification_engine: &mut NotificationEngine,
        trailed_values: &mut TrailedValues,
        watch_lists: &mut KeyedVec<PredicateId, Vec<Watcher>>,
        assignments: &Assignments,
    ) {
        // First we resize the watch list to accomodate the new nogood
        if predicate.get_domain().id() as usize >= watch_lists.len() {
            watch_lists.resize((predicate.get_domain().id() + 1) as usize, Vec::default());
        }

        let predicate_id =
            notification_engine.track_predicate(predicate, trailed_values, assignments);

        while watch_lists.len() <= predicate_id.index() {
            let _ = watch_lists.push(Vec::default());
        }

        watch_lists[predicate_id].push(watcher);
    }
}

/// Nogood management
impl NogoodPropagator {
    /// Removes learned nogoods if there are too many learned nogoods.
    /// Nogood management follows ideas based on the three-tiered clause system from "Improving
    /// SAT Solvers by Exploiting Empirical Characteristics of CDCL" and "Improving Implementation
    /// of SAT Competitions 2017–2019 Winners" with a slight change (see below).
    ///
    /// The removal process is as follows.
    /// Learned nogoods are partitioned into three categories: high-, mid-, and low-lbd nogoods.
    /// Each tier has a predefined limit on the number of nogoods it may store.
    /// If a tier has more nogoods than the limit prescribes, roughly half of the nogoods from the
    /// tier are removed. High- and mid-tier nogoods remove the least active nogoods, whereas
    /// low-tier nogoods are removed based on lbd and size.
    ///
    /// Nogoods can move from higher tiers to lower tiers if the lbd drops sufficiently low, but
    /// not the other way around. This is the main difference regarding this aspect from the paper
    /// "Improving Implementation of SAT Competitions 2017–2019 Winners".
    fn clean_up_learned_nogoods_if_needed(
        &mut self,
        context: PropagationContext,
        reason_store: &mut ReasonStore,
    ) {
        // Only remove learned nogoods if there are too many.

        // The procedure is divided into four stages (for simplicity of implementation).
        // For each tier, if the number of nogoods exceeds the predefined threshold for that tier:
        //  1. Promote nogoods that achieved a sufficiently low lbd to the next tier.
        //  2. Sort the nogoods according to a criteria.
        //    * Criteria for high- and mid-lbd nogoods: activity (higher activity better)
        //    * Criteria for low-lbd nogoods: LBD, and tie-break on size (lower values better)
        //  3. Remove the bottom half of the nogoods, skipping propagating nogoods. This only
        //     removes the nogood ids from their respective tier.

        // Once the above is done for all tiers, then:
        //  4. Remove deleted nogoods from the watchers.

        // Note: in other works, instead of deleting nogoods, they get demoted to the previous tier.
        // The rational for our choice is that demoting many nogoods will likely trigger clean up
        // of the previous tier, and demoted nogoods have low activities, so they would be targets
        // for deletion anyway.
        // TODO: check this.

        let mut removed_at_least_one_nogood = false;
        // Process high-lbd nogoods.
        if self.learned_nogood_ids.high_lbd.len() > self.parameters.limit_high_lbd_nogoods {
            self.promote_high_lbd_nogoods();

            NogoodPropagator::sort_nogoods_by_decreasing_activity(
                &mut self.learned_nogood_ids.high_lbd,
                &self.nogood_info,
            );

            removed_at_least_one_nogood |= NogoodPropagator::remove_roughly_bottom_half_nogood_ids(
                &mut self.learned_nogood_ids.high_lbd,
                &mut self.nogood_info,
                &self.nogood_predicates,
                &mut self.deleted_nogood_ids,
                context,
                reason_store,
            );
        }

        // Process mid-lbd nogoods.
        if self.learned_nogood_ids.mid_lbd.len() > self.parameters.limit_mid_lbd_nogoods {
            self.promote_mid_lbd_nogoods();

            NogoodPropagator::sort_nogoods_by_decreasing_activity(
                &mut self.learned_nogood_ids.mid_lbd,
                &self.nogood_info,
            );

            removed_at_least_one_nogood |= NogoodPropagator::remove_roughly_bottom_half_nogood_ids(
                &mut self.learned_nogood_ids.mid_lbd,
                &mut self.nogood_info,
                &self.nogood_predicates,
                &mut self.deleted_nogood_ids,
                context,
                reason_store,
            );
        }

        // Process low-lbd nogoods.
        if self.learned_nogood_ids.low_lbd.len() > self.parameters.limit_low_lbd_nogoods {
            NogoodPropagator::sort_nogoods_by_increasing_lbd_and_size(
                &mut self.learned_nogood_ids.low_lbd,
                &self.nogood_predicates,
                &self.nogood_info,
            );

            removed_at_least_one_nogood |= NogoodPropagator::remove_roughly_bottom_half_nogood_ids(
                &mut self.learned_nogood_ids.low_lbd,
                &mut self.nogood_info,
                &self.nogood_predicates,
                &mut self.deleted_nogood_ids,
                context,
                reason_store,
            );
        }

        if removed_at_least_one_nogood {
            self.remove_deleted_nogoods_from_watchers(context);
        }
    }

    fn has_a_watched_predicate_falsified_at_root_level(
        nogood: &[Predicate],
        context: PropagationContext,
    ) -> bool {
        let watcher1 = nogood[0];
        let watcher2 = nogood[1];
        context.is_predicate_falsified(watcher1)
            && context
                .assignments
                .get_decision_level_for_predicate(&!watcher1)
                .expect("Falsified predicates must have a decision level.")
                == 0
            || context.is_predicate_falsified(watcher2)
                && context
                    .assignments
                    .get_decision_level_for_predicate(&!watcher2)
                    .expect("Falsified predicates must have a decision level.")
                    == 0
    }

    /// Remove deleted nogoods from watchers.
    /// Also removes some but not all trivially false nogoods on the way.
    ///
    /// TODO: the function is implemented as going through all watchers.
    /// If we only store few learned nogoods, but have many permanent nogoods,
    /// then this function will be called often and may be a bottleneck.
    /// Note that this is not a realistic scenario.
    fn remove_deleted_nogoods_from_watchers(&mut self, context: PropagationContext) {
        // The idea is to go through the watchers and remove watchers that contain deleted nogoods.

        // On the way, if we detect that a nogood is trivially falsified at the root level, or if
        // its cached predicate is falsified at the root,
        // then it is also deleted and removed from the watchers.

        // The code below goes through the watchers.
        // We maintain a flag that signals if the process below will require another pass.
        // This can happen when the cached predicate is falsified at the root level.
        //
        // Recall that each nogood is watched twice.
        // When we encounter a watcher where its cached predicate is falsified
        // at the root, we mark it as deleted. However, we could have encountered
        // the other watcher (with a different cached predicate) attached to this
        // nogood prior to encountering this watcher with a falsified cached predicate;
        // to ensure that we delete this other watcher as well, we require another pass
        //
        // We expect that root-level falsified cached predicates are rare in practice,
        // so the second pass will not take place often.
        let mut another_pass_needed = false;

        // We also track if we have deleted a trivial nogood, because afterwards we need to also
        // delete this nogood from the structures that contain nogood ids.
        // As above, we expect to rarely happen, so most of the time, this flag will stay false.
        let mut trivial_nogood_deleted = false;

        for i in 0..self.watch_lists.len() {
            let index = PredicateId::create_from_index(i);
            self.watch_lists[index].retain(|watcher| {
                // If the nogood has been deleted, do not keep this watcher
                if self.nogood_info[watcher.nogood_id].is_deleted {
                    false
                } else if NogoodPropagator::has_a_watched_predicate_falsified_at_root_level(
                    &self.nogood_predicates[watcher.nogood_id],
                    context,
                ) {
                    // If the nogood is falsified at the root level, mark the nogood
                    // for deletion, and do not keep this watcher.
                    self.nogood_info[watcher.nogood_id].is_deleted = true;
                    trivial_nogood_deleted = true;
                    false
                }
                // Is the cached predicate falsified at the root level?
                else if context.is_predicate_falsified(watcher.cached_predicate)
                    && context
                        .assignments
                        .get_decision_level_for_predicate(&!watcher.cached_predicate)
                        .expect("Falsified predicates must have a decision level.")
                        == 0
                {
                    // Mark that a nogood has been deleted due to the cached predicate.
                    another_pass_needed = true;
                    trivial_nogood_deleted = true;
                    self.nogood_info[watcher.nogood_id].is_deleted = true;
                    false
                } else {
                    true
                }
            });
        }

        if another_pass_needed {
            for i in 0..self.watch_lists.len() {
                let index = PredicateId::create_from_index(i);
                self.watch_lists[index].retain(|watcher| {
                    // If the nogood has been deleted, do not keep this watcher
                    !self.nogood_info[watcher.nogood_id].is_deleted
                });
            }
        }

        if trivial_nogood_deleted {
            self.learned_nogood_ids
                .high_lbd
                .retain(|nogood_id| !self.nogood_info[nogood_id].is_deleted);

            self.learned_nogood_ids
                .mid_lbd
                .retain(|nogood_id| !self.nogood_info[nogood_id].is_deleted);

            self.learned_nogood_ids
                .low_lbd
                .retain(|nogood_id| !self.nogood_info[nogood_id].is_deleted);
        }
    }

    // Attempts to remove the bottom half from nogood_ids.
    // The removed nogood ids are transfered to the deleted_ids vector.
    // Returns true if at least one nogood has been removed.
    //
    // A nogood is not removed if it is currently propagating at a non-root level.
    // This means that the function may remove some nogoods from the first half if
    // some of the bottom nogoods are currently propagating.
    fn remove_roughly_bottom_half_nogood_ids(
        nogood_ids: &mut Vec<NogoodId>,
        nogood_info: &mut KeyedVec<NogoodId, NogoodInfo>,
        nogoods: &KeyedVec<NogoodId, Vec<Predicate>>,
        deleted_ids: &mut Vec<NogoodId>,
        context: PropagationContext,
        reason_store: &mut ReasonStore,
    ) -> bool {
        // The removal is done in two phases.
        // 1) Nogoods are deleted in the database, but the ids are not removed from nogood_ids.
        // 2) The corresponding ids are removed from the nogood_ids.
        // Recall that these deleted nogoods are not yet removed from the watch lists.

        // Schedule to remove at least nogood, likely more than one.
        let mut num_nogoods_to_remove = max(nogood_ids.len() / 2, 1);
        // Note the 'rev', since we are removing the bottom half.
        // The aim is to remove half of the nogoods,
        // but less could be removed if many are involved in propagation.
        for &id in nogood_ids.iter().rev() {
            if num_nogoods_to_remove == 0 {
                // We are done removing nogoods.
                break;
            }

            // Skip propagating nogoods.
            if NogoodPropagator::is_nogood_propagating(&nogoods[id], context, reason_store, id)
                && context
                    .assignments
                    .get_decision_level_for_predicate(&!nogoods[id][0])
                    .expect("A propagating predicate must have a decision level.")
                    == 0
            {
                continue;
            }

            // Delete the nogood.

            // Note that the deleted nogood is still kept in the database but it will not be used
            // for propagation. A new nogood may take the place of a deleted nogood, this makes it
            // simpler, since other nogood ids remain unchanged.
            nogood_info[id].is_deleted = true;
            deleted_ids.push(id);

            num_nogoods_to_remove -= 1;
        }

        // Now we remove the nogood ids from vector;
        // note that this does not remove it from the database or watchers.
        let num_nogoods_before_removal = nogood_ids.len();
        nogood_ids.retain(|&id| !nogood_info[id].is_deleted);
        let num_nogoods_after_removal = nogood_ids.len();

        num_nogoods_before_removal != num_nogoods_after_removal
    }

    /// Goes through all of the "high" LBD nogoods and promotes nogoods which have been updated to
    /// a "low" LBD.
    fn promote_high_lbd_nogoods(&mut self) {
        self.learned_nogood_ids.high_lbd.retain(|id| {
            // If the LBD is still high, the nogood stays in the high LBD category.
            if self.nogood_info[*id].lbd >= self.parameters.lbd_high {
                true
            }
            // Should be placed in the low lbd tier?
            else if self.nogood_info[*id].lbd <= self.parameters.lbd_low {
                self.learned_nogood_ids.low_lbd.push(*id);
                false
            } else {
                // Place in the mid lbd tier.
                self.learned_nogood_ids.mid_lbd.push(*id);
                false
            }
        })
    }

    fn promote_mid_lbd_nogoods(&mut self) {
        self.learned_nogood_ids.mid_lbd.retain(|id| {
            // Still a mid-tier nogood?
            if self.nogood_info[*id].lbd > self.parameters.lbd_low {
                // Since this nogood is in the mid-lbd tier,
                // and the above check concluded that its lbd is not low enough
                // for the low-lbd tier,
                // it stays in the mid-lbd tier.
                true
            } else {
                // Promote to low-lbd tier.
                self.learned_nogood_ids.low_lbd.push(*id);
                false
            }
        })
    }

    fn sort_nogoods_by_decreasing_activity(
        nogood_ids: &mut [NogoodId],
        nogood_info: &KeyedVec<NogoodId, NogoodInfo>,
    ) {
        nogood_ids.sort_unstable_by(|&id1, &id2| {
            let nogood1 = &nogood_info[id1];
            let nogood2 = &nogood_info[id2];
            // Notice that nogood2 goes first in the next line (and not nogood1)
            nogood2.activity.partial_cmp(&nogood1.activity).unwrap()
        });
    }

    fn sort_nogoods_by_increasing_lbd_and_size(
        nogood_ids: &mut [NogoodId],
        nogoods: &KeyedVec<NogoodId, Vec<Predicate>>,
        nogood_info: &KeyedVec<NogoodId, NogoodInfo>,
    ) {
        nogood_ids.sort_unstable_by(|&id1, &id2| {
            let lbd1 = nogood_info[id1].lbd;
            let lbd2 = nogood_info[id2].lbd;

            if lbd1 != lbd2 {
                // Recall that lower LBD is better.
                lbd1.cmp(&lbd2)
            } else {
                // As a tie-breaker, a smaller nogoods is better.

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
            self.nogood_info[id].block_bumps = false;
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
        let nogood = &self.nogood_predicates[nogood_id];
        let inference_code = self.inference_codes[nogood_id];

        if self.nogood_info[nogood_id].is_deleted {
            // The nogood has already been deleted, meaning that it could be that the call to
            // `propagate` would not find any propagations using it due to the watchers being
            // deleted
            return Ok(());
        }

        // First we get the number of falsified predicates
        let has_falsified_predicate = nogood
            .iter()
            .any(|predicate| context.evaluate_predicate(*predicate).is_some_and(|x| !x));

        // If at least one predicate is false, then the nogood can be skipped
        if has_falsified_predicate {
            return Ok(());
        }

        let num_satisfied_predicates = nogood
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| x))
            .count();

        let nogood_len = nogood.len();

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood_len {
            return Err(PropagatorConflict {
                conjunction: nogood.iter().copied().collect::<PropositionalConjunction>(),
                inference_code,
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
                .find(|predicate| context.evaluate_predicate(**predicate).is_none())
                .unwrap()
                .not();

            assert!(nogood.iter().any(|p| *p == propagated_predicate.not()));

            // Cannot use lazy explanations when propagating from scratch
            // since the propagated predicate may not be at position zero.
            // but we cannot change the nogood since this function is with nonmutable self.
            //
            // So an eager reason is constructed
            let reason: PropositionalConjunction = nogood
                .iter()
                .filter(|p| **p != !propagated_predicate)
                .copied()
                .collect();

            context.post(propagated_predicate, reason, inference_code)?;
        }
        Ok(())
    }

    /// Checks for each nogood whether the first two predicates in the nogood are being watched
    fn debug_is_properly_watched(&self, notification_engine: &mut NotificationEngine) -> bool {
        let mut is_watching = |predicate: Predicate, nogood_id: NogoodId| -> bool {
            pumpkin_assert_moderate!(notification_engine
                .get_id_for_predicate(predicate)
                .is_some());
            let predicate_id = notification_engine.get_id_for_predicate(predicate).unwrap();
            self.watch_lists[predicate_id]
                .iter()
                .copied()
                .any(|watcher| watcher.nogood_id == nogood_id)
        };

        for nogood in self.nogood_predicates.iter().enumerate() {
            let nogood_id = NogoodId {
                id: nogood.0 as u32,
            };

            if self.nogood_info[nogood_id].is_deleted {
                // If the clause is deleted then it will have no watchers
                assert!(
                    !is_watching(nogood.1[0], nogood_id) && !is_watching(nogood.1[1], nogood_id)
                );
                continue;
            }

            if !(is_watching(nogood.1[0], nogood_id) && is_watching(nogood.1[1], nogood_id)) {
                eprintln!("Nogood id: {}", nogood_id.id);
                eprintln!("Nogood: {nogood:?}");
                eprintln!("watching 0: {}", is_watching(nogood.1[0], nogood_id));
                eprintln!("watching 1: {}", is_watching(nogood.1[1], nogood_id));
            }

            assert!(is_watching(nogood.1[0], nogood_id) && is_watching(nogood.1[1], nogood_id));
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
        let inference_code = solver.new_inference_code();
        let dummy = solver.new_variable(0, 1);
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let propagator = solver
            .new_propagator(NogoodPropagator::default())
            .expect("no empty domains");

        let _ = solver.increase_lower_bound_and_notify(propagator, dummy.id(), dummy, 1);

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let mut context = PropagationContextMut::new(
                &mut solver.trailed_values,
                &mut solver.assignments,
                &mut solver.reason_store,
                &mut solver.semantic_minimiser,
                &mut solver.notification_engine,
                propagator,
            );

            downcast_to_nogood_propagator(propagator, &mut solver.propagator_store)
                .add_nogood(nogood.into(), inference_code, &mut context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(propagator, a.id(), a, 3);
        let _ = solver.increase_lower_bound_and_notify(propagator, b.id(), b, 0);

        solver.propagate_until_fixed_point(propagator).expect("");

        let _ = solver.increase_lower_bound_and_notify(propagator, c.id(), c, 15);

        solver.propagate(propagator).expect("");

        assert_eq!(solver.upper_bound(b), 0);

        let reason_lb = solver.get_reason_int(predicate!(b <= 0));
        assert_eq!(conjunction!([a >= 2] & [c >= 10]), reason_lb);
    }

    #[test]
    fn unsat() {
        let mut solver = TestSolver::default();
        let inference_code = solver.new_inference_code();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let propagator = solver
            .new_propagator(NogoodPropagator::default())
            .expect("no empty domains");

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let mut context = PropagationContextMut::new(
                &mut solver.trailed_values,
                &mut solver.assignments,
                &mut solver.reason_store,
                &mut solver.semantic_minimiser,
                &mut solver.notification_engine,
                propagator,
            );

            downcast_to_nogood_propagator(propagator, &mut solver.propagator_store)
                .add_nogood(nogood.into(), inference_code, &mut context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(propagator, a.id(), a, 3);
        let _ = solver.increase_lower_bound_and_notify(propagator, b.id(), b, 1);
        let _ = solver.increase_lower_bound_and_notify(propagator, c.id(), c, 15);

        let result = solver.propagate_until_fixed_point(propagator);
        assert!(result.is_err());
    }
}
