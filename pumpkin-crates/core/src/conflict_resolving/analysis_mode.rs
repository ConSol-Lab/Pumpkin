use itertools::Itertools;

use crate::basic_types::PredicateId;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::containers::HashSet;
use crate::containers::KeyValueHeap;
use crate::engine::Lbd;
use crate::engine::Reason;
use crate::predicates::Predicate;
use crate::predicates::PredicateIdGenerator;
use crate::predicates::PredicateType;
use crate::proof::InferenceCode;
use crate::propagation::PropagationContext;
use crate::propagation::ReadDomains;
use crate::propagators::nogoods::NogoodId;
use crate::propagators::nogoods::NogoodPropagator;
use crate::propagators::nogoods::NogoodPropagatorStatistics;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::state::PropagationStatusCP;
use crate::variables::DomainId;

/// Determines the different type of resolution-based analysis modes that are supported.
#[derive(Debug, Clone, Copy)]
pub enum AnalysisMode {
    /// Standard conflict analysis which returns as soon as the first unit implication point is
    /// found (i.e. when a nogood is created which only contains a single predicate from the
    /// current decision level).
    OneUIP,
    /// An alternative to 1-UIP which stops as soon as the learned nogood only creates decision
    /// predicates.
    AllDecision,
    /// Learns CPIP nogoods (i.e., nogoods which only have predicates from the current decision
    /// level which reason over a single variable when learning) in combination with extended nogood
    /// propagation.
    ExtendedCPIP,
    /// Learns 1UIP nogoods in combination with extended nogood and applies extended nogood
    /// propagation whenever possible.
    ExtendedOneUIP,
    /// Learns CPIP nogoods in combination with extended nogood propagation but rather than stopping
    /// at the first point where extended nogood propagation can take place, it stops when
    /// extended nogood propagation can adjust a bound upon learning.
    BoundsExtendedCPIP,
}

impl AnalysisMode {
    /// Returns whether CPIP nogoods are created by the [`AnalysisMode`].
    pub fn uses_cpip(&self) -> bool {
        matches!(
            self,
            AnalysisMode::ExtendedCPIP | AnalysisMode::BoundsExtendedCPIP
        )
    }

    /// Returns whether the provided [`Predicate`] (which became true at `decision_level`) should
    /// be processed further.
    ///
    /// If false is returned, then the provided [`Predicate`] is added directly to the nogood.
    pub fn predicate_should_be_processed(
        &self,
        predicate: Predicate,
        decision_level: usize,
        context: &ConflictAnalysisContext,
    ) -> bool {
        match self {
            AnalysisMode::OneUIP
            | AnalysisMode::ExtendedCPIP
            | AnalysisMode::ExtendedOneUIP
            | AnalysisMode::BoundsExtendedCPIP => {
                // The predicate should be processed further if it is not from the current decision
                // level
                decision_level == context.get_checkpoint()
            }
            AnalysisMode::AllDecision => {
                // The predicate should be processed further if it is not a decision
                !context.is_decision_predicate(predicate)
            }
        }
    }

    /// Returns whether to continue resolving.
    pub fn should_continue_resolving(
        &self,
        to_process_heap: &KeyValueHeap<PredicateId, u32>,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> bool {
        match self {
            AnalysisMode::OneUIP | AnalysisMode::ExtendedOneUIP => {
                // We wait until there is only a single element from the current decision level
                // left.
                to_process_heap.num_nonremoved_elements() > 1
            }
            AnalysisMode::AllDecision => {
                // We wait until there are only decisions left.
                to_process_heap.num_nonremoved_elements() > 0
            }
            AnalysisMode::ExtendedCPIP => {
                // We wait until there are only elements over a single variable left.
                //
                // TODO: compute this incrementally
                to_process_heap
                    .keys()
                    .map(|predicate_id| {
                        predicate_id_generator
                            .get_predicate(predicate_id)
                            .get_domain()
                    })
                    .unique()
                    .count()
                    > 1
            }
            AnalysisMode::BoundsExtendedCPIP => {
                // We wait until extended nogood propagation can propagate a bound.
                //
                // Firstly, there should be only elements over a single element.
                // Secondly, one of the following should hold:
                // - There is a lower-bound present but no upper-bound OR there is an upper-bound
                //   present but no lower-bound
                // - There are only holes present
                // - There is an equality present (would necessarily lead to a single predicate due
                //   to semantic minimisation)
                let present_domain_ids = to_process_heap
                    .keys()
                    .map(|predicate_id| {
                        predicate_id_generator
                            .get_predicate(predicate_id)
                            .get_domain()
                    })
                    .unique()
                    .collect::<Vec<_>>();
                if present_domain_ids.len() > 1 {
                    true
                } else {
                    // We calculate the number of predicate types from the current decision
                    // level (note that they are necessarily over a
                    // single variable) to determine when bound
                    // propagation can take place.
                    let (mut lower_bounds, mut upper_bounds, mut _disequalities, mut equalities) =
                        (0, 0, 0, 0);
                    for predicate_id in to_process_heap.keys() {
                        let predicate = predicate_id_generator.get_predicate(predicate_id);
                        match predicate.get_predicate_type() {
                            PredicateType::LowerBound => lower_bounds += 1,
                            PredicateType::NotEqual => _disequalities += 1,
                            PredicateType::Equal => equalities += 1,
                            PredicateType::UpperBound => upper_bounds += 1,
                        }
                    }
                    // We return true if we cannot propagate any bounds
                    //
                    // We can propagate bounds in the following situations:
                    // - There is a lower-bound present but no upper-bound OR there is an
                    //   upper-bound present but no lower-bound
                    // - There are only holes present
                    // - There is an equality present (would necessarily lead to a single
                    // predicate due to semantic minimisation)
                    !((lower_bounds > 0 && upper_bounds == 0)
                        || (lower_bounds == 0 && upper_bounds > 0)
                        || (lower_bounds == 0 && upper_bounds == 0)
                        || equalities > 0)
                }
            }
        }
    }

    /// Removes the left-over predicates in `to_process_heap` after
    /// [`AnalysisMode::should_continue_resolving`] returned false (e.g., when finding the 1UIP, the
    /// `to_process_heap` will contain the asserting predicate) and returns the number of
    /// elements which were left in the `to_process_heap`.
    pub fn remove_final_predicates(
        &self,
        to_process_heap: &mut KeyValueHeap<PredicateId, u32>,
        predicate_id_generator: &mut PredicateIdGenerator,
        processed_nogood_predicates: &mut Vec<Predicate>,
    ) -> usize {
        let num_removed = to_process_heap.num_nonremoved_elements();

        match self {
            AnalysisMode::ExtendedCPIP | AnalysisMode::BoundsExtendedCPIP => {
                // When using extended UIP, we need to ensure that all of the remaining predicates
                // are added to the domain.
                pumpkin_assert_simple!(
                    to_process_heap.num_nonremoved_elements() > 0,
                    "There should be at least one element in the final nogood"
                );
                pumpkin_assert_moderate!(
                    to_process_heap
                        .keys()
                        .map(|predicate_id| predicate_id_generator
                            .get_predicate(predicate_id)
                            .get_domain())
                        .unique()
                        .count()
                        == 1,
                    "There should be only one variable in the final nogood from teh current decision level"
                );

                let propagating_domain = predicate_id_generator
                    .get_predicate(*to_process_heap.peek_max().unwrap().0)
                    .get_domain();

                // We need to add all of the remaining predicates to the nogood; due to the way in
                // which the extended UIP is calculated, this could be multiple elements.
                while to_process_heap.num_nonremoved_elements() > 0 {
                    let predicate = Self::pop_predicate_from_conflict_nogood(
                        to_process_heap,
                        predicate_id_generator,
                    );
                    pumpkin_assert_simple!(predicate.get_domain() == propagating_domain);
                    processed_nogood_predicates.push(predicate);
                }
            }
            AnalysisMode::OneUIP | AnalysisMode::AllDecision | AnalysisMode::ExtendedOneUIP => {
                if to_process_heap.num_nonremoved_elements() > 0 {
                    let last_predicate = Self::pop_predicate_from_conflict_nogood(
                        to_process_heap,
                        predicate_id_generator,
                    );
                    processed_nogood_predicates.push(last_predicate);
                } else {
                    pumpkin_assert_simple!(
                        matches!(self, AnalysisMode::AllDecision),
                        "If the heap is empty when extracting the final nogood then we should be performing all decision learning"
                    )
                }
            }
        }

        num_removed
    }

    /// Removes the element with the highest value from `to_process_heap` and returns the
    /// corresponding [`Predicate`].
    pub fn pop_predicate_from_conflict_nogood(
        to_process_heap: &mut KeyValueHeap<PredicateId, u32>,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> Predicate {
        let next_predicate_id = to_process_heap.pop_max().unwrap();
        predicate_id_generator.get_predicate(next_predicate_id)
    }

    /// Returns a [`WatcherProcessingStatus`] based on the [`Predicate`] pointed to by `index` in
    /// `nogood_predicates`.
    pub(crate) fn process_potential_watcher(
        &self,
        context: &mut PropagationContext,
        nogood_predicates: &[PredicateId],
        index: usize,
    ) -> WatcherProcessingStatus {
        match self {
            AnalysisMode::ExtendedCPIP | AnalysisMode::BoundsExtendedCPIP => {
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
            AnalysisMode::OneUIP | AnalysisMode::AllDecision | AnalysisMode::ExtendedOneUIP => {
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
            AnalysisMode::ExtendedCPIP | AnalysisMode::BoundsExtendedCPIP => {
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
            AnalysisMode::OneUIP | AnalysisMode::AllDecision | AnalysisMode::ExtendedOneUIP => {
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
            AnalysisMode::ExtendedCPIP | AnalysisMode::BoundsExtendedCPIP => {
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
            AnalysisMode::OneUIP | AnalysisMode::AllDecision | AnalysisMode::ExtendedOneUIP => {}
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
            AnalysisMode::ExtendedCPIP | AnalysisMode::BoundsExtendedCPIP => lbd_helper
                .compute_lbd(
                    &nogood
                        .iter()
                        .filter(|predicate| context.evaluate_predicate(**predicate).is_some())
                        .copied()
                        .collect::<Vec<_>>(),
                    context,
                ),
            AnalysisMode::OneUIP | AnalysisMode::AllDecision | AnalysisMode::ExtendedOneUIP => {
                // Skip the zero-th predicate since it is unassigned,
                // but will be assigned at the level of the predicate at index one.
                lbd_helper.compute_lbd(&nogood[1..], context)
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
