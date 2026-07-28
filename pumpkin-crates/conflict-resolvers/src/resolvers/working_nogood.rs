use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::AnalysisMode;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::KeyValueHeap;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateIdGenerator;
use pumpkin_core::propagation::PredicateId;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::statistics::Statistic;
use pumpkin_core::statistics::StatisticLogger;
use pumpkin_core::variables::DomainId;

use crate::minimisers::IterativeMinimiser;
use crate::minimisers::ProcessingResult;

#[derive(Debug, Clone)]
pub(crate) struct WorkingNogood {
    /// Heap containing the predicates which still need to be processed; sorted non-increasing
    /// based on trail-index where implied predicates are processed first.
    pub(crate) to_process_heap: KeyValueHeap<PredicateId, u32>,
    /// Predicates which have been processed and have been determined to be (potentially) part of
    /// the nogood.
    ///
    /// Note that this structure may contain duplicates which are removed at the end by semantic
    /// minimisation.
    pub(crate) processed_nogood_predicates: Vec<Predicate>,
    /// A helper for keeping track of how many [`Predicate`]s concerning a specific [`DomainId`]
    /// are present in the working nogood.
    ///
    /// This is used when determining when to stop resolving when using CPIP learning (see
    /// [`AnalysisMode::CPIP`]).
    pub(crate) unique_variable_helper: HashMap<DomainId, u32>,
    /// Whether to perform iterative minimisation.
    ///
    /// Iterative minimisation is semantic minimisation applied *while* resolving.
    pub(crate) iterative_minimisation: bool,
    /// The structure used for iterative minimisation.
    pub(crate) iterative_minimiser: IterativeMinimiser,
    pub(crate) iterative_minimisation_statistics: IterativeMinimisationStatistics,
}

create_statistics_struct!(IterativeMinimisationStatistics {
    /// The number of removed predicates by iterative minimisation.
    num_removed: usize,
    /// The number of removed predicates from the current decision level.
    num_removed_current_decision_level: usize,
    /// The number of removed predicates from the previous decision level.
    num_removed_previous_decision_level: usize
});

/// Indicates whether a [`Predicate`] is redundant or non-redundant when adding it to the nogood.
#[derive(Clone, Copy, Debug)]
pub(crate) enum IterativeRedundancyStatus {
    Redundant,
    NonRedundant,
}

/// Indicates whether a [`Predicate`] was replaced during iterative minimisation.
///
/// If we have the case that the predicates [x >= v] and [x <= v] are present in the nogood, then
/// we can potentially replace it with [x == v]. However, if [x == v] would be the next predicate
/// to be resolved upon, then we cannot add it to the nogood. Hence, we indicate whether this
/// replacement is possible using this enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ReplacementStatus {
    NotReplaced,
    Replaced,
}

impl WorkingNogood {
    pub(crate) fn log_statistics(&self, statistic_logger: StatisticLogger) {
        if self.iterative_minimisation {
            self.iterative_minimisation_statistics
                .log(statistic_logger.clone());
            self.iterative_minimiser
                .log_statistics(statistic_logger.clone());
        }
    }

    pub(crate) fn clean_up(&mut self) {
        self.processed_nogood_predicates.clear();
        self.to_process_heap.clear();

        // TODO: make more efficient
        if self.iterative_minimisation {
            self.iterative_minimiser.clear();
        }
        self.unique_variable_helper.clear();
    }

    /// Add the predicate to the current conflict nogood if we know it needs to be added.
    ///
    /// If a `root_explanation_context` is provided, then root-level assignments are explained as
    /// well in the proof log.
    pub(crate) fn add_predicate_to_conflict_nogood(
        &mut self,
        predicate: Predicate,
        mode: AnalysisMode,
        context: &mut ConflictAnalysisContext,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) {
        let dec_level = context
            .get_checkpoint_for_predicate(predicate)
            .unwrap_or_else(|| {
                panic!(
                    "Expected predicate {predicate} to be assigned but bounds were ({}, {})",
                    context.lower_bound(&predicate.get_domain()),
                    context.lower_bound(&predicate.get_domain()),
                )
            });
        // Ignore root level predicates.
        if dec_level == 0 {
            context.explain_root_assignment(predicate);

            if self.iterative_minimisation {
                self.iterative_minimiser.apply_predicate(predicate, context);
            }
        }
        // 1UIP
        // If the variables are from the current decision level then we want to potentially add
        // them to the heap, otherwise we add it to the predicates from lower-decision levels
        //
        // All-decision Learning
        // If the variables are not decisions then we want to potentially add them to the heap,
        // otherwise we add it to the decision predicates which have been discovered previously
        else if mode.predicate_should_be_processed(predicate, dec_level, context) {
            let predicate_id = predicate_id_generator.get_id(predicate);
            // The first time we encounter the predicate, we initialise its value in the
            // heap.
            //
            // Note that if the predicate is already in the heap, no action needs to be
            // taken. It can happen that a predicate is returned
            // multiple times as a reason for other predicates.

            // TODO: could improve the heap structure to be more user-friendly.

            // Here we manually adjust the size of the heap to accommodate new elements.
            while self.to_process_heap.len() <= predicate_id.index() {
                let next_id = PredicateId::create_from_index(self.to_process_heap.len());
                self.to_process_heap.grow(next_id, 0);
                self.to_process_heap.delete_key(next_id);
            }

            // Then we check whether the predicate was not already present in the heap, if
            // this is not the case then we insert it
            if !self.to_process_heap.is_key_present(predicate_id)
                && *self.to_process_heap.get_value(predicate_id) == 0
            {
                if let IterativeRedundancyStatus::Redundant = self.check_for_iterative_redundancy(
                    predicate,
                    context,
                    predicate_id,
                    predicate_id_generator,
                    mode,
                ) {
                    if dec_level == context.get_checkpoint() {
                        self.iterative_minimisation_statistics
                            .num_removed_current_decision_level += 1
                    } else {
                        self.iterative_minimisation_statistics
                            .num_removed_previous_decision_level += 1
                    }
                    return;
                }

                context.predicate_appeared_in_conflict(predicate);

                // The goal is to traverse predicate in reverse order of the trail.
                //
                // However some predicates may share the trail position. For example, if a
                // predicate that was posted to trail resulted in
                // some other predicates being true, then all
                // these predicates would have the same trail position.
                //
                // When considering the predicates in reverse order of the trail, the
                // implicitly set predicates are posted after the
                // explicitly set one, but they all have the same
                // trail position.
                //
                // To remedy this, we make a tie-breaking scheme to prioritise implied
                // predicates over explicit predicates. This is done
                // by assigning explicitly set predicates the
                // value `2 * trail_position`, whereas implied predicates get `2 *
                // trail_position + 1`.
                let heap_value = get_heap_value(predicate, context);

                // We restore the key and since we know that the value is 0, we can safely
                // increment with `heap_value`
                self.to_process_heap.restore_key(predicate_id);
                self.to_process_heap.increment(predicate_id, heap_value);
                mode.add_predicate_to_nogood(predicate, &mut self.unique_variable_helper);

                pumpkin_assert_moderate!(
                    *self.to_process_heap.get_value(predicate_id) == heap_value,
                    "The value in the heap should be the same as was added"
                )
            }
        } else {
            // We do not check for duplicate, we simply add the predicate.
            // Semantic minimisation will later remove duplicates and do other processing.
            self.processed_nogood_predicates.push(predicate);
        }
    }

    pub(crate) fn pop_predicate_from_conflict_nogood(
        &mut self,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) -> Predicate {
        let next_predicate_id = self.to_process_heap.pop_max().unwrap();
        let predicate = predicate_id_generator.get_predicate(next_predicate_id);
        mode.remove_predicate_from_nogood(predicate, &mut self.unique_variable_helper);
        predicate
    }

    /// Checks whether the provided [`Predicate`] is redundant given the current working nogood.
    pub(crate) fn check_for_iterative_redundancy(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext<'_>,
        predicate_id: PredicateId,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) -> IterativeRedundancyStatus {
        if !self.iterative_minimisation {
            return IterativeRedundancyStatus::NonRedundant;
        }

        // We ask the iterative minimiser the status of the predicate.
        let process_predicate = self
            .iterative_minimiser
            .process_predicate(predicate, context);

        // Based on the status, we proceed accordingly.
        match process_predicate {
            ProcessingResult::Redundant => {
                // The provided predicate is redundant.
                //
                // The key is not currently present, but it has been assigned a value; we need
                // to reset that value to 0.
                if predicate_id.index() < self.to_process_heap.len() {
                    self.to_process_heap.set_value(predicate_id, 0);
                }
                // Then we delete the key if it was present.
                self.to_process_heap.delete_key(predicate_id);

                self.iterative_minimisation_statistics.num_removed += 1;

                // We know that the element is redundant, so we can indicate that it does not need
                // to be processed.
                IterativeRedundancyStatus::Redundant
            }
            ProcessingResult::ReplacedPresent { removed } => {
                // First, we remove the predicates.
                self.remove_predicates(removed, predicate_id_generator, mode);

                // Then we apply the provided predicate to the domain after removing all of the
                // previous predicates.
                self.iterative_minimiser.apply_predicate(predicate, context);

                // We also know that the provided predicate is not redundant so we can add it to
                // the nogood.
                IterativeRedundancyStatus::NonRedundant
            }
            ProcessingResult::PossiblyReplacedWithNew {
                potentially_removed: previous,
                new_predicate,
                removed,
            } => {
                // Adding the new predicate would lead it to be replaced with another predicate
                // (e.g. we are adding [x >= 5] and run into the situation that [x >= 5] /\ [x != 5]
                // -> [x >= 6] occurs; in this case, the removed predicate would be [x != 5] and the
                // new_predicate would be [x >= 6]).
                //
                // It is important to not enter a loop here due to implied predicates. In the
                // previous example, if the next predicate to be resolved upon is [x >= 6], then a
                // loop could be entered in which [x >= 5] /\ [x != 5] are merged into [x >= 6],
                // after which the reason for [x >= 6] is [x >= 5] /\ [x != 5] -> [x >= 6], which
                // are then merged back, etc.
                //
                // To resolve this issue, we only replace the provided predicate when the new
                // predicate would not be the next one to be resolved upon.
                self.iterative_minimisation_statistics.num_removed += 1;

                // First, we remove the predicates that are removed either way.
                self.remove_predicates(removed, predicate_id_generator, mode);

                // We split into two cases:
                // 1. The new predicate is of the current decision level.
                // 2. The new predicate is of a previous decision level.
                if context.get_checkpoint_for_predicate(new_predicate).unwrap()
                    == context.get_checkpoint()
                {
                    // Next, we check whether we can replace the elements with `new_predicate`.
                    if ReplacementStatus::Replaced
                        == self.replace_if_possible_current_level(
                            context,
                            previous,
                            new_predicate,
                            predicate_id_generator,
                            mode,
                        )
                    {
                        self.to_process_heap.set_value(predicate_id, 0);
                        self.to_process_heap.delete_key(predicate_id);

                        // We can replace the elements with `new_predicate`, so we indicate that we
                        // do not need to add `predicate`.
                        IterativeRedundancyStatus::Redundant
                    } else {
                        // We cannot replace the elements, so we add `predicate` to the iterative
                        // minimiser.
                        self.iterative_minimiser.apply_predicate(predicate, context);

                        // And we indicate that we need to add `predicate` to the nogood.
                        IterativeRedundancyStatus::NonRedundant
                    }
                } else {
                    // If `new_predicate` is from a previous decision level, then we can always
                    // replace the elements, so we do this directly.
                    self.replace_previous_level(
                        context,
                        previous,
                        new_predicate,
                        predicate_id_generator,
                        mode,
                    );

                    self.to_process_heap.set_value(predicate_id, 0);
                    self.to_process_heap.delete_key(predicate_id);

                    // And we indicate that we do not need to add `predicate` to the nogood.
                    IterativeRedundancyStatus::Redundant
                }
            }
            ProcessingResult::NotRedundant => {
                // `predicate` is not redundant and we can add it directly to the nogood.
                self.iterative_minimiser.apply_predicate(predicate, context);
                IterativeRedundancyStatus::NonRedundant
            }
        }
    }

    /// Removes the provided predicates from the predicates to be resolved upon, or the ones
    /// already in the nogood.
    fn remove_predicates(
        &mut self,
        removed: Vec<Predicate>,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) {
        // The provided predicate has replaced a multitude of other predicates -> we need
        // to remove all of these predicates.
        //
        // Hence, we go over all of the removed predicates.
        for removed_predicate in removed {
            self.iterative_minimisation_statistics.num_removed += 1;

            // And we also remove it from the iterative minimiser itself.
            self.iterative_minimiser.remove_predicate(removed_predicate);

            let removed_id = predicate_id_generator.get_id(removed_predicate);
            // We differentiate between two cases:
            // 1. The removed predicate is from the current decision level and we need to remove it
            //    from the heap.
            // 2. The removed predicate is from the previous decision level, and we remove it from
            //    there.
            if self.to_process_heap.is_key_present(removed_id) {
                mode.remove_predicate_from_nogood(
                    removed_predicate,
                    &mut self.unique_variable_helper,
                );
                // The key is not currently present, but it has been assigned a value; we
                // need to reset that value to 0.
                if removed_id.index() < self.to_process_heap.len() {
                    self.to_process_heap.set_value(removed_id, 0);
                }
                self.to_process_heap.delete_key(removed_id);
            } else if let Some(position) = self
                .processed_nogood_predicates
                .iter()
                .position(|predicate| *predicate == removed_predicate)
            {
                let _ = self.processed_nogood_predicates.remove(position);
            }
        }
    }

    /// Replaces the provided `element` with `new_predicate` if `new_predicate` would not be the
    /// next element to be resolved upon.
    fn replace_if_possible_current_level(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        element: Predicate,
        new_predicate: Predicate,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) -> ReplacementStatus {
        // We first calculate the value in the heap of the new_predicate.
        let heap_value = get_heap_value(new_predicate, context);

        // Then we check whether this is a lower value than the current maximum in the heap.
        //
        // If it has a higher value than the current maximum in the heap, then we check whether
        // there is only a single element in the heap, and it is the `element` that we are trying
        // to replace (i.e., replacing `element` with `new_predicate` would make `new_predicate`
        // the asserting atomic constraint).
        if heap_value
            < self
                .to_process_heap
                .peek_max()
                .map(|(_, value)| *value)
                .unwrap_or_default()
            || (self.to_process_heap.num_nonremoved_elements() == 1
                && self
                    .to_process_heap
                    .peek_max()
                    .map(|(&predicate_id, _)| {
                        predicate_id == predicate_id_generator.get_id(element)
                    })
                    .unwrap())
        {
            // If it is, then we can safely replace `element` with `new_predicate`
            //
            // First, we remove `element`.
            if context.get_checkpoint_for_predicate(element).unwrap() == context.get_checkpoint() {
                let element_id = predicate_id_generator.get_id(element);
                if self.to_process_heap.is_key_present(element_id) {
                    mode.remove_predicate_from_nogood(element, &mut self.unique_variable_helper);
                }

                // The key is not currently present, but it has been assigned a value; we
                // need to reset that value to 0.
                if element_id.index() < self.to_process_heap.len() {
                    self.to_process_heap.set_value(element_id, 0);
                }
                self.to_process_heap.delete_key(element_id);
            } else {
                if let Some(index) = self
                    .processed_nogood_predicates
                    .iter()
                    .position(|predicate| *predicate == element)
                {
                    let _ = self.processed_nogood_predicates.remove(index);
                }
            }
            self.iterative_minimiser.remove_predicate(element);

            // Then we add it to the current nogood.
            self.add_predicate_to_conflict_nogood(
                new_predicate,
                mode,
                context,
                predicate_id_generator,
            );

            // And we return that `element` was removed.
            return ReplacementStatus::Replaced;
        }

        // `new_predicate` would be the element to be removed, so we cannot replace `element`.
        ReplacementStatus::NotReplaced
    }

    /// Replaces `element` with `new_predicate`, where we know that `new_predicate` and `element`
    /// are from the previous checkpoint.
    fn replace_previous_level(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        element: Predicate,
        new_predicate: Predicate,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) {
        pumpkin_assert_simple!(
            get_heap_value(new_predicate, context)
                < self
                    .to_process_heap
                    .peek_max()
                    .map(|(_, value)| *value)
                    .unwrap_or(u32::MAX)
        );

        // We remove `element` from the previous decision level.
        if let Some(index) = self
            .processed_nogood_predicates
            .iter()
            .position(|predicate| *predicate == element)
        {
            let _ = self.processed_nogood_predicates.remove(index);
        }

        // And we remove `element` from the iterative minimiser.
        self.iterative_minimiser.remove_predicate(element);

        // Next, we add the `new_predicate` to the nogood.
        self.add_predicate_to_conflict_nogood(new_predicate, mode, context, predicate_id_generator);
    }
}

/// Returns the value assigned to the provided [`Predicate`] in the heap.
///
/// The idea is that implied predicates (i.e., predicates which are not explicitly on the trail)
/// should be resolved upon before the predicates which are explicitly on the trail.
///
/// Panics if the provided [`Predicate`] is not currently true on the trail.
fn get_heap_value(predicate: Predicate, context: &mut ConflictAnalysisContext<'_>) -> u32 {
    if context.get_state().is_on_trail(predicate) {
        context
            .get_state()
            .trail_position(predicate)
            .expect("Predicate should be true during conflict analysis") as u32
            * 2
    } else {
        context
            .get_state()
            .trail_position(predicate)
            .expect("Predicate should be true during conflict analysis") as u32
            * 2
            + 1
    }
}
