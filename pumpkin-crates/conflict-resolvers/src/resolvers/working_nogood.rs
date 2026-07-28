use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::AnalysisMode;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::KeyValueHeap;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicates::Lbd;
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
    /// Computes the LBD for nogoods.
    lbd_helper: Lbd,
}

create_statistics_struct!(IterativeMinimisationStatistics {
    /// The number of removed predicates by iterative minimisation.
    num_removed: usize,
    /// The number of removed predicates from the current decision level.
    num_removed_current_decision_level: usize,
    /// The number of removed predicates from the previous decision level.
    num_removed_previous_decision_level: usize
});

impl WorkingNogood {
    pub(crate) fn new(iterative_minimisation: bool) -> Self {
        Self {
            to_process_heap: Default::default(),
            processed_nogood_predicates: Default::default(),
            unique_variable_helper: Default::default(),
            iterative_minimisation,
            iterative_minimiser: Default::default(),
            iterative_minimisation_statistics: Default::default(),
            lbd_helper: Default::default(),
        }
    }
}

/// Internal methods for removing from/adding to the working nogood.
impl WorkingNogood {
    /// Adds a predicate to the current working nogood which is from the current checkpoint.
    fn add_predicate_current_checkpoint(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext<'_>,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) {
        // We first retrieve the value that the predicate will get in the heap
        let heap_value = get_heap_value(predicate, context);
        // And its corresponding predicate id
        let predicate_id = predicate_id_generator.get_id(predicate);

        // Next, we restore the key in the heap
        self.to_process_heap.restore_key(predicate_id);
        // And increment its value
        self.to_process_heap.increment(predicate_id, heap_value);

        // We also update the unique variable helper structure
        mode.add_predicate_to_nogood(predicate, &mut self.unique_variable_helper);

        // If we are performing iterative minimisation, then we also add it to the iterative
        // minimiser
        if self.iterative_minimisation {
            self.iterative_minimiser.apply_predicate(predicate, context);
        }
    }

    /// Adds a predicate to the current working nogood which is from the current checkpoint.
    fn add_predicate_previous_checkpoint(
        &mut self,
        predicate: Predicate,
        context: &ConflictAnalysisContext<'_>,
    ) {
        // We push it directly into a vector since we do not need to resolve upon it
        self.processed_nogood_predicates.push(predicate);

        // If we are performing iterative minimisation, then we also add it to the iterative
        // minimiser
        if self.iterative_minimisation {
            self.iterative_minimiser.apply_predicate(predicate, context);
        }
    }

    /// Adds a predicate to the current working nogood which is from the root-level.
    fn add_predicate_root_level(
        &mut self,
        predicate: Predicate,
        context: &ConflictAnalysisContext<'_>,
    ) {
        // We do not add it to the nogood, but we add it to the iterative minimiser if it is used
        if self.iterative_minimisation {
            self.iterative_minimiser.apply_predicate(predicate, context);
        }
    }

    /// Removes a predicate from the current working nogood which is from the current decision
    /// level.
    fn remove_predicate_from_current_checkpoint(
        &mut self,
        predicate: Predicate,
        predicate_id: PredicateId,
        mode: AnalysisMode,
    ) {
        // We first check whether the element is actually present in the heap
        if self.to_process_heap.is_key_present(predicate_id) {
            // If it is, then we update the unique variable helper structure
            mode.remove_predicate_from_nogood(predicate, &mut self.unique_variable_helper);

            // And we update the iterative minimiser if it is used
            if self.iterative_minimisation {
                self.iterative_minimiser.remove_predicate(predicate);
            }
        }

        // Next, we set its value to 0
        if predicate_id.index() < self.to_process_heap.len() {
            self.to_process_heap.set_value(predicate_id, 0);
        }
        // Then we delete the key
        self.to_process_heap.delete_key(predicate_id);
    }

    /// Removes a predicate from the current working nogood which is from the previous decision
    /// level.
    fn remove_predicate_previous_checkpoint(&mut self, removed_predicate: Predicate) {
        // We first try to find the position of the predicate
        if let Some(position) = self
            .processed_nogood_predicates
            .iter()
            .position(|predicate| *predicate == removed_predicate)
        {
            // If we can find the position, then we remove it from the processed noogod predicates
            let _ = self.processed_nogood_predicates.remove(position);

            // If we are performing iterative minimisation, then we also remove it from the
            // iterative minimiser
            if self.iterative_minimisation {
                self.iterative_minimiser.remove_predicate(removed_predicate);
            }
        }
    }
}

/// Methods for interacting with the working nogood.
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

    /// Returns the next [`Predicate`] to resolve upon based on the trail.
    pub(crate) fn pop_predicate_from_conflict_nogood(
        &mut self,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) -> Predicate {
        let next_predicate_id = self.to_process_heap.pop_max().unwrap();
        let predicate = predicate_id_generator.get_predicate(next_predicate_id);

        mode.remove_predicate_from_nogood(predicate, &mut self.unique_variable_helper);

        if self.iterative_minimisation {
            self.iterative_minimiser.remove_predicate(predicate);
        }

        predicate
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

            self.add_predicate_root_level(predicate, context);
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
                if self.is_redundant(
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

                self.add_predicate_current_checkpoint(
                    predicate,
                    context,
                    predicate_id_generator,
                    mode,
                );

                pumpkin_assert_moderate!(
                    *self.to_process_heap.get_value(predicate_id) == heap_value,
                    "The value in the heap should be the same as was added"
                )
            }
        } else {
            // We do not check for duplicate, we simply add the predicate.
            // Semantic minimisation will later remove duplicates and do other processing.
            self.add_predicate_previous_checkpoint(predicate, context);
        }
    }
}

/// Methods which reason about the semantic redundancy during conflict analysis.
impl WorkingNogood {
    /// Returns true if the provided [`Predicate`] was redundant and false otherwise.
    ///
    /// Note that this method also adjusts internal data structures
    pub(crate) fn is_redundant(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext<'_>,
        predicate_id: PredicateId,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) -> bool {
        if !self.iterative_minimisation {
            return false;
        }

        // We ask the iterative minimiser the status of the predicate.
        let process_predicate = self
            .iterative_minimiser
            .process_predicate(predicate, context);

        // Based on the status, we proceed accordingly.
        match process_predicate {
            ProcessingResult::Redundant => {
                // The provided predicate is redundant.
                self.remove_predicate_from_current_checkpoint(predicate, predicate_id, mode);

                self.iterative_minimisation_statistics.num_removed += 1;

                // We know that the element is redundant, so we can indicate that it does not need
                // to be processed.
                true
            }
            ProcessingResult::ReplacedPresent { removed } => {
                // First, we remove the predicates.
                self.remove_redundant_predicates(removed, predicate_id_generator, mode);

                // We also know that the provided predicate is not redundant so we can add it to
                // the nogood.
                false
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
                self.remove_redundant_predicates(removed, predicate_id_generator, mode);

                // We split into two cases:
                // 1. The new predicate is of the current decision level.
                // 2. The new predicate is of a previous decision level.
                if context.get_checkpoint_for_predicate(new_predicate).unwrap()
                    == context.get_checkpoint()
                {
                    // Next, we check whether we can replace the elements with `new_predicate`.
                    if self.was_replaced(
                        context,
                        previous,
                        new_predicate,
                        predicate_id_generator,
                        mode,
                    ) {
                        self.remove_predicate_from_current_checkpoint(
                            predicate,
                            predicate_id,
                            mode,
                        );

                        // We can replace the elements with `new_predicate`, so we indicate that we
                        // do not need to add `predicate`.
                        true
                    } else {
                        // And we indicate that we need to add `predicate` to the nogood.
                        false
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

                    self.remove_predicate_from_current_checkpoint(predicate, predicate_id, mode);

                    // And we indicate that we do not need to add `predicate` to the nogood.
                    true
                }
            }
            ProcessingResult::NotRedundant => false,
        }
    }

    /// Removes the provided predicates from the predicates to be resolved upon, or the ones
    /// already in the nogood.
    fn remove_redundant_predicates(
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

            let removed_id = predicate_id_generator.get_id(removed_predicate);
            // We differentiate between two cases:
            // 1. The removed predicate is from the current decision level and we need to remove it
            //    from the heap.
            // 2. The removed predicate is from the previous decision level, and we remove it from
            //    there.
            if self.to_process_heap.is_key_present(removed_id) {
                self.remove_predicate_from_current_checkpoint(removed_predicate, removed_id, mode);
            } else {
                self.remove_predicate_previous_checkpoint(removed_predicate);
            }
        }
    }

    /// Returns true if `element` could be replaced by `new_predicate` and false otherwise.
    ///
    /// A replacement is not possible if `new_predicate` would be the next element to be resolved
    /// upon when added.
    fn was_replaced(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        element: Predicate,
        new_predicate: Predicate,
        predicate_id_generator: &mut PredicateIdGenerator,
        mode: AnalysisMode,
    ) -> bool {
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

                self.remove_predicate_from_current_checkpoint(element, element_id, mode);
            } else {
                self.remove_predicate_previous_checkpoint(element);
            }

            // Then we add it to the current nogood.
            self.add_predicate_to_conflict_nogood(
                new_predicate,
                mode,
                context,
                predicate_id_generator,
            );

            // And we return that `element` was removed.
            return true;
        }

        // `new_predicate` would be the element to be removed, so we cannot replace `element`.
        false
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

    /// Calculates the literal block distance (LBD) of the nogood.
    pub(crate) fn lbd(&mut self, context: &mut ConflictAnalysisContext<'_>) -> u32 {
        self.lbd_helper
            .compute_lbd(&self.processed_nogood_predicates, context)
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
