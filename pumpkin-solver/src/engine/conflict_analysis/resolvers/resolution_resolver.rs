use super::ConflictResolver;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::branching::Brancher;
use crate::containers::KeyValueHeap;
use crate::containers::StorageKey;
use crate::engine::conflict_analysis::minimisers::Mode;
use crate::engine::conflict_analysis::minimisers::RecursiveMinimiser;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::LearnedNogood;
use crate::engine::propagation::CurrentNogood;
use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// Resolve conflicts according to the CDCL procedure.
///
/// This conflict resolver will derive a nogood that is implied by the constraints already present
/// in the solver. This new nogood is added as a constraint to the solver, and the solver
/// backtracks to the decision level at which the new constraint propagates.
#[derive(Clone, Debug, Default)]
pub(crate) struct ResolutionResolver {
    /// Heap containing the predicates which still need to be processed; sorted non-increasing
    /// based on trail-index where implied predicates are processed first.
    to_process_heap: KeyValueHeap<PredicateId, u32>,
    /// The generator is used in combination with the heap to keep track of which predicates are
    /// stored in the heap.
    predicate_id_generator: PredicateIdGenerator,
    /// Predicates which have been processed and have been determined to be (potentially) part of
    /// the nogood.
    ///
    /// Note that this structure may contain duplicates which are removed at the end by semantic
    /// minimisation.
    processed_nogood_predicates: Vec<Predicate>,
    /// A minimiser which recursively determines whether a predicate is redundant in the nogood
    recursive_minimiser: RecursiveMinimiser,
    /// Whether the resolver employs 1-UIP or all-decision learning.
    mode: AnalysisMode,
    /// Re-usable buffer which reasons are written into.
    reason_buffer: Vec<Predicate>,
}

#[derive(Debug, Clone, Copy, Default)]
/// Determines which type of learning is performed by the resolver.
pub(crate) enum AnalysisMode {
    #[default]
    /// Stanard conflict analysis which returns as soon as the first unit implication point is
    /// found (i.e. when a nogood is created which only contains a single predicate from the
    /// current decision level)
    OneUIP,
    /// An alternative to 1-UIP which stops as soon as the learned nogood only creates decision
    /// predicates.
    AllDecision,
}

impl ResolutionResolver {
    pub(crate) fn with_mode(mode: AnalysisMode) -> Self {
        Self {
            mode,
            ..Default::default()
        }
    }
}

impl ConflictResolver for ResolutionResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) -> Option<LearnedNogood> {
        self.clean_up();

        // Initialise the data structures with the conflict nogood.
        for predicate in context
            .get_conflict_nogood(context.is_completing_proof)
            .iter()
        {
            self.add_predicate_to_conflict_nogood(
                *predicate,
                context.assignments,
                context.brancher,
                self.mode,
                context.is_completing_proof,
            );
        }
        // Record conflict nogood size statistics.
        let num_initial_conflict_predicates =
            self.to_process_heap.num_nonremoved_elements() + self.processed_nogood_predicates.len();
        context
            .counters
            .learned_clause_statistics
            .average_conflict_size
            .add_term(num_initial_conflict_predicates as u64);

        // In the case of 1UIP
        // Keep refining the conflict nogood until there is only one predicate from the current
        // decision level
        //
        // In the case of all-decision learning
        // Keep refining the conflict nogood until there are no non-decision predicates left
        //
        // There is an exception special case:
        // When posting the decision [x = v], it gets decomposed into two decisions ([x >= v] & [x
        // <= v]). In this case there will be two predicates left from the current decision
        // level, and both will be decisions. This is accounted for below.
        while {
            match self.mode {
                AnalysisMode::OneUIP => self.to_process_heap.num_nonremoved_elements() > 1,
                AnalysisMode::AllDecision => self.to_process_heap.num_nonremoved_elements() > 0,
            }
        } {
            // Replace the predicate from the nogood that has been assigned last on the trail.
            //
            // This is done in two steps:
            // 1) Pop the predicate last assigned on the trail from the nogood.
            let next_predicate = self.pop_predicate_from_conflict_nogood();

            // 2) Add the reason of the next_predicate to the nogood.

            // 2.a) Here we treat the special case: if the next predicate is a decision, this means
            // that we are done with analysis since the only remaining predicate in the heap is the
            // other decision.
            if context.assignments.is_decision_predicate(&next_predicate) {
                // As a simple workaround, we add the currently analysed predicate to set of
                // predicates from the lower predicate level, and stop analysis.
                //
                // Semantic minimisation will ensure the bound predicates get converted into an
                // equality decision predicate.
                while self.to_process_heap.num_nonremoved_elements() != 1 {
                    let predicate = self.pop_predicate_from_conflict_nogood();
                    let predicate_replacement = if context
                        .assignments
                        .is_decision_predicate(&predicate)
                    {
                        predicate
                    } else {
                        // Note that we decompose [x == v] into the two predicates [x >= v] and [x
                        // <= v] and that these have distinct trail entries (where [x >= v] has a
                        // lower trail position than [x <= v])
                        //
                        // However, this can lead to [x <= v] to be processed *before* [x >= v -
                        // y], meaning that these implied predicates should be replaced with their
                        // reason
                        self.reason_buffer.clear();
                        ConflictAnalysisContext::get_propagation_reason(
                            predicate,
                            context.assignments,
                            CurrentNogood::new(
                                &self.to_process_heap,
                                &self.processed_nogood_predicates,
                                &self.predicate_id_generator,
                            ),
                            context.reason_store,
                            context.propagators,
                            context.proof_log,
                            context.unit_nogood_step_ids,
                            &mut self.reason_buffer,
                        );

                        if self.reason_buffer.is_empty() {
                            // In the case when the proof is being completed, it could be the case
                            // that the reason for a root-level propagation is empty; this
                            // predicate will be filtered out by the semantic minimisation
                            pumpkin_assert_simple!(context.is_completing_proof);
                            predicate
                        } else {
                            pumpkin_assert_simple!(predicate.is_lower_bound_predicate() || predicate.is_not_equal_predicate(), "A non-decision predicate in the nogood should be either a lower-bound or a not-equals predicate but it was {predicate} with reason {:?}", self.reason_buffer);
                            pumpkin_assert_simple!(
                                self.reason_buffer.len() == 1 && self.reason_buffer[0].is_lower_bound_predicate(),
                                "The reason for the only propagated predicates left on the trail should be lower-bound predicates, but the reason for {predicate} was {:?}",
                                self.reason_buffer,
                            );

                            self.reason_buffer[0]
                        }
                    };

                    // We push to `predicates_lower_decision_level` since this structure will be
                    // used for creating the final nogood
                    self.processed_nogood_predicates.push(predicate_replacement);
                }
                // It could be the case that the final predicate in the nogood is implied, in
                // this case we eagerly replace it since the conflict analysis output assumes
                // that a single variable is propagating.
                //
                // For example, let's say we have made the decision [x == v] (which is
                // decomposed into [x >= v] and [x <= v]).
                //
                // Now let's say we have the nogood [[x>= v - 1], [x <= v]], then we have a
                // final element [x >= v - 1] left in the heap which is
                // implied. This could mean that we end up with 2 predicates in
                // the conflict nogood which goes against the 2-watcher scheme so we eagerly
                // replace it here!
                //
                // If it is an initial bound then it will be removed by semantic minimisation when
                // extracting the final nogood.
                //
                // TODO: This leads to a less general explanation!
                if !context
                    .assignments
                    .is_decision_predicate(&self.peek_predicate_from_conflict_nogood())
                    && !context
                        .assignments
                        .is_initial_bound(self.peek_predicate_from_conflict_nogood())
                {
                    let predicate = self.peek_predicate_from_conflict_nogood();

                    self.reason_buffer.clear();
                    ConflictAnalysisContext::get_propagation_reason(
                        predicate,
                        context.assignments,
                        CurrentNogood::new(
                            &self.to_process_heap,
                            &self.processed_nogood_predicates,
                            &self.predicate_id_generator,
                        ),
                        context.reason_store,
                        context.propagators,
                        context.proof_log,
                        context.unit_nogood_step_ids,
                        &mut self.reason_buffer,
                    );
                    pumpkin_assert_simple!(predicate.is_lower_bound_predicate() || predicate.is_not_equal_predicate() , "If the final predicate in the conflict nogood is not a decision predicate then it should be either a lower-bound predicate or a not-equals predicate but was {predicate}");
                    pumpkin_assert_simple!(
                        self.reason_buffer.len() == 1 && self.reason_buffer[0].is_lower_bound_predicate(),
                        "The reason for the decision predicate should be a lower-bound predicate but was {}", self.reason_buffer[0]
                    );
                    self.replace_predicate_in_conflict_nogood(predicate, self.reason_buffer[0]);
                }

                // The final predicate in the heap will get pushed in `extract_final_nogood`
                self.processed_nogood_predicates.push(next_predicate);
                break;
            }

            // 2.b) Standard case, get the reason for the predicate and add it to the nogood.
            self.reason_buffer.clear();
            ConflictAnalysisContext::get_propagation_reason(
                next_predicate,
                context.assignments,
                CurrentNogood::new(
                    &self.to_process_heap,
                    &self.processed_nogood_predicates,
                    &self.predicate_id_generator,
                ),
                context.reason_store,
                context.propagators,
                context.proof_log,
                context.unit_nogood_step_ids,
                &mut self.reason_buffer,
            );

            for i in 0..self.reason_buffer.len() {
                self.add_predicate_to_conflict_nogood(
                    self.reason_buffer[i],
                    context.assignments,
                    context.brancher,
                    self.mode,
                    context.is_completing_proof,
                );
            }
        }

        Some(self.extract_final_nogood(context))
    }

    fn process(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: &Option<LearnedNogood>,
    ) -> Result<(), ()> {
        let learned_nogood = learned_nogood.as_ref().expect("Expected nogood");

        context.backtrack(learned_nogood.backjump_level);
        Ok(())
    }
}

impl ResolutionResolver {
    /// Clears all data structures to prepare for the new conflict analysis.
    fn clean_up(&mut self) {
        self.processed_nogood_predicates.clear();
        self.predicate_id_generator.clear();
        self.to_process_heap.clear();
    }

    fn add_predicate_to_conflict_nogood(
        &mut self,
        predicate: Predicate,
        assignments: &Assignments,
        brancher: &mut dyn Brancher,
        mode: AnalysisMode,
        is_logging_complete_proof: bool,
    ) {
        let dec_level = assignments
            .get_decision_level_for_predicate(&predicate)
            .unwrap_or_else(|| {
                panic!(
                    "Expected predicate {predicate} to be assigned but bounds were ({}, {})",
                    assignments.get_lower_bound(predicate.get_domain()),
                    assignments.get_upper_bound(predicate.get_domain()),
                )
            });
        // Ignore root level predicates.
        if !is_logging_complete_proof && dec_level == 0 {
            // do nothing
        }
        // 1UIP
        // If the variables are from the current decision level then we want to potentially add
        // them to the heap, otherwise we add it to the predicates from lower-decision levels
        //
        // All-decision Learning
        // If the variables are not decisions then we want to potentially add them to the heap,
        // otherwise we add it to the decision predicates which have been discovered previously
        else if match mode {
            AnalysisMode::OneUIP => dec_level == assignments.get_decision_level(),
            AnalysisMode::AllDecision => !assignments.is_decision_predicate(&predicate),
        } {
            let predicate_id = self.predicate_id_generator.get_id(predicate);
            // The first time we encounter the predicate, we initialise its value in the
            // heap.
            //
            // Note that if the predicate is already in the heap, no action needs to be
            // taken. It can happen that a predicate is returned
            // multiple times as a reason for other predicates.

            // TODO: could improve the heap structure to be more user-friendly.

            // Here we manually adjust the size of the heap to accommodate new elements.
            while self.to_process_heap.len() <= predicate_id.index() {
                let next_id = PredicateId {
                    id: self.to_process_heap.len() as u32,
                };
                self.to_process_heap.grow(next_id, 0);
                self.to_process_heap.delete_key(next_id);
            }

            // Then we check whether the predicate was not already present in the heap, if
            // this is not the case then we insert it
            if !self.to_process_heap.is_key_present(predicate_id)
                && *self.to_process_heap.get_value(predicate_id) == 0
            {
                brancher.on_appearance_in_conflict_predicate(predicate);

                let trail_position = assignments.get_trail_position(&predicate).unwrap();

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
                let heap_value = if assignments.trail[trail_position].predicate == predicate {
                    trail_position * 2
                } else {
                    trail_position * 2 + 1
                };

                // We restore the key and since we know that the value is 0, we can safely
                // increment with `heap_value`
                self.to_process_heap.restore_key(predicate_id);
                self.to_process_heap
                    .increment(predicate_id, heap_value as u32);

                pumpkin_assert_moderate!(
                    *self.to_process_heap.get_value(predicate_id) == heap_value.try_into().unwrap(),
                    "The value in the heap should be the same as was added"
                )
            }
        } else {
            // We do not check for duplicate, we simply add the predicate.
            // Semantic minimisation will later remove duplicates and do other processing.
            self.processed_nogood_predicates.push(predicate);
        }
    }

    fn pop_predicate_from_conflict_nogood(&mut self) -> Predicate {
        let next_predicate_id = self.to_process_heap.pop_max().unwrap();
        self.predicate_id_generator
            .get_predicate(next_predicate_id)
            .unwrap()
    }

    fn peek_predicate_from_conflict_nogood(&self) -> Predicate {
        let next_predicate_id = self.to_process_heap.peek_max().unwrap().0;
        self.predicate_id_generator
            .get_predicate(*next_predicate_id)
            .unwrap()
    }

    fn replace_predicate_in_conflict_nogood(
        &mut self,
        predicate: Predicate,
        replacement: Predicate,
    ) {
        self.predicate_id_generator
            .replace_predicate(predicate, replacement);
    }

    fn extract_final_nogood(&mut self, context: &mut ConflictAnalysisContext) -> LearnedNogood {
        // The final nogood is composed of the predicates encountered from the lower decision
        // levels, plus the predicate remaining in the heap.

        // First we obtain a semantically minimised nogood.
        //
        // We reuse the vector with lower decision levels for simplicity.
        if self.to_process_heap.num_nonremoved_elements() > 0 {
            let last_predicate = self.pop_predicate_from_conflict_nogood();
            self.processed_nogood_predicates.push(last_predicate);
        } else {
            pumpkin_assert_simple!(matches!(self.mode, AnalysisMode::AllDecision), "If the heap is empty when extracting the final nogood then we should be performing all decision learning")
        }

        // First we minimise the nogood using semantic minimisation to remove duplicates but we
        // avoid equality merging (since some of these literals could potentailly be removed by
        // recursive minimisation)
        let mut clean_nogood: Vec<Predicate> = context.semantic_minimiser.minimise(
            &self.processed_nogood_predicates,
            context.assignments,
            if !context.should_minimise {
                // If we do not minimise then we do the equality
                // merging in the first iteration of removing
                // duplicates
                Mode::EnableEqualityMerging
            } else {
                Mode::DisableEqualityMerging
            },
        );

        if context.should_minimise {
            // Then we perform recursive minimisation to remove the dominated predicates
            self.recursive_minimiser
                .remove_dominated_predicates(&mut clean_nogood, context);

            // We perform a final semantic minimisation call which allows the merging of the
            // equality predicates which remain in the nogood
            let size_before_semantic_minimisation = clean_nogood.len();
            clean_nogood = context.semantic_minimiser.minimise(
                &clean_nogood,
                context.assignments,
                Mode::EnableEqualityMerging,
            );
            context
                .counters
                .learned_clause_statistics
                .average_number_of_removed_literals_semantic
                .add_term((size_before_semantic_minimisation - clean_nogood.len()) as u64);
        }

        // Sorting does the trick with placing the correct predicates at the first two positions,
        // however this can be done more efficiently, since we only need the first two positions
        // to be properly sorted.
        //
        // TODO: Do not sort but do a linear scan to find the correct placement of the predicates
        clean_nogood.sort_by_key(|p| context.assignments.get_trail_position(p).unwrap());
        clean_nogood.reverse();
        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if clean_nogood.len() > 1 {
            context
                .assignments
                .get_decision_level_for_predicate(&clean_nogood[1])
                .unwrap()
        } else {
            // For unit nogoods, the solver backtracks to the root level.
            0
        };

        pumpkin_assert_advanced!(clean_nogood
            .iter()
            .skip(1)
            .all(|p| context.assignments.is_predicate_satisfied(*p)));

        // TODO: asserting predicate may be bumped twice, probably not a problem.
        for predicate in clean_nogood.iter() {
            context
                .brancher
                .on_appearance_in_conflict_predicate(*predicate);
        }

        LearnedNogood {
            backjump_level,
            predicates: clean_nogood,
        }
    }
}
