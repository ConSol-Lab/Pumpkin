use std::ops::Deref;

use super::ConflictResolver;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::moving_averages::MovingAverage;
use crate::branching::Brancher;
use crate::containers::KeyValueHeap;
use crate::containers::StorageKey;
use crate::engine::Lbd;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::MinimisationContext;
use crate::engine::conflict_analysis::Mode;
use crate::engine::conflict_analysis::NogoodMinimiser;
use crate::engine::conflict_analysis::RecursiveMinimiser;
use crate::engine::constraint_satisfaction_solver::NogoodLabel;
use crate::engine::propagation::CurrentNogood;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::proof::RootExplanationContext;
use crate::proof::explain_root_assignment;
use crate::propagators::nogoods::NogoodPropagator;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::state::PropagatorHandle;

/// Resolve conflicts according to the CDCL procedure.
///
/// This conflict resolver will derive a nogood that is implied by the constraints already present
/// in the solver. This new nogood is added as a constraint to the solver, and the solver
/// backtracks to the decision level at which the new constraint propagates.
#[derive(Clone, Debug)]
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
    /// The handle to the nogood propagator that learned nogoods are added to.
    nogood_propagator_handle: PropagatorHandle<NogoodPropagator>,
    /// Computes the LBD for nogoods.
    lbd_helper: Lbd,
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

impl ConflictResolver for ResolutionResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        let learned_nogood = self.learn_nogood(context);

        let current_checkpoint = context.state.get_checkpoint();
        context
            .counters
            .learned_clause_statistics
            .average_backtrack_amount
            .add_term((current_checkpoint - learned_nogood.backjump_level) as u64);

        context.counters.engine_statistics.sum_of_backjumps +=
            current_checkpoint as u64 - 1 - learned_nogood.backjump_level as u64;
        if current_checkpoint - learned_nogood.backjump_level > 1 {
            context.counters.engine_statistics.num_backjumps += 1;
        }

        // important to notify about the conflict _before_ backtracking removes literals from
        // the trail -> although in the current version this does nothing but notify that a
        // conflict happened
        context.restart_strategy.notify_conflict(
            self.lbd_helper
                .compute_lbd(&learned_nogood.predicates, &context.state.assignments),
            context.state.assignments.get_pruned_value_count(),
        );

        context.backtrack(learned_nogood.backjump_level);

        let constraint_tag = context
            .proof_log
            .log_deduction(
                learned_nogood.predicates.iter().copied(),
                &context.state.variable_names,
                &mut context.state.constraint_tags,
            )
            .expect("Failed to write proof log");

        let inference_code = context
            .state
            .create_inference_code(constraint_tag, NogoodLabel);

        if learned_nogood.predicates.len() == 1 {
            let _ = context
                .unit_nogood_inference_codes
                .insert(!learned_nogood.predicates[0], inference_code);
        }

        context
            .counters
            .learned_clause_statistics
            .num_unit_nogoods_learned += (learned_nogood.predicates.len() == 1) as u64;

        context
            .counters
            .learned_clause_statistics
            .average_learned_nogood_length
            .add_term(learned_nogood.predicates.len() as u64);

        self.add_learned_nogood(context, learned_nogood, inference_code);
    }
}

impl ResolutionResolver {
    pub(crate) fn new(
        nogood_propagator_handle: PropagatorHandle<NogoodPropagator>,
        mode: AnalysisMode,
    ) -> Self {
        Self {
            nogood_propagator_handle,
            mode,
            to_process_heap: Default::default(),
            predicate_id_generator: Default::default(),
            processed_nogood_predicates: Default::default(),
            recursive_minimiser: Default::default(),
            reason_buffer: Default::default(),
            lbd_helper: Default::default(),
        }
    }

    pub(crate) fn learn_nogood(&mut self, context: &mut ConflictAnalysisContext) -> LearnedNogood {
        self.clean_up();

        let conflict_nogood = context.get_conflict_nogood();

        // Initialise the data structures with the conflict nogood.
        for predicate in conflict_nogood.iter() {
            let should_explain = context.proof_log.is_logging_inferences();
            self.add_predicate_to_conflict_nogood(
                *predicate,
                context.brancher,
                self.mode,
                &mut RootExplanationContext {
                    proof_log: context.proof_log,
                    unit_nogood_inference_codes: context.unit_nogood_inference_codes,
                    state: context.state,
                },
                should_explain,
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

            // 2) Get the reason for the predicate and add it to the nogood.
            self.reason_buffer.clear();
            ConflictAnalysisContext::get_propagation_reason(
                next_predicate,
                CurrentNogood::new(
                    &self.to_process_heap,
                    &self.processed_nogood_predicates,
                    &self.predicate_id_generator,
                ),
                context.proof_log,
                context.unit_nogood_inference_codes,
                &mut self.reason_buffer,
                context.state,
            );

            for i in 0..self.reason_buffer.len() {
                let should_explain = context.proof_log.is_logging_inferences();
                self.add_predicate_to_conflict_nogood(
                    self.reason_buffer[i],
                    context.brancher,
                    self.mode,
                    &mut RootExplanationContext {
                        proof_log: context.proof_log,
                        unit_nogood_inference_codes: context.unit_nogood_inference_codes,
                        state: context.state,
                    },
                    should_explain,
                );
            }
        }

        self.extract_final_nogood(context)
    }

    fn add_learned_nogood(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: LearnedNogood,
        inference_code: InferenceCode,
    ) {
        let (nogood_propagator, mut propagation_context) = context
            .state
            .get_propagator_mut_with_context(self.nogood_propagator_handle);
        let nogood_propagator =
            nogood_propagator.expect("nogood propagator handle should refer to nogood propagator");

        nogood_propagator.add_asserting_nogood(
            learned_nogood.predicates,
            inference_code,
            &mut propagation_context,
            context.counters,
        );
    }

    /// Clears all data structures to prepare for the new conflict analysis.
    fn clean_up(&mut self) {
        self.processed_nogood_predicates.clear();
        self.predicate_id_generator.clear();
        self.to_process_heap.clear();
    }

    /// Add the predicate to the current conflict nogood if we know it needs to be added.
    ///
    /// If a `root_explanation_context` is provided, then root-level assignments are explained as
    /// well in the proof log.
    fn add_predicate_to_conflict_nogood(
        &mut self,
        predicate: Predicate,
        brancher: &mut dyn Brancher,
        mode: AnalysisMode,
        context: &mut RootExplanationContext,
        should_explain: bool,
    ) {
        let dec_level = context
            .state
            .assignments
            .get_checkpoint_for_predicate(&predicate)
            .unwrap_or_else(|| {
                panic!(
                    "Expected predicate {predicate} to be assigned but bounds were ({}, {})",
                    context
                        .state
                        .assignments
                        .get_lower_bound(predicate.get_domain()),
                    context
                        .state
                        .assignments
                        .get_upper_bound(predicate.get_domain()),
                )
            });
        // Ignore root level predicates.
        if dec_level == 0 {
            // do nothing, only possibly explain the predicate in the proof
            if should_explain {
                explain_root_assignment(context, predicate);
            }
        }
        // 1UIP
        // If the variables are from the current decision level then we want to potentially add
        // them to the heap, otherwise we add it to the predicates from lower-decision levels
        //
        // All-decision Learning
        // If the variables are not decisions then we want to potentially add them to the heap,
        // otherwise we add it to the decision predicates which have been discovered previously
        else if match mode {
            AnalysisMode::OneUIP => dec_level == context.state.assignments.get_checkpoint(),
            AnalysisMode::AllDecision => {
                !context.state.assignments.is_decision_predicate(&predicate)
            }
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

                let trail_position = context
                    .state
                    .assignments
                    .get_trail_position(&predicate)
                    .unwrap();

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
                let heap_value =
                    if context.state.assignments.trail[trail_position].predicate == predicate {
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
        self.predicate_id_generator.get_predicate(next_predicate_id)
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
            pumpkin_assert_simple!(
                matches!(self.mode, AnalysisMode::AllDecision),
                "If the heap is empty when extracting the final nogood then we should be performing all decision learning"
            )
        }

        // First we minimise the nogood using semantic minimisation to remove duplicates but we
        // avoid equality merging (since some of these literals could potentailly be removed by
        // recursive minimisation)
        context
            .semantic_minimiser
            .set_mode(if !context.should_minimise {
                // If we do not minimise then we do the equality
                // merging in the first iteration of removing
                // duplicates
                Mode::EnableEqualityMerging
            } else {
                Mode::DisableEqualityMerging
            });
        context.semantic_minimiser.minimise(
            MinimisationContext {
                proof_log: context.proof_log,
                unit_nogood_inference_codes: context.unit_nogood_inference_codes,
                counters: context.counters,
                state: context.state,
            },
            &mut self.processed_nogood_predicates,
        );

        if context.should_minimise {
            // Then we perform recursive minimisation to remove the dominated predicates
            self.recursive_minimiser.minimise(
                MinimisationContext {
                    proof_log: context.proof_log,
                    unit_nogood_inference_codes: context.unit_nogood_inference_codes,
                    counters: context.counters,
                    state: context.state,
                },
                &mut self.processed_nogood_predicates,
            );

            // We perform a final semantic minimisation call which allows the merging of the
            // equality predicates which remain in the nogood
            let size_before_semantic_minimisation = self.processed_nogood_predicates.len();
            context
                .semantic_minimiser
                .set_mode(Mode::EnableEqualityMerging);
            context.semantic_minimiser.minimise(
                MinimisationContext {
                    proof_log: context.proof_log,
                    unit_nogood_inference_codes: context.unit_nogood_inference_codes,
                    counters: context.counters,
                    state: context.state,
                },
                &mut self.processed_nogood_predicates,
            );
            context
                .counters
                .learned_clause_statistics
                .average_number_of_removed_atomic_constraints_semantic
                .add_term(
                    (size_before_semantic_minimisation - self.processed_nogood_predicates.len())
                        as u64,
                );
        }

        let learned_nogood =
            LearnedNogood::create_from_vec(self.processed_nogood_predicates.clone(), context);

        pumpkin_assert_advanced!(
            learned_nogood
                .predicates
                .iter()
                .skip(1)
                .all(|p| context.state.assignments.is_predicate_satisfied(*p))
        );

        // TODO: asserting predicate may be bumped twice, probably not a problem.
        for predicate in learned_nogood.predicates.iter() {
            context
                .brancher
                .on_appearance_in_conflict_predicate(*predicate);
        }

        learned_nogood
    }
}

/// A structure which stores a learned nogood
///
/// There are two assumptions:
/// - The asserting literal (i.e. the literal of the current decision level) is placed at the `0`th
///   index of [`LearnedNogood::literals`].
/// - A literal from the second-highest decision level is placed at the `1`st index of
///   [`LearnedNogood::literals`].
#[derive(Clone, Debug)]
pub(crate) struct LearnedNogood {
    pub(crate) predicates: Vec<Predicate>,
    pub(crate) backjump_level: usize,
}

impl Deref for LearnedNogood {
    type Target = [Predicate];

    fn deref(&self) -> &Self::Target {
        &self.predicates
    }
}

impl LearnedNogood {
    /// Creates a [`LearnedNogood`] from the provided [`Vec`].
    ///
    /// This method automatically ensures that the invariants of nogoods hold; see [`LearnedNogood`]
    /// for more details on these invariants.
    pub(crate) fn create_from_vec(
        mut clean_nogood: Vec<Predicate>,
        context: &ConflictAnalysisContext,
    ) -> Self {
        // We perform a linear scan to maintain the two invariants:
        // - The predicate from the current decision level is placed at index 0
        // - The predicate from the highest decision level below the current is placed at index 1
        let mut index = 1;
        let mut highest_level_below_current = 0;
        while index < clean_nogood.len() {
            let predicate = clean_nogood[index];
            let dl = context
                .state
                .get_checkpoint_for_predicate(predicate)
                .unwrap();

            if dl == context.state.get_checkpoint() {
                clean_nogood.swap(0, index);
                index -= 1;
            } else if dl > highest_level_below_current {
                highest_level_below_current = dl;
                clean_nogood.swap(1, index);
            }

            index += 1;
        }

        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if clean_nogood.len() > 1 {
            context
                .state
                .get_checkpoint_for_predicate(clean_nogood[1])
                .unwrap()
        } else {
            // For unit nogoods, the solver backtracks to the root level.
            0
        };

        Self {
            predicates: clean_nogood,
            backjump_level,
        }
    }
}
