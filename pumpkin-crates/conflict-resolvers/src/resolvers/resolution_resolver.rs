use std::ops::ControlFlow;

use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::conflict_resolving::ConflictResolver;
use pumpkin_core::containers::KeyValueHeap;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicates::Lbd;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateIdGenerator;
use pumpkin_core::propagation::PredicateId;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::CurrentNogood;
use pumpkin_core::statistics::Statistic;
use pumpkin_core::statistics::StatisticLogger;
use pumpkin_core::statistics::moving_averages::CumulativeMovingAverage;
use pumpkin_core::statistics::moving_averages::MovingAverage;

use crate::minimisers::IterativeMinimiser;
use crate::minimisers::NogoodMinimiser;
use crate::minimisers::ProcessingResult;
use crate::minimisers::RecursiveMinimiser;
use crate::minimisers::SemanticMinimisationMode;
use crate::minimisers::SemanticMinimiser;

/// [`ConflictResolver`] which resolves conflicts according to the CDCL procedure.
///
/// This conflict resolver will derive a nogood that is implied by the constraints already present
/// in the solver. This new nogood is added as a constraint to the solver, and the solver
/// backtracks to the decision level at which the new constraint propagates.
///
/// The [`ResolutionResolver`] can be used in two [`AnalysisMode`]s:
/// - [`AnalysisMode::OneUIP`] - Resolves until finding the unit implication point.
/// - [AnalysisMode::AllDecision] - Resolves until the learned nogood contains only decisions.
///
/// For an in-depth explanation and overview of CDCL and UIP, see \[1\].
///
/// # Bibliography
/// \[1\] J. Marques-Silva, I. Lynce, and S. Malik, ‘Conflict-driven clause learning SAT solvers’,
/// Handbook of satisfiability, pp. 131–153, 2009.
#[derive(Clone, Debug)]
pub struct ResolutionResolver {
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
    /// Whether the resolver employs 1-UIP or all-decision learning.
    mode: AnalysisMode,
    /// Re-usable buffer which reasons are written into.
    reason_buffer: Vec<Predicate>,
    /// Computes the LBD for nogoods.
    lbd_helper: Lbd,

    /// A minimiser which recursively determines whether a predicate is redundant in the nogood
    recursive_minimiser: RecursiveMinimiser,
    semantic_minimiser: SemanticMinimiser,

    statistics: LearnedNogoodStatistics,

    should_minimise: bool,
    iterative_minimisation: bool,

    iterative_minimiser: IterativeMinimiser,
}

impl Default for ResolutionResolver {
    fn default() -> Self {
        ResolutionResolver::new(AnalysisMode::OneUIP, true, false)
    }
}

create_statistics_struct!(
    /// The statistics related to clause learning
    LearnedNogoodStatistics {
        /// The average number of elements in the conflict explanation
        average_conflict_size: CumulativeMovingAverage<u64>,
        /// The number of learned clauses which have a size of 1
        num_unit_nogoods_learned: u64,
        /// The average length of the learned nogood
        average_learned_nogood_length: CumulativeMovingAverage<u64>,
        /// The average number of levels which have been backtracked by the solver (e.g. when a learned clause is created)
        average_backtrack_amount: CumulativeMovingAverage<u64>,
        /// The average literal-block distance (LBD) metric for newly added learned nogoods
        average_lbd: CumulativeMovingAverage<u64>,
        iterative_minimisation_statistics: IterativeMinimisationStatistics
});

create_statistics_struct!(IterativeMinimisationStatistics { num_removed: usize });

#[derive(Debug, Clone, Copy, Default)]
/// Determines which type of learning is performed by the [`ResolutionResolver`].
pub enum AnalysisMode {
    #[default]
    /// Standard conflict analysis which returns as soon as the first unit implication point is
    /// found (i.e. when a nogood is created which only contains a single predicate from the
    /// current decision level)
    OneUIP,
    /// An alternative to 1-UIP which stops as soon as the learned nogood only creates decision
    /// predicates.
    AllDecision,
}

impl ConflictResolver for ResolutionResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        self.learn_nogood(context);

        let lbd = self
            .lbd_helper
            .compute_lbd(&self.processed_nogood_predicates, context);

        // Update statistics
        self.statistics.average_lbd.add_term(lbd as u64);
        self.statistics.num_unit_nogoods_learned +=
            (self.processed_nogood_predicates.len() == 1) as u64;
        self.statistics
            .average_learned_nogood_length
            .add_term(self.processed_nogood_predicates.len() as u64);

        let backtrack_level =
            context.process_learned_nogood(self.processed_nogood_predicates.clone(), lbd);

        self.statistics
            .average_backtrack_amount
            .add_term((context.get_checkpoint() - backtrack_level) as u64);
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.statistics.log(statistic_logger.clone());
        self.semantic_minimiser
            .log_statistics(statistic_logger.clone());
        self.recursive_minimiser.log_statistics(statistic_logger);
    }
}

impl ResolutionResolver {
    pub fn new(mode: AnalysisMode, should_minimise: bool, iterative_minimisation: bool) -> Self {
        Self {
            mode,
            to_process_heap: Default::default(),
            predicate_id_generator: Default::default(),
            processed_nogood_predicates: Default::default(),
            reason_buffer: Default::default(),
            lbd_helper: Default::default(),
            recursive_minimiser: Default::default(),
            semantic_minimiser: Default::default(),
            statistics: Default::default(),
            should_minimise,
            iterative_minimisation,
            iterative_minimiser: Default::default(),
        }
    }

    pub(crate) fn learn_nogood(&mut self, context: &mut ConflictAnalysisContext) {
        self.clean_up();

        let conflict_nogood = context.get_conflict_nogood();

        // println!("======================================================================");
        // println!("C: {conflict_nogood:?}");

        // Initialise the data structures with the conflict nogood.
        for predicate in conflict_nogood.iter() {
            self.add_predicate_to_conflict_nogood(*predicate, self.mode, context);
        }

        // Record conflict nogood size statistics.
        let num_initial_conflict_predicates =
            self.to_process_heap.num_nonremoved_elements() + self.processed_nogood_predicates.len();
        self.statistics
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
            // println!(
            //     "WORKING NOGOOD: {:?}",
            //     self.processed_nogood_predicates
            //         .iter()
            //         .copied()
            //         .chain(
            //             self.to_process_heap
            //                 .keys()
            //                 .map(|id| self.predicate_id_generator.get_predicate(id))
            //         )
            //         .collect::<Vec<_>>()
            // );
            // Replace the predicate from the nogood that has been assigned last on the trail.
            //
            // This is done in two steps:
            // 1) Pop the predicate last assigned on the trail from the nogood.
            let next_predicate = self.pop_predicate_from_conflict_nogood();
            if self.iterative_minimisation {
                // println!("Removing {next_predicate:?}");
                self.iterative_minimiser.remove_predicate(next_predicate);
            }

            // 2) Get the reason for the predicate and add it to the nogood.
            self.reason_buffer.clear();

            let _ = context.get_propagation_reason(
                next_predicate,
                CurrentNogood::new(
                    &self.to_process_heap,
                    &self.processed_nogood_predicates,
                    &self.predicate_id_generator,
                ),
                &mut self.reason_buffer,
            );

            // println!("R: {:?} -> {next_predicate}", self.reason_buffer);

            for i in 0..self.reason_buffer.len() {
                self.add_predicate_to_conflict_nogood(self.reason_buffer[i], self.mode, context);
            }
        }

        self.extract_final_nogood(context);
    }

    /// Clears all data structures to prepare for the new conflict analysis.
    fn clean_up(&mut self) {
        self.processed_nogood_predicates.clear();
        self.predicate_id_generator.clear();
        self.to_process_heap.clear();

        // TODO: make more efficient
        if self.iterative_minimisation {
            self.iterative_minimiser = IterativeMinimiser::default()
        }
    }

    /// Add the predicate to the current conflict nogood if we know it needs to be added.
    ///
    /// If a `root_explanation_context` is provided, then root-level assignments are explained as
    /// well in the proof log.
    fn add_predicate_to_conflict_nogood(
        &mut self,
        predicate: Predicate,
        mode: AnalysisMode,
        context: &mut ConflictAnalysisContext,
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
            // println!("{predicate:?} ROOT LEVEL");
            self.iterative_minimiser.apply_predicate(predicate);

            context.explain_root_assignment(predicate);
        }
        // 1UIP
        // If the variables are from the current decision level then we want to potentially add
        // them to the heap, otherwise we add it to the predicates from lower-decision levels
        //
        // All-decision Learning
        // If the variables are not decisions then we want to potentially add them to the heap,
        // otherwise we add it to the decision predicates which have been discovered previously
        else {
            let predicate_id = self.predicate_id_generator.get_id(predicate);
            if self.iterative_minimisation
                && let ControlFlow::Break(_) =
                    self.check_for_iterative_redundancy(predicate, context, predicate_id)
            {
                return;
            }
            if match mode {
                AnalysisMode::OneUIP => dec_level == context.get_checkpoint(),
                AnalysisMode::AllDecision => !context.is_decision_predicate(predicate),
            } {
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

                if self.iterative_minimisation
                    || (!self.to_process_heap.is_key_present(predicate_id)
                        && *self.to_process_heap.get_value(predicate_id) == 0)
                {
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
                    self.to_process_heap.set_value(predicate_id, heap_value);

                    pumpkin_assert_simple!(
                        *self.to_process_heap.get_value(predicate_id) == heap_value,
                        "The value in the heap should be the same as was added"
                    );
                }
            } else {
                // We do not check for duplicate, we simply add the predicate.
                // Semantic minimisation will later remove duplicates and do other processing.
                self.processed_nogood_predicates.push(predicate);
            }
        }
    }

    fn check_for_iterative_redundancy(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext<'_>,
        predicate_id: PredicateId,
    ) -> ControlFlow<()> {
        let process_predicate = self
            .iterative_minimiser
            .process_predicate(predicate, context);
        // println!("\tRESULT ({predicate:?}): {process_predicate:?}");
        match process_predicate {
            ProcessingResult::Redundant => {
                if !self.to_process_heap.is_key_present(predicate_id) {
                    // println!("\t\tNot present - {predicate:?}");
                    if predicate_id.index() < self.to_process_heap.len() {
                        self.to_process_heap.set_value(predicate_id, 0);
                    }
                    self.to_process_heap.delete_key(predicate_id);
                }

                self.statistics
                    .iterative_minimisation_statistics
                    .num_removed += 1;
                return ControlFlow::Break(());
            }
            ProcessingResult::ReplacedPresent { removed } => {
                for removed_predicate in removed {
                    self.statistics
                        .iterative_minimisation_statistics
                        .num_removed += 1;
                    self.iterative_minimiser.remove_predicate(removed_predicate);
                    let removed_id = self.predicate_id_generator.get_id(removed_predicate);
                    if self.to_process_heap.is_key_present(removed_id) {
                        self.to_process_heap.delete_key(removed_id);
                    } else {
                        if let Some(position) = self
                            .processed_nogood_predicates
                            .iter()
                            .position(|predicate| *predicate == removed_predicate)
                        {
                            let _ = self.processed_nogood_predicates.remove(position);
                        }
                    }
                }

                self.iterative_minimiser.apply_predicate(predicate);
            }
            ProcessingResult::PossiblyReplacedWithNew {
                removed: previous,
                new_predicate,
            } => {
                self.statistics
                    .iterative_minimisation_statistics
                    .num_removed += 1;

                if context.get_checkpoint_for_predicate(new_predicate).unwrap()
                    == context.get_checkpoint()
                {
                    if let ControlFlow::Break(_) =
                        self.replace_if_possible_current_level(context, previous, new_predicate)
                    {
                        self.to_process_heap.set_value(predicate_id, 0);
                        self.to_process_heap.delete_key(predicate_id);
                        return ControlFlow::Break(());
                    } else {
                        self.iterative_minimiser.apply_predicate(predicate);
                    }
                } else {
                    if let ControlFlow::Break(_) =
                        self.replace_if_possible_previous_level(context, previous, new_predicate)
                    {
                        self.to_process_heap.set_value(predicate_id, 0);
                        self.to_process_heap.delete_key(predicate_id);
                        return ControlFlow::Break(());
                    } else {
                        self.iterative_minimiser.apply_predicate(predicate);
                    }
                }
            }
            ProcessingResult::NotRedundant => {
                self.iterative_minimiser.apply_predicate(predicate);
            }
        }
        ControlFlow::Continue(())
    }

    // fn check_redundancy_current_level(
    //     &mut self,
    //     predicate: Predicate,
    //     context: &mut ConflictAnalysisContext<'_>,
    // ) -> ControlFlow<()> {
    //     for element in self
    //         .to_process_heap
    //         .keys()
    //         .map(|predicate_id| self.predicate_id_generator.get_predicate(predicate_id))
    //         .filter(|element| {
    //             element.get_domain() == predicate.get_domain()
    //                 && context.get_state().trail_position(*element)
    //                     != context.get_state().trail_position(predicate)
    //         })
    //         .collect::<Vec<_>>()
    //     {
    //         match (element.get_predicate_type(), predicate.get_predicate_type()) {
    //             (PredicateType::Equal, PredicateType::NotEqual)
    //             | (PredicateType::Equal, PredicateType::UpperBound)
    //             | (PredicateType::Equal, PredicateType::LowerBound) => {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_current_dl += 1;
    //                 return ControlFlow::Break(());
    //             }
    //             (PredicateType::UpperBound, PredicateType::UpperBound) => {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_current_dl += 1;
    //                 if element.get_right_hand_side() <= predicate.get_right_hand_side() {
    //                     return ControlFlow::Break(());
    //                 } else {
    //                     let element_id = self.predicate_id_generator.get_id(element);
    //                     self.to_process_heap.delete_key(element_id);
    //                 }
    //             }
    //             (PredicateType::UpperBound, PredicateType::NotEqual) => {
    //                 if element.get_right_hand_side() == predicate.get_right_hand_side() {
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_current_dl += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_replaced_with_new_current_dl += 1;
    //                     let domain = element.get_domain();
    //
    //                     let new_predicate = predicate!(domain <= element.get_right_hand_side() -
    // 1);                     self.replace_if_possible_current_level(context, element,
    // new_predicate)?;                 } else if element.get_right_hand_side() <
    // predicate.get_right_hand_side() {                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_current_dl += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     return ControlFlow::Break(());
    //                 }
    //             }
    //             (PredicateType::LowerBound, PredicateType::LowerBound) => {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_current_dl += 1;
    //                 if element.get_right_hand_side() >= predicate.get_right_hand_side() {
    //                     return ControlFlow::Break(());
    //                 } else {
    //                     let element_id = self.predicate_id_generator.get_id(element);
    //                     self.to_process_heap.delete_key(element_id);
    //                 }
    //             }
    //             (PredicateType::LowerBound, PredicateType::NotEqual) => {
    //                 if element.get_right_hand_side() == predicate.get_right_hand_side() {
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_current_dl += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_replaced_with_new_current_dl += 1;
    //                     let domain = element.get_domain();
    //
    //                     let new_predicate = predicate!(domain >= element.get_right_hand_side() +
    // 1);                     self.replace_if_possible_current_level(context, element,
    // new_predicate)?;                 } else if element.get_right_hand_side() >
    // predicate.get_right_hand_side() {                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_current_dl += 1;
    //                     return ControlFlow::Break(());
    //                 }
    //             }
    //             (PredicateType::UpperBound, PredicateType::LowerBound)
    //                 if element.get_right_hand_side() == predicate.get_right_hand_side() =>
    //             {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_current_dl += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_replaced_with_new_current_dl += 1;
    //                 let domain = element.get_domain();
    //
    //                 let new_predicate = predicate!(domain == element.get_right_hand_side());
    //                 self.replace_if_possible_current_level(context, element, new_predicate)?;
    //             }
    //
    //             _ => {}
    //         }
    //     }
    //     ControlFlow::Continue(())
    // }

    fn replace_if_possible_current_level(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        element: Predicate,
        new_predicate: Predicate,
    ) -> ControlFlow<()> {
        let heap_value = get_heap_value(new_predicate, context);

        if heap_value
            < self
                .to_process_heap
                .peek_max()
                .map(|(_, value)| *value)
                .unwrap_or_default()
        {
            let element_id = self.predicate_id_generator.get_id(element);
            self.to_process_heap.delete_key(element_id);

            // println!("Removing previous: {element:?}");
            self.iterative_minimiser.remove_predicate(element);
            self.add_predicate_to_conflict_nogood(new_predicate, self.mode, context);

            return ControlFlow::Break(());
        }
        ControlFlow::Continue(())
    }

    // fn check_redundancy_previous_level(
    //     &mut self,
    //     predicate: Predicate,
    //     context: &mut ConflictAnalysisContext<'_>,
    // ) -> ControlFlow<()> {
    //     for element in self
    //         .processed_nogood_predicates
    //         .iter()
    //         .filter(|element| {
    //             element.get_domain() == predicate.get_domain()
    //                 && context.get_state().trail_position(**element)
    //                     != context.get_state().trail_position(predicate)
    //         })
    //         .copied()
    //         .collect::<Vec<_>>()
    //     {
    //         match (element.get_predicate_type(), predicate.get_predicate_type()) {
    //             (PredicateType::Equal, PredicateType::NotEqual)
    //             | (PredicateType::Equal, PredicateType::UpperBound)
    //             | (PredicateType::Equal, PredicateType::LowerBound) => {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_previous_dl += 1;
    //                 return ControlFlow::Break(());
    //             }
    //             (PredicateType::UpperBound, PredicateType::UpperBound) => {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_previous_dl += 1;
    //                 if element.get_right_hand_side() <= predicate.get_right_hand_side() {
    //                     return ControlFlow::Break(());
    //                 } else {
    //                     if let Some(index) = self
    //                         .processed_nogood_predicates
    //                         .iter()
    //                         .position(|predicate| *predicate == element)
    //                     {
    //                         let _ = self.processed_nogood_predicates.remove(index);
    //                     }
    //                 }
    //             }
    //             (PredicateType::UpperBound, PredicateType::NotEqual) => {
    //                 if element.get_right_hand_side() == predicate.get_right_hand_side() {
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_previous_dl += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_replaced_with_new_previous_dl += 1;
    //
    //                     let domain = element.get_domain();
    //                     let new_predicate = predicate!(domain <= element.get_right_hand_side() -
    // 1);                     self.replace_if_possible_previous_level(context, element,
    // new_predicate)?;                 } else if element.get_right_hand_side() <
    // predicate.get_right_hand_side() {                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_previous_dl += 1;
    //                     return ControlFlow::Break(());
    //                 }
    //             }
    //             (PredicateType::LowerBound, PredicateType::LowerBound) => {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_previous_dl += 1;
    //                 if element.get_right_hand_side() >= predicate.get_right_hand_side() {
    //                     return ControlFlow::Break(());
    //                 } else {
    //                     if let Some(index) = self
    //                         .processed_nogood_predicates
    //                         .iter()
    //                         .position(|predicate| *predicate == element)
    //                     {
    //                         let _ = self.processed_nogood_predicates.remove(index);
    //                     }
    //                 }
    //             }
    //             (PredicateType::LowerBound, PredicateType::NotEqual) => {
    //                 if element.get_right_hand_side() == predicate.get_right_hand_side() {
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_previous_dl += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_replaced_with_new_previous_dl += 1;
    //
    //                     let domain = element.get_domain();
    //                     let new_predicate = predicate!(domain >= element.get_right_hand_side() +
    // 1);                     self.replace_if_possible_previous_level(context, element,
    // new_predicate)?;                 } else if element.get_right_hand_side() >
    // predicate.get_right_hand_side() {                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed += 1;
    //                     self.statistics
    //                         .iterative_minimisation_statistics
    //                         .num_removed_previous_dl += 1;
    //                     return ControlFlow::Break(());
    //                 }
    //             }
    //             (PredicateType::UpperBound, PredicateType::LowerBound)
    //                 if element.get_right_hand_side() == predicate.get_right_hand_side() =>
    //             {
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_removed_previous_dl += 1;
    //                 self.statistics
    //                     .iterative_minimisation_statistics
    //                     .num_replaced_with_new_previous_dl += 1;
    //
    //                 let domain = element.get_domain();
    //                 let new_predicate = predicate!(domain == element.get_right_hand_side());
    //                 self.replace_if_possible_previous_level(context, element, new_predicate)?;
    //             }
    //
    //             _ => {}
    //         }
    //     }
    //     ControlFlow::Continue(())
    // }

    fn replace_if_possible_previous_level(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        element: Predicate,
        new_predicate: Predicate,
    ) -> ControlFlow<()> {
        let heap_value = get_heap_value(new_predicate, context);

        if heap_value
            < self
                .to_process_heap
                .peek_max()
                .map(|(_, value)| *value)
                .unwrap_or_default()
        {
            if let Some(index) = self
                .processed_nogood_predicates
                .iter()
                .position(|predicate| *predicate == element)
            {
                let _ = self.processed_nogood_predicates.remove(index);
            }

            // println!("Removing previous: {element:?}");
            self.iterative_minimiser.remove_predicate(element);

            self.add_predicate_to_conflict_nogood(new_predicate, self.mode, context);

            return ControlFlow::Break(());
        }
        ControlFlow::Continue(())
    }

    fn pop_predicate_from_conflict_nogood(&mut self) -> Predicate {
        let next_predicate_id = self.to_process_heap.pop_max().unwrap();
        self.predicate_id_generator.get_predicate(next_predicate_id)
    }

    fn extract_final_nogood(&mut self, context: &mut ConflictAnalysisContext) {
        // The final nogood is composed of the predicates encountered from the lower decision
        // levels, plus the predicate remaining in the heap.

        // First we obtain a semantically minimised nogood.
        //
        // We reuse the vector with lower decision levels for simplicity.
        // println!(
        //     "FINAL NOGOOD: {:?}",
        //     self.processed_nogood_predicates
        //         .iter()
        //         .copied()
        //         .chain(
        //             self.to_process_heap
        //                 .keys()
        //                 .map(|id| self.predicate_id_generator.get_predicate(id))
        //         )
        //         .collect::<Vec<_>>()
        // );
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
        self.semantic_minimiser.set_mode(if !self.should_minimise {
            // If we do not minimise then we do the equality
            // merging in the first iteration of removing
            // duplicates
            SemanticMinimisationMode::EnableEqualityMerging
        } else {
            SemanticMinimisationMode::DisableEqualityMerging
        });
        self.semantic_minimiser
            .minimise(context, &mut self.processed_nogood_predicates);

        if self.should_minimise {
            // Then we perform recursive minimisation to remove the dominated predicates
            self.recursive_minimiser
                .minimise(context, &mut self.processed_nogood_predicates);

            // We perform a final semantic minimisation call which allows the merging of the
            // equality predicates which remain in the nogood
            self.semantic_minimiser
                .set_mode(SemanticMinimisationMode::EnableEqualityMerging);
            self.semantic_minimiser
                .minimise(context, &mut self.processed_nogood_predicates);
        }

        pumpkin_assert_advanced!(
            self.processed_nogood_predicates
                .iter()
                .filter(|p| context.evaluate_predicate(**p) == Some(true))
                .count()
                >= self.processed_nogood_predicates.len() - 1,
            "Not all predicates evaluated to true: {:?}",
            self.processed_nogood_predicates
                .iter()
                .filter(|p| context.evaluate_predicate(**p) != Some(true))
                .collect::<Vec<_>>()
        );

        // TODO: asserting predicate may be bumped twice, probably not a problem.
        for predicate in self.processed_nogood_predicates.iter() {
            context.predicate_appeared_in_conflict(*predicate);
        }
    }
}

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
