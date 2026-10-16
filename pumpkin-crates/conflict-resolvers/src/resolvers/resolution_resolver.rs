use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::asserts::pumpkin_assert_eq_simple;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::conflict_resolving::ConflictResolver;
use pumpkin_core::containers::HashSet;
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Lbd;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateIdGenerator;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::statistics::Statistic;
use pumpkin_core::statistics::StatisticLogger;
use pumpkin_core::statistics::moving_averages::CumulativeMovingAverage;
use pumpkin_core::statistics::moving_averages::MovingAverage;
use pumpkin_core::variables::DomainId;

use crate::minimisers::NogoodMinimiser;
use crate::minimisers::RecursiveMinimiser;
use crate::minimisers::SemanticMinimisationMode;
use crate::minimisers::SemanticMinimiser;
use crate::resolvers::AnalysisMode;
use crate::resolvers::WorkingNogood;
use crate::resolvers::get_heap_value;

/// [`ConflictResolver`] which resolves conflicts according to the CDCL procedure.
///
/// This conflict resolver will derive a nogood that is implied by the constraints already present
/// in the solver. This new nogood is added as a constraint to the solver, and the solver
/// backtracks to the decision level at which the new constraint propagates.
///
/// The [`ResolutionResolver`] learns the nogoods specified by the provided [`AnalysisMode`].
///
/// For an in-depth explanation and overview of CDCL and UIP, see \[1\].
///
/// # Bibliography
/// \[1\] J. Marques-Silva, I. Lynce, and S. Malik, ‘Conflict-driven clause learning SAT solvers’,
/// Handbook of satisfiability, pp. 131–153, 2009.
#[derive(Clone, Debug)]
pub struct ResolutionResolver {
    working_nogood: WorkingNogood,
    /// The generator is used in combination with the heap to keep track of which predicates are
    /// stored in the heap.
    predicate_id_generator: PredicateIdGenerator,
    /// The type of learning that the resolver employs (e.g., 1UIP, All-decision).
    mode: AnalysisMode,
    /// Re-usable buffer which reasons are written into.
    reason_buffer: Vec<Predicate>,
    /// A minimiser which recursively determines whether a predicate is redundant in the nogood.
    recursive_minimiser: RecursiveMinimiser,
    /// A minimiser which determines whether a predicate is redundant in the nogood based on its
    /// semantic meaning.
    semantic_minimiser: SemanticMinimiser,
    /// Computes the LBD for nogoods.
    lbd_helper: Lbd,

    /// The statistics of the learned nogoods.
    statistics: LearnedNogoodStatistics,

    /// Whether nogood minimisation should be applied.
    ///
    /// Note that semantic minimisation is always applied to remove duplicates.
    recursive_minimisation: bool,

    /// Variables whose predicates are resolved away, so that no learned nogood contains them.
    ///
    /// The brancher must never make a decision over one of these variables.
    forbidden_variables: HashSet<DomainId>,
}

impl Default for ResolutionResolver {
    fn default() -> Self {
        ResolutionResolver::new(AnalysisMode::OneUIP, false, true)
    }
}

create_statistics_struct!(
    /// The statistics related to clause learning
    LearnedNogoodStatistics {
        nogood_statistics: NogoodStatistics,
        cpip_statistics: CpipStatistics,
        forbidden_variable_statistics: ForbiddenVariableStatistics,
    }
);

create_statistics_struct!(ForbiddenVariableStatistics {
    /// The number of resolutions on a predicate from the current decision level which was
    /// over a forbidden variable when the analysis mode would otherwise have stopped.
    num_resolutions_on_forbidden_asserting_predicates: usize,
    /// The number of resolutions on a predicate from a previous decision level which was over a
    /// forbidden variable.
    num_resolutions_on_forbidden_previous_checkpoint_predicates: usize,
});

create_statistics_struct!(NogoodStatistics {
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
});

create_statistics_struct!(CpipStatistics {
        /// The average number of predicates which describe the domain of the propagating variable when
        /// using CPIP learning.
        average_number_of_predicates_describing_domain_cpip: CumulativeMovingAverage<usize>,
        /// The number of nogoods which have more than one predicate concerning the propagating variable (i.e., CPIP nogoods).
        num_cpip_nogood_learned: usize,
        /// The number of nogoods which one predicate concerning the propagating variable.
        num_regular_nogood_learned: usize,
});

impl ConflictResolver for ResolutionResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        let learned_nogood = self.learn_nogood(context);

        let lbd = self.lbd_helper.compute_lbd(&learned_nogood, context);

        // Update statistics
        self.statistics
            .nogood_statistics
            .average_lbd
            .add_term(lbd as u64);
        self.statistics.nogood_statistics.num_unit_nogoods_learned +=
            (learned_nogood.len() == 1) as u64;
        self.statistics
            .nogood_statistics
            .average_learned_nogood_length
            .add_term(learned_nogood.len() as u64);

        let backtrack_level =
            context.process_learned_nogood(learned_nogood, lbd, self.mode.uses_cpip());

        self.statistics
            .nogood_statistics
            .average_backtrack_amount
            .add_term((context.get_checkpoint() - backtrack_level) as u64);
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.statistics
            .nogood_statistics
            .log(statistic_logger.clone());
        self.working_nogood.log_statistics(statistic_logger.clone());
        if self.mode.uses_cpip() {
            self.statistics
                .cpip_statistics
                .log(statistic_logger.attach_to_prefix("IterativeMinimisation"));
        }
        if !self.forbidden_variables.is_empty() {
            self.statistics
                .forbidden_variable_statistics
                .log(statistic_logger.clone());
        }

        self.semantic_minimiser
            .log_statistics(statistic_logger.clone());
        self.recursive_minimiser.log_statistics(statistic_logger);
    }
}

impl ResolutionResolver {
    pub fn new(
        mode: AnalysisMode,
        recursive_minimisation: bool,
        iterative_minimisation: bool,
    ) -> Self {
        Self {
            mode,
            predicate_id_generator: Default::default(),
            reason_buffer: Default::default(),
            recursive_minimiser: Default::default(),
            semantic_minimiser: Default::default(),
            statistics: Default::default(),
            recursive_minimisation,
            working_nogood: WorkingNogood::new(iterative_minimisation),
            lbd_helper: Default::default(),
            forbidden_variables: Default::default(),
        }
    }

    /// Sets the variables that no learned nogood may contain.
    ///
    /// The brancher must never make a decision over one of these variables; otherwise conflict
    /// analysis panics when it tries to resolve on such a decision.
    pub fn set_forbidden_variables(&mut self, forbidden_variables: HashSet<DomainId>) {
        self.forbidden_variables = forbidden_variables;
    }

    pub(crate) fn learn_nogood(&mut self, context: &mut ConflictAnalysisContext) -> Vec<Predicate> {
        self.clean_up();

        let conflict_nogood = context.get_conflict_nogood();

        // Initialise the data structures with the conflict nogood.
        for predicate in conflict_nogood.iter() {
            self.working_nogood.add_predicate_to_conflict_nogood(
                *predicate,
                self.mode,
                context,
                &mut self.predicate_id_generator,
            );
        }

        // Record conflict nogood size statistics.
        self.statistics
            .nogood_statistics
            .average_conflict_size
            .add_term(
                (self.working_nogood.num_current_checkpoint()
                    + self.working_nogood.num_previous_checkpoint()) as u64,
            );

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
        while self.mode.should_continue_resolving(
            &self.predicate_id_generator,
            &self.working_nogood,
            &self.forbidden_variables,
        ) {
            if !self.mode.should_continue_resolving_for_mode(
                &self.predicate_id_generator,
                &self.working_nogood,
            ) {
                self.statistics
                    .forbidden_variable_statistics
                    .num_resolutions_on_forbidden_asserting_predicates += 1;
            }

            // Replace the predicate from the nogood that has been assigned last on the trail.
            //
            // This is done in two steps:
            // 1) Pop the predicate last assigned on the trail from the nogood.
            let next_predicate = self
                .working_nogood
                .pop_max_predicate(&self.predicate_id_generator, self.mode);

            // 2) Get the reason for the predicate and add it to the nogood.
            self.reason_buffer.clear();

            let _ = context.get_propagation_reason(
                next_predicate,
                self.working_nogood
                    .get_current_nogood(&self.predicate_id_generator),
                &mut self.reason_buffer,
            );

            for i in 0..self.reason_buffer.len() {
                self.working_nogood.add_predicate_to_conflict_nogood(
                    self.reason_buffer[i],
                    self.mode,
                    context,
                    &mut self.predicate_id_generator,
                );
            }
        }

        self.extract_final_nogood(context)
    }

    fn extract_final_nogood(&mut self, context: &mut ConflictAnalysisContext) -> Vec<Predicate> {
        // The final nogood is composed of the predicates encountered from the lower decision
        // levels, plus the predicate(s) remaining in the heap.

        // Depending on what mode we are in, we first remove the elements which are remaining in
        // the heap.
        let mut learned_nogood = self
            .working_nogood
            .drain_learned_nogood(
                &self.predicate_id_generator,
                self.mode,
                &mut self.statistics.cpip_statistics,
            )
            .collect::<Vec<_>>();

        let has_resolved_forbidden_predicates =
            self.resolve_forbidden_previous_checkpoint_predicates(context, &mut learned_nogood);

        self.minimise_learned_nogood(
            context,
            &mut learned_nogood,
            has_resolved_forbidden_predicates,
        );

        pumpkin_assert_simple!(
            learned_nogood
                .iter()
                .all(|predicate| !self.forbidden_variables.contains(&predicate.get_domain())),
            "the learned nogood {learned_nogood:?} contains a forbidden variable"
        );

        // Next, we indicate that the predicates in the nogood appeared in the conflict.
        //
        // TODO: asserting predicate may be bumped twice, probably not a problem.
        for predicate in learned_nogood.iter() {
            context.predicate_appeared_in_conflict(*predicate);
        }

        learned_nogood
    }

    /// Replaces each predicate from a previous decision level that is over a forbidden variable by
    /// its reason, until the nogood contains none.
    ///
    /// The predicate with the largest trail position is resolved first. Its reason only contains
    /// predicates with smaller trail positions from the same or lower decision levels, so this
    /// terminates and the nogood remains asserting.
    ///
    /// Returns whether a predicate was resolved.
    fn resolve_forbidden_previous_checkpoint_predicates(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: &mut Vec<Predicate>,
    ) -> bool {
        if self.forbidden_variables.is_empty() {
            return false;
        }

        let mut has_resolved = false;
        while let Some(index) = learned_nogood
            .iter()
            .enumerate()
            .filter(|(_, predicate)| self.forbidden_variables.contains(&predicate.get_domain()))
            .max_by_key(|(_, predicate)| get_heap_value(**predicate, context))
            .map(|(index, _)| index)
        {
            let forbidden_predicate = learned_nogood.swap_remove(index);
            has_resolved = true;
            pumpkin_assert_simple!(
                context
                    .get_checkpoint_for_predicate(forbidden_predicate)
                    .unwrap()
                    < context.get_checkpoint(),
                "the predicate {forbidden_predicate} over a forbidden variable is from the current decision level"
            );

            self.statistics
                .forbidden_variable_statistics
                .num_resolutions_on_forbidden_previous_checkpoint_predicates += 1;

            self.reason_buffer.clear();
            let _ = context.get_propagation_reason(
                forbidden_predicate,
                CurrentNogood::from(learned_nogood.as_slice()),
                &mut self.reason_buffer,
            );

            for &reason_predicate in self.reason_buffer.iter() {
                if context
                    .get_checkpoint_for_predicate(reason_predicate)
                    .unwrap()
                    == 0
                {
                    context.explain_root_assignment(reason_predicate);
                } else if !learned_nogood.contains(&reason_predicate) {
                    learned_nogood.push(reason_predicate);
                }
            }
        }

        has_resolved
    }

    /// Minimises the learned nogood.
    ///
    /// This uses a combination of recursive minimisation and semantic minimisation depending on
    /// the options passed.
    ///
    /// If `has_resolved_forbidden_predicates`, then predicates were added after iterative
    /// minimisation, so semantic minimisation is also applied.
    fn minimise_learned_nogood(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        learned_nogood: &mut Vec<Predicate>,
        has_resolved_forbidden_predicates: bool,
    ) {
        if self.working_nogood.iterative_minimisation() && !has_resolved_forbidden_predicates {
            // If we have performed iterative minimisation, then we do not need to do semantic
            // minimisation.

            if self.recursive_minimisation {
                // If iterative minimisation and recursive minimisation are active, then we split
                // all of the `[x == v]` predicates into `[x >= v]` and `[x <= v]` so that they can
                // be independently considered by recursive minimisation.
                split_equalities(learned_nogood);
            }
        } else {
            // If iterative minimisation is not active, then we will first use semantic
            // minimisation to remove duplicates and semantically redundant predicates.
            self.semantic_minimiser
                .set_mode(if !self.recursive_minimisation {
                    // If we do not use recursive minimisation then we merge `[x == v]` predicates
                    // up-front.
                    SemanticMinimisationMode::EnableEqualityMerging
                } else {
                    // Otherwise, we keep them as `[x >= v]` and [x <= v] so that they can be
                    // independently considered by recursive minimisation.
                    SemanticMinimisationMode::DisableEqualityMerging
                });
            self.semantic_minimiser.minimise(context, learned_nogood);
        }

        if self.recursive_minimisation {
            // Then we perform recursive minimisation to remove the dominated predicates
            self.recursive_minimiser.minimise(context, learned_nogood);

            // Then we merge `[x >= v]` and `[x <= v]` into `[x == v]`
            merge_equalities(learned_nogood);
        }

        pumpkin_assert_advanced!(
            learned_nogood
                .iter()
                .filter(|p| context.evaluate_predicate(**p) == Some(true))
                .count()
                >= learned_nogood.len() - 1,
            "Not all predicates evaluated to true: {:?}",
            learned_nogood
                .iter()
                .filter(|p| context.evaluate_predicate(**p) != Some(true))
                .collect::<Vec<_>>()
        );
    }

    /// Clears all data structures to prepare for the new conflict analysis.
    fn clean_up(&mut self) {
        self.predicate_id_generator.clear();
    }
}

/// Traverses the learned nogood and applies the following transformation: `[x == v] -> [x >= v],
/// [x <= v]`.
fn split_equalities(learned_nogood: &mut Vec<Predicate>) {
    // We go over each element in the nogood.
    let mut i = 0;

    let mut expected_len = learned_nogood.len();

    while i < learned_nogood.len() {
        let predicate = learned_nogood[i];

        // Check whether it is a predicate of the form [x == v]
        if predicate.is_equality_predicate() {
            // If it is, then we remove it from the learned nogood.
            let _ = learned_nogood.swap_remove(i);

            let domain = predicate.get_domain();
            let rhs = predicate.get_right_hand_side();

            // And then add [x >= v] and [x <= v]
            learned_nogood.push(predicate!(domain >= rhs));
            learned_nogood.push(predicate!(domain <= rhs));

            expected_len += 1;
        } else {
            i += 1;
        }
    }

    pumpkin_assert_eq_simple!(expected_len, learned_nogood.len());
}

fn merge_equalities(learned_nogood: &mut Vec<Predicate>) {
    // We keep track of the lower-bound and upper-bound predicates which have the potential to be
    // turned into equalities.
    //
    // These will be temporarily removed from the nogood and added back at the end, if we cannot
    // find the predicates to turn them into equalities.
    let mut lower_bounds: HashSet<Predicate> = HashSet::default();
    let mut upper_bounds: HashSet<Predicate> = HashSet::default();

    let mut expected_len = learned_nogood.len();

    // We go over each element in the learned nogood.
    let mut i = 0;
    while i < learned_nogood.len() {
        let predicate = learned_nogood[i];

        // If they are either a lower-bound or an upper-bound, then we need to check whether we
        // have found the opposite upper-bound or lower-bound.
        if predicate.is_lower_bound_predicate() {
            // First, we preemptively remove the element from the nogood.
            let _ = learned_nogood.swap_remove(i);

            let domain = predicate.get_domain();
            let rhs = predicate.get_right_hand_side();

            // We check whether we have already seen the upper-bound which would make this an
            // equality.
            if upper_bounds.contains(&predicate!(domain <= rhs)) {
                // If we have, then we remove it from the upper-bounds.
                let _ = upper_bounds.remove(&predicate!(domain <= rhs));

                // And we add the equality to the learned nogood
                learned_nogood.push(predicate!(domain == rhs));

                expected_len -= 1;

                continue;
            }

            // If we have not found a corresponding upper-bound, then we add it to our known
            // lower-bounds.
            let _ = lower_bounds.insert(predicate);
        } else if predicate.is_upper_bound_predicate() {
            // First, we preemptively remove the element from the nogood.
            let _ = learned_nogood.swap_remove(i);

            let domain = predicate.get_domain();
            let rhs = predicate.get_right_hand_side();

            // We check whether we have already seen the lower-bound which would make this an
            // equality.
            if lower_bounds.contains(&predicate!(domain >= rhs)) {
                // If we have, then we remove it from the upper-bounds.
                let _ = lower_bounds.remove(&predicate!(domain >= rhs));

                // And we add the equality to the learned nogood
                learned_nogood.push(predicate!(domain == rhs));

                expected_len -= 1;

                continue;
            }

            // If we have not found a corresponding lower-bound, then we add it to our known
            // upper-bounds.
            let _ = upper_bounds.insert(predicate);
        } else {
            i += 1;
        }
    }

    // Now we add all of the bound predicates for which the corresponding equality has not been
    // found to the learned nogood.
    lower_bounds
        .drain()
        .chain(upper_bounds.drain())
        .for_each(|predicate| learned_nogood.push(predicate));

    pumpkin_assert_eq_simple!(expected_len, learned_nogood.len());
}
