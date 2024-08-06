use super::ConflictAnalysisContext;
use super::RecursiveMinimiser;
use super::SemanticMinimiser;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::ClauseReference;
use crate::basic_types::KeyedVec;
use crate::engine::clause_allocators::ClauseInterface;
use crate::engine::core::Core;
use crate::engine::propagation::PropagatorId;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Clone, Default, Debug)]
/// The outcome of clause learning.
pub(crate) struct ConflictAnalysisResult {
    /// The new learned clause with the propagating literal after backjumping at index 0 and the
    /// literal with the next highest decision level at index 1.
    pub(crate) learned_literals: Vec<Literal>,
    /// The decision level to backtrack to.
    pub(crate) backjump_level: usize,
}

#[derive(Default, Debug)]
pub(crate) struct ResolutionConflictAnalyser {
    // data structures used for conflict analysis
    seen: KeyedVec<PropositionalVariable, bool>,
    analysis_result: ConflictAnalysisResult,

    /// A clause minimiser which uses a recursive minimisation approach to remove dominated
    /// literals (see [`RecursiveMinimiser`]).
    recursive_minimiser: RecursiveMinimiser,
    /// A clause minimiser which uses a semantic minimisation approach (see [`SemanticMinimiser`]).
    semantic_minimiser: SemanticMinimiser,
}

impl ResolutionConflictAnalyser {
    /// Compute the 1-UIP clause based on the current conflict. According to \[1\] a unit
    /// implication point (UIP), "represents an alternative decision assignment at the current
    /// decision level that results in the same conflict" (i.e. no matter what the variable at the
    /// UIP is assigned, the current conflict will be found again given the current decisions). In
    /// the context of implication graphs used in SAT-solving, a UIP is present at decision
    /// level `d` when the number of literals in the learned clause assigned at decision level
    /// `d` is 1.
    ///
    /// The learned clause which is created by
    /// this method contains a single variable at the current decision level (stored at index 0
    /// of [`ConflictAnalysisResult::learned_literals`]); the variable with the second highest
    /// decision level is stored at index 1 in [`ConflictAnalysisResult::learned_literals`] and its
    /// decision level is (redundantly) stored in [`ConflictAnalysisResult::backjump_level`], which
    /// is used when backtracking in ([`ConstraintSatisfactionSolver`]).
    ///
    /// # Bibliography
    /// \[1\] J. Marques-Silva, I. Lynce, and S. Malik, ‘Conflict-driven clause learning SAT
    /// solvers’, in Handbook of satisfiability, IOS press, 2021
    pub(crate) fn compute_1uip(
        &mut self,
        context: &mut ConflictAnalysisContext,
    ) -> ConflictAnalysisResult {
        self.seen.resize(
            context
                .assignments_propositional
                .num_propositional_variables() as usize,
            false,
        );

        pumpkin_assert_simple!(self.debug_conflict_analysis_proconditions(context));

        // Note that in position 0, we placed a dummy literal.
        // The point is that we allocate space for the asserting literal,
        // which will by convention be placed at index 0
        self.analysis_result
            .learned_literals
            .resize(1, context.assignments_propositional.true_literal);
        self.analysis_result.backjump_level = 0;

        let mut num_current_decision_level_literals_to_inspect = 0;
        let mut next_trail_index = context.assignments_propositional.num_trail_entries() - 1;
        let mut next_literal: Option<Literal> = None;

        loop {
            pumpkin_assert_moderate!(Self::debug_1uip_conflict_analysis_check_next_literal(
                next_literal,
                context
            ));
            // note that the 'next_literal' is only None in the first iteration
            let clause_reference = if let Some(propagated_literal) = next_literal {
                context.get_propagation_clause_reference(propagated_literal, &mut |_| {})
            } else {
                let conflict = context.get_conflict_reason_clause_reference(&mut |_| {});
                context
                    .counters
                    .average_conflict_size
                    .add_term(context.clause_allocator[conflict].len() as u64);
                conflict
            };
            context
                .learned_clause_manager
                .update_clause_lbd_and_bump_activity(
                    clause_reference,
                    context.assignments_propositional,
                    context.clause_allocator,
                );

            // process the reason literal
            // 	i.e., perform resolution and update other related internal data structures

            // note that the start index will be either 0 or 1 - the idea is to skip the 0th literal
            // in case the clause represents a propagation
            let start_index = next_literal.is_some() as usize;
            for &reason_literal in
                &context.clause_allocator[clause_reference].get_literal_slice()[start_index..]
            {
                // only consider non-root assignments that have not been considered before
                let is_root_assignment = context
                    .assignments_propositional
                    .is_literal_root_assignment(reason_literal);
                let seen = self.seen[reason_literal.get_propositional_variable()];

                if !is_root_assignment && !seen {
                    // mark the variable as seen so that we do not process it more than once
                    self.seen[reason_literal.get_propositional_variable()] = true;

                    context
                        .brancher
                        .on_appearance_in_conflict_literal(reason_literal);
                    if let Some(reason_domain) = context
                        .variable_literal_mappings
                        .get_domain_literal(reason_literal)
                    {
                        context
                            .brancher
                            .on_appearance_in_conflict_integer(reason_domain);
                    }

                    let literal_decision_level = context
                        .assignments_propositional
                        .get_literal_assignment_level(reason_literal);

                    let is_current_level_assignment = literal_decision_level
                        == context.assignments_propositional.get_decision_level();

                    num_current_decision_level_literals_to_inspect +=
                        is_current_level_assignment as usize;

                    // literals from previous decision levels are considered for the learned clause
                    if !is_current_level_assignment {
                        self.analysis_result.learned_literals.push(reason_literal);
                        // the highest decision level literal must be placed at index 1 to prepare
                        // the clause for propagation
                        if literal_decision_level > self.analysis_result.backjump_level {
                            self.analysis_result.backjump_level = literal_decision_level;

                            let last_index = self.analysis_result.learned_literals.len() - 1;

                            self.analysis_result.learned_literals[last_index] =
                                self.analysis_result.learned_literals[1];

                            self.analysis_result.learned_literals[1] = reason_literal;
                        }
                    }
                }
            }

            // after resolution took place, find the next literal on the trail that is relevant for
            // this conflict only literals that have been seen so far are relevant
            //  note that there may be many literals that are not relevant
            while !self.seen[context
                .assignments_propositional
                .get_trail_entry(next_trail_index)
                .get_propositional_variable()]
            {
                next_trail_index -= 1;
                pumpkin_assert_advanced!(
                    context
                        .assignments_propositional
                        .get_literal_assignment_level(
                            context
                                .assignments_propositional
                                .get_trail_entry(next_trail_index)
                        )
                        == context.assignments_propositional.get_decision_level(),
                    "The current decision level trail has been overrun,
                     mostly likely caused by an incorrectly implemented cp propagator?"
                );
            }

            // make appropriate adjustments to prepare for the next iteration
            next_literal = Some(
                context
                    .assignments_propositional
                    .get_trail_entry(next_trail_index),
            );
            // the same literal cannot be encountered more than once on the trail, so we can clear
            // the flag here
            self.seen[next_literal.unwrap().get_propositional_variable()] = false;
            num_current_decision_level_literals_to_inspect -= 1;
            next_trail_index -= 1;

            // once the counters hits zero we stop, the 1UIP has been found
            //  the next literal is the asserting literal
            if num_current_decision_level_literals_to_inspect == 0 {
                self.analysis_result.learned_literals[0] = !next_literal.unwrap();
                break;
            }
        }

        // clear the seen flags for literals in the learned clause
        //  note that other flags have already been cleaned above in the previous loop
        for literal in &self.analysis_result.learned_literals {
            self.seen[literal.get_propositional_variable()] = false;
        }

        if context.internal_parameters.learning_clause_minimisation {
            pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result(false, context));

            self.recursive_minimiser
                .remove_dominated_literals(context, &mut self.analysis_result);

            self.semantic_minimiser
                .minimise(context, &mut self.analysis_result);
        }

        context
            .explanation_clause_manager
            .clean_up_explanation_clauses(context.clause_allocator);

        pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result(false, context));
        // the return value is stored in the input 'analysis_result'
        self.analysis_result.clone()
    }

    // computes the learned clause containing only decision literals and stores it in
    // 'analysis_result'
    #[allow(dead_code)]
    fn compute_all_decision_learning(
        &mut self,
        is_extracting_core: bool,
        context: &mut ConflictAnalysisContext,
    ) {
        self.compute_all_decision_learning_helper(None, is_extracting_core, context, |_| {});
    }

    // the helper is used to facilitate usage when extracting the clausal core
    //  normal conflict analysis would use 'compute_all_decision_learning'
    fn compute_all_decision_learning_helper(
        &mut self,
        mut next_literal: Option<Literal>,
        is_extracting_core: bool,
        context: &mut ConflictAnalysisContext,
        mut on_analysis_step: impl FnMut(AnalysisStep),
    ) {
        self.seen.resize(
            context
                .assignments_propositional
                .num_propositional_variables() as usize,
            false,
        );

        // the code is similar to 1uip learning with small differences to accomodate the
        // all-decision learning scheme
        pumpkin_assert_simple!(
            next_literal.is_some() || self.debug_conflict_analysis_proconditions(context)
        ); // when using this function when extracting the core, no conflict acutally takes place,
           // but the preconditions expect a conflict clause, so we skip this check

        self.analysis_result.learned_literals.clear();
        self.analysis_result.backjump_level = 0;

        let mut num_propagated_literals_left_to_inspect = 0;
        let mut next_trail_index = context.assignments_propositional.num_trail_entries() - 1;

        loop {
            // Note that the 'next_literal' is given as input.
            //  If it is none, it is none in the first iteration only
            let clause_reference = if let Some(propagated_literal) = next_literal {
                context.get_propagation_clause_reference(propagated_literal, &mut on_analysis_step)
            } else {
                context.get_conflict_reason_clause_reference(&mut on_analysis_step)
            };
            context
                .learned_clause_manager
                .update_clause_lbd_and_bump_activity(
                    clause_reference,
                    context.assignments_propositional,
                    context.clause_allocator,
                );

            // process the reason literal
            // 	i.e., perform resolution and update other related internal data structures
            let start_index = next_literal.is_some() as usize;
            // note that the start index will be either 0 or 1 - the idea is to skip the 0th literal
            // in case the clause represents a propagation
            for &reason_literal in
                &context.clause_allocator[clause_reference].get_literal_slice()[start_index..]
            {
                if self.seen[reason_literal.get_propositional_variable()] {
                    continue;
                }

                // only consider non-root assignments that have not been considered before
                let is_root_assignment = context
                    .assignments_propositional
                    .is_literal_root_assignment(reason_literal);

                if !is_root_assignment {
                    // mark the variable as seen so that we do not process it more than once
                    self.seen[reason_literal.get_propositional_variable()] = true;

                    context
                        .brancher
                        .on_appearance_in_conflict_literal(reason_literal);
                    if let Some(reason_domain) = context
                        .variable_literal_mappings
                        .get_domain_literal(reason_literal)
                    {
                        context
                            .brancher
                            .on_appearance_in_conflict_integer(reason_domain);
                    }

                    num_propagated_literals_left_to_inspect += context
                        .assignments_propositional
                        .is_literal_propagated(reason_literal)
                        as i32;

                    // only decision literals are kept for the learned clause
                    if context
                        .assignments_propositional
                        .is_literal_decision(reason_literal)
                    {
                        on_analysis_step(AnalysisStep::Unit(reason_literal));
                        self.analysis_result.learned_literals.push(reason_literal);
                    }
                } else if is_root_assignment {
                    on_analysis_step(AnalysisStep::Unit(reason_literal));
                }
            }

            if num_propagated_literals_left_to_inspect == 0 {
                break;
            }

            // after resolution took place, find the next literal on the trail that is relevant for
            // this conflict only literals that have been seen so far are relevant
            //  note that there may be many literals that are not relevant
            while !self.seen[context
                .assignments_propositional
                .get_trail_entry(next_trail_index)
                .get_propositional_variable()]
                || context.assignments_propositional.is_literal_decision(
                    context
                        .assignments_propositional
                        .get_trail_entry(next_trail_index),
                )
            {
                next_trail_index -= 1;
            }

            // make appropriate adjustments to prepare for the next iteration
            next_literal = Some(
                context
                    .assignments_propositional
                    .get_trail_entry(next_trail_index),
            );
            // the same literal cannot be encountered more than once on the trail, so we can clear
            // the flag here
            self.seen[next_literal.unwrap().get_propositional_variable()] = false;
            next_trail_index -= 1;
            num_propagated_literals_left_to_inspect -= 1;

            pumpkin_assert_simple!(
                context
                    .assignments_propositional
                    .is_literal_propagated(next_literal.unwrap()),
                "Sanity check: the next literal on the trail select must be a propagated literal."
            );
        }

        // clear the seen flags for literals in the learned clause
        //  note that other flags have already been cleaned above in the previous loop
        for literal in &self.analysis_result.learned_literals {
            self.seen[literal.get_propositional_variable()] = false;
        }

        // set the literals in the learned clause in the expected order
        //  the propagated literal at index 0
        //  the second highest decision level literal at index 1

        // the above could have been updated during the analysis
        //  but instead we do it here using this helper function
        let place_max_in_front = |lits: &mut [Literal]| {
            let assignments = &context.assignments_propositional;
            let mut max_index: usize = 0;
            let mut max_level = assignments.get_literal_assignment_level(lits[max_index]);
            for i in lits.iter().enumerate() {
                let new_level = assignments.get_literal_assignment_level(*i.1);
                if max_level < new_level {
                    max_index = i.0;
                    max_level = new_level;
                }
            }
            lits.swap(0, max_index);
        };

        place_max_in_front(self.analysis_result.learned_literals.as_mut_slice());
        if self.analysis_result.learned_literals.len() > 2 {
            place_max_in_front(&mut self.analysis_result.learned_literals[1..]);
        }

        if self.analysis_result.learned_literals.len() > 1 {
            self.analysis_result.backjump_level = context
                .assignments_propositional
                .get_literal_assignment_level(self.analysis_result.learned_literals[1]);
        }

        context
            .explanation_clause_manager
            .clean_up_explanation_clauses(context.clause_allocator);

        pumpkin_assert_moderate!(
            self.debug_check_conflict_analysis_result(is_extracting_core, context)
        );
        // the return value is stored in the input 'analysis_result'
    }

    pub(crate) fn get_conflict_reasons(
        &mut self,
        context: &mut ConflictAnalysisContext,
        on_analysis_step: impl FnMut(AnalysisStep),
    ) {
        let next_literal = if context.solver_state.is_infeasible_under_assumptions() {
            Some(!context.solver_state.get_violated_assumption())
        } else {
            None
        };
        self.compute_all_decision_learning_helper(next_literal, true, context, on_analysis_step);
    }

    pub(crate) fn compute_clausal_core(
        &mut self,
        context: &mut ConflictAnalysisContext,
    ) -> Result<Core, Literal> {
        pumpkin_assert_simple!(self.debug_check_core_extraction(context));

        if context.solver_state.is_infeasible() {
            return Ok(Core::Empty);
        }

        let violated_assumption = context.solver_state.get_violated_assumption();

        // we consider three cases:
        //  1. The assumption is falsified at the root level
        //  2. The assumption is inconsistent with other assumptions, e.g., x and !x given as
        //     assumptions
        //  3. Standard case

        // Case one: the assumption is falsified at the root level
        if context
            .assignments_propositional
            .is_literal_root_assignment(violated_assumption)
        {
            Ok(Core::RootLevel {
                root_level_assumption_literal: violated_assumption,
            })
        }
        // Case two: the assumption is inconsistent with other assumptions (i.e. the assumptions
        // contain both literal 'x' and '!x')
        //
        // We return the literal which has conflicting assumptions
        else if !context
            .assignments_propositional
            .is_literal_propagated(violated_assumption)
        {
            Err(violated_assumption)
        }
        // Case three: the standard case - proceed with core extraction
        //
        // Performs resolution on all implied assumptions until only decision assumptions are left.
        // The violating assumption is used as the starting point at this point, any reason
        // clause encountered will contains only assumptions, but some assumptions might be
        // implied.
        //
        // This corresponds to the all-decision CDCL learning scheme
        else {
            self.compute_all_decision_learning_helper(
                Some(!violated_assumption),
                true,
                context,
                |_| {},
            );
            self.analysis_result
                .learned_literals
                .push(!violated_assumption);
            pumpkin_assert_moderate!(self.debug_check_clausal_core(violated_assumption, context));
            Ok(Core::Standard {
                negated_assumption_literals: self.analysis_result.learned_literals.clone(),
            })
        }
    }

    /// In [`ResolutionConflictAnalyser::compute_1uip`], [`Literal`]s are examined in reverse
    /// order on the trail. The examined [`Literal`]s are expected to be:
    ///  1. From the same decision level; i.e. the current (last) decision level
    ///  2. Propagated, unless the [`Literal`] is the decision [`Literal`] of the current decision
    ///     level
    ///  3. Not root assignments
    ///
    /// Failing any of the conditions above means something went wrong with the conflict analysis,
    /// e.g., some explanation was faulty and caused the solver to overrun the trail
    ///
    /// Note that in the first iteration, the `next_literal` will be set to [`None`],
    /// so we can skip this check
    fn debug_1uip_conflict_analysis_check_next_literal(
        next_literal: Option<Literal>,
        context: &ConflictAnalysisContext,
    ) -> bool {
        match next_literal {
            None => true,
            Some(next_literal) => {
                if context
                    .assignments_propositional
                    .is_literal_root_assignment(next_literal)
                {
                    return false;
                }

                let is_propagated = context
                    .assignments_propositional
                    .is_literal_propagated(next_literal);

                let current_decision_level = context.assignments_propositional.get_decision_level();

                let is_decision_literal_of_current_level = context
                    .assignments_propositional
                    .is_literal_decision(next_literal);

                let is_assigned_at_current_decision_level = context
                    .assignments_propositional
                    .get_literal_assignment_level(next_literal)
                    == current_decision_level;

                (is_propagated || is_decision_literal_of_current_level)
                    && is_assigned_at_current_decision_level
            }
        }
    }

    fn debug_check_conflict_analysis_result(
        &self,
        is_extracting_core: bool,
        context: &ConflictAnalysisContext,
    ) -> bool {
        // debugging method: performs sanity checks on the learned clause

        let assignments = &context.assignments_propositional;
        let learned_lits = &self.analysis_result.learned_literals;

        assert!(
            self.analysis_result.backjump_level < context.get_decision_level(),
            "Backjump level must be lower than the current level."
        );

        assert!(
            learned_lits
                .iter()
                .all(|&literal| !assignments.is_literal_root_assignment(literal)),
            "No root level literals may be present in a learned clause."
        );

        if !is_extracting_core {
            // When this method is called during core extraction, the decision level is not
            // necessarily the decision level of learned_literals[0].
            assert_eq!(
                context.get_decision_level(),
                assignments.get_literal_assignment_level(self.analysis_result.learned_literals[0]),
                "The asserting literal must be at the highest level."
            );
        }

        assert!(
            learned_lits[1..].iter().all(|&literal| {
                assignments.get_literal_assignment_level(literal) != context.get_decision_level()
            }),
            "There may be only one literal at the highest decision level"
        );

        assert!(
            learned_lits[1..]
                .iter()
                .all(|&literal| { assignments.is_literal_assigned_false(literal) }),
            "All literals apart from the propagating literal are assigned false"
        );

        if learned_lits.len() >= 2 {
            assert_eq!(
                self.analysis_result.backjump_level,
                assignments.get_literal_assignment_level(learned_lits[1]),
                "Assertion level seems wrong."
            );

            let second_max_level = assignments.get_literal_assignment_level(learned_lits[1]);

            assert!(
                learned_lits[1..].iter().all(|&literal| {
                    assignments.get_literal_assignment_level(literal) <= second_max_level
                }),
                "The literal at position 1 must be at the second highest level"
            );
        }
        true
    }

    fn debug_conflict_analysis_proconditions(&mut self, context: &ConflictAnalysisContext) -> bool {
        pumpkin_assert_simple!(
            context.solver_state.conflicting(),
            "Solver expected to be in conflict state for conflict analysis."
        );

        pumpkin_assert_simple!(
            self.seen.len() as u32
                == context
                    .assignments_propositional
                    .num_propositional_variables()
        );
        pumpkin_assert_simple!(context.explanation_clause_manager.is_empty());
        pumpkin_assert_simple!(!context.assignments_propositional.is_at_the_root_level());
        pumpkin_assert_advanced!(self.seen.iter().all(|b| !b));

        true
    }

    fn debug_check_core_extraction(&self, context: &ConflictAnalysisContext) -> bool {
        if context.solver_state.is_infeasible() {
            true
        } else if context.solver_state.is_infeasible_under_assumptions() {
            pumpkin_assert_simple!(
                context
                    .assignments_propositional
                    .is_literal_assigned_false(context.solver_state.get_violated_assumption()),
                "Violated assumption is expected to be assigned false."
            );

            pumpkin_assert_moderate!(context
                .assumptions
                .contains(&context.solver_state.get_violated_assumption()));
            true
        } else {
            panic!(
                "Cannot extract core unless the solver is either infeasible
                 or infeasible under assumptions."
            );
        }
    }

    fn debug_check_clausal_core(
        &self,
        violated_assumption: Literal,
        context: &ConflictAnalysisContext,
    ) -> bool {
        pumpkin_assert_moderate!(
            self.analysis_result
                .learned_literals
                .iter()
                .all(|&core_literal| context.assumptions.contains(&!core_literal)),
            "Each core literal must be part of the assumptions."
        );
        pumpkin_assert_moderate!(
            self.analysis_result
                .learned_literals
                .iter()
                .all(|&core_literal| core_literal == !violated_assumption
                    || context
                        .assignments_propositional
                        .is_literal_decision(!core_literal)),
            "Each core literal (except the violated literal) must be a decision."
        );
        true
    }
}

#[derive(Clone, Debug)]
#[allow(variant_size_differences)]
pub(crate) enum AnalysisStep<'a> {
    AllocatedClause(ClauseReference),
    Propagation {
        propagator: PropagatorId,
        conjunction: &'a [Literal],
        propagated: Literal,
    },
    Unit(Literal),
}
