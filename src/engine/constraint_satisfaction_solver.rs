use super::cp::CPEngineDataStructures;
use super::sat::SATEngineDataStructures;
use super::{AssignmentsInteger, AssignmentsPropositional, SATCPMediator};
use crate::arguments::ArgumentHandler;
use crate::basic_types::{
    BranchingDecision, CSPSolverExecutionFlag, ClauseAdditionOutcome, ClauseReference,
    IntegerVariable, Literal, PropagationStatusCP, PropagationStatusClausal,
    PropagationStatusOneStepCP, PropagatorIdentifier, PropositionalConjunction,
    PropositionalVariable, Stopwatch,
};

use crate::engine::{DebugHelper, DomainManager};
use crate::propagators::ConstraintProgrammingPropagator;
use crate::pumpkin_asserts::*;

pub struct ConstraintSatisfactionSolver {
    state: CSPSolverState,
    sat_data_structures: SATEngineDataStructures,
    cp_data_structures: CPEngineDataStructures,
    cp_propagators: Vec<Box<dyn ConstraintProgrammingPropagator>>,
    sat_cp_mediator: SATCPMediator,
    seen: Vec<bool>,
    counters: Counters,
    internal_parameters: ConstraintSatisfactionSolverInternalParameters,
    stopwatch: Stopwatch,
}

//methods that offer basic functionality
impl ConstraintSatisfactionSolver {
    pub fn new(argument_handler: &ArgumentHandler) -> ConstraintSatisfactionSolver {
        let mut csp_solver = ConstraintSatisfactionSolver {
            state: CSPSolverState::new(),
            sat_data_structures: SATEngineDataStructures::new(argument_handler),
            cp_data_structures: CPEngineDataStructures::new(argument_handler),
            cp_propagators: vec![],
            sat_cp_mediator: SATCPMediator::new(),
            seen: vec![],
            counters: Counters::new(
                argument_handler.get_integer_argument("num-conflicts-per-restart") as u64,
            ),
            internal_parameters: ConstraintSatisfactionSolverInternalParameters::new(
                argument_handler,
            ),
            stopwatch: Stopwatch::new(i64::MAX),
        };

        //we introduce a dummy variable set to true at the root level
        //  this is useful for convenience when a fact needs to be expressed that is always true
        //  e.g., this makes writing propagator explanations easier for corner cases
        let root_variable = csp_solver
            .sat_cp_mediator
            .create_new_propositional_variable(&mut csp_solver.sat_data_structures);
        let true_literal = Literal::new(root_variable, true);

        csp_solver
            .sat_data_structures
            .assignments_propositional
            .true_literal = true_literal;

        csp_solver
            .sat_data_structures
            .assignments_propositional
            .false_literal = !true_literal;

        csp_solver.sat_cp_mediator.true_literal = true_literal;
        csp_solver.sat_cp_mediator.false_literal = !true_literal;

        csp_solver.add_unit_clause(true_literal);

        csp_solver
    }

    pub fn solve_under_assumptions(
        &mut self,
        assumptions: &[Literal],
        time_limit_in_seconds: i64,
    ) -> CSPSolverExecutionFlag {
        self.initialise(assumptions, time_limit_in_seconds);
        self.solve_internal()
    }

    pub fn extract_core(&mut self) -> Vec<Literal> {
        pumpkin_assert_simple!(
            self.state.is_infeasible_under_assumptions(),
            "Cannot extract core unless the solver is in the infeasible under assumption state."
        );
        todo!();
    }

    pub fn solve(&mut self, time_limit_in_seconds: i64) -> CSPSolverExecutionFlag {
        let dummy_assumptions: Vec<Literal> = vec![];
        self.solve_under_assumptions(&dummy_assumptions, time_limit_in_seconds)
    }

    pub fn reset_variable_selection(&mut self, random_seed: i64) {
        pumpkin_assert_simple!(self.state.is_ready());
        self.sat_data_structures
            .propositional_variable_selector
            .reset(random_seed);
    }

    pub fn get_state(&self) -> &CSPSolverState {
        &self.state
    }

    pub fn create_new_propositional_variable(&mut self) -> PropositionalVariable {
        self.sat_cp_mediator
            .create_new_propositional_variable(&mut self.sat_data_structures)
    }

    pub fn create_new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
    ) -> IntegerVariable {
        self.sat_cp_mediator.create_new_integer_variable(
            lower_bound,
            upper_bound,
            &mut self.sat_data_structures,
            &mut self.cp_data_structures,
        )
    }

    pub fn get_propositional_assignments(&self) -> &AssignmentsPropositional {
        &self.sat_data_structures.assignments_propositional
    }

    pub fn get_lower_bound_literal(
        &self,
        integer_variable: IntegerVariable,
        lower_bound: i32,
    ) -> Literal {
        self.sat_cp_mediator
            .get_lower_bound_literal(integer_variable, lower_bound)
    }

    pub fn get_integer_assignments(&self) -> &AssignmentsInteger {
        &self.cp_data_structures.assignments_integer
    }

    pub fn set_solution_guided_search(&mut self) {
        pumpkin_assert_simple!(
            self.state.has_solution(),
            "Cannot set solution guided search without a solution in the solver."
        );

        for variable in self
            .sat_data_structures
            .assignments_propositional
            .get_propositional_variables()
        {
            //variable values get assigned the value as in the current assignment
            //note: variables created after calling this method may follow a different strategy
            let new_truth_value = self
                .sat_data_structures
                .assignments_propositional
                .is_variable_assigned_true(variable);

            self.sat_data_structures
                .propositional_value_selector
                .update_and_freeze(variable, new_truth_value);
        }
    }

    pub fn set_fixed_phases_for_variables(&mut self, literals: &[Literal]) {
        for literal in literals {
            self.sat_data_structures
                .propositional_value_selector
                .update_and_freeze(literal.get_propositional_variable(), literal.is_positive());
        }
    }

    pub fn restore_state_at_root(&mut self) {
        pumpkin_assert_simple!(self.state.has_solution() && self.get_decision_level() > 0);

        self.backtrack(0);
        self.state.declare_ready();
    }
}

//methods that serve as the main building blocks
impl ConstraintSatisfactionSolver {
    fn initialise(&mut self, assumptions: &[Literal], time_limit_in_seconds: i64) {
        let num_propositional_variables = self
            .sat_data_structures
            .assignments_propositional
            .num_propositional_variables() as usize;

        self.state.declare_solving();
        self.stopwatch.reset(time_limit_in_seconds);
        self.sat_data_structures.assumptions = assumptions.to_owned();
        self.seen.resize(num_propositional_variables, false);
    }

    fn solve_internal(&mut self) -> CSPSolverExecutionFlag {
        loop {
            if self.stopwatch.get_remaining_time_budget() <= 0 {
                self.state.declare_timeout();
                return CSPSolverExecutionFlag::Timeout;
            }

            self.propagate_enqueued();

            if self.state.no_conflict() {
                if self.should_restart() {
                    self.perform_restart_during_search()
                }

                self.sat_data_structures
                    .assignments_propositional
                    .increase_decision_level();
                self.cp_data_structures
                    .assignments_integer
                    .increase_decision_level();

                match self.sat_data_structures.get_next_branching_decision() {
                    Some(branching_decision) => match branching_decision {
                        BranchingDecision::Assumption { assumption_literal } => {
                            //Case 1: the assumption is unassigned, assign it
                            if self
                                .sat_data_structures
                                .assignments_propositional
                                .is_literal_unassigned(assumption_literal)
                            {
                                self.sat_data_structures
                                    .assignments_propositional
                                    .enqueue_decision_literal(assumption_literal);
                            //Case 2: the assumption has already been set to true
                            //  this happens when other assumptions propagated the literal
                            //  or the assumption is already set to true at the root level
                            } else if self
                                .sat_data_structures
                                .assignments_propositional
                                .is_literal_assigned_true(assumption_literal)
                            {
                                //in this case, do nothing
                                //  note that the solver will then increase the decision level without enqueuing a decision literal
                                //  this is necessary because by convention the solver will try to assign the i-th assumption literal at decision level i+1
                            }
                            //Case 3: the assumption literal is in conflict with the input assumption
                            //  which means the instance is infeasible under the current assumptions
                            else {
                                pumpkin_assert_moderate!(
                                    self.sat_data_structures
                                        .assignments_propositional
                                        .get_literal_assignment_level(assumption_literal)
                                        == 0
                                        || self
                                            .sat_data_structures
                                            .assignments_propositional
                                            .is_literal_propagated(assumption_literal),
                                );

                                self.state
                                    .declare_infeasible_under_assumptions(assumption_literal);
                                return CSPSolverExecutionFlag::InfeasibleUnderAssumptions;
                            }
                        }
                        BranchingDecision::StandardDecision { decision_literal } => {
                            self.counters.num_decisions += 1;
                            self.sat_data_structures
                                .assignments_propositional
                                .enqueue_decision_literal(decision_literal);
                        }
                    },
                    None => {
                        self.state.declare_solution_found();
                        return CSPSolverExecutionFlag::Feasible;
                    }
                }
            } else {
                if self
                    .sat_data_structures
                    .assignments_propositional
                    .is_at_the_root_level()
                {
                    self.state.declare_infeasible();
                    return CSPSolverExecutionFlag::Infeasible;
                }

                let conflict_reference = self.get_conflict_clause();
                let analysis_result = self.analyse_conflict(conflict_reference);
                self.counters.num_unit_clauses_learned +=
                    (analysis_result.learned_literals.len() == 1) as u64;
                self.process_conflict_analysis_result(analysis_result);

                self.state.declare_solving();

                self.sat_data_structures.decay_clause_activities();
                self.sat_data_structures
                    .propositional_variable_selector
                    .decay_activities();
            }
        }
    }

    //changes the state based on the conflict analysis result given as input
    //i.e., adds the learned clause to the database, backtracks, enqueues the propagated literal, and updates internal data structures for simple moving averages
    //note that no propagation is done, this is left to the solver
    fn process_conflict_analysis_result(&mut self, analysis_result: ConflictAnalysisResult) {
        //unit clauses are treated in a special way: they are added as decision literals at decision level 0
        if analysis_result.learned_literals.len() == 1 {
            self.backtrack(0);
            let unit_clause = analysis_result.learned_literals[0];
            pumpkin_assert_simple!(
                self.sat_data_structures
                    .assignments_propositional
                    .is_literal_unassigned(unit_clause),
                "Do not expect to learn a literal that is already set."
            );

            self.sat_data_structures
                .assignments_propositional
                .enqueue_decision_literal(unit_clause);
            //state_.UpdateMovingAveragesForRestarts(1);
        } else {
            //int lbd = state_.ComputeLBD(&analysis_result_.learned_clause_literals[0] + 1, analysis_result_.learned_clause_literals.size() - 1);
            //state_.UpdateMovingAveragesForRestarts(lbd);

            self.backtrack(analysis_result.backjump_level);

            let propagated_literal = analysis_result.learned_literals[0];

            let learned_clause_reference = self
                .sat_data_structures
                .add_clause_unchecked(analysis_result.learned_literals, true);

            self.sat_data_structures
                .assignments_propositional
                .enqueue_propagated_literal(propagated_literal, learned_clause_reference.id);
        }
    }

    fn get_conflict_clause(&mut self) -> ClauseReference {
        pumpkin_assert_simple!(self.state.conflict_detected());
        if self.state.is_clausal_conflict() {
            self.state.get_conflict_clause_reference()
        } else {
            let failure_literals = self
                .state
                .get_conflict_reason_cp()
                .clone()
                .into_iter()
                .map(|p| !self.sat_cp_mediator.get_predicate_literal(p))
                .collect();

            self.sat_data_structures
                .add_explanation_clause_unchecked(failure_literals)
        }
    }

    fn should_restart(&self) -> bool {
        self.counters.num_conflicts_until_restart == 0
    }

    fn perform_restart_during_search(&mut self) {
        pumpkin_assert_simple!(self.get_decision_level() > 0);

        self.backtrack(0);

        self.sat_data_structures
            .shrink_learned_clause_database_if_needed();

        self.counters.num_conflicts_until_restart =
            self.internal_parameters.num_conflicts_per_restart;

        self.counters.num_conflicts += 1;
    }

    fn is_conflict_clause_set(&self) -> bool {
        true
    }

    fn backtrack(&mut self, backtrack_level: u32) {
        pumpkin_assert_simple!(backtrack_level < self.get_decision_level());

        self.sat_data_structures.backtrack(backtrack_level);
        self.cp_data_structures.backtrack(backtrack_level);
        //  note that sat_cp_mediator sync should be called after the sat/cp data structures backtrack
        self.sat_cp_mediator.synchronise(
            &self.sat_data_structures.assignments_propositional,
            &self.cp_data_structures.assignments_integer,
        );
        //for now all propagators are called to synchronise
        //  in the future this will be improved in two ways:
        //      allow incremental synchronisation
        //      only call the subset of propagators that were notified since last backtrack
        for propagator_id in 0..self.cp_propagators.len() {
            let domains = DomainManager::new(
                propagator_id,
                &mut self.cp_data_structures.assignments_integer,
            );
            self.cp_propagators[propagator_id].synchronise(&domains);
        }
    }

    fn analyse_conflict(&mut self, conflict_reference: ClauseReference) -> ConflictAnalysisResult {
        pumpkin_assert_simple!(
            self.seen.len() as u32
                == self
                    .sat_data_structures
                    .assignments_propositional
                    .num_propositional_variables()
        );

        let mut analysis_result = ConflictAnalysisResult {
            learned_literals: vec![
                self.sat_data_structures
                    .assignments_propositional
                    .true_literal,
            ], //the convention is to place the asserting literal at index zero, we are allocating space for it now, using the true_literal as a placeholder
            backjump_level: 0,
        };

        let mut num_current_decision_level_literals = 0;
        let mut next_trail_index = self
            .sat_data_structures
            .assignments_propositional
            .trail
            .len()
            - 1;
        let mut next_literal: Option<Literal> = None; //none signals that this is the first iteration where the conflict reference should be used
        loop {
            pumpkin_assert_moderate!(
                next_literal.is_none()
                    || self
                        .sat_data_structures
                        .assignments_propositional
                        .is_variable_propagated(next_literal.unwrap().get_propositional_variable())
                        && self
                            .sat_data_structures
                            .assignments_propositional
                            .get_variable_assignment_level(
                                next_literal.unwrap().get_propositional_variable()
                            )
                            == self
                                .sat_data_structures
                                .assignments_propositional
                                .get_decision_level()
            );

            let reason_clause_reference = if let Some(propagated_literal) = next_literal {
                self.sat_cp_mediator
                    .get_propagation_reason_clause_reference(
                        propagated_literal,
                        &mut self.sat_data_structures,
                        &self.cp_data_structures,
                        &mut self.cp_propagators,
                    )
            } else {
                conflict_reference
            };

            self.sat_data_structures
                .update_clause_lbd_and_bump_activity(reason_clause_reference);

            //process the reason literal
            //	i.e., perform resolution and update other related internal data structures
            let mut index = (next_literal.is_some()) as u32; //note that the index will be either 0 or 1 - the idea is to skip the 0th literal in case the clause represents a propagation
            while index < self.sat_data_structures.clause_allocator[reason_clause_reference].len() {
                //only consider non-root assignments that have not been considered before
                let reason_literal =
                    self.sat_data_structures.clause_allocator[reason_clause_reference][index];
                if !self
                    .sat_data_structures
                    .assignments_propositional
                    .is_literal_root_assignment(reason_literal)
                    && !self.seen[reason_literal.get_propositional_variable()]
                {
                    //mark the variable as seen so that we do not process it more than once
                    self.seen[reason_literal.get_propositional_variable()] = true;

                    self.sat_data_structures
                        .propositional_variable_selector
                        .bump_activity(reason_literal.get_propositional_variable());

                    let literal_decision_level = self
                        .sat_data_structures
                        .assignments_propositional
                        .get_literal_assignment_level(reason_literal);
                    let is_current_level_assignment = literal_decision_level
                        == self
                            .sat_data_structures
                            .assignments_propositional
                            .get_decision_level();

                    num_current_decision_level_literals += is_current_level_assignment as u32;

                    //literals from previous decision levels are considered for the learnt clause
                    if !is_current_level_assignment {
                        analysis_result.learned_literals.push(reason_literal);
                        //the highest decision level literal must be placed at index 1 to prepare the clause for propagation
                        if literal_decision_level > analysis_result.backjump_level {
                            analysis_result.backjump_level = literal_decision_level;

                            //todo use the built in swapper

                            let last_index = analysis_result.learned_literals.len() - 1;
                            analysis_result.learned_literals[last_index] =
                                analysis_result.learned_literals[1];

                            analysis_result.learned_literals[1] = reason_literal;
                        }
                    }
                }
                index += 1;
            }

            //after resolution took place
            //find the next literal on the trail
            //expand a node of the current decision level
            //find a literal that you have already seen in conflict analysis - recall that each literal may be found on the trail only once
            //literals that have not been seen yet are not important for this conflict so we can skip them
            while !self.seen[self.sat_data_structures.assignments_propositional.trail
                [next_trail_index]
                .get_propositional_variable()]
            {
                next_trail_index -= 1;
                pumpkin_assert_advanced!(self.sat_data_structures.assignments_propositional.get_literal_assignment_level(self.sat_data_structures.assignments_propositional.trail[next_trail_index]) == self.sat_data_structures.assignments_propositional.get_decision_level(),
                    "The current decision level trail has been overrun, mostly likely caused by an incorrectly implemented cp propagator?");
            }

            //make appropriate adjustments to prepare for the next iteration
            next_literal =
                Some(self.sat_data_structures.assignments_propositional.trail[next_trail_index]);
            self.seen[next_literal.unwrap().get_propositional_variable()] = false; //the same literal cannot be encountered more than once on the trail, so we can clear the flag here
            num_current_decision_level_literals -= 1;
            next_trail_index -= 1;

            //once the counters hits zero we stop, the 1UIP has been found
            if num_current_decision_level_literals == 0 {
                break;
            }
        }
        analysis_result.learned_literals[0] = !next_literal.unwrap();

        //clear the seen flags for literals in the learned clause, since these were not cleaned up above
        for literal in &analysis_result.learned_literals {
            self.seen[literal.get_propositional_variable()] = false;
        }

        self.sat_data_structures.clean_up_explanation_clauses();

        //pumpkin_assert_advanced(analysis_result.CheckCorrectnessAfterConflictAnalysis(state_), "There is an issues with the derived learned clause after analysis!");
        //if (internal_parameters_.use_clause_minimisation_) { learned_clause_minimiser_.RemoveImplicationGraphDominatedLiteralsBetter(analysis_result); }
        //pumpkin_assert_advanced(analysis_result.CheckCorrectnessAfterConflictAnalysis(state_), "After minimisation the learned clause has an issue!");

        analysis_result
    }

    fn propagate_enqueued(&mut self) {
        let num_assigned_variables_old = self
            .sat_data_structures
            .assignments_propositional
            .num_assigned_propositional_variables();

        loop {
            self.sat_cp_mediator
                .synchronise_propositional_trail_based_on_integer_trail(
                    &mut self.sat_data_structures.assignments_propositional,
                    &self.cp_data_structures.assignments_integer,
                );

            let propagation_status_clausal = self.sat_data_structures.propagate_clauses();

            if let PropagationStatusClausal::ConflictDetected { reason_code } =
                propagation_status_clausal
            {
                self.state
                    .declare_clausal_conflict(ClauseReference { id: reason_code });

                break;
            }

            self.sat_cp_mediator
                .synchronise_integer_trail_based_on_propositional_trail(
                    &self.sat_data_structures.assignments_propositional,
                    &mut self.cp_data_structures,
                    &mut self.cp_propagators,
                );

            //propagate boolean propagators - todo add these special-case propagators

            //propagate (conventional) CP propagators
            let propagation_status_one_step_cp = self.propagate_cp_one_step();

            match propagation_status_one_step_cp {
                PropagationStatusOneStepCP::ConflictDetected {
                    failure_reason: conflict_reason,
                } => {
                    self.sat_cp_mediator
                        .synchronise_propositional_trail_based_on_integer_trail(
                            &mut self.sat_data_structures.assignments_propositional,
                            &self.cp_data_structures.assignments_integer,
                        );

                    self.state.declare_cp_conflict(conflict_reason);
                    break;
                }
                PropagationStatusOneStepCP::PropagationHappened => {
                    //do nothing, the result will be that the clausal propagator will go next
                    //  recall that the idea is to always propagate simpler propagators before more complex ones
                    //  after a cp propagation was done one step, it is time to go to the clausal propagator
                }
                PropagationStatusOneStepCP::FixedPoint => {
                    break;
                }
            } //end match
        }

        self.counters.num_conflicts += self.state.conflict_detected() as u64;
        self.counters.num_conflicts_until_restart -= self.state.conflict_detected() as u64;

        self.counters.num_propagations +=
            self.sat_data_structures
                .assignments_propositional
                .num_assigned_propositional_variables() as u64
                - num_assigned_variables_old as u64;

        //Only check fixed point propagation if there was no reported conflict.
        pumpkin_assert_extreme!(
            self.state.conflict_detected()
                || DebugHelper::debug_fixed_point_propagation(
                    &self.cp_data_structures.assignments_integer,
                    &self.sat_data_structures,
                    &self.cp_propagators,
                )
        );
    }

    fn propagate_cp_one_step(&mut self) -> PropagationStatusOneStepCP {
        while !self.cp_data_structures.propagator_queue.is_empty() {
            let num_predicates_on_trail_before = self
                .cp_data_structures
                .assignments_integer
                .num_trail_entries();
            let propagator_identifier = self.cp_data_structures.propagator_queue.pop();
            let propagator = &mut self.cp_propagators[propagator_identifier.id as usize];
            let mut domains = DomainManager::new(
                propagator_identifier.id as usize,
                &mut self.cp_data_structures.assignments_integer,
            );

            let propagation_status_cp = propagator.propagate(&mut domains);

            match propagation_status_cp {
                //if there was a conflict, then stop any further propagation and proceed to conflict analysis
                PropagationStatusCP::ConflictDetected { failure_reason } => {
                    pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                        &self.cp_data_structures.assignments_integer,
                        &failure_reason,
                        propagator.as_ref(),
                        propagator_identifier,
                    ));

                    return PropagationStatusOneStepCP::ConflictDetected { failure_reason };
                }
                PropagationStatusCP::NoConflictDetected => {
                    //if at least one integer domain change was made, stop further propagation
                    //  the point is to go to the clausal propagator before continuing with other propagators
                    let num_propagations_done = self
                        .cp_data_structures
                        .assignments_integer
                        .num_trail_entries()
                        - num_predicates_on_trail_before;

                    if num_propagations_done > 0 {
                        //notify other propagators
                        //  note that during propagators, predicates are placed on the assignment_integer trail
                        //      but no notifying is done for propagators
                        //  this is because the propagator does not have all the info on which propagators to notify when propagating
                        //here we do the notification by removing the predicates from the trail, and apply them in the same order
                        //  but this time notify all propagators of the relevant changes
                        //  note that even the propagator that did the changes needs to be notified
                        //  since propagators are not required to propagate until a fixed point in one step

                        //the current solution of copying from the trail, popping, and reapplying is not ideal
                        //  todo think about better ways
                        let propagations = self
                            .cp_data_structures
                            .assignments_integer
                            .get_last_predicates_on_trail(num_propagations_done);
                        self.cp_data_structures
                            .assignments_integer
                            .undo_trail(num_propagations_done);

                        for predicate in propagations {
                            self.cp_data_structures.apply_predicate(
                                &predicate,
                                Some(propagator_identifier),
                                &mut self.cp_propagators,
                            );
                        }

                        return PropagationStatusOneStepCP::PropagationHappened;
                    }
                }
            }
        }
        PropagationStatusOneStepCP::FixedPoint
    }
}

//methods for adding constraints (propagators and clauses)
impl ConstraintSatisfactionSolver {
    pub fn add_propagator(&mut self, propagator_to_add: Box<dyn ConstraintProgrammingPropagator>) {
        pumpkin_assert_simple!(propagator_to_add.priority() <= 3, "The propagator priority exceeds 3. Currently we only support values up to 3, but this can easily be changed if there is a good reason.");

        self.sat_data_structures
            .clause_allocator
            .reduce_id_limit_by_one();

        let new_propagator_id = PropagatorIdentifier {
            id: self.cp_propagators.len() as u32,
        };
        self.cp_propagators.push(propagator_to_add);

        let new_propagator = &mut self.cp_propagators[new_propagator_id.id as usize];
        let mut domains = DomainManager::new(
            new_propagator_id.id as usize,
            &mut self.cp_data_structures.assignments_integer,
        );

        self.cp_data_structures
            .watch_list_cp
            .add_watches_for_propagator(new_propagator.as_ref(), new_propagator_id);

        new_propagator.initialise_at_root(&mut domains);

        let root_status = new_propagator.initialise_at_root(&mut domains);

        pumpkin_assert_simple!(root_status.no_conflict(), "For now we crash when adding a new propagator that detects a conflict at the root node, even though this is not necessarily an error. Should handle better in the future.");

        self.propagate_enqueued();
        pumpkin_assert_simple!(self.state.no_conflict(), "Root conflict detected after adding propagator, for now we crash the program but this may not necessarily be an error.");
    }

    pub fn add_permanent_clause(&mut self, literals: Vec<Literal>) -> ClauseAdditionOutcome {
        self.sat_data_structures.add_permanent_clause(literals)
    }

    pub fn add_permanent_implication_unchecked(&mut self, lhs: Literal, rhs: Literal) {
        self.sat_data_structures
            .add_permanent_implication_unchecked(lhs, rhs);
    }

    pub fn add_permanent_ternary_clause_unchecked(&mut self, a: Literal, b: Literal, c: Literal) {
        self.sat_data_structures
            .add_permanent_ternary_clause_unchecked(a, b, c);
    }

    pub fn add_unit_clause(&mut self, unit_clause: Literal) -> ClauseAdditionOutcome {
        pumpkin_assert_simple!(self.get_decision_level() == 0);
        pumpkin_assert_simple!(self.is_propagation_complete());

        //if the literal representing the unit clause is unassigned, assign it
        if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_unassigned(unit_clause)
        {
            self.sat_data_structures
                .assignments_propositional
                .enqueue_decision_literal(unit_clause);

            self.propagate_enqueued();

            if self.state.conflict_detected() {
                ClauseAdditionOutcome::Infeasible
            } else {
                ClauseAdditionOutcome::NoConflictDetected
            }
        }
        //the unit clause is already present, no need to do anything
        else if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_assigned_true(unit_clause)
        {
            ClauseAdditionOutcome::NoConflictDetected
        }
        //the unit clause is falsified at the root level
        else {
            ClauseAdditionOutcome::Infeasible
        }
    }
}

//methods for getting simple info out of the solver
impl ConstraintSatisfactionSolver {
    pub fn is_propagation_complete(&self) -> bool {
        self.sat_data_structures
            .is_clausal_propagation_at_fixed_point()
            && self.cp_data_structures.propagator_queue.is_empty()
    }

    fn get_decision_level(&self) -> u32 {
        pumpkin_assert_moderate!(
            self.sat_data_structures
                .assignments_propositional
                .get_decision_level()
                == self
                    .cp_data_structures
                    .assignments_integer
                    .get_decision_level()
        );
        self.sat_data_structures
            .assignments_propositional
            .get_decision_level()
    }
}

struct Counters {
    pub num_decisions: u64,
    pub num_conflicts: u64,
    pub num_propagations: u64,
    pub num_unit_clauses_learned: u64,
    pub num_conflicts_until_restart: u64,
    pub num_restarts: u64,
}

impl Counters {
    fn new(num_conflicts_until_restart: u64) -> Counters {
        Counters {
            num_decisions: 0,
            num_conflicts: 0,
            num_propagations: 0,
            num_unit_clauses_learned: 0,
            num_conflicts_until_restart,
            num_restarts: 0,
        }
    }
}

pub struct ConflictAnalysisResult {
    pub learned_literals: Vec<Literal>,
    pub backjump_level: u32,
}

#[derive(Default)]
enum CSPSolverStateInternal {
    #[default]
    Ready,
    Solving,
    ContainsSolution,
    ConflictClausal {
        conflict_clause_reference: ClauseReference,
    },
    ConflictCP {
        conflict_reason: PropositionalConjunction,
    },
    Infeasible,
    InfeasibleUnderAssumptions {
        violated_assumption: Literal,
    },
    Timeout,
}

pub struct CSPSolverState {
    internal_state: CSPSolverStateInternal,
}

impl CSPSolverState {
    pub fn new() -> CSPSolverState {
        CSPSolverState {
            internal_state: CSPSolverStateInternal::Ready,
        }
    }

    pub fn is_ready(&self) -> bool {
        matches!(self.internal_state, CSPSolverStateInternal::Ready)
    }

    pub fn no_conflict(&self) -> bool {
        !self.conflict_detected()
    }

    pub fn conflict_detected(&self) -> bool {
        self.is_clausal_conflict() || self.is_cp_conflict()
    }

    pub fn is_clausal_conflict(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::ConflictClausal {
                conflict_clause_reference: _
            }
        )
    }

    pub fn is_cp_conflict(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::ConflictCP { conflict_reason: _ }
        )
    }

    pub fn is_infeasible_under_assumptions(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::InfeasibleUnderAssumptions {
                violated_assumption: _
            }
        )
    }

    pub fn get_violated_assumption(&self) -> Literal {
        if let CSPSolverStateInternal::InfeasibleUnderAssumptions {
            violated_assumption,
        } = self.internal_state
        {
            violated_assumption
        } else {
            panic!("Cannot extract violated assumption without getting the solver into the infeasible under assumptions state.");
        }
    }

    pub fn get_conflict_clause_reference(&self) -> ClauseReference {
        if let CSPSolverStateInternal::ConflictClausal {
            conflict_clause_reference,
        } = self.internal_state
        {
            conflict_clause_reference
        } else {
            panic!("Cannot extract conflict clause if solver is not in a clausal conflict.");
        }
    }

    pub fn get_conflict_reason_cp(&self) -> &PropositionalConjunction {
        if let CSPSolverStateInternal::ConflictCP { conflict_reason } = &self.internal_state {
            conflict_reason
        } else {
            panic!("Cannot extract conflict reason of a cp propagator if solver is not in a cp conflict.");
        }
    }

    pub fn timeout(&self) -> bool {
        matches!(self.internal_state, CSPSolverStateInternal::Timeout)
    }

    pub fn has_solution(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::ContainsSolution
        )
    }

    fn declare_ready(&mut self) {
        pumpkin_assert_simple!(self.has_solution());
        self.internal_state = CSPSolverStateInternal::Ready;
    }

    fn declare_solving(&mut self) {
        pumpkin_assert_simple!(self.is_ready() || self.conflict_detected());
        self.internal_state = CSPSolverStateInternal::Solving;
    }

    fn declare_infeasible(&mut self) {
        self.internal_state = CSPSolverStateInternal::Infeasible;
    }

    fn declare_clausal_conflict(&mut self, failure_reference: ClauseReference) {
        self.internal_state = CSPSolverStateInternal::ConflictClausal {
            conflict_clause_reference: failure_reference,
        };
    }

    fn declare_cp_conflict(&mut self, failure_reason: PropositionalConjunction) {
        self.internal_state = CSPSolverStateInternal::ConflictCP {
            conflict_reason: failure_reason,
        };
    }

    fn declare_solution_found(&mut self) {
        self.internal_state = CSPSolverStateInternal::ContainsSolution;
    }

    fn declare_timeout(&mut self) {
        self.internal_state = CSPSolverStateInternal::Timeout;
    }

    fn declare_infeasible_under_assumptions(&mut self, violated_assumption: Literal) {
        self.internal_state = CSPSolverStateInternal::InfeasibleUnderAssumptions {
            violated_assumption,
        }
    }
}

pub struct ConstraintSatisfactionSolverInternalParameters {
    pub num_conflicts_per_restart: u64,
}

impl ConstraintSatisfactionSolverInternalParameters {
    pub fn new(
        argument_handler: &ArgumentHandler,
    ) -> ConstraintSatisfactionSolverInternalParameters {
        ConstraintSatisfactionSolverInternalParameters {
            num_conflicts_per_restart: argument_handler
                .get_integer_argument("num-conflicts-per-restart")
                as u64,
        }
    }
}
