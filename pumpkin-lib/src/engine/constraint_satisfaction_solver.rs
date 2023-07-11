use super::clause_allocators::ClauseAllocatorBasic;
use super::cp::CPEngineDataStructures;
use super::sat::SATEngineDataStructures;
use super::{
    AssignmentsInteger, AssignmentsPropositional, GlucoseRestartStrategy, LearnedClauseManager,
    LearnedClauseMinimiser, SATCPMediator, SatOptions,
};
use crate::basic_types::sequence_generators::SequenceGeneratorType;
use crate::basic_types::{
    BranchingDecision, CSPSolverExecutionFlag, ConflictInfo, ConstraintOperationError, DomainId,
    Literal, PropagationStatusCP, PropagationStatusOneStepCP, PropositionalVariable, Stopwatch,
};
use crate::engine::clause_allocators::ClauseInterface;
use crate::engine::{DebugHelper, DomainManager};
use crate::propagators::clausal_propagators::{ClausalPropagatorBasic, ClausalPropagatorInterface};
use crate::propagators::ConstraintProgrammingPropagator;
use crate::{
    pumpkin_assert_advanced, pumpkin_assert_extreme, pumpkin_assert_moderate, pumpkin_assert_simple,
};
use log::warn;
use std::fs::File;
use std::io::Write;

pub type ClausalPropagator = ClausalPropagatorBasic;
pub type ClauseAllocator = ClauseAllocatorBasic;

pub struct ConstraintSatisfactionSolver {
    state: CSPSolverState,
    sat_data_structures: SATEngineDataStructures,
    cp_data_structures: CPEngineDataStructures,
    clausal_propagator: ClausalPropagator,
    learned_clause_manager: LearnedClauseManager,
    learned_clause_minimiser: LearnedClauseMinimiser,
    restart_strategy: GlucoseRestartStrategy,
    cp_propagators: Vec<Box<dyn ConstraintProgrammingPropagator>>,
    sat_cp_mediator: SATCPMediator,
    seen: Vec<bool>,
    counters: Counters,
    internal_parameters: SatisfactionSolverOptions,
    stopwatch: Stopwatch,
    analysis_result: ConflictAnalysisResult,
}

pub struct SatisfactionSolverOptions {
    // see the main.rs parameters for more details
    /// Parameters related to restarts
    pub restart_sequence_generator_type: SequenceGeneratorType,
    pub restart_base_interval: u64,
    pub restart_min_num_conflicts_before_first_restart: u64,
    pub restart_lbd_coef: f64,
    pub restart_num_assigned_coef: f64,
    pub restart_num_assigned_window: u64,
    pub restart_geometric_coef: Option<f64>,

    pub learning_clause_minimisation: bool,

    /// Certificate output file or None if certificate output is disabled.
    pub certificate_file: Option<File>,
}

//methods that offer basic functionality
impl ConstraintSatisfactionSolver {
    pub fn new(
        sat_options: SatOptions,
        solver_options: SatisfactionSolverOptions,
    ) -> ConstraintSatisfactionSolver {
        let mut csp_solver = ConstraintSatisfactionSolver {
            state: CSPSolverState::default(),
            sat_data_structures: SATEngineDataStructures::default(),
            cp_data_structures: CPEngineDataStructures::default(),
            clausal_propagator: ClausalPropagator::default(),
            learned_clause_manager: LearnedClauseManager::new(sat_options),
            learned_clause_minimiser: LearnedClauseMinimiser::default(),
            restart_strategy: GlucoseRestartStrategy::new(&solver_options),
            cp_propagators: vec![],
            sat_cp_mediator: SATCPMediator::default(),
            seen: vec![],
            counters: Counters::new(),
            internal_parameters: solver_options,
            stopwatch: Stopwatch::new(i64::MAX),
            analysis_result: ConflictAnalysisResult::default(),
        };

        //we introduce a dummy variable set to true at the root level
        //  this is useful for convenience when a fact needs to be expressed that is always true
        //  e.g., this makes writing propagator explanations easier for corner cases
        let root_variable = csp_solver
            .sat_cp_mediator
            .create_new_propositional_variable(
                &mut csp_solver.clausal_propagator,
                &mut csp_solver.sat_data_structures,
            );
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

        let result = csp_solver.add_unit_clause(true_literal);
        pumpkin_assert_simple!(result.is_ok());

        csp_solver
    }

    pub fn solve_under_assumptions(
        &mut self,
        assumptions: &[Literal],
        time_limit_in_seconds: i64,
    ) -> CSPSolverExecutionFlag {
        if self.state.is_infeasible() {
            return CSPSolverExecutionFlag::Infeasible;
        }

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
        self.sat_cp_mediator.create_new_propositional_variable(
            &mut self.clausal_propagator,
            &mut self.sat_data_structures,
        )
    }

    pub fn create_new_integer_variable(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        self.sat_cp_mediator.create_new_integer_variable(
            lower_bound,
            upper_bound,
            &mut self.clausal_propagator,
            &mut self.sat_data_structures,
            &mut self.cp_data_structures,
        )
    }

    pub fn new_literals(&mut self) -> impl Iterator<Item = Literal> + '_ {
        std::iter::from_fn(|| Some(self.create_new_propositional_variable()))
            .map(|var| Literal::new(var, true))
    }

    pub fn get_propositional_assignments(&self) -> &AssignmentsPropositional {
        &self.sat_data_structures.assignments_propositional
    }

    pub fn get_lower_bound_literal(&self, integer_variable: DomainId, lower_bound: i32) -> Literal {
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

            self.learned_clause_manager
                .shrink_learned_clause_database_if_needed(
                    &mut self.sat_data_structures,
                    &mut self.clausal_propagator,
                );

            self.propagate_enqueued();

            if self.state.no_conflict() {
                if self.restart_strategy.should_restart() {
                    self.restart_during_search();
                }

                self.declare_new_decision_level();

                let branching_result = self.enqueue_next_decision();
                if let Err(flag) = branching_result {
                    return flag;
                }
            }
            //conflict
            else {
                if self
                    .sat_data_structures
                    .assignments_propositional
                    .is_at_the_root_level()
                {
                    self.state.declare_infeasible();
                    return CSPSolverExecutionFlag::Infeasible;
                }

                self.resolve_conflict();

                self.learned_clause_manager.decay_clause_activities();

                self.sat_data_structures
                    .propositional_variable_selector
                    .decay_activities();
            }
        }
    }

    fn enqueue_next_decision(&mut self) -> Result<(), CSPSolverExecutionFlag> {
        match self.sat_data_structures.get_next_branching_decision() {
            Some(branching_decision) => match branching_decision {
                BranchingDecision::Assumption { assumption_literal } => {
                    let success = self.enqueue_assumption_literal(assumption_literal);
                    if !success {
                        return Err(CSPSolverExecutionFlag::InfeasibleUnderAssumptions);
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
                return Err(CSPSolverExecutionFlag::Feasible);
            }
        }
        Ok(())
    }

    // returns true if the assumption was successfully enqueued, and false otherwise
    fn enqueue_assumption_literal(&mut self, assumption_literal: Literal) -> bool {
        //Case 1: the assumption is unassigned, assign it
        if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_unassigned(assumption_literal)
        {
            self.sat_data_structures
                .assignments_propositional
                .enqueue_decision_literal(assumption_literal);
            true
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
            true
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
            false
        }
    }

    fn declare_new_decision_level(&mut self) {
        self.sat_data_structures
            .assignments_propositional
            .increase_decision_level();
        self.cp_data_structures
            .assignments_integer
            .increase_decision_level();
    }

    fn write_to_certificate(&mut self) -> std::io::Result<()> {
        if let Some(cert_file) = &mut self.internal_parameters.certificate_file {
            for lit in &self.analysis_result.learned_literals {
                if lit.is_negative() {
                    cert_file.write_all("-".as_bytes())?;
                }
                cert_file.write_all(
                    format!("{} ", &lit.get_propositional_variable().index().to_string())
                        .as_bytes(),
                )?;
            }
            cert_file.write_all("0\n".as_bytes())?;
        }
        Ok(())
    }

    //changes the state based on the conflict analysis result given as input
    //i.e., adds the learned clause to the database, backtracks, enqueues the propagated literal, and updates internal data structures for simple moving averages
    //note that no propagation is done, this is left to the solver
    fn resolve_conflict(&mut self) {
        pumpkin_assert_moderate!(self.state.conflicting());

        //compute the learned clause
        self.compute_1uip(); //the result is stored in self.analysis_result

        //now process the learned clause, i.e., add it to the clause database
        if let Err(write_error) = self.write_to_certificate() {
            warn!(
                "Failed to update the certificate file, error message: {}",
                write_error
            );
        }

        //unit clauses are treated in a special way: they are added as decision literals at decision level 0
        if self.analysis_result.learned_literals.len() == 1 {
            //important to notify about the conflict _before_ backtracking removes literals from the trail
            self.restart_strategy.notify_conflict(
                1,
                self.sat_data_structures
                    .assignments_propositional
                    .num_assigned_propositional_variables(),
            );

            self.backtrack(0);

            let unit_clause = self.analysis_result.learned_literals[0];

            pumpkin_assert_simple!(
                self.sat_data_structures
                    .assignments_propositional
                    .is_literal_unassigned(unit_clause),
                "Do not expect to learn a literal that is already set."
            );

            self.sat_data_structures
                .assignments_propositional
                .enqueue_decision_literal(unit_clause);

            self.counters.num_unit_clauses_learned +=
                (self.analysis_result.learned_literals.len() == 1) as u64;
        } else {
            //important to get trail length before the backtrack
            let num_variables_assigned_before_conflict = self
                .sat_data_structures
                .assignments_propositional
                .num_assigned_propositional_variables();

            self.backtrack(self.analysis_result.backjump_level);

            self.learned_clause_manager.add_learned_clause(
                self.analysis_result.learned_literals.clone(), //todo not ideal with clone
                &mut self.clausal_propagator,
                &mut self.sat_data_structures.assignments_propositional,
                &mut self.sat_data_structures.clause_allocator,
            );

            let lbd = self.learned_clause_manager.compute_lbd_for_literals(
                &self.analysis_result.learned_literals,
                &self.sat_data_structures.assignments_propositional,
            );

            self.restart_strategy
                .notify_conflict(lbd, num_variables_assigned_before_conflict);
        }

        self.state.declare_solving();
    }

    //a restart differs from backtracking to level zero
    //   in that a restart backtrack to decision level zero and then performs additional operations,
    //      e.g., clean up learned clauses, adjust restart frequency, etc.
    fn restart_during_search(&mut self) {
        if self.get_decision_level() == 0 {
            return;
        }

        self.backtrack(0);

        self.restart_strategy.notify_restart();
    }

    fn backtrack(&mut self, backtrack_level: u32) {
        pumpkin_assert_simple!(backtrack_level < self.get_decision_level());

        self.sat_data_structures.backtrack(backtrack_level);

        self.clausal_propagator.synchronise(
            self.sat_data_structures
                .assignments_propositional
                .num_assigned_propositional_variables() as usize,
        );

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
                propagator_id as u32,
                &mut self.cp_data_structures.assignments_integer,
            );
            self.cp_propagators[propagator_id].synchronise(&domains);
        }
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

            let clausal_propagation_status = self.clausal_propagator.propagate(
                &mut self.sat_data_structures.assignments_propositional,
                &mut self.sat_data_structures.clause_allocator,
            );

            if let Err(conflict_info) = clausal_propagation_status {
                self.state.declare_conflict(conflict_info);
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
                PropagationStatusOneStepCP::ConflictDetected { failure_reason } => {
                    self.sat_cp_mediator
                        .synchronise_propositional_trail_based_on_integer_trail(
                            &mut self.sat_data_structures.assignments_propositional,
                            &self.cp_data_structures.assignments_integer,
                        );
                    self.state.declare_conflict(ConflictInfo::Explanation {
                        propositional_conjunction: failure_reason,
                    });
                    //self.state.declare_cp_conflict(conflict_reason);
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

        self.counters.num_conflicts += self.state.conflicting() as u64;

        self.counters.num_propagations +=
            self.sat_data_structures
                .assignments_propositional
                .num_assigned_propositional_variables() as u64
                - num_assigned_variables_old as u64;

        //Only check fixed point propagation if there was no reported conflict.
        pumpkin_assert_extreme!(
            self.state.conflicting()
                || DebugHelper::debug_fixed_point_propagation(
                    &self.clausal_propagator,
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
            let propagator_id = self.cp_data_structures.propagator_queue.pop();
            let propagator = &mut self.cp_propagators[propagator_id as usize];
            let mut domains = DomainManager::new(
                propagator_id,
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
                        propagator_id,
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
                                Some(propagator_id),
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
    pub fn add_propagator(
        &mut self,
        propagator_to_add: Box<dyn ConstraintProgrammingPropagator>,
    ) -> bool {
        pumpkin_assert_simple!(propagator_to_add.priority() <= 3, "The propagator priority exceeds 3. Currently we only support values up to 3, but this can easily be changed if there is a good reason.");

        let new_propagator_id = self.cp_propagators.len() as u32;
        self.cp_propagators.push(propagator_to_add);

        let new_propagator = &mut self.cp_propagators[new_propagator_id as usize];
        let mut domains = DomainManager::new(
            new_propagator_id,
            &mut self.cp_data_structures.assignments_integer,
        );

        self.cp_data_structures
            .watch_list_cp
            .add_watches_for_propagator(new_propagator.as_ref(), new_propagator_id);

        if new_propagator
            .initialise_at_root(&mut domains)
            .conflict_detected()
        {
            false
        } else {
            self.propagate_enqueued();

            self.state.conflicting()
        }
    }

    pub fn add_permanent_clause(
        &mut self,
        literals: Vec<Literal>,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_moderate!(self.is_propagation_complete());

        if self.state.is_infeasible() {
            return Err(ConstraintOperationError::InfeasibleState);
        }

        let result = self.clausal_propagator.add_permanent_clause(
            literals,
            &mut self.sat_data_structures.assignments_propositional,
            &mut self.sat_data_structures.clause_allocator,
        );

        if result.is_err() {
            self.state.declare_infeasible()
        }
        result
    }

    pub fn add_permanent_implication_unchecked(&mut self, lhs: Literal, rhs: Literal) {
        self.clausal_propagator.add_permanent_implication_unchecked(
            lhs,
            rhs,
            &mut self.sat_data_structures.clause_allocator,
        );
    }

    pub fn add_permanent_ternary_clause_unchecked(&mut self, a: Literal, b: Literal, c: Literal) {
        self.clausal_propagator
            .add_permanent_ternary_clause_unchecked(
                a,
                b,
                c,
                &mut self.sat_data_structures.clause_allocator,
            );
    }

    pub fn add_unit_clause(
        &mut self,
        unit_clause: Literal,
    ) -> Result<(), ConstraintOperationError> {
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

            if self.state.conflicting() {
                Err(ConstraintOperationError::InfeasibleClause)
            } else {
                Ok(())
            }
        }
        //the unit clause is already present, no need to do anything
        else if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_assigned_true(unit_clause)
        {
            Ok(())
        }
        //the unit clause is falsified at the root level
        else {
            Err(ConstraintOperationError::InfeasibleClause)
        }
    }
}

//methods for getting simple info out of the solver
impl ConstraintSatisfactionSolver {
    pub fn is_propagation_complete(&self) -> bool {
        self.clausal_propagator.is_propagation_complete(
            self.sat_data_structures
                .assignments_propositional
                .trail
                .len(),
        ) && self.cp_data_structures.propagator_queue.is_empty()
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

//methods for conflict analysis
impl ConstraintSatisfactionSolver {
    //computes the 1uip and stores it in 'analysis_result'
    pub fn compute_1uip(&mut self) {
        pumpkin_assert_simple!(
            self.seen.len() as u32
                == self
                    .sat_data_structures
                    .assignments_propositional
                    .num_propositional_variables()
        );
        pumpkin_assert_simple!(self.sat_cp_mediator.explanation_clause_manager.is_empty());
        pumpkin_assert_simple!(!self
            .sat_data_structures
            .assignments_propositional
            .is_at_the_root_level());
        pumpkin_assert_advanced!(self.seen.iter().all(|b| !b));

        self.analysis_result.learned_literals.resize(
            1,
            self.sat_data_structures
                .assignments_propositional
                .true_literal, //dummy literal, the point is that we allocate space for the asserting literal, which will by convention be placed at index 0
        );
        self.analysis_result.backjump_level = 0;

        let mut num_current_decision_level_literals_to_inspect = 0;
        let mut next_trail_index = self
            .sat_data_structures
            .assignments_propositional
            .trail
            .len()
            - 1;
        let mut next_literal: Option<Literal> = None;

        loop {
            pumpkin_assert_moderate!(self.debug_conflict_analysis_check_next_literal(
                next_literal,
                &self.sat_data_structures
            ));
            //note that the 'next_literal' is only None in the first iterator
            let clause_reference = if let Some(propagated_literal) = next_literal {
                self.sat_cp_mediator.get_propagation_clause_reference(
                    propagated_literal,
                    &self.clausal_propagator,
                    &mut self.sat_data_structures,
                    &self.cp_data_structures,
                    &mut self.cp_propagators,
                )
            } else {
                self.sat_cp_mediator.get_conflict_reason_clause_reference(
                    self.state.get_conflict_info(),
                    &mut self.sat_data_structures,
                    &self.cp_data_structures,
                    &mut self.cp_propagators,
                )
            };

            //todo
            //for simplicity we bump every clause, even though it is not meaningful to bump virtual binary and explanation clauses
            //in the future we could look into avoiding this
            //  it somewhat depends on how we decide to handle explanation clauses in the future
            //also we allocate new clauses for each explanation, this may change in the future
            self.learned_clause_manager
                .update_clause_lbd_and_bump_activity(
                    clause_reference,
                    &self.sat_data_structures.assignments_propositional,
                    &mut self.sat_data_structures.clause_allocator,
                );

            //process the reason literal
            //	i.e., perform resolution and update other related internal data structures
            let start_index = (next_literal.is_some()) as usize; //note that the start index will be either 0 or 1 - the idea is to skip the 0th literal in case the clause represents a propagation
            for &reason_literal in &self.sat_data_structures.clause_allocator[clause_reference]
                .get_literal_slice()[start_index..]
            {
                //only consider non-root assignments that have not been considered before
                let is_root_assignment = self
                    .sat_data_structures
                    .assignments_propositional
                    .is_literal_root_assignment(reason_literal);
                let seen = self.seen[reason_literal.get_propositional_variable()];

                if !is_root_assignment && !seen {
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

                    num_current_decision_level_literals_to_inspect +=
                        is_current_level_assignment as usize;

                    //literals from previous decision levels are considered for the learnt clause
                    if !is_current_level_assignment {
                        self.analysis_result.learned_literals.push(reason_literal);
                        //the highest decision level literal must be placed at index 1 to prepare the clause for propagation
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

            //after resolution took place, find the next literal on the trail that is relevant for this conflict
            //only literals that have been seen so far are relevant
            //  note that there may be many literals that are not relevant
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
            num_current_decision_level_literals_to_inspect -= 1;
            next_trail_index -= 1;

            //once the counters hits zero we stop, the 1UIP has been found
            //  the next literal is the asserting literal
            if num_current_decision_level_literals_to_inspect == 0 {
                self.analysis_result.learned_literals[0] = !next_literal.unwrap();
                break;
            }
        }

        //clear the seen flags for literals in the learned clause
        //  note that other flags have already been cleaned above in the previous loop
        for literal in &self.analysis_result.learned_literals {
            self.seen[literal.get_propositional_variable()] = false;
        }

        if self.internal_parameters.learning_clause_minimisation {
            pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result());

            self.learned_clause_minimiser.remove_dominated_literals(
                &mut self.analysis_result,
                &self.clausal_propagator,
                &mut self.sat_data_structures,
                &self.cp_data_structures,
                &mut self.sat_cp_mediator,
                &mut self.cp_propagators,
            );
        }

        self.sat_cp_mediator
            .explanation_clause_manager
            .clean_up_explanation_clauses(&mut self.sat_data_structures.clause_allocator);

        pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result());
        //the return value is stored in the input 'analysis_result'
    }

    fn debug_check_conflict_analysis_result(&self) -> bool {
        //debugging method: performs sanity checks on the learned clause

        let assignments = &self.sat_data_structures.assignments_propositional;
        let learned_lits = &self.analysis_result.learned_literals;

        assert!(
            self.analysis_result.backjump_level < self.get_decision_level(),
            "Backjump level must be lower than the current level."
        );

        assert!(
            learned_lits
                .iter()
                .all(|&literal| !assignments.is_literal_root_assignment(literal)),
            "No root level literals may be present in a learned clause."
        );

        assert!(
            self.get_decision_level()
                == assignments
                    .get_literal_assignment_level(self.analysis_result.learned_literals[0]),
            "The asserting literal must be at the highest level."
        );

        assert!(
            learned_lits[1..].iter().all(|&literal| {
                assignments.get_literal_assignment_level(literal) != self.get_decision_level()
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
            assert!(
                self.analysis_result.backjump_level
                    == assignments.get_literal_assignment_level(learned_lits[1]),
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

    fn debug_conflict_analysis_check_next_literal(
        &self,
        next_literal: Option<Literal>,
        sat_data_structures: &SATEngineDataStructures,
    ) -> bool {
        //in conflict analysis, literals are examined in reverse order on the trail
        //the examined literals are expected to be:
        //  1. from the same decision level - the current (last) decision level
        //  2. propagated, unless the literal is the decision literal of the current decision level
        //  3. not root assignments
        //failing any of the conditions above means something went wrong with the conflict analysis, e.g., some explanation was faulty and caused the solver to overrun the trail

        //note that in the first iteration, the next_literal will be set to None, so we can skip this check
        match next_literal {
            None => true,
            Some(next_literal) => {
                if sat_data_structures
                    .assignments_propositional
                    .is_literal_root_assignment(next_literal)
                {
                    return false;
                }

                let is_propagated = sat_data_structures
                    .assignments_propositional
                    .is_literal_propagated(next_literal);

                let current_decision_level = sat_data_structures
                    .assignments_propositional
                    .get_decision_level() as usize;

                let decision_level_start_index = sat_data_structures
                    .assignments_propositional
                    .trail_delimiter[current_decision_level - 1]
                    as usize; //the literal is not a root assignment at this point so we can do -1

                let is_decision_literal_of_current_level =
                    sat_data_structures.assignments_propositional.trail[decision_level_start_index]
                        == next_literal;

                let is_assigned_at_current_decision_level = sat_data_structures
                    .assignments_propositional
                    .get_literal_assignment_level(next_literal)
                    == current_decision_level as u32;

                (is_propagated || is_decision_literal_of_current_level)
                    && is_assigned_at_current_decision_level
            }
        }
    }
}

struct Counters {
    pub num_decisions: u64,
    pub num_conflicts: u64,
    pub num_propagations: u64,
    pub num_unit_clauses_learned: u64,
}

impl Counters {
    fn new() -> Counters {
        Counters {
            num_decisions: 0,
            num_conflicts: 0,
            num_propagations: 0,
            num_unit_clauses_learned: 0,
        }
    }
}

#[derive(Clone, Default)]
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
    /*ConflictClausal {
        conflict_clause: ConflictInfo,
    },
    ConflictCP {
        conflict_reason: PropositionalConjunction,
    },*/
    Conflict {
        conflict_info: ConflictInfo,
    },
    Infeasible,
    InfeasibleUnderAssumptions {
        violated_assumption: Literal,
    },
    Timeout,
}

#[derive(Default)]
pub struct CSPSolverState {
    internal_state: CSPSolverStateInternal,
}

impl CSPSolverState {
    pub fn is_ready(&self) -> bool {
        matches!(self.internal_state, CSPSolverStateInternal::Ready)
    }

    pub fn no_conflict(&self) -> bool {
        !self.conflicting()
    }

    pub fn conflicting(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::Conflict { conflict_info: _ }
        )
        //self.is_clausal_conflict() || self.is_cp_conflict()
    }

    /*pub fn is_clausal_conflict(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::ConflictClausal { conflict_clause: _ }
        )
    }

    pub fn is_cp_conflict(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::ConflictCP { conflict_reason: _ }
        )
    }*/

    pub fn is_infeasible(&self) -> bool {
        matches!(self.internal_state, CSPSolverStateInternal::Infeasible)
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

    pub fn get_conflict_info(&self) -> &ConflictInfo {
        if let CSPSolverStateInternal::Conflict { conflict_info } = &self.internal_state {
            conflict_info
        } else {
            panic!("Cannot extract conflict clause if solver is not in a clausal conflict.");
        }
    }

    /*pub fn get_conflict_clause(&self) -> ConflictInfo {
        if let CSPSolverStateInternal::ConflictClausal { conflict_clause } = self.internal_state {
            conflict_clause
        } else {
            panic!("Cannot extract conflict clause if solver is not in a clausal conflict.");
        }
    }*/

    /*pub fn get_conflict_reason_cp(&self) -> &PropositionalConjunction {
        if let CSPSolverStateInternal::ConflictCP { conflict_reason } = &self.internal_state {
            conflict_reason
        } else {
            panic!("Cannot extract conflict reason of a cp propagator if solver is not in a cp conflict.");
        }
    }*/

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
        pumpkin_assert_simple!(self.has_solution() && !self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::Ready;
    }

    fn declare_solving(&mut self) {
        pumpkin_assert_simple!((self.is_ready() || self.conflicting()) && !self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::Solving;
    }

    fn declare_infeasible(&mut self) {
        self.internal_state = CSPSolverStateInternal::Infeasible;
    }

    fn declare_conflict(&mut self, conflict_info: ConflictInfo) {
        pumpkin_assert_simple!(!self.conflicting());
        self.internal_state = CSPSolverStateInternal::Conflict { conflict_info };
    }

    fn declare_solution_found(&mut self) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::ContainsSolution;
    }

    fn declare_timeout(&mut self) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::Timeout;
    }

    fn declare_infeasible_under_assumptions(&mut self, violated_assumption: Literal) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::InfeasibleUnderAssumptions {
            violated_assumption,
        }
    }
}

//the defaults below are used only for the tests
//  could rethink if we can do this a different way, e.g., using cli default values
impl Default for SatisfactionSolverOptions {
    fn default() -> Self {
        SatisfactionSolverOptions {
            certificate_file: None,
            restart_sequence_generator_type: SequenceGeneratorType::Constant,
            restart_base_interval: 50,
            restart_min_num_conflicts_before_first_restart: 10000,
            restart_lbd_coef: 1.25,
            restart_num_assigned_coef: 1.4,
            restart_num_assigned_window: 5000,
            restart_geometric_coef: None,
            learning_clause_minimisation: true,
        }
    }
}

impl Default for ConstraintSatisfactionSolver {
    fn default() -> Self {
        ConstraintSatisfactionSolver::new(
            SatOptions::default(),
            SatisfactionSolverOptions::default(),
        )
    }
}
