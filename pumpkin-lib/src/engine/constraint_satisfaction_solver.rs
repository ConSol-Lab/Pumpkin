use std::fmt::Debug;
use std::fmt::Formatter;
use std::fs::File;
use std::io::Write;
use std::time::Instant;

use log::info;
use log::warn;

use crate::basic_types::moving_average::CumulativeMovingAverage;
use crate::basic_types::moving_average::MovingAverageInterface;
use crate::basic_types::sequence_generators::SequenceGeneratorType;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConflictInfo;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::DomainId;
use crate::basic_types::Inconsistency;
use crate::basic_types::KeyedVec;
use crate::basic_types::Literal;
use crate::basic_types::Predicate;
use crate::basic_types::PropagationStatusOneStepCP;
use crate::basic_types::PropositionalVariable;
use crate::basic_types::Stopwatch;
use crate::basic_types::StorageKey;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::clause_allocators::ClauseAllocatorBasic;
use crate::engine::clause_allocators::ClauseInterface;
use crate::engine::cp::CPEngineDataStructures;
use crate::engine::debug_helper::DebugDyn;
use crate::engine::sat::SATEngineDataStructures;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::DebugHelper;
use crate::engine::GlucoseRestartStrategy;
use crate::engine::LearnedClauseManager;
use crate::engine::LearnedClauseMinimiser;
use crate::engine::PropagatorConstructorContext;
use crate::engine::PropagatorId;
use crate::engine::SATCPMediator;
use crate::engine::SatOptions;
use crate::propagators::clausal_propagators::ClausalPropagatorBasic;
use crate::propagators::clausal_propagators::ClausalPropagatorInterface;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

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
    seen: KeyedVec<PropositionalVariable, bool>,
    counters: Counters,
    internal_parameters: SatisfactionSolverOptions,
    stopwatch: Stopwatch,
    analysis_result: ConflictAnalysisResult,
}

impl Debug for ConstraintSatisfactionSolver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let cp_propagators: Vec<_> = self
            .cp_propagators
            .iter()
            .map(|_| DebugDyn::from("ConstraintProgrammingPropagator"))
            .collect();
        f.debug_struct("ConstraintSatisfactionSolver")
            .field("state", &self.state)
            .field("sat_data_structures", &self.sat_data_structures)
            .field("cp_data_structures", &self.cp_data_structures)
            .field("clausal_propagator", &self.clausal_propagator)
            .field("learned_clause_manager", &self.learned_clause_manager)
            .field("learned_clause_minimiser", &self.learned_clause_minimiser)
            .field("restart_strategy", &self.restart_strategy)
            .field("cp_propagators", &cp_propagators)
            .field("sat_cp_mediator", &self.sat_cp_mediator)
            .field("seen", &self.seen)
            .field("counters", &self.counters)
            .field("internal_parameters", &self.internal_parameters)
            .field("stopwatch", &self.stopwatch)
            .field("analysis_result", &self.analysis_result)
            .finish()
    }
}

#[derive(Debug)]
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

// methods that offer basic functionality
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
            seen: KeyedVec::default(),
            counters: Counters::default(),
            internal_parameters: solver_options,
            stopwatch: Stopwatch::new(i64::MAX),
            analysis_result: ConflictAnalysisResult::default(),
        };

        // we introduce a dummy variable set to true at the root level
        //  this is useful for convenience when a fact needs to be expressed that is always true
        //  e.g., this makes writing propagator explanations easier for corner cases
        let root_variable = csp_solver
            .sat_cp_mediator
            .create_new_propositional_variable(
                &mut csp_solver.cp_data_structures.watch_list_propositional,
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
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        if self.state.is_infeasible() {
            return CSPSolverExecutionFlag::Infeasible;
        }

        let start_time = Instant::now();

        self.initialise(assumptions, time_limit_in_seconds);
        let result = self.solve_internal(brancher);

        self.counters.time_spent_in_solver += start_time.elapsed().as_millis() as u64;

        result
    }

    /// Returns an unsatisfiable core.
    ///
    /// We define an unsatisfiable core as a clause containing only negated assumption literals,
    /// which is implied by the formula. Alternatively, it is the negation of a conjunction of
    /// assumptions which cannot be satisfied together with the rest of the formula. The clause is
    /// not necessarily unique or minimal.
    ///
    /// The unsatisfiable core can be verified with reverse unit propagation (RUP).
    ///
    /// *Notes:*
    ///   - If the solver is not in an unsatisfied state, this method will panic.
    ///   - If the solver is in an unsatisfied state, but solving was done without assumptions,
    ///   this will return an empty vector.
    ///   - If the assumptions are inconsistent, i.e. both literal x and !x are assumed, an error
    ///   is returned, with the literal being one of the inconsistent assumptions.
    ///
    /// # Example usage
    /// ```rust
    /// // We construct the following SAT instance:
    /// //   (x0 \/ x1 \/ x2) /\ (x0 \/ !x1 \/ x2)
    /// // And solve under the assumptions:
    /// //   !x0 /\ x1 /\ !x2
    /// # use pumpkin_lib::engine::ConstraintSatisfactionSolver;
    /// # use pumpkin_lib::basic_types::{PropositionalVariable, Literal};
    /// # use pumpkin_lib::branching::IndependentVariableValueBrancher;
    /// let solver = ConstraintSatisfactionSolver::default();
    ///
    /// let mut solver = ConstraintSatisfactionSolver::default();
    /// let x = solver.new_literals().take(3).collect::<Vec<_>>();
    ///
    /// solver.add_permanent_clause(vec![x[0], x[1], x[2]]);
    /// solver.add_permanent_clause(vec![x[0], !x[1], x[2]]);
    ///
    /// let assumptions = [!x[0], x[1], !x[2]];
    /// let variables = solver
    ///     .get_propositional_assignments()
    ///     .get_propositional_variables()
    ///     .collect::<Vec<_>>();
    /// let mut brancher =
    ///     IndependentVariableValueBrancher::default_over_all_propositional_variables(&solver);
    /// solver.solve_under_assumptions(&assumptions, i64::MAX, &mut brancher);
    ///
    /// let core = solver
    ///     .extract_clausal_core(&mut brancher)
    ///     .expect("the instance is unsatisfiable");
    ///
    /// // The order of the literals in the core is undefined, so we check for unordered equality.
    /// assert_eq!(
    ///     core.len(),
    ///     assumptions.len(),
    ///     "the core has the length of the number of assumptions"
    /// );
    /// assert!(
    ///     core.iter().all(|&lit| assumptions.contains(&!lit)),
    ///     "all literals in the core are negated assumptions"
    /// );
    /// ```
    pub fn extract_clausal_core(
        &mut self,
        brancher: &mut impl Brancher,
    ) -> Result<Vec<Literal>, Literal> {
        pumpkin_assert_simple!(self.debug_check_core_extraction());

        if self.state.is_infeasible() {
            return Ok(vec![]);
        }

        let violated_assumption = self.state.get_violated_assumption();

        // we consider three cases:
        //  1. The assumption is falsified at the root level
        //  2. The assumption is inconsistent with other assumptions, e.g., x and !x given as
        //     assumptions
        //  3. Standard case

        // Case one: the assumption is falsified at the root level
        if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_root_assignment(violated_assumption)
        {
            self.restore_state_at_root(brancher);
            Ok(vec![violated_assumption])
        }
        // Case two: the assumption is inconsistent with other assumptions
        //  i.e., the assumptions contain both literal 'x' and '~x'
        //  not sure what would be the best output in this case, possibly a special flag?
        //      for now we return the reason (x && ~x)
        else if !self
            .sat_data_structures
            .assignments_propositional
            .is_literal_propagated(violated_assumption)
        {
            self.restore_state_at_root(brancher);
            Err(violated_assumption)
        }
        // Case three: the standard case, proceed with core extraction
        // performs resolution on all implied assumptions until only decision assumptions are left
        //  the violating assumption is used as the starting point
        //  at this point, any reason clause encountered will contains only assumptions, but some
        // assumptions might be implied  this corresponds to the all-decision CDCL learning
        // scheme
        else {
            self.compute_all_decision_learning_helper(Some(!violated_assumption), true, brancher);
            self.analysis_result
                .learned_literals
                .push(!violated_assumption);
            pumpkin_assert_moderate!(self.debug_check_clausal_core(violated_assumption));
            self.restore_state_at_root(brancher);
            Ok(self.analysis_result.learned_literals.clone())
        }
    }

    pub fn solve(
        &mut self,
        time_limit_in_seconds: i64,
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        let dummy_assumptions: Vec<Literal> = vec![];
        self.solve_under_assumptions(&dummy_assumptions, time_limit_in_seconds, brancher)
    }

    pub fn get_state(&self) -> &CSPSolverState {
        &self.state
    }

    pub fn log_statistics(&self) {
        self.counters.log_statistics()
    }

    pub fn create_new_propositional_variable(&mut self) -> PropositionalVariable {
        self.sat_cp_mediator.create_new_propositional_variable(
            &mut self.cp_data_structures.watch_list_propositional,
            &mut self.clausal_propagator,
            &mut self.sat_data_structures,
        )
    }

    pub fn create_new_integer_variable(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        self.sat_cp_mediator.create_new_domain(
            lower_bound,
            upper_bound,
            &mut self.clausal_propagator,
            &mut self.sat_data_structures,
            &mut self.cp_data_structures,
        )
    }

    /// Creates an integer variable with a domain containing only the values in `values`
    pub fn create_new_integer_variable_sparse(&mut self, mut values: Vec<i32>) -> DomainId {
        assert!(
            !values.is_empty(),
            "cannot create a variable with an empty domain"
        );

        values.sort();
        values.dedup();

        let lower_bound = values[0];
        let upper_bound = values[values.len() - 1];

        let domain_id = self.create_new_integer_variable(lower_bound, upper_bound);

        let mut next_idx = 0;
        for value in lower_bound..=upper_bound {
            if value == values[next_idx] {
                next_idx += 1;
            } else {
                self.cp_data_structures
                    .assignments_integer
                    .remove_value_from_domain(domain_id, value, None)
                    .expect("the domain should not be empty");
                self.sat_data_structures
                    .assignments_propositional
                    .enqueue_decision_literal(self.sat_cp_mediator.get_inequality_literal(
                        domain_id,
                        value,
                        &self.cp_data_structures.assignments_integer,
                    ))
            }
        }
        pumpkin_assert_simple!(
            next_idx == values.len(),
            "Expected all values to have been processed"
        );

        domain_id
    }

    pub fn new_literals(&mut self) -> impl Iterator<Item = Literal> + '_ {
        std::iter::from_fn(|| Some(self.create_new_propositional_variable()))
            .map(|var| Literal::new(var, true))
    }

    pub fn get_propositional_assignments(&self) -> &AssignmentsPropositional {
        &self.sat_data_structures.assignments_propositional
    }

    pub fn get_lower_bound_literal(&self, domain: DomainId, lower_bound: i32) -> Literal {
        self.sat_cp_mediator.get_lower_bound_literal(
            domain,
            lower_bound,
            &self.cp_data_structures.assignments_integer,
        )
    }

    pub fn get_upper_bound_literal(&self, domain: DomainId, upper_bound: i32) -> Literal {
        self.sat_cp_mediator.get_upper_bound_literal(
            domain,
            upper_bound,
            &self.cp_data_structures.assignments_integer,
        )
    }

    pub fn get_integer_assignments(&self) -> &AssignmentsInteger {
        &self.cp_data_structures.assignments_integer
    }

    pub fn restore_state_at_root(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(
            self.state.has_solution() && self.get_decision_level() > 0
                || self.state.is_infeasible_under_assumptions()
        );

        self.backtrack(0, brancher);
        self.state.declare_ready();
    }

    pub fn get_literal(&self, predicate: Predicate) -> Literal {
        self.sat_cp_mediator
            .get_predicate_literal(predicate, &self.cp_data_structures.assignments_integer)
    }
}

// methods that serve as the main building blocks
impl ConstraintSatisfactionSolver {
    fn initialise(&mut self, assumptions: &[Literal], time_limit_in_seconds: i64) {
        pumpkin_assert_simple!(!self.state.is_infeasible_under_assumptions(), "Solver is not expected to be in the infeasible under assumptions state when initialising. Missed extracting the core?");

        let num_propositional_variables = self
            .sat_data_structures
            .assignments_propositional
            .num_propositional_variables() as usize;

        self.state.declare_solving();
        self.stopwatch.reset(time_limit_in_seconds);
        self.sat_data_structures.assumptions = assumptions.to_owned();
        self.seen.resize(num_propositional_variables, false);
    }

    fn solve_internal(&mut self, brancher: &mut impl Brancher) -> CSPSolverExecutionFlag {
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
                    self.restart_during_search(brancher);
                }

                self.declare_new_decision_level();

                let branching_result = self.enqueue_next_decision(brancher);
                if let Err(flag) = branching_result {
                    return flag;
                }
            }
            // conflict
            else {
                if self
                    .sat_data_structures
                    .assignments_propositional
                    .is_at_the_root_level()
                {
                    self.state.declare_infeasible();
                    return CSPSolverExecutionFlag::Infeasible;
                }

                self.resolve_conflict(brancher);

                self.learned_clause_manager.decay_clause_activities();

                brancher.on_conflict()
            }
        }
    }

    fn enqueue_next_decision(
        &mut self,
        brancher: &mut impl Brancher,
    ) -> Result<(), CSPSolverExecutionFlag> {
        if let Some(assumption_literal) = self.sat_data_structures.peek_next_assumption_literal() {
            let success = self.enqueue_assumption_literal(assumption_literal);
            if !success {
                return Err(CSPSolverExecutionFlag::Infeasible);
            }
            Ok(())
        } else {
            let decided_literal = brancher.next_decision(&SelectionContext::new(
                &self.cp_data_structures.assignments_integer,
                &self.sat_data_structures.assignments_propositional,
                &self.sat_cp_mediator,
            ));
            if let Some(literal) = decided_literal {
                self.counters.num_decisions += 1;
                self.sat_data_structures
                    .assignments_propositional
                    .enqueue_decision_literal(literal);
                Ok(())
            } else {
                self.state.declare_solution_found();
                Err(CSPSolverExecutionFlag::Feasible)
            }
        }
    }

    // returns true if the assumption was successfully enqueued, and false otherwise
    fn enqueue_assumption_literal(&mut self, assumption_literal: Literal) -> bool {
        // Case 1: the assumption is unassigned, assign it
        if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_unassigned(assumption_literal)
        {
            self.sat_data_structures
                .assignments_propositional
                .enqueue_decision_literal(assumption_literal);
            true
        // Case 2: the assumption has already been set to true
        //  this happens when other assumptions propagated the literal
        //  or the assumption is already set to true at the root level
        } else if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_assigned_true(assumption_literal)
        {
            // in this case, do nothing
            //  note that the solver will then increase the decision level without enqueuing a
            // decision literal  this is necessary because by convention the solver will
            // try to assign the i-th assumption literal at decision level i+1
            true
        }
        // Case 3: the assumption literal is in conflict with the input assumption
        //  which means the instance is infeasible under the current assumptions
        else {
            self.state
                .declare_infeasible_under_assumptions(assumption_literal);
            false
        }
    }

    fn declare_new_decision_level(&mut self) {
        self.sat_data_structures
            .assignments_propositional
            .increase_decision_level();
        self.cp_data_structures.increase_decision_level();
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

    // changes the state based on the conflict analysis result given as input
    // i.e., adds the learned clause to the database, backtracks, enqueues the propagated literal,
    // and updates internal data structures for simple moving averages note that no propagation
    // is done, this is left to the solver
    fn resolve_conflict(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_moderate!(self.state.conflicting());

        // compute the learned clause
        //  the result is stored in self.analysis_result
        self.compute_1uip(brancher);

        self.process_learned_clause(brancher);

        self.state.declare_solving();
    }

    fn process_learned_clause(&mut self, brancher: &mut impl Brancher) {
        if let Err(write_error) = self.write_to_certificate() {
            warn!(
                "Failed to update the certificate file, error message: {}",
                write_error
            );
        }

        // unit clauses are treated in a special way: they are added as decision literals at
        // decision level 0
        if self.analysis_result.learned_literals.len() == 1 {
            // important to notify about the conflict _before_ backtracking removes literals from
            // the trail
            self.restart_strategy.notify_conflict(
                1,
                self.sat_data_structures
                    .assignments_propositional
                    .num_trail_entries(),
            );

            self.backtrack(0, brancher);

            let unit_clause = self.analysis_result.learned_literals[0];

            self.sat_data_structures
                .assignments_propositional
                .enqueue_decision_literal(unit_clause);

            self.counters.num_unit_clauses_learned +=
                (self.analysis_result.learned_literals.len() == 1) as u64;
        } else {
            self.counters
                .average_learned_clause_length
                .add_term(self.analysis_result.learned_literals.len() as u64);

            // important to get trail length before the backtrack
            let num_variables_assigned_before_conflict = &self
                .sat_data_structures
                .assignments_propositional
                .num_trail_entries();

            self.counters
                .average_backtrack_amount
                .add_term((self.get_decision_level() - self.analysis_result.backjump_level) as u64);
            self.backtrack(self.analysis_result.backjump_level, brancher);

            self.learned_clause_manager.add_learned_clause(
                self.analysis_result.learned_literals.clone(), // todo not ideal with clone
                &mut self.clausal_propagator,
                &mut self.sat_data_structures.assignments_propositional,
                &mut self.sat_data_structures.clause_allocator,
            );

            let lbd = self.learned_clause_manager.compute_lbd_for_literals(
                &self.analysis_result.learned_literals,
                &self.sat_data_structures.assignments_propositional,
            );

            self.restart_strategy
                .notify_conflict(lbd, *num_variables_assigned_before_conflict);
        }
    }

    // a 'restart' differs from backtracking to level zero
    //   in that a restart backtracks to decision level zero and then performs additional
    // operations,      e.g., clean up learned clauses, adjust restart frequency, etc.
    fn restart_during_search(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(
            self.sat_data_structures.are_all_assumptions_assigned(),
            "Sanity check: restarts should not trigger whilst assigning assumptions"
        );

        // no point backtracking past the assumption level
        if self.get_decision_level() <= self.sat_data_structures.assumptions.len() {
            return;
        }

        self.backtrack(0, brancher);

        self.restart_strategy.notify_restart();
    }

    fn backtrack(&mut self, backtrack_level: usize, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(backtrack_level < self.get_decision_level());

        let unassigned_literals = self.sat_data_structures.backtrack(backtrack_level);

        unassigned_literals.for_each(|literal| {
            brancher.on_unassign_literal(literal);
            // TODO: We should also backtrack on the integer variables here
        });

        self.clausal_propagator.synchronise(
            self.sat_data_structures
                .assignments_propositional
                .num_trail_entries(),
        );

        self.cp_data_structures.backtrack(
            backtrack_level,
            &self.sat_data_structures.assignments_propositional,
        );
        //  note that sat_cp_mediator sync should be called after the sat/cp data structures
        // backtrack
        self.sat_cp_mediator.synchronise(
            &self.sat_data_structures.assignments_propositional,
            &self.cp_data_structures.assignments_integer,
        );
        // for now all propagators are called to synchronise
        //  in the future this will be improved in two ways:
        //      allow incremental synchronisation
        //      only call the subset of propagators that were notified since last backtrack
        for propagator_id in 0..self.cp_propagators.len() {
            let context = self
                .cp_data_structures
                .create_propagation_context(&self.sat_data_structures.assignments_propositional);

            self.cp_propagators[propagator_id].synchronise(&context);
        }
    }

    fn propagate_enqueued(&mut self) {
        let num_assigned_variables_old = self
            .cp_data_structures
            .assignments_integer
            .num_trail_entries();

        loop {
            let conflict_info = self
                .sat_cp_mediator
                .synchronise_propositional_trail_based_on_integer_trail(
                    &mut self.sat_data_structures.assignments_propositional,
                    &self.cp_data_structures.assignments_integer,
                    &mut self.clausal_propagator,
                    &mut self.sat_data_structures.clause_allocator,
                );

            if let Some(conflict_info) = conflict_info {
                // The previous propagation triggered an empty domain.
                self.state.declare_conflict(conflict_info);
                break;
            }

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
                    &mut self.sat_data_structures.assignments_propositional,
                    &mut self.cp_data_structures,
                    &mut self.cp_propagators,
                )
                .expect("should not be an error");

            // propagate boolean propagators - todo add these special-case propagators

            // propagate (conventional) CP propagators
            let propagation_status_one_step_cp = self.propagate_cp_one_step();

            match propagation_status_one_step_cp {
                PropagationStatusOneStepCP::PropagationHappened => {
                    // do nothing, the result will be that the clausal propagator will go next
                    //  recall that the idea is to always propagate simpler propagators before more
                    // complex ones  after a cp propagation was done one step,
                    // it is time to go to the clausal propagator
                }
                PropagationStatusOneStepCP::FixedPoint => {
                    break;
                }
                PropagationStatusOneStepCP::ConflictDetected { conflict_info } => {
                    let _ = self
                        .sat_cp_mediator
                        .synchronise_propositional_trail_based_on_integer_trail(
                            &mut self.sat_data_structures.assignments_propositional,
                            &self.cp_data_structures.assignments_integer,
                            &mut self.clausal_propagator,
                            &mut self.sat_data_structures.clause_allocator,
                        );
                    self.state.declare_conflict(conflict_info);
                    break;
                }
            } // end match
        }

        self.counters.num_conflicts += self.state.conflicting() as u64;

        self.counters.num_propagations += self
            .cp_data_structures
            .assignments_integer
            .num_trail_entries() as u64
            - num_assigned_variables_old as u64;

        // Only check fixed point propagation if there was no reported conflict.
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
        if self.cp_data_structures.propagator_queue.is_empty() {
            return PropagationStatusOneStepCP::FixedPoint;
        }

        let propagator_id = self.cp_data_structures.propagator_queue.pop();
        let propagator = &mut self.cp_propagators[propagator_id.0 as usize];
        let mut context = self.cp_data_structures.create_propagation_context_mut(
            &mut self.sat_data_structures.assignments_propositional,
        );

        match propagator.propagate(&mut context) {
            // An empty domain conflict will be caught by the clausal propagator.
            Err(Inconsistency::EmptyDomain) => PropagationStatusOneStepCP::PropagationHappened,

            // A propagator-specific reason for the current conflict.
            Err(Inconsistency::Other(conflict_info)) => {
                if let ConflictInfo::Explanation(ref propositional_conjunction) = conflict_info {
                    pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                        &self.cp_data_structures.assignments_integer,
                        &self.sat_data_structures.assignments_propositional,
                        &self.sat_cp_mediator,
                        propositional_conjunction,
                        propagator.as_ref(),
                        propagator_id,
                    ));
                }

                PropagationStatusOneStepCP::ConflictDetected { conflict_info }
            }

            Ok(()) => {
                let _ = self.cp_data_structures.process_domain_events(
                    &mut self.cp_propagators,
                    &mut self.sat_data_structures.assignments_propositional,
                );

                PropagationStatusOneStepCP::PropagationHappened
            }
        }
    }
}

// methods for adding constraints (propagators and clauses)
impl ConstraintSatisfactionSolver {
    /// Post a new propagator to the solver. If unsatisfiability can be immediately determined
    /// through propagation, this will return `false`. If not, this returns `true`.
    ///
    /// The caller should ensure the solver is in the root state before calling this, either
    /// because no call to [`Self::solve()`] has been made, or because
    /// [`Self::restore_state_at_root()`] was called.
    ///
    /// If the solver is already in a conflicting state, i.e. a previous call to this method
    /// already returned `false`, calling this again will not alter the solver in any way, and
    /// `false` will be returned again.
    pub fn add_propagator<Constructor>(&mut self, constructor: Constructor) -> bool
    where
        Constructor: CPPropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        if self.state.conflicting()
            || self.state.is_infeasible()
            || self.state.is_infeasible_under_assumptions()
        {
            return false;
        }

        let new_propagator_id = PropagatorId(self.cp_propagators.len() as u32);
        let constructor_context = PropagatorConstructorContext::new(
            &mut self.cp_data_structures.watch_list_cp,
            &mut self.cp_data_structures.watch_list_propositional,
            new_propagator_id,
        );

        let propagator_to_add = constructor.create_boxed(constructor_context);

        pumpkin_assert_simple!(propagator_to_add.priority() <= 3, "The propagator priority exceeds 3. Currently we only support values up to 3, but this can easily be changed if there is a good reason.");

        self.cp_propagators.push(propagator_to_add);

        let new_propagator = &mut self.cp_propagators[new_propagator_id];
        let mut context = self.cp_data_structures.create_propagation_context_mut(
            &mut self.sat_data_structures.assignments_propositional,
        );

        if new_propagator.initialise_at_root(&mut context).is_err() {
            self.state.declare_infeasible();
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
        pumpkin_assert_moderate!(!self.state.is_infeasible_under_assumptions());
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

        // if the literal representing the unit clause is unassigned, assign it
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
        // the unit clause is already present, no need to do anything
        else if self
            .sat_data_structures
            .assignments_propositional
            .is_literal_assigned_true(unit_clause)
        {
            Ok(())
        }
        // the unit clause is falsified at the root level
        else {
            Err(ConstraintOperationError::InfeasibleClause)
        }
    }
}

// methods for getting simple info out of the solver
impl ConstraintSatisfactionSolver {
    pub fn is_propagation_complete(&self) -> bool {
        self.clausal_propagator.is_propagation_complete(
            self.sat_data_structures
                .assignments_propositional
                .num_trail_entries(),
        ) && self.cp_data_structures.propagator_queue.is_empty()
    }

    fn get_decision_level(&self) -> usize {
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

// methods for conflict analysis
impl ConstraintSatisfactionSolver {
    // computes the 1uip and stores it in 'analysis_result'
    fn compute_1uip(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(self.debug_conflict_analysis_proconditions());

        self.analysis_result.learned_literals.resize(
            1,
            self.sat_data_structures
                .assignments_propositional
                .true_literal, /* dummy literal, the point is that we allocate space for the
                                * asserting literal, which will by convention be placed at index
                                * 0 */
        );
        self.analysis_result.backjump_level = 0;

        let mut num_current_decision_level_literals_to_inspect = 0;
        let mut next_trail_index = self
            .sat_data_structures
            .assignments_propositional
            .num_trail_entries()
            - 1;
        let mut next_literal: Option<Literal> = None;

        loop {
            pumpkin_assert_moderate!(Self::debug_1uip_conflict_analysis_check_next_literal(
                next_literal,
                &self.sat_data_structures.assignments_propositional
            ));
            // note that the 'next_literal' is only None in the first iteration
            let clause_reference = if let Some(propagated_literal) = next_literal {
                self.sat_cp_mediator.get_propagation_clause_reference(
                    propagated_literal,
                    &self.clausal_propagator,
                    &mut self.sat_data_structures,
                    &mut self.cp_data_structures,
                )
            } else {
                let conflict = self.sat_cp_mediator.get_conflict_reason_clause_reference(
                    self.state.get_conflict_info(),
                    &mut self.sat_data_structures,
                    &mut self.cp_data_structures,
                );
                self.counters
                    .average_conflict_size
                    .add_term(self.sat_data_structures.clause_allocator[conflict].len() as u64);
                conflict
            };
            self.learned_clause_manager
                .update_clause_lbd_and_bump_activity(
                    clause_reference,
                    &self.sat_data_structures.assignments_propositional,
                    &mut self.sat_data_structures.clause_allocator,
                );

            // process the reason literal
            // 	i.e., perform resolution and update other related internal data structures
            let start_index = next_literal.is_some() as usize; // note that the start index will be either 0 or 1 - the idea is to skip the 0th literal
                                                               // in case the clause represents a propagation
            for &reason_literal in &self.sat_data_structures.clause_allocator[clause_reference]
                .get_literal_slice()[start_index..]
            {
                // only consider non-root assignments that have not been considered before
                let is_root_assignment = self
                    .sat_data_structures
                    .assignments_propositional
                    .is_literal_root_assignment(reason_literal);
                let seen = self.seen[reason_literal.get_propositional_variable()];

                if !is_root_assignment && !seen {
                    // mark the variable as seen so that we do not process it more than once
                    self.seen[reason_literal.get_propositional_variable()] = true;

                    // TODO: add case for integer variables
                    brancher.on_appearance_in_conflict_literal(reason_literal);

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
            while !self.seen[self
                .sat_data_structures
                .assignments_propositional
                .get_trail_entry(next_trail_index)
                .get_propositional_variable()]
            {
                next_trail_index -= 1;
                pumpkin_assert_advanced!(self.sat_data_structures.assignments_propositional.get_literal_assignment_level(self.sat_data_structures.assignments_propositional.get_trail_entry(next_trail_index)) == self.sat_data_structures.assignments_propositional.get_decision_level(),
                    "The current decision level trail has been overrun, mostly likely caused by an incorrectly implemented cp propagator?");
            }

            // make appropriate adjustments to prepare for the next iteration
            next_literal = Some(
                self.sat_data_structures
                    .assignments_propositional
                    .get_trail_entry(next_trail_index),
            );
            self.seen[next_literal.unwrap().get_propositional_variable()] = false; // the same literal cannot be encountered more than once on the trail, so we can clear
                                                                                   // the flag here
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

        if self.internal_parameters.learning_clause_minimisation {
            pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result(false));

            self.learned_clause_minimiser.remove_dominated_literals(
                &mut self.analysis_result,
                &self.clausal_propagator,
                &mut self.sat_data_structures,
                &mut self.cp_data_structures,
                &mut self.sat_cp_mediator,
            );
        }

        self.sat_cp_mediator
            .explanation_clause_manager
            .clean_up_explanation_clauses(&mut self.sat_data_structures.clause_allocator);

        pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result(false));
        // the return value is stored in the input 'analysis_result'
    }

    // computes the learned clause containing only decision literals and stores it in
    // 'analysis_result'
    #[allow(dead_code)]
    fn compute_all_decision_learning(
        &mut self,
        is_extracting_core: bool,
        brancher: &mut impl Brancher,
    ) {
        self.compute_all_decision_learning_helper(None, is_extracting_core, brancher);
    }

    // the helper is used to facilitate usage when extracting the clausal core
    //  normal conflict analysis would use 'compute_all_decision_learning'
    fn compute_all_decision_learning_helper(
        &mut self,
        mut next_literal: Option<Literal>,
        is_extracting_core: bool,
        brancher: &mut impl Brancher,
    ) {
        // the code is similar to 1uip learning with small differences to accomodate the
        // all-decision learning scheme
        pumpkin_assert_simple!(
            next_literal.is_some() || self.debug_conflict_analysis_proconditions()
        ); // when using this function when extracting the core, no conflict acutally takes place, but
           // the preconditions expect a conflict clause, so we skip this check

        self.analysis_result.learned_literals.clear();
        self.analysis_result.backjump_level = 0;

        let mut num_propagated_literals_left_to_inspect = 0;
        let mut next_trail_index = self
            .sat_data_structures
            .assignments_propositional
            .num_trail_entries()
            - 1;

        loop {
            // note that the 'next_literal' is only None in the first iteration
            let clause_reference = if let Some(propagated_literal) = next_literal {
                self.sat_cp_mediator.get_propagation_clause_reference(
                    propagated_literal,
                    &self.clausal_propagator,
                    &mut self.sat_data_structures,
                    &mut self.cp_data_structures,
                )
            } else {
                self.sat_cp_mediator.get_conflict_reason_clause_reference(
                    self.state.get_conflict_info(),
                    &mut self.sat_data_structures,
                    &mut self.cp_data_structures,
                )
            };
            self.learned_clause_manager
                .update_clause_lbd_and_bump_activity(
                    clause_reference,
                    &self.sat_data_structures.assignments_propositional,
                    &mut self.sat_data_structures.clause_allocator,
                );

            // process the reason literal
            // 	i.e., perform resolution and update other related internal data structures
            let start_index = next_literal.is_some() as usize; // note that the start index will be either 0 or 1 - the idea is to skip the 0th literal
                                                               // in case the clause represents a propagation
            for &reason_literal in &self.sat_data_structures.clause_allocator[clause_reference]
                .get_literal_slice()[start_index..]
            {
                // only consider non-root assignments that have not been considered before
                let is_root_assignment = self
                    .sat_data_structures
                    .assignments_propositional
                    .is_literal_root_assignment(reason_literal);
                let seen = self.seen[reason_literal.get_propositional_variable()];

                if !is_root_assignment && !seen {
                    // mark the variable as seen so that we do not process it more than once
                    self.seen[reason_literal.get_propositional_variable()] = true;

                    // TODO: add case for integer variables
                    brancher.on_appearance_in_conflict_literal(reason_literal);

                    num_propagated_literals_left_to_inspect +=
                        self.sat_data_structures
                            .assignments_propositional
                            .is_literal_propagated(reason_literal) as i32;

                    // only decision literals are kept for the learned clause
                    if self
                        .sat_data_structures
                        .assignments_propositional
                        .is_literal_decision(reason_literal)
                    {
                        self.analysis_result.learned_literals.push(reason_literal);
                    }
                }
            }

            if num_propagated_literals_left_to_inspect == 0 {
                break;
            }

            // after resolution took place, find the next literal on the trail that is relevant for
            // this conflict only literals that have been seen so far are relevant
            //  note that there may be many literals that are not relevant
            while !self.seen[self
                .sat_data_structures
                .assignments_propositional
                .get_trail_entry(next_trail_index)
                .get_propositional_variable()]
                || self
                    .sat_data_structures
                    .assignments_propositional
                    .is_literal_decision(
                        self.sat_data_structures
                            .assignments_propositional
                            .get_trail_entry(next_trail_index),
                    )
            {
                next_trail_index -= 1;
            }

            // make appropriate adjustments to prepare for the next iteration
            next_literal = Some(
                self.sat_data_structures
                    .assignments_propositional
                    .get_trail_entry(next_trail_index),
            );
            self.seen[next_literal.unwrap().get_propositional_variable()] = false; // the same literal cannot be encountered more than once on the trail, so we can clear
                                                                                   // the flag here
            next_trail_index -= 1;
            num_propagated_literals_left_to_inspect -= 1;

            pumpkin_assert_simple!(
                self.sat_data_structures
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
            let assignments = &self.sat_data_structures.assignments_propositional;
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
            self.analysis_result.backjump_level = self
                .sat_data_structures
                .assignments_propositional
                .get_literal_assignment_level(self.analysis_result.learned_literals[1]);
        }

        self.sat_cp_mediator
            .explanation_clause_manager
            .clean_up_explanation_clauses(&mut self.sat_data_structures.clause_allocator);

        pumpkin_assert_moderate!(self.debug_check_conflict_analysis_result(is_extracting_core));
        // the return value is stored in the input 'analysis_result'
    }

    fn debug_check_conflict_analysis_result(&self, is_extracting_core: bool) -> bool {
        // debugging method: performs sanity checks on the learned clause

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

        if !is_extracting_core {
            // When this method is called during core extraction, the decision level is not
            // necessarily the decision level of learned_literals[0].
            assert_eq!(
                self.get_decision_level(),
                assignments.get_literal_assignment_level(self.analysis_result.learned_literals[0]),
                "The asserting literal must be at the highest level."
            );
        }

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

    fn debug_1uip_conflict_analysis_check_next_literal(
        next_literal: Option<Literal>,
        assignments_propositional: &AssignmentsPropositional,
    ) -> bool {
        // in conflict analysis, literals are examined in reverse order on the trail
        // the examined literals are expected to be:
        //  1. from the same decision level - the current (last) decision level
        //  2. propagated, unless the literal is the decision literal of the current decision level
        //  3. not root assignments
        // failing any of the conditions above means something went wrong with the conflict
        // analysis, e.g., some explanation was faulty and caused the solver to overrun the trail

        // note that in the first iteration, the next_literal will be set to None, so we can skip
        // this check
        match next_literal {
            None => true,
            Some(next_literal) => {
                if assignments_propositional.is_literal_root_assignment(next_literal) {
                    return false;
                }

                let is_propagated = assignments_propositional.is_literal_propagated(next_literal);

                let current_decision_level = assignments_propositional.get_decision_level();

                let is_decision_literal_of_current_level =
                    assignments_propositional.is_literal_decision(next_literal);

                let is_assigned_at_current_decision_level = assignments_propositional
                    .get_literal_assignment_level(next_literal)
                    == current_decision_level;

                (is_propagated || is_decision_literal_of_current_level)
                    && is_assigned_at_current_decision_level
            }
        }
    }

    fn debug_check_core_extraction(&self) -> bool {
        if self.state.is_infeasible() {
            true
        } else if self.state.is_infeasible_under_assumptions() {
            pumpkin_assert_simple!(
                self.sat_data_structures
                    .assignments_propositional
                    .is_literal_assigned_false(self.state.get_violated_assumption()),
                "Violated assumption is expected to be assigned false."
            );

            pumpkin_assert_moderate!(self
                .sat_data_structures
                .assumptions
                .contains(&self.state.get_violated_assumption()));
            true
        } else {
            panic!("Cannot extract core unless the solver is either infeasible or infeasible under assumptions.");
        }
    }

    fn debug_conflict_analysis_proconditions(&mut self) -> bool {
        pumpkin_assert_simple!(self.state.conflicting());

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

        true
    }

    fn debug_check_clausal_core(&self, violated_assumption: Literal) -> bool {
        pumpkin_assert_moderate!(
            self.analysis_result
                .learned_literals
                .iter()
                .all(|&core_literal| self
                    .sat_data_structures
                    .assumptions
                    .contains(&!core_literal)),
            "Each core literal must be part of the assumptions."
        );
        pumpkin_assert_moderate!(
            self.analysis_result
                .learned_literals
                .iter()
                .all(|&core_literal| core_literal == !violated_assumption
                    || self
                        .sat_data_structures
                        .assignments_propositional
                        .is_literal_decision(!core_literal)),
            "Each core literal (except the violated literal) must be a decision."
        );
        true
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct Counters {
    pub num_decisions: u64,
    pub num_conflicts: u64,
    average_conflict_size: CumulativeMovingAverage,
    num_propagations: u64,
    num_unit_clauses_learned: u64,
    average_learned_clause_length: CumulativeMovingAverage,
    time_spent_in_solver: u64,
    average_backtrack_amount: CumulativeMovingAverage,
}

const STATISTIC_PREFIX: &str = "x";

impl Counters {
    fn log_statistics(&self) {
        info!(
            "{STATISTIC_PREFIX} Number of Decisions: {}",
            self.num_decisions
        );
        info!(
            "{STATISTIC_PREFIX} Number of Conflicts: {}",
            self.num_conflicts
        );
        info!(
            "{STATISTIC_PREFIX} Average Size of Conflict Explanation: {}",
            self.average_conflict_size.value()
        );
        info!(
            "{STATISTIC_PREFIX} Number of Propagations: {}",
            self.num_propagations
        );
        info!(
            "{STATISTIC_PREFIX} Number of Learned Unit Clauses: {}",
            self.num_unit_clauses_learned
        );
        info!(
            "{STATISTIC_PREFIX} Average Learned Clause Length: {}",
            self.average_learned_clause_length.value()
        );
        info!(
            "{STATISTIC_PREFIX} Time Spent in Solver in ms: {}",
            self.time_spent_in_solver
        );
        info!(
            "{STATISTIC_PREFIX} Average Backtrack Amount: {}",
            self.average_backtrack_amount.value()
        )
    }
}

#[derive(Clone, Default, Debug)]
pub struct ConflictAnalysisResult {
    pub learned_literals: Vec<Literal>,
    pub backjump_level: usize,
}

#[derive(Default, Debug)]
enum CSPSolverStateInternal {
    #[default]
    Ready,
    Solving,
    ContainsSolution,
    // ConflictClausal {
    // conflict_clause: ConflictInfo,
    // },
    // ConflictCP {
    // conflict_reason: PropositionalConjunction,
    // },
    Conflict {
        conflict_info: ConflictInfo,
    },
    Infeasible,
    InfeasibleUnderAssumptions {
        violated_assumption: Literal,
    },
    Timeout,
}

#[derive(Default, Debug)]
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
        // self.is_clausal_conflict() || self.is_cp_conflict()
    }

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
        pumpkin_assert_simple!(
            self.has_solution() && !self.is_infeasible() || self.is_infeasible_under_assumptions()
        );
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

// the defaults below are used only for the tests
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

#[cfg(test)]
mod tests {
    use super::ConstraintSatisfactionSolver;
    use crate::basic_types::CSPSolverExecutionFlag;
    use crate::basic_types::Literal;
    use crate::branching::IndependentVariableValueBrancher;

    fn is_same_core(core1: &[Literal], core2: &[Literal]) -> bool {
        core1.len() == core2.len() && core2.iter().all(|lit| core1.contains(lit))
    }

    fn is_result_the_same(
        res1: &Result<Vec<Literal>, Literal>,
        res2: &Result<Vec<Literal>, Literal>,
    ) -> bool {
        // if the two results disagree on the outcome, can already return false
        if res1.is_err() && res2.is_ok() || res1.is_ok() && res2.is_err() {
            println!("diff");
            println!("{:?}", res1.clone().unwrap());
            false
        }
        // if both results are errors, check if the two errors are the same
        else if res1.is_err() {
            println!("err");
            res1.clone().unwrap_err().get_propositional_variable()
                == res2.clone().unwrap_err().get_propositional_variable()
        }
        // otherwise the two results are both ok
        else {
            println!("ok");
            is_same_core(&res1.clone().unwrap(), &res2.clone().unwrap())
        }
    }

    fn run_test(
        mut solver: ConstraintSatisfactionSolver,
        assumptions: Vec<Literal>,
        expected_flag: CSPSolverExecutionFlag,
        expected_result: Result<Vec<Literal>, Literal>,
    ) {
        let mut search =
            IndependentVariableValueBrancher::default_over_all_propositional_variables(&solver);
        let flag = solver.solve_under_assumptions(&assumptions, i64::MAX, &mut search);
        assert!(flag == expected_flag, "The flags do not match.");

        if matches!(flag, CSPSolverExecutionFlag::Infeasible) {
            assert!(
                is_result_the_same(&solver.extract_clausal_core(&mut search), &expected_result),
                "The result is not the same"
            );
        }
    }

    fn create_instance1() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = Literal::new(solver.create_new_propositional_variable(), true);
        let lit2 = Literal::new(solver.create_new_propositional_variable(), true);

        let _ = solver.add_permanent_clause(vec![lit1, lit2]);
        let _ = solver.add_permanent_clause(vec![lit1, !lit2]);
        let _ = solver.add_permanent_clause(vec![!lit1, lit2]);
        (solver, vec![lit1, lit2])
    }

    #[test]
    fn simple_core_extraction_1_1() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[0], !lits[1]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![!lits[0]]),
        )
    }

    #[test]
    fn simple_core_extraction_1_2() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[1], !lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![!lits[1]]),
        );
    }

    #[test]
    fn simple_core_extraction_1_infeasible() {
        let (mut solver, lits) = create_instance1();
        let _ = solver.add_permanent_clause(vec![!lits[0], !lits[1]]);
        run_test(
            solver,
            vec![!lits[1], !lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![]),
        );
    }

    #[test]
    fn simple_core_extraction_1_core_before_inconsistency() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[1], lits[1]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![!lits[1]]), // the core gets computed before inconsistency is detected
        );
    }

    fn create_instance2() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = Literal::new(solver.create_new_propositional_variable(), true);
        let lit2 = Literal::new(solver.create_new_propositional_variable(), true);
        let lit3 = Literal::new(solver.create_new_propositional_variable(), true);

        let _ = solver.add_permanent_clause(vec![lit1, lit2, lit3]);
        let _ = solver.add_permanent_clause(vec![lit1, !lit2, lit3]);
        (solver, vec![lit1, lit2, lit3])
    }

    #[test]
    fn simple_core_extraction_2_1() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], lits[1], !lits[2]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![lits[0], !lits[1], lits[2]]),
        );
    }

    #[test]
    fn simple_core_extraction_2_long_assumptions_with_inconsistency_at_the_end() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], lits[1], !lits[2], lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![lits[0], !lits[1], lits[2]]), /* could return inconsistent assumptions,
                                                   * however inconsistency will not be detected
                                                   * given the order of the assumptions */
        );
    }

    #[test]
    fn simple_core_extraction_2_inconsistent_long_assumptions() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], !lits[0], !lits[1], !lits[1], lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            Err(lits[0]),
        );
    }

    fn create_instance3() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = Literal::new(solver.create_new_propositional_variable(), true);
        let lit2 = Literal::new(solver.create_new_propositional_variable(), true);
        let lit3 = Literal::new(solver.create_new_propositional_variable(), true);
        let _ = solver.add_permanent_clause(vec![lit1, lit2, lit3]);
        (solver, vec![lit1, lit2, lit3])
    }

    #[test]
    fn simple_core_extraction_3_1() {
        let (solver, lits) = create_instance3();
        run_test(
            solver,
            vec![!lits[0], !lits[1], !lits[2]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(vec![lits[0], lits[1], lits[2]]),
        );
    }

    #[test]
    fn simple_core_extraction_3_2() {
        let (solver, lits) = create_instance3();
        run_test(
            solver,
            vec![!lits[0], !lits[1]],
            CSPSolverExecutionFlag::Feasible,
            Ok(vec![]), // will be ignored in the test
        );
    }
}
