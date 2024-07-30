//! Houses the solver which attempts to find a solution to a Constraint Satisfaction Problem (CSP)
//! using a Lazy Clause Generation approach.
use std::fmt::Debug;
use std::fmt::Formatter;
use std::time::Instant;

use log::warn;
use rand::rngs::SmallRng;
use rand::SeedableRng;

use super::conflict_analysis::ConflictAnalysisNogoodContext;
use super::conflict_analysis::LearnedNogood;
use super::conflict_analysis::ResolutionNogoodConflictAnalyser;
use super::conflict_analysis::SemanticMinimiser;
use super::nogoods::Lbd;
use super::termination::TerminationCondition;
use super::variables::IntegerVariable;
use super::variables::Literal;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::statistic_logging::statistic_logger::log_statistic;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Random;
use crate::basic_types::SolutionReference;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::cp::PropagatorQueue;
use crate::engine::cp::WatchListCP;
use crate::engine::debug_helper::DebugDyn;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::PropagatorId;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::engine::DebugHelper;
use crate::engine::IntDomainEvent;
use crate::engine::RestartOptions;
use crate::engine::RestartStrategy;
use crate::predicate;
use crate::proof::ProofLog;
use crate::propagators::nogood::NogoodPropagator;
use crate::propagators::nogood::NogoodPropagatorConstructor;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variable_names::VariableNames;
#[cfg(doc)]
use crate::Solver;

/// A solver which attempts to find a solution to a Constraint Satisfaction Problem (CSP) using
/// a Lazy Clause Generation (LCG [\[1\]](https://people.eng.unimelb.edu.au/pstuckey/papers/cp09-lc.pdf))
/// approach.
///
/// The solver maintains two views of the problem, a Constraint Programming (CP) view and a SAT
/// view. It requires that all of the propagators which are added, are able to explain the
/// propagations and conflicts they have made/found. It then uses standard SAT concepts such as
/// 1UIP (see \[2\]) to learn clauses (also called nogoods in the CP field, see \[3\]) to avoid
/// unnecessary exploration of the search space while utilizing the search procedure benefits from
/// constraint programming (e.g. by preventing the exponential blow-up of problem encodings).
///
/// # Practical
/// The [`ConstraintSatisfactionSolver`] makes use of certain options which allow the user to
/// influence the behaviour of the solver; see the [`SatisfactionSolverOptions`].
///
/// The solver switches between making decisions using implementations of the [`Brancher`] (which
/// are passed to the [`ConstraintSatisfactionSolver::solve`] method) and propagation (use
/// [`ConstraintSatisfactionSolver::add_propagator`] to add a propagator). If a conflict is found by
/// any of the propagators (including the clausal one) then the solver will analyse the conflict
/// using 1UIP reasoning and backtrack if possible.
///
/// ## Example
/// This example will show how to set-up the [`ConstraintSatisfactionSolver`] to solve a simple not
/// equals problem between two variables. Note that any constraint is added in the form of
/// propagators.
/// ```
/// # use pumpkin_lib::engine::ConstraintSatisfactionSolver;
/// # use pumpkin_lib::propagators::arithmetic::linear_not_equal::LinearNotEqualConstructor;
/// # use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
/// # use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
/// # use pumpkin_lib::engine::variables::IntegerVariable;
/// # use pumpkin_lib::engine::variables::TransformableVariable;
/// # use pumpkin_lib::engine::termination::indefinite::Indefinite;
/// // We create a solver with default options (note that this is only possible in a testing environment)
/// let mut solver = ConstraintSatisfactionSolver::default();
///
/// // Now we create the two variables for which we want to define the propagator
/// let x = solver.create_new_integer_variable(0, 10, None);
/// let y = solver.create_new_integer_variable(0, 10, None);
///
/// // We add the propagator to the solver and check that adding the propagator did not cause a conflict
/// //  'x != y' is represented using the propagator for 'x - y != 0'
/// let no_root_level_conflict = solver.add_propagator(LinearNotEqualConstructor::new([x.into(), y.scaled(-1)].into(), 0));
/// assert!(no_root_level_conflict);
///
/// // We create a branching strategy, in our case we will simply use the default one
/// let mut brancher = IndependentVariableValueBrancher::default_over_all_variables(&solver);
///
/// // Then we solve the problem given a time-limit and a branching strategy
/// let result = solver.solve(&mut Indefinite, &mut brancher);
///
/// // Now we check that the result is feasible and that the chosen values for the two variables are different
/// assert_eq!(result, CSPSolverExecutionFlag::Feasible);
/// assert!(
///     solver.get_assigned_integer_value(&x).unwrap() != solver.get_assigned_integer_value(&y).unwrap()
/// );
/// ```
///
/// # Bibliography
/// \[1\] T. Feydy and P. J. Stuckey, ‘Lazy clause generation reengineered’, in International
/// Conference on Principles and Practice of Constraint Programming, 2009, pp. 352–366.
///
/// \[2\] J. Marques-Silva, I. Lynce, and S. Malik, ‘Conflict-driven clause learning SAT
/// solvers’, in Handbook of satisfiability, IOS press, 2021
///
/// \[3\] F. Rossi, P. Van Beek, and T. Walsh, ‘Constraint programming’, Foundations of Artificial
/// Intelligence, vol. 3, pp. 181–211, 2008.
#[allow(dead_code)]
pub struct ConstraintSatisfactionSolver {
    /// The solver continuously changes states during the search.
    /// The state helps track additional information and contributes to making the code clearer.
    pub(crate) state: CSPSolverState,
    /// The list of propagators. Propagators live here and are queried when events (domain changes)
    /// happen. The list is only traversed during synchronisation for now.
    propagators: Vec<Box<dyn Propagator>>,
    /// Tracks information about the restarts. Occassionally the solver will undo all its decisions
    /// and start the search from the root note. Note that learned clauses and other state
    /// information is kept after a restart.
    restart_strategy: RestartStrategy,
    /// Holds the assumptions when the solver is queried to solve under assumptions.
    assumptions: Vec<Predicate>,
    /// Performs conflict analysis, core extraction, and minimisation.
    conflict_nogood_analyser: ResolutionNogoodConflictAnalyser,
    semantic_minimiser: SemanticMinimiser,
    /// Tracks information related to the assignments of integer variables.
    pub(crate) assignments: Assignments,
    /// Contains information on which propagator to notify upon
    /// integer events, e.g., lower or upper bound change of a variable.
    watch_list_cp: WatchListCP,
    /// Dictates the order in which propagators will be called to propagate.
    propagator_queue: PropagatorQueue,
    /// Handles storing information about propagation reasons, which are used later to construct
    /// explanations during conflict analysis
    pub(crate) reason_store: ReasonStore,
    /// Contains events that need to be processed to notify propagators of event occurrences.
    /// Used as a helper storage vector to avoid reallocation, and to take away ownership from the
    /// events in assignments.
    event_drain: Vec<(IntDomainEvent, DomainId)>,
    /// A set of counters updated during the search.
    counters: Counters,
    /// Miscellaneous constant parameters used by the solver.
    internal_parameters: SatisfactionSolverOptions,
    /// The names of the variables in the solver.
    variable_names: VariableNames,
    /// Computes the LBD for nogoods.
    lbd_helper: Lbd,
}

impl Debug for ConstraintSatisfactionSolver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let cp_propagators: Vec<_> = self
            .propagators
            .iter()
            .map(|_| DebugDyn::from("Propagator"))
            .collect();
        f.debug_struct("ConstraintSatisfactionSolver")
            .field("state", &self.state)
            .field("assumptions", &self.assumptions)
            .field("restart_strategy", &self.restart_strategy)
            .field("cp_propagators", &cp_propagators)
            .field("counters", &self.counters)
            .field("internal_parameters", &self.internal_parameters)
            .finish()
    }
}

impl Default for ConstraintSatisfactionSolver {
    fn default() -> Self {
        ConstraintSatisfactionSolver::new(SatisfactionSolverOptions::default())
    }
}

/// Options for the [`Solver`] which determine how it behaves.
#[derive(Debug)]
pub struct SatisfactionSolverOptions {
    /// The options used by the restart strategy.
    pub restart_options: RestartOptions,
    /// Whether learned clause minimisation should take place
    pub learning_clause_minimisation: bool,
    /// A random number generator which is used by the [`Solver`] to determine randomised values.
    pub random_generator: SmallRng,
    /// The proof log for the solver.
    pub proof_log: ProofLog,
}

impl Default for SatisfactionSolverOptions {
    fn default() -> Self {
        SatisfactionSolverOptions {
            restart_options: RestartOptions::default(),
            learning_clause_minimisation: true,
            random_generator: SmallRng::seed_from_u64(42),
            proof_log: ProofLog::default(),
        }
    }
}

impl ConstraintSatisfactionSolver {
    fn get_nogood_propagator_id() -> PropagatorId {
        PropagatorId(0)
    }

    fn notify_nogood_propagator(
        event: IntDomainEvent,
        domain: DomainId,
        cp_propagators: &mut [Box<dyn Propagator>],
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
    ) {
        pumpkin_assert_moderate!(cp_propagators[0].name() == "NogoodPropagator");
        let nogood_propagator_id = Self::get_nogood_propagator_id();
        // The nogood propagator is implicitly subscribed to every domain event for every variable.
        // For this reason, its local id matches the domain id.
        // This is special only for the nogood propagator.
        let local_id = LocalId::from(domain.id);
        Self::notify_propagator(
            nogood_propagator_id,
            local_id,
            event,
            cp_propagators,
            propagator_queue,
            assignments,
        );
    }

    fn notify_propagator(
        propagator_id: PropagatorId,
        local_id: LocalId,
        event: IntDomainEvent,
        cp_propagators: &mut [Box<dyn Propagator>],
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
    ) {
        let context = PropagationContext::new(assignments);

        let enqueue_decision =
            cp_propagators[propagator_id.0 as usize].notify(context, local_id, event.into());

        if enqueue_decision == EnqueueDecision::Enqueue {
            propagator_queue.enqueue_propagator(
                propagator_id,
                cp_propagators[propagator_id.0 as usize].priority(),
            );
        }
    }

    /// Process the stored domain events that happens as a result of decision/propagation predicates
    /// to the trail. Propagators are notified and enqueued if needed about the domain events.
    fn notify_propagators_about_domain_events(&mut self) {
        assert!(self.event_drain.is_empty());

        // Eagerly adding since the drain operation lazily removes elements from internal data
        // structures.
        self.assignments.drain_domain_events().for_each(|e| {
            self.event_drain.push(e);
        });

        for (event, domain) in self.event_drain.drain(..) {
            // println!("\tnoti {} {}", domain, event);
            // Special case: the nogood propagator is notified about each event.
            Self::notify_nogood_propagator(
                event,
                domain,
                &mut self.propagators,
                &mut self.propagator_queue,
                &mut self.assignments,
            );
            // Now notify other propagators subscribed to this event.
            for propagator_var in self.watch_list_cp.get_affected_propagators(event, domain) {
                let propagator_id = propagator_var.propagator;
                let local_id = propagator_var.variable;
                Self::notify_propagator(
                    propagator_id,
                    local_id,
                    event,
                    &mut self.propagators,
                    &mut self.propagator_queue,
                    &mut self.assignments,
                );
            }
        }
    }

    /// This is a temporary accessor to help refactoring.
    pub fn get_solution_reference(&self) -> SolutionReference<'_> {
        SolutionReference::new(&self.assignments)
    }

    #[allow(dead_code)]
    pub(crate) fn is_conflicting(&self) -> bool {
        self.state.is_conflicting()
    }

    #[allow(dead_code)]
    pub(crate) fn declare_ready(&mut self) {
        self.state.declare_ready()
    }
}

// methods that offer basic functionality
impl ConstraintSatisfactionSolver {
    pub fn new(solver_options: SatisfactionSolverOptions) -> ConstraintSatisfactionSolver {
        let mut csp_solver = ConstraintSatisfactionSolver {
            state: CSPSolverState::default(),
            assumptions: Vec::default(),
            assignments: Assignments::default(),
            watch_list_cp: WatchListCP::default(),
            propagator_queue: PropagatorQueue::new(5),
            reason_store: ReasonStore::default(),
            event_drain: vec![],
            conflict_nogood_analyser: ResolutionNogoodConflictAnalyser::default(),
            restart_strategy: RestartStrategy::new(solver_options.restart_options),
            propagators: vec![],
            counters: Counters::default(),
            internal_parameters: solver_options,
            variable_names: VariableNames::default(),
            semantic_minimiser: SemanticMinimiser::default(),
            lbd_helper: Lbd::default(),
        };

        // As a convention, the assignments contain a dummy domain_id=0, which represents a 0-1
        // variable that is assigned to one. We use it to represent predicates that are
        // trivially true. We need to adjust other data structures to take this into account.
        csp_solver.watch_list_cp.grow();
        let dummy_id = Predicate::trivially_true().get_domain();

        csp_solver
            .variable_names
            .add_integer(dummy_id, "Dummy".to_owned());

        let _ = csp_solver.add_propagator(NogoodPropagatorConstructor);

        assert!(dummy_id.id == 0);
        assert!(csp_solver.assignments.get_lower_bound(dummy_id) == 1);
        assert!(csp_solver.assignments.get_upper_bound(dummy_id) == 1);

        csp_solver
    }

    pub fn solve(
        &mut self,
        termination: &mut impl TerminationCondition,
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        let dummy_assumptions: Vec<Predicate> = vec![];
        self.solve_under_assumptions(&dummy_assumptions, termination, brancher)
    }

    pub fn solve_under_assumptions(
        &mut self,
        assumptions: &[Predicate],
        termination: &mut impl TerminationCondition,
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        if self.state.is_inconsistent() {
            return CSPSolverExecutionFlag::Infeasible;
        }

        let start_time = Instant::now();

        self.initialise(assumptions);
        let result = self.solve_internal(termination, brancher);

        self.counters.time_spent_in_solver += start_time.elapsed().as_millis() as u64;

        result
    }

    pub fn get_state(&self) -> &CSPSolverState {
        &self.state
    }

    pub fn get_random_generator(&mut self) -> &mut impl Random {
        &mut self.internal_parameters.random_generator
    }

    pub fn log_statistics(&self) {
        self.counters.log_statistics()
    }

    pub fn create_new_literal(&mut self, name: Option<String>) -> Literal {
        let domain_id = self.create_new_integer_variable(0, 1, name);
        Literal::new(predicate![domain_id == 1])
    }

    /// Create a new integer variable. Its domain will have the given lower and upper bounds.
    pub fn create_new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: Option<String>,
    ) -> DomainId {
        assert!(
            !self.state.is_inconsistent(),
            "Variables cannot be created in an inconsistent state"
        );

        let domain_id = self.assignments.grow(lower_bound, upper_bound);
        self.watch_list_cp.grow();

        if let Some(name) = name {
            self.variable_names.add_integer(domain_id, name);
        }

        domain_id
    }

    /// Creates an integer variable with a domain containing only the values in `values`
    pub fn create_new_integer_variable_sparse(
        &mut self,
        mut values: Vec<i32>,
        name: Option<String>,
    ) -> DomainId {
        assert!(
            !values.is_empty(),
            "cannot create a variable with an empty domain"
        );

        values.sort();
        values.dedup();

        let lower_bound = values[0];
        let upper_bound = values[values.len() - 1];

        let domain_id = self.create_new_integer_variable(lower_bound, upper_bound, name);

        let mut next_idx = 0;
        for value in lower_bound..=upper_bound {
            if value == values[next_idx] {
                next_idx += 1;
            } else {
                self.assignments
                    .remove_value_from_domain(domain_id, value, None)
                    .expect("the domain should not be empty");
            }
        }
        pumpkin_assert_simple!(
            next_idx == values.len(),
            "Expected all values to have been processed"
        );

        domain_id
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
    ///   - If the solver is in an unsatisfied state, but solving was done without assumptions, this
    ///     will return an empty vector.
    ///   - If the assumptions are inconsistent, i.e. both literal x and !x are assumed, an error is
    ///     returned, with the literal being one of the inconsistent assumptions.
    ///
    /// # Example usage
    /// ```rust
    /// // We construct the following SAT instance:
    /// //   (x0 \/ x1 \/ x2) /\ (x0 \/ !x1 \/ x2)
    /// // And solve under the assumptions:
    /// //   !x0 /\ x1 /\ !x2
    /// # use pumpkin_lib::Solver;
    /// # use pumpkin_lib::variables::PropositionalVariable;
    /// # use pumpkin_lib::variables::Literal;
    /// # use pumpkin_lib::termination::Indefinite;
    /// # use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
    /// # use pumpkin_lib::results::SatisfactionResultUnderAssumptions;
    /// let mut solver = Solver::default();
    /// let x = vec![
    ///     solver.new_literal(),
    ///     solver.new_literal(),
    ///     solver.new_literal(),
    /// ];
    ///
    /// solver.add_clause([x[0], x[1], x[2]]);
    /// solver.add_clause([x[0], !x[1], x[2]]);
    ///
    /// let assumptions = [!x[0], x[1], !x[2]];
    /// let mut termination = Indefinite;
    /// let mut brancher = solver.default_brancher_over_all_propositional_variables();
    /// let result =
    ///     solver.satisfy_under_assumptions(&mut brancher, &mut termination, &assumptions);
    ///
    /// if let SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(
    ///     mut unsatisfiable,
    /// ) = result
    /// {
    ///     {
    ///         let core = unsatisfiable.extract_core();
    ///
    ///         // The order of the literals in the core is undefined, so we check for unordered
    ///         // equality.
    ///         assert_eq!(
    ///             core.len(),
    ///             assumptions.len(),
    ///             "the core has the length of the number of assumptions"
    ///         );
    ///         assert!(
    ///             core.iter().all(|&lit| assumptions.contains(&!lit)),
    ///             "all literals in the core are negated assumptions"
    ///         );
    ///     }
    /// }
    /// ```
    pub fn extract_clausal_core(
        &mut self,
        _brancher: &mut impl Brancher,
    ) -> Result<Vec<Predicate>, Predicate> {
        todo!();
        // let mut conflict_analysis_context = ConflictAnalysisContext {
        // assumptions: &self.assumptions,
        // clausal_propagator: &self.clausal_propagator,
        // assignments: &self.assignments,
        // assignments_propositional: &self.assignments_propositional,
        // internal_parameters: &self.internal_parameters,
        // solver_state: &mut self.state,
        // brancher,
        // clause_allocator: &mut self.clause_allocator,
        // explanation_clause_manager: &mut self.explanation_clause_manager,
        // reason_store: &mut self.reason_store,
        // counters: &mut self.counters,
        // learned_clause_manager: &mut self.learned_clause_manager,
        // restart_strategy: &mut self.restart_strategy,
        // };
        //
        // let core = self
        // .conflict_analyser
        // .compute_clausal_core(&mut conflict_analysis_context);
        //
        // if !self.state.is_infeasible() {
        // self.restore_state_at_root(brancher);
        // }
        //
        // core
    }

    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        let literal_is_true = self.assignments.is_predicate_satisfied(literal.into());
        let opposite_literal_is_true = self.assignments.is_predicate_satisfied((!literal).into());

        pumpkin_assert_moderate!(!(literal_is_true && opposite_literal_is_true));

        // If both the literal is not true and its negation is not true then the literal is
        // unassigned
        if !literal_is_true && !opposite_literal_is_true {
            None
        } else {
            Some(literal_is_true)
        }
    }

    /// Get the lower bound for the given variable.
    pub fn get_lower_bound(&self, variable: &impl IntegerVariable) -> i32 {
        variable.lower_bound(&self.assignments)
    }

    /// Get the upper bound for the given variable.
    pub fn get_upper_bound(&self, variable: &impl IntegerVariable) -> i32 {
        variable.upper_bound(&self.assignments)
    }

    /// Determine whether `value` is in the domain of `variable`.
    pub fn integer_variable_contains(&self, variable: &impl IntegerVariable, value: i32) -> bool {
        variable.contains(&self.assignments, value)
    }

    /// Get the assigned integer for the given variable. If it is not assigned, `None` is returned.
    pub fn get_assigned_integer_value(&self, variable: &impl IntegerVariable) -> Option<i32> {
        let lb = self.get_lower_bound(variable);
        let ub = self.get_upper_bound(variable);

        if lb == ub {
            Some(lb)
        } else {
            None
        }
    }

    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        if let Err(write_error) = proof.unsat(&self.variable_names) {
            warn!(
                "Failed to update the certificate file, error message: {}",
                write_error
            );
        }
    }

    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Predicate) {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);

        if let Err(write_error) = proof.optimal(bound, &self.variable_names) {
            warn!(
                "Failed to update the certificate file, error message: {}",
                write_error
            );
        }
    }

    pub fn restore_state_at_root(&mut self, brancher: &mut impl Brancher) {
        if self.assignments.get_decision_level() != 0 {
            self.backtrack(0, brancher);
            self.state.declare_ready();
        }
    }
}

// methods that serve as the main building blocks
impl ConstraintSatisfactionSolver {
    fn initialise(&mut self, assumptions: &[Predicate]) {
        pumpkin_assert_simple!(
            !self.state.is_infeasible_under_assumptions(),
            "Solver is not expected to be in the infeasible under assumptions state when initialising.
             Missed extracting the core?"
        );
        self.state.declare_solving();
        assumptions.clone_into(&mut self.assumptions);
    }

    fn solve_internal(
        &mut self,
        termination: &mut impl TerminationCondition,
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        loop {
            if termination.should_stop() {
                self.state.declare_timeout();
                return CSPSolverExecutionFlag::Timeout;
            }

            // println!("before prop. {}", self.assignments.get_decision_level());
            // for t in self.assignments.trail.iter() {
            // println!("\t{} {}", t.predicate, t.reason.is_none());
            // }
            //
            // for d in self.assignments.get_domains() {
            // println!(
            // "{}: [{}, {}]",
            // d,
            // self.assignments.get_lower_bound(d),
            // self.assignments.get_upper_bound(d)
            // );
            // }

            self.propagate();

            // println!("after prop. {}", self.assignments.get_decision_level());
            // for t in self.assignments.trail.iter() {
            // println!("\t{} {}", t.predicate, t.reason.is_none());
            // }
            //
            // for d in self.assignments.get_domains() {
            // println!(
            // "{}: [{}, {}]",
            // d,
            // self.assignments.get_lower_bound(d),
            // self.assignments.get_upper_bound(d)
            // );
            // }

            if self.state.no_conflict() {
                self.declare_new_decision_level();

                // println!("-------------------\nDEC LVL NEW");

                // Restarts should only occur after a new decision level has been declared to
                // account for the fact that all assumptions should be assigned when restarts take
                // place. Since one assumption is posted per decision level, all assumptions are
                // assigned when the decision level is strictly larger than the number of
                // assumptions.
                if self.restart_strategy.should_restart() {
                    self.restart_during_search(brancher);
                    self.declare_new_decision_level();
                }

                let branching_result = self.make_next_decision(brancher);

                if let Err(flag) = branching_result {
                    return flag;
                }
            }
            // conflict
            else {
                // println!("\tconflict {}", self.assignments.get_decision_level());
                //
                // for t in self.assignments.trail.iter() {
                // println!("\t\t{} {}", t.predicate, t.reason.is_none());
                // }

                if self.get_decision_level() == 0 {
                    self.state.declare_infeasible();
                    return CSPSolverExecutionFlag::Infeasible;
                }

                self.resolve_conflict_with_nogood(brancher);

                brancher.on_conflict();
                self.decay_nogood_activities();
            }
        }
    }

    fn decay_nogood_activities(&mut self) {
        let nogood_propagator_index = self
            .propagators
            .iter()
            .position(|propagator| propagator.name() == "NogoodPropagator")
            .expect("There has to be a nogood propagator!");

        match self.propagators[nogood_propagator_index].downcast_mut::<NogoodPropagator>() {
            Some(nogood_propagator) => {
                nogood_propagator.decay_nogood_activities();
            }
            None => panic!("Provided propagator should be the nogood propagator"),
        }
    }

    fn make_next_decision(
        &mut self,
        brancher: &mut impl Brancher,
    ) -> Result<(), CSPSolverExecutionFlag> {
        // Set the next decision to be an assumption, if there are assumptions left.
        // Currently assumptions are implemented by adding an assumption predicate
        // at separate decision levels.
        if let Some(assumption_literal) = self.peek_next_assumption_predicate() {
            self.enqueue_assumption_predicate(assumption_literal)
        }
        // Otherwise proceed with standard branching.
        else {
            let context = &mut SelectionContext::new(
                &self.assignments,
                &mut self.internal_parameters.random_generator,
            );
            // If there is a next decision, make the decision.
            if let Some(decision_predicate) = brancher.next_decision(context) {
                self.counters.num_decisions += 1;
                self.assignments
                    .post_predicate(decision_predicate, None)
                    .expect("Decisions are expected not to fail.");
                Ok(())
            }
            // Otherwise there are no more decisions to be made,
            // all predicates have been applied without a conflict,
            // meaning the problem is feasible.
            else {
                self.state.declare_solution_found();
                Err(CSPSolverExecutionFlag::Feasible)
            }
        }
    }

    /// Posts the assumption predicate.
    /// If the predicate is already true, nothing happens to the state.
    /// If the predicate is neither true nor false, and the state is set such that the
    /// predicate is now true.
    /// If the predicate is false, returns an Err(CSPSolverExecutionFlag::Infeasible).
    pub(crate) fn enqueue_assumption_predicate(
        &mut self,
        assumption_predicate: Predicate,
    ) -> Result<(), CSPSolverExecutionFlag> {
        match self.assignments.post_predicate(assumption_predicate, None) {
            // The assumption is set to true. Note that the predicate may have been already true.
            // This could happen when other assumptions propagated the predicate
            // or the assumption is already set to true at the root level.
            Ok(_) => Ok(()),
            // The assumption predicate is in conflict with the input assumptions,
            // which means the instance is infeasible under the current assumptions.
            Err(_) => {
                self.state
                    .declare_infeasible_under_assumptions(assumption_predicate);
                Err(CSPSolverExecutionFlag::Infeasible)
            }
        }
    }

    pub(crate) fn declare_new_decision_level(&mut self) {
        self.assignments.increase_decision_level();
        self.reason_store.increase_decision_level();
    }

    /// Changes the state based on the conflict analysis. It performs the following:
    /// - Derives a nogood using our CP version of the 1UIP scheme.
    /// - Adds the learned nogood to the database.
    /// - Performs backtracking.
    /// - Enqueues the propagated [`Predicate`] of the learned nogood.
    /// - Todo: Updates the internal data structures (e.g. for the restart strategy or the learned
    ///   clause manager)
    ///
    /// # Note
    /// This method performs no propagation, this is left up to the solver afterwards.
    fn resolve_conflict_with_nogood(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_moderate!(self.state.is_conflicting());

        let learned_nogood = self.compute_learned_nogood(brancher);
        // println!("NOGOOD LEARNING: {:?}", learned_nogood.predicates);
        self.process_learned_nogood(learned_nogood, brancher);
        self.state.declare_solving();
    }

    fn compute_learned_nogood(&mut self, brancher: &mut impl Brancher) -> LearnedNogood {
        let mut conflict_analysis_context = ConflictAnalysisNogoodContext {
            assignments: &self.assignments,
            counters: &mut self.counters,
            solver_state: &mut self.state,
            reason_store: &mut self.reason_store,
            brancher,
            semantic_minimiser: &mut self.semantic_minimiser,
            propagators: &mut self.propagators,
        };
        self.conflict_nogood_analyser
            .compute_1uip(&mut conflict_analysis_context)
    }

    fn process_learned_nogood(
        &mut self,
        learned_nogood: LearnedNogood,
        brancher: &mut impl Brancher,
    ) {
        // backjump or restart
        // ask the nogood propagator to add this nogood and propagate

        let learned_clause = learned_nogood
            .predicates
            .iter()
            .map(|&predicate| !predicate);
        if let Err(write_error) = self
            .internal_parameters
            .proof_log
            .log_learned_clause(learned_clause, &self.variable_names)
        {
            warn!(
                "Failed to update the certificate file, error message: {}",
                write_error
            );
        }

        // todo: I am not sure we need to treat unit nogoods in a special way? Simply backtrack and
        // post? The only issue may be that backtracking to 0 is not the same as a restart since
        // the restart also notifies the restart strategy, but is this a big difference?
        // Think about this.

        // Note that previous version of controlling restarts through number of propositional
        // variables is no longer applicable.
        // todo add LBD.

        // For now we do it simple -> log some statistical data before posting.

        self.counters.num_unit_clauses_learned += (learned_nogood.predicates.len() == 1) as u64;
        // important to notify about the conflict _before_ backtracking removes literals from
        // the trail -> although in the current version this does nothing but notify that a conflict
        // happened
        self.restart_strategy.notify_conflict(
            self.lbd_helper
                .compute_lbd(&learned_nogood.predicates, &self.assignments),
            self.assignments.get_pruned_value_count(),
        );

        self.counters
            .average_learned_clause_length
            .add_term(learned_nogood.predicates.len() as u64);

        if learned_nogood.backjump_level > 0 {
            self.counters
                .average_backtrack_amount
                .add_term((self.get_decision_level() - learned_nogood.backjump_level) as u64);
        }

        self.backtrack(learned_nogood.backjump_level, brancher);

        self.add_learned_nogood(learned_nogood);
    }

    fn add_learned_nogood(&mut self, learned_nogood: LearnedNogood) {
        // println!("NOgood {:?}", learned_nogood);

        let nogood_propagator_index = self
            .propagators
            .iter()
            .position(|propagator| propagator.name() == "NogoodPropagator")
            .expect("There has to be a nogood propagator!");
        let mut context = PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            &mut self.semantic_minimiser,
            Self::get_nogood_propagator_id(),
        );

        // println!("ADDING LEARNED: {:?}", learned_nogood.predicates);
        ConstraintSatisfactionSolver::add_asserting_nogood_to_nogood_propagator(
            &mut self.propagators[nogood_propagator_index],
            learned_nogood.predicates,
            &mut context,
        )
    }

    fn add_asserting_nogood_to_nogood_propagator(
        nogood_propagator: &mut Box<dyn Propagator>,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) {
        match nogood_propagator.downcast_mut::<NogoodPropagator>() {
            Some(nogood_propagator) => nogood_propagator.add_asserting_nogood(nogood, context),
            None => panic!("Provided propagator should be the nogood propagator"),
        }
    }

    /// Performs a restart during the search process; it is only called when it has been determined
    /// to be necessary by the [`ConstraintSatisfactionSolver::restart_strategy`]. A 'restart'
    /// differs from backtracking to level zero in that a restart backtracks to decision level
    /// zero, cleans up nogoods, and notifies the restart strategy.
    fn restart_during_search(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(
            self.get_decision_level() > self.assumptions.len(),
            "Sanity check: restarts should not trigger whilst assigning assumptions"
        );

        // no point backtracking past the assumption level
        if self.get_decision_level() <= self.assumptions.len() {
            return;
        }

        self.backtrack(0, brancher);

        self.restart_strategy.notify_restart();
    }

    pub(crate) fn backtrack(&mut self, backtrack_level: usize, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(backtrack_level < self.get_decision_level());

        self.assignments
            .synchronise(backtrack_level)
            .iter()
            .for_each(|(domain_id, previous_value)| {
                brancher.on_unassign_integer(*domain_id, *previous_value)
            });

        self.reason_store.synchronise(backtrack_level);
        self.propagator_queue.clear();
        // For now all propagators are called to synchronise, in the future this will be improved in
        // two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator_id in 0..self.propagators.len() {
            let context = PropagationContext::new(&self.assignments);
            self.propagators[propagator_id].synchronise(&context);
        }

        brancher.synchronise(&self.assignments);

        self.event_drain.clear();
    }

    fn compute_reason_for_empty_domain(&mut self) -> PropositionalConjunction {
        // The empty domain happened after posting the last predicate on the trail.
        // The reason for this empty domain is computed as the reason for the bounds before the last
        // trail predicate was posted, plus the reason for the last trail predicate.

        // The last predicate on the trail reveals the domain id that has resulted
        // in an empty domain.
        let entry = self.assignments.get_last_entry_on_trail();
        assert!(
            entry.reason.is_some(),
            "Cannot cause an empty domain using a decision."
        );
        let conflict_domain = entry.predicate.get_domain();
        assert!(
            entry.old_lower_bound != self.assignments.get_lower_bound(conflict_domain)
                || entry.old_upper_bound != self.assignments.get_upper_bound(conflict_domain),
            "One of the two bounds had to change."
        );

        let propagation_context = PropagationContext::new(&self.assignments);

        // Look up the reason for the bound that changed.
        // The reason for changing the bound cannot be a decision, so we can safely unwrap.
        let reason_changing_bound = ReasonStore::get_or_compute_new(
            &mut self.reason_store,
            entry.reason.unwrap(),
            &propagation_context,
            &mut self.propagators,
        )
        .unwrap();

        let mut empty_domain_reason: Vec<Predicate> = vec![
            predicate!(conflict_domain >= entry.old_lower_bound),
            predicate!(conflict_domain <= entry.old_upper_bound),
        ];

        empty_domain_reason.append(&mut reason_changing_bound.to_vec());
        empty_domain_reason.into()
    }

    /// Main propagation loop.
    pub(crate) fn propagate(&mut self) {
        // Record the number of predicates on the trail for statistics purposes.
        let num_assigned_variables_old = self.assignments.num_trail_entries();
        // The initial domain events are due to the decision predicate.
        self.notify_propagators_about_domain_events();
        // Keep propagating until there are unprocessed propagators, or a conflict is detected.
        while let Some(propagator_id) = self.propagator_queue.pop_new() {
            let propagator = &mut self.propagators[propagator_id.0 as usize];
            let context = PropagationContextMut::new(
                &mut self.assignments,
                &mut self.reason_store,
                &mut self.semantic_minimiser,
                propagator_id,
            );

            match propagator.propagate(context) {
                Ok(_) => {
                    // Notify other propagators of the propagations and continue.
                    self.notify_propagators_about_domain_events();
                }
                Err(inconsistency) => match inconsistency {
                    // A propagator did a change that resulted in an empty domain.
                    Inconsistency::EmptyDomain => {
                        let empty_domain_reason = self.compute_reason_for_empty_domain();

                        // todo: As a temporary solution, we remove the last trail element.
                        // This way we guarantee that the assignment is consistent, which is needed
                        // for the conflict analysis data structures. The proper alternative would
                        // be to forbid the assignments from getting into an inconsistent state.
                        self.assignments.remove_last_trail_element();

                        let stored_conflict_info = StoredConflictInfo::EmptyDomain {
                            conflict_nogood: empty_domain_reason,
                        };
                        self.state.declare_conflict(stored_conflict_info);
                        break;
                    }
                    // A propagator-specific reason for the current conflict.
                    Inconsistency::Conflict { conflict_nogood } => {
                        pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                            &self.assignments,
                            &conflict_nogood,
                            propagator.as_ref(),
                            propagator_id,
                        ));

                        // println!("{:?}\n{}", conflict_nogood, propagator_id.0);

                        let stored_conflict_info = StoredConflictInfo::Propagator {
                            conflict_nogood,
                            propagator_id,
                        };
                        self.state.declare_conflict(stored_conflict_info);
                        break;
                    }
                },
            }
        }
        // Record statistics.
        self.counters.num_conflicts += self.state.is_conflicting() as u64;
        self.counters.num_propagations +=
            self.assignments.num_trail_entries() as u64 - num_assigned_variables_old as u64;
        // Only check fixed point propagation if there was no reported conflict,
        // since otherwise the state may be inconsistent.
        pumpkin_assert_extreme!(
            self.state.is_conflicting()
                || DebugHelper::debug_fixed_point_propagation(&self.assignments, &self.propagators,)
        );
    }

    fn peek_next_assumption_predicate(&self) -> Option<Predicate> {
        // The convention is that at decision level i, the (i-1)th assumption is posted.
        // Note that decisions start being posted start at 1, hence the minus one.
        let next_assumption_index = self.get_decision_level() - 1;
        self.assumptions.get(next_assumption_index).copied()
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
    pub fn add_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> Result<(), ConstraintOperationError>
    where
        Constructor: PropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        if self.state.is_inconsistent() {
            return Err(ConstraintOperationError::InfeasiblePropagator);
        }

        let new_propagator_id = PropagatorId(self.propagators.len() as u32);
        let mut constructor_context =
            PropagatorConstructorContext::new(&mut self.watch_list_cp, new_propagator_id);

        let propagator_to_add = constructor.create_boxed(&mut constructor_context);

        pumpkin_assert_simple!(
            propagator_to_add.priority() <= 3,
            "The propagator priority exceeds 3.
             Currently we only support values up to 3,
             but this can easily be changed if there is a good reason."
        );

        self.propagators.push(propagator_to_add);

        let new_propagator = &mut self.propagators[new_propagator_id];
        let context = PropagationContext::new(&self.assignments);
        if new_propagator.initialise_at_root(context).is_err() {
            self.state.declare_infeasible();
            Err(ConstraintOperationError::InfeasiblePropagator)
        } else {
            self.propagator_queue
                .enqueue_propagator(new_propagator_id, new_propagator.priority());

            self.propagate();

            if self.state.no_conflict() {
                Ok(())
            } else {
                Err(ConstraintOperationError::InfeasiblePropagator)
            }
        }
    }

    pub fn post_predicate(&mut self, predicate: Predicate) -> Result<(), ConstraintOperationError> {
        assert!(
            self.get_decision_level() == 0,
            "Can only post predicates at the root level."
        );

        if self.state.is_infeasible() {
            Err(ConstraintOperationError::InfeasibleState)
        } else {
            match self.assignments.post_predicate(predicate, None) {
                Ok(_) => Ok(()),
                Err(_) => Err(ConstraintOperationError::InfeasibleNogood),
            }
        }
    }

    pub fn add_nogood(&mut self, nogood: Vec<Predicate>) -> Result<(), ConstraintOperationError> {
        let mut propagation_context = PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            &mut self.semantic_minimiser,
            Self::get_nogood_propagator_id(),
        );
        let nogood_propagator_id = Self::get_nogood_propagator_id();
        ConstraintSatisfactionSolver::add_nogood_to_nogood_propagator(
            &mut self.propagators[nogood_propagator_id],
            nogood,
            &mut propagation_context,
        )?;
        // temporary hack for the nogood propagator that does propagation from scratch
        self.propagator_queue.enqueue_propagator(PropagatorId(0), 0);
        self.propagate();
        if self.state.is_infeasible() {
            Err(ConstraintOperationError::InfeasibleState)
        } else {
            Ok(())
        }
    }

    fn add_nogood_to_nogood_propagator(
        nogood_propagator: &mut Box<dyn Propagator>,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) -> Result<(), ConstraintOperationError> {
        match nogood_propagator.downcast_mut::<NogoodPropagator>() {
            Some(nogood_propagator) => nogood_propagator.add_nogood(nogood, context),
            None => {
                panic!("Provided propagator should be the nogood propagator",)
            }
        }
    }

    /// Creates a clause from `literals` and adds it to the current formula.
    ///
    /// If the formula becomes trivially unsatisfiable, a [`ConstraintOperationError`] will be
    /// returned. Subsequent calls to this m\Zethod will always return an error, and no
    /// modification of the solver will take place.
    pub fn add_clause(
        &mut self,
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_simple!(
            self.get_decision_level() == 0,
            "Clauses can only be added in the root"
        );

        // todo: took as input literals, but now we have nogoods?
        // also remove the add_clause with add_nogood
        // Imko: I think we can simply negate the clause and retrieve a nogood, e.g. if we have the
        // clause `[x1 >= 5] \/ [x2 != 3] \/ [x3 <= 5]`, then it **cannot** be the case that `[x1 <
        // 5] /\ [x2 = 3] /\ [x3 > 5]`
        let mut are_all_falsified_at_root = true;
        let predicates = predicates
            .into_iter()
            .map(|predicate| {
                are_all_falsified_at_root &= self.assignments.is_predicate_falsified(predicate);
                !predicate
            })
            .collect::<Vec<_>>();

        if predicates.is_empty() {
            self.state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    ConstraintOperationError::InfeasibleClause,
                ));
            return Err(ConstraintOperationError::InfeasibleClause);
        }

        if are_all_falsified_at_root {
            self.state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    ConstraintOperationError::InfeasibleClause,
                ));
            return Err(ConstraintOperationError::InfeasibleClause);
        }

        if let Err(constraint_operation_error) = self.add_nogood(predicates) {
            self.state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    constraint_operation_error,
                ));
            return Err(constraint_operation_error);
        }
        Ok(())
        // pumpkin_assert_moderate!(!self.state.is_infeasible_under_assumptions());
        // pumpkin_assert_moderate!(self.is_propagation_complete());
        //
        // if self.state.is_infeasible() {
        // return Err(ConstraintOperationError::InfeasibleState);
        // }
        //
        // let literals: Vec<Literal> = literals.into_iter().collect();
        //
        // let nogood: Vec<Predicate> = literals
        // .iter()
        // .map(|literal| {
        // self.variable_literal_mappings
        // .get_predicates(*literal)
        // .next()
        // .unwrap()
        // .not()
        // })
        // .collect();
        //
        // let result = self.clausal_propagator.add_permanent_clause(
        // literals,
        // &mut self.assignments_propositional,
        // &mut self.clause_allocator,
        // );
        //
        // if result.is_err() {
        // self.state.declare_infeasible();
        // return Err(ConstraintOperationError::InfeasibleClause);
        // }
        //
        // hack: not actually a learned nogood but can work
        // self.add_learned_nogood(LearnedNogood {
        // predicates: nogood,
        // backjump_level: 0,
        // });
        //
        // self.propagate();
        //
        // if self.state.is_infeasible() {
        // self.state.declare_infeasible();
        // return Err(ConstraintOperationError::InfeasibleClause);
        // }
        //
        // Ok(())
    }

    pub(crate) fn get_decision_level(&self) -> usize {
        self.assignments.get_decision_level()
    }
}

/// Structure responsible for storing several statistics of the solving process of the
/// [`ConstraintSatisfactionSolver`].
#[derive(Default, Debug, Copy, Clone)]
pub(crate) struct Counters {
    pub(crate) num_decisions: u64,
    pub(crate) num_conflicts: u64,
    pub(crate) average_conflict_size: CumulativeMovingAverage,
    num_propagations: u64,
    num_unit_clauses_learned: u64,
    average_learned_clause_length: CumulativeMovingAverage,
    time_spent_in_solver: u64,
    average_backtrack_amount: CumulativeMovingAverage,
}

impl Counters {
    fn log_statistics(&self) {
        log_statistic("numberOfDecisions", self.num_decisions);
        log_statistic("numberOfConflicts", self.num_conflicts);
        log_statistic(
            "averageSizeOfConflictExplanation",
            self.average_conflict_size.value(),
        );
        log_statistic("numberOfPropagations", self.num_propagations);
        log_statistic("numberOfLearnedUnitClauses", self.num_unit_clauses_learned);
        log_statistic(
            "averageLearnedClauseLength",
            self.average_learned_clause_length.value(),
        );
        log_statistic("timeSpentInSolverInMilliseconds", self.time_spent_in_solver);
        log_statistic(
            "averageBacktrackAmount",
            self.average_backtrack_amount.value(),
        );
    }
}

#[derive(Default, Debug)]
enum CSPSolverStateInternal {
    #[default]
    Ready,
    Solving,
    ContainsSolution,
    Conflict {
        conflict_info: StoredConflictInfo,
    },
    Infeasible,
    InfeasibleUnderAssumptions {
        violated_assumption: Predicate,
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
        !self.is_conflicting()
    }

    pub fn is_conflicting(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::Conflict { conflict_info: _ }
        )
        // self.is_clausal_conflict() || self.is_cp_conflict()
    }

    pub fn is_infeasible(&self) -> bool {
        matches!(self.internal_state, CSPSolverStateInternal::Infeasible)
    }

    /// Determines whether the current state is inconsistent; i.e. whether it is conflicting,
    /// infeasible or infeasible under assumptions
    pub fn is_inconsistent(&self) -> bool {
        self.is_conflicting() || self.is_infeasible() || self.is_infeasible_under_assumptions()
    }

    pub fn is_infeasible_under_assumptions(&self) -> bool {
        matches!(
            self.internal_state,
            CSPSolverStateInternal::InfeasibleUnderAssumptions {
                violated_assumption: _
            }
        )
    }

    pub fn get_violated_assumption(&self) -> Predicate {
        if let CSPSolverStateInternal::InfeasibleUnderAssumptions {
            violated_assumption,
        } = self.internal_state
        {
            violated_assumption
        } else {
            panic!(
                "Cannot extract violated assumption without getting the solver into the infeasible
                 under assumptions state."
            );
        }
    }

    pub fn get_conflict_info(&self) -> &StoredConflictInfo {
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

    pub(crate) fn declare_ready(&mut self) {
        self.internal_state = CSPSolverStateInternal::Ready;
    }

    pub fn declare_solving(&mut self) {
        pumpkin_assert_simple!((self.is_ready() || self.is_conflicting()) && !self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::Solving;
    }

    fn declare_infeasible(&mut self) {
        self.internal_state = CSPSolverStateInternal::Infeasible;
    }

    fn declare_conflict(&mut self, conflict_info: StoredConflictInfo) {
        pumpkin_assert_simple!(!self.is_conflicting());
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

    fn declare_infeasible_under_assumptions(&mut self, violated_assumption: Predicate) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::InfeasibleUnderAssumptions {
            violated_assumption,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ConstraintSatisfactionSolver;
    use crate::basic_types::CSPSolverExecutionFlag;
    use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::termination::indefinite::Indefinite;

    #[allow(dead_code)]
    fn is_same_core(core1: &[Predicate], core2: &[Predicate]) -> bool {
        core1.len() == core2.len() && core2.iter().all(|lit| core1.contains(lit))
    }

    #[allow(dead_code)]
    fn is_result_the_same(
        res1: &Result<Vec<Predicate>, Predicate>,
        res2: &Result<Vec<Predicate>, Predicate>,
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
            res1.clone().unwrap_err() == res2.clone().unwrap_err()
        }
        // otherwise the two results are both ok
        else {
            println!("ok");
            is_same_core(&res1.clone().unwrap(), &res2.clone().unwrap())
        }
    }

    #[allow(dead_code)]
    fn run_test(
        mut solver: ConstraintSatisfactionSolver,
        assumptions: Vec<Predicate>,
        expected_flag: CSPSolverExecutionFlag,
        expected_result: Result<Vec<Predicate>, Predicate>,
    ) {
        let mut brancher = IndependentVariableValueBrancher::default_over_all_variables(&solver);
        let flag = solver.solve_under_assumptions(&assumptions, &mut Indefinite, &mut brancher);
        assert!(flag == expected_flag, "The flags do not match.");

        if matches!(flag, CSPSolverExecutionFlag::Infeasible) {
            assert!(
                is_result_the_same(
                    &solver.extract_clausal_core(&mut brancher),
                    &expected_result
                ),
                "The result is not the same"
            );
        }
    }

    // TODO: readd this
    // fn create_instance1() -> (ConstraintSatisfactionSolver, Vec<Predicate>) {
    // let mut solver = ConstraintSatisfactionSolver::default();
    // let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
    // let lit2 = Literal::new(solver.create_new_propositional_variable(None), true);
    //
    // let _ = solver.add_clause([lit1, lit2]);
    // let _ = solver.add_clause([lit1, !lit2]);
    // let _ = solver.add_clause([!lit1, lit2]);
    // (solver, vec![lit1, lit2])
    // }

    // #[test]
    // fn simple_core_extraction_1_1() {
    // let (solver, lits) = create_instance1();
    // run_test(
    // solver,
    // vec![!lits[0], !lits[1]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![!lits[0]]),
    // )
    // }
    //
    // #[test]
    // fn simple_core_extraction_1_2() {
    // let (solver, lits) = create_instance1();
    // run_test(
    // solver,
    // vec![!lits[1], !lits[0]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![!lits[1]]),
    // );
    // }
    //
    // #[test]
    // fn simple_core_extraction_1_infeasible() {
    // let (mut solver, lits) = create_instance1();
    // let _ = solver.add_clause([!lits[0], !lits[1]]);
    // run_test(
    // solver,
    // vec![!lits[1], !lits[0]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![]),
    // );
    // }
    //
    // #[test]
    // fn simple_core_extraction_1_core_before_inconsistency() {
    // let (solver, lits) = create_instance1();
    // run_test(
    // solver,
    // vec![!lits[1], lits[1]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![!lits[1]]), // the core gets computed before inconsistency is detected
    // );
    // }
    //
    // fn create_instance2() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
    // let mut solver = ConstraintSatisfactionSolver::default();
    // let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
    // let lit2 = Literal::new(solver.create_new_propositional_variable(None), true);
    // let lit3 = Literal::new(solver.create_new_propositional_variable(None), true);
    //
    // let _ = solver.add_clause([lit1, lit2, lit3]);
    // let _ = solver.add_clause([lit1, !lit2, lit3]);
    // (solver, vec![lit1, lit2, lit3])
    // }
    //
    // #[test]
    // fn simple_core_extraction_2_1() {
    // let (solver, lits) = create_instance2();
    // run_test(
    // solver,
    // vec![!lits[0], lits[1], !lits[2]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![lits[0], !lits[1], lits[2]]),
    // );
    // }
    //
    // #[test]
    // fn simple_core_extraction_2_long_assumptions_with_inconsistency_at_the_end() {
    // let (solver, lits) = create_instance2();
    // run_test(
    // solver,
    // vec![!lits[0], lits[1], !lits[2], lits[0]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![lits[0], !lits[1], lits[2]]), /* could return inconsistent assumptions,
    // however inconsistency will not be detected
    // given the order of the assumptions */
    // );
    // }
    //
    // #[test]
    // fn simple_core_extraction_2_inconsistent_long_assumptions() {
    // let (solver, lits) = create_instance2();
    // run_test(
    // solver,
    // vec![!lits[0], !lits[0], !lits[1], !lits[1], lits[0]],
    // CSPSolverExecutionFlag::Infeasible,
    // Err(lits[0]),
    // );
    // }
    //
    // fn create_instance3() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
    // let mut solver = ConstraintSatisfactionSolver::default();
    // let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
    // let lit2 = Literal::new(solver.create_new_propositional_variable(None), true);
    // let lit3 = Literal::new(solver.create_new_propositional_variable(None), true);
    // let _ = solver.add_clause([lit1, lit2, lit3]);
    // (solver, vec![lit1, lit2, lit3])
    // }
    //
    // #[test]
    // fn simple_core_extraction_3_1() {
    // let (solver, lits) = create_instance3();
    // run_test(
    // solver,
    // vec![!lits[0], !lits[1], !lits[2]],
    // CSPSolverExecutionFlag::Infeasible,
    // Ok(vec![lits[0], lits[1], lits[2]]),
    // );
    // }
    //
    // #[test]
    // fn simple_core_extraction_3_2() {
    // let (solver, lits) = create_instance3();
    // run_test(
    // solver,
    // vec![!lits[0], !lits[1]],
    // CSPSolverExecutionFlag::Feasible,
    // Ok(vec![]), // will be ignored in the test
    // );
    // }
    //
    // #[test]
    // fn negative_upper_bound() {
    // let mut solver = ConstraintSatisfactionSolver::default();
    // let domain_id = solver.create_new_integer_variable(0, 10, None);
    //
    // let result = solver.get_literal(predicate![domain_id <= -2]);
    // assert_eq!(result, solver.assignments_propositional.false_literal);
    // }
    //
    // #[test]
    // fn lower_bound_literal_lower_than_lower_bound_should_be_true_literal() {
    // let mut solver = ConstraintSatisfactionSolver::default();
    // let domain_id = solver.create_new_integer_variable(0, 10, None);
    // let result = solver.get_literal(predicate![domain_id >= -2]);
    // assert_eq!(result, solver.assignments_propositional.true_literal);
    // }
    //
    // #[test]
    // fn new_domain_with_negative_lower_bound() {
    // let lb = -2;
    // let ub = 2;
    //
    // let mut solver = ConstraintSatisfactionSolver::default();
    // let domain_id = solver.create_new_integer_variable(lb, ub, None);
    //
    // assert_eq!(lb, solver.assignments.get_lower_bound(domain_id));
    //
    // assert_eq!(ub, solver.assignments.get_upper_bound(domain_id));
    //
    // assert_eq!(
    // solver.assignments_propositional.true_literal,
    // solver.get_literal(predicate![domain_id >= lb])
    // );
    //
    // assert_eq!(
    // solver.assignments_propositional.false_literal,
    // solver.get_literal(predicate![domain_id <= lb - 1])
    // );
    //
    // assert!(solver
    // .assignments_propositional
    // .is_literal_unassigned(solver.get_literal(predicate![domain_id == lb])));
    //
    // assert_eq!(
    // solver.assignments_propositional.false_literal,
    // solver.get_literal(predicate![domain_id == lb - 1])
    // );
    //
    // for value in (lb + 1)..ub {
    // let literal = solver.get_literal(predicate![domain_id >= value]);
    //
    // assert!(solver
    // .assignments_propositional
    // .is_literal_unassigned(literal));
    //
    // assert!(solver
    // .assignments_propositional
    // .is_literal_unassigned(solver.get_literal(predicate![domain_id == value])));
    // }
    //
    // assert_eq!(
    // solver.assignments_propositional.false_literal,
    // solver.get_literal(predicate![domain_id >= ub + 1])
    // );
    // assert_eq!(
    // solver.assignments_propositional.true_literal,
    // solver.get_literal(predicate![domain_id <= ub])
    // );
    // assert!(solver
    // .assignments_propositional
    // .is_literal_unassigned(solver.get_literal(predicate![domain_id == ub])));
    // assert_eq!(
    // solver.assignments_propositional.false_literal,
    // solver.get_literal(predicate![domain_id == ub + 1])
    // );
    // }
    //
    // #[test]
    // fn check_correspondence_predicates_creating_new_int_domain() {
    // let mut solver = ConstraintSatisfactionSolver::default();
    //
    // let lower_bound = 0;
    // let upper_bound = 10;
    // let domain_id = solver.create_new_integer_variable(lower_bound, upper_bound, None);
    //
    // for bound in lower_bound + 1..upper_bound {
    // let lower_bound_predicate = predicate![domain_id >= bound];
    // let equality_predicate = predicate![domain_id == bound];
    // for predicate in [lower_bound_predicate, equality_predicate] {
    // let literal = solver.get_literal(predicate);
    // assert!(
    // solver.variable_literal_mappings.literal_to_predicates[literal]
    // .contains(&predicate)
    // )
    // }
    // }
    // }
}
