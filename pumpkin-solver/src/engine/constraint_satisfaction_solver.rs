//! Houses the solver which attempts to find a solution to a Constraint Satisfaction Problem (CSP)
//! using a Lazy Clause Generation approach.
use std::collections::VecDeque;
use std::fmt::Debug;
use std::num::NonZero;
use std::time::Instant;

use clap::ValueEnum;
use drcp_format::steps::StepId;
use rand::rngs::SmallRng;
use rand::SeedableRng;

use super::conflict_analysis::AnalysisMode;
use super::conflict_analysis::ConflictAnalysisContext;
use super::conflict_analysis::LearnedNogood;
use super::conflict_analysis::NoLearningResolver;
use super::conflict_analysis::SemanticMinimiser;
use super::nogoods::Lbd;
use super::propagation::contexts::StatefulPropagationContext;
use super::propagation::store::PropagatorStore;
use super::propagation::PropagatorId;
use super::solver_statistics::SolverStatistics;
use super::termination::TerminationCondition;
use super::variables::IntegerVariable;
use super::variables::Literal;
use super::ResolutionResolver;
use super::TrailedAssignments;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashMap;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Random;
use crate::basic_types::SolutionReference;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::engine::conflict_analysis::ConflictResolver as Resolver;
use crate::engine::cp::PropagatorQueue;
use crate::engine::cp::WatchListCP;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::engine::DebugHelper;
use crate::engine::IntDomainEvent;
use crate::engine::RestartOptions;
use crate::engine::RestartStrategy;
use crate::predicate;
use crate::proof::ProofLog;
use crate::propagators::nogoods::LearningOptions;
use crate::propagators::nogoods::NogoodPropagator;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::statistics::statistic_logger::StatisticLogger;
use crate::statistics::statistic_logging::should_log_statistics;
use crate::statistics::Statistic;
use crate::variable_names::VariableNames;
#[cfg(doc)]
use crate::Solver;

/// A solver which attempts to find a solution to a Constraint Satisfaction Problem (CSP) using
/// a Lazy Clause Generation (LCG [\[1\]](https://people.eng.unimelb.edu.au/pstuckey/papers/cp09-lc.pdf))
/// approach.
///
/// It requires that all of the propagators which are added, are able to explain the
/// propagations and conflicts they have made/found. It then uses standard SAT concepts such as
/// 1UIP (see \[2\]) to learn clauses (also called nogoods in the CP field, see \[3\]) to avoid
/// unnecessary exploration of the search space while utilizing the search procedure benefits from
/// constraint programming (e.g. by preventing the exponential blow-up of problem encodings).
///
/// # Practical
/// The [`ConstraintSatisfactionSolver`] makes use of certain options which allow the user to
/// influence the behaviour of the solver; see for example the [`SatisfactionSolverOptions`].
///
/// The solver switches between making decisions using implementations of the [`Brancher`] (which
/// are passed to the [`ConstraintSatisfactionSolver::solve`] method) and propagation (use
/// [`ConstraintSatisfactionSolver::add_propagator`] to add a propagator). If a conflict is found by
/// any of the propagators then the solver will analyse the conflict
/// using 1UIP reasoning and backtrack if possible.
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
#[derive(Debug)]
pub struct ConstraintSatisfactionSolver {
    /// The solver continuously changes states during the search.
    /// The state helps track additional information and contributes to making the code clearer.
    pub(crate) state: CSPSolverState,
    /// The list of propagators. Propagators live here and are queried when events (domain changes)
    /// happen. The list is only traversed during synchronisation for now.
    propagators: PropagatorStore,
    /// Tracks information about the restarts. Occassionally the solver will undo all its decisions
    /// and start the search from the root note. Note that learned clauses and other state
    /// information is kept after a restart.
    restart_strategy: RestartStrategy,
    /// Holds the assumptions when the solver is queried to solve under assumptions.
    assumptions: Vec<Predicate>,
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
    /// Contains events that need to be processed to notify propagators of backtrack
    /// [`IntDomainEvent`] occurrences (i.e. [`IntDomainEvent`]s being undone).
    backtrack_event_drain: Vec<(IntDomainEvent, DomainId)>,
    last_notified_cp_trail_index: usize,
    /// A set of counters updated during the search.
    solver_statistics: SolverStatistics,
    /// Miscellaneous constant parameters used by the solver.
    internal_parameters: SatisfactionSolverOptions,
    /// The names of the variables in the solver.
    variable_names: VariableNames,
    /// Computes the LBD for nogoods.
    lbd_helper: Lbd,
    /// A map from clause references to nogood step ids in the proof.
    unit_nogood_step_ids: HashMap<Predicate, StepId>,
    /// The resolver which is used upon a conflict.
    conflict_resolver: Box<dyn Resolver>,

    pub(crate) stateful_assignments: TrailedAssignments,
}

impl Default for ConstraintSatisfactionSolver {
    fn default() -> Self {
        ConstraintSatisfactionSolver::new(SatisfactionSolverOptions::default())
    }
}

/// The result of [`ConstraintSatisfactionSolver::extract_clausal_core`]; there are 2 cases:
/// 1. In the case of [`CoreExtractionResult::ConflictingAssumption`], two assumptions have been
///    given which directly conflict with one another; e.g. if the assumptions `[x, !x]` have been
///    given then the result of [`ConstraintSatisfactionSolver::extract_clausal_core`] will be a
///    [`CoreExtractionResult::ConflictingAssumption`] containing `x`.
/// 2. The standard case is when a [`CoreExtractionResult::Core`] is returned which contains (a
///    subset of) the assumptions which led to conflict.
#[derive(Debug, Clone)]
pub enum CoreExtractionResult {
    /// Conflicting assumptions were provided; e.g. in the case of the assumptions `[x, !x]`, this
    /// result will contain `!x`
    ConflictingAssumption(Predicate),
    /// The standard case where this result contains the core consisting of (a subset of) the
    /// assumptions which led to conflict.
    Core(Vec<Predicate>),
}

/// During search, the CP solver will inevitably evaluate partial assignments that violate at
/// least one constraint. When this happens, conflict resolution is applied to restore the
/// solver to a state from which it can continue the search.
///
/// The manner in which conflict resolution is done greatly impacts the performance of the
/// solver.
#[derive(ValueEnum, Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum ConflictResolver {
    NoLearning,
    #[default]
    UIP,
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
    /// The resolver used for conflict analysis
    pub conflict_resolver: ConflictResolver,
    /// The options which influence the learning of the solver.
    pub learning_options: LearningOptions,
}

impl Default for SatisfactionSolverOptions {
    fn default() -> Self {
        SatisfactionSolverOptions {
            restart_options: RestartOptions::default(),
            learning_clause_minimisation: true,
            random_generator: SmallRng::seed_from_u64(42),
            proof_log: ProofLog::default(),
            conflict_resolver: ConflictResolver::default(),
            learning_options: LearningOptions::default(),
        }
    }
}

impl ConstraintSatisfactionSolver {
    pub(crate) fn get_nogood_propagator_id() -> PropagatorId {
        PropagatorId(0)
    }

    fn process_backtrack_events(
        watch_list_cp: &mut WatchListCP,
        backtrack_event_drain: &mut Vec<(IntDomainEvent, DomainId)>,
        assignments: &mut Assignments,
        propagators: &mut PropagatorStore,
    ) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if watch_list_cp.is_watching_any_backtrack_events() {
            backtrack_event_drain.extend(assignments.drain_backtrack_domain_events());

            if backtrack_event_drain.is_empty() {
                return false;
            }

            for (event, domain) in backtrack_event_drain.drain(..) {
                for propagator_var in
                    watch_list_cp.get_backtrack_affected_propagators(event, domain)
                {
                    let propagator = &mut propagators[propagator_var.propagator];
                    let context = PropagationContext::new(assignments);

                    propagator.notify_backtrack(context, propagator_var.variable, event.into())
                }
            }
        }
        true
    }

    fn notify_nogood_propagator(
        event: IntDomainEvent,
        domain: DomainId,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
        stateful_assignments: &mut TrailedAssignments,
    ) {
        pumpkin_assert_moderate!(
            propagators[Self::get_nogood_propagator_id()].name() == "NogoodPropagator"
        );
        let nogood_propagator_id = Self::get_nogood_propagator_id();
        // The nogood propagator is implicitly subscribed to every domain event for every variable.
        // For this reason, its local id matches the domain id.
        // This is special only for the nogood propagator.
        let local_id = LocalId::from(domain.id);
        Self::notify_propagator(
            nogood_propagator_id,
            local_id,
            event,
            propagators,
            propagator_queue,
            assignments,
            stateful_assignments,
        );
    }

    fn notify_propagator(
        propagator_id: PropagatorId,
        local_id: LocalId,
        event: IntDomainEvent,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
        stateful_assignments: &mut TrailedAssignments,
    ) {
        let context = StatefulPropagationContext::new(stateful_assignments, assignments);

        let enqueue_decision = propagators[propagator_id].notify(context, local_id, event.into());

        if enqueue_decision == EnqueueDecision::Enqueue {
            propagator_queue
                .enqueue_propagator(propagator_id, propagators[propagator_id].priority());
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
            // Special case: the nogood propagator is notified about each event.
            Self::notify_nogood_propagator(
                event,
                domain,
                &mut self.propagators,
                &mut self.propagator_queue,
                &mut self.assignments,
                &mut self.stateful_assignments,
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
                    &mut self.stateful_assignments,
                );
            }
        }
        self.last_notified_cp_trail_index = self.assignments.num_trail_entries();
    }

    /// This is a temporary accessor to help refactoring.
    pub fn get_solution_reference(&self) -> SolutionReference<'_> {
        SolutionReference::new(&self.assignments)
    }

    pub(crate) fn is_conflicting(&self) -> bool {
        self.state.is_conflicting()
    }

    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) -> std::io::Result<()> {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        proof.unsat(&self.variable_names)
    }

    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Predicate) -> std::io::Result<()> {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        proof.optimal(bound, &self.variable_names)
    }

    fn complete_proof(&mut self) {
        pumpkin_assert_simple!(
            self.is_conflicting(),
            "Proof attempted to be completed while not in conflicting state"
        );
        let mut conflict_analysis_context = ConflictAnalysisContext {
            assignments: &mut self.assignments,
            counters: &mut self.solver_statistics,
            solver_state: &mut self.state,
            reason_store: &mut self.reason_store,
            brancher: &mut DummyBrancher,
            semantic_minimiser: &mut self.semantic_minimiser,
            propagators: &mut self.propagators,
            last_notified_cp_trail_index: &mut self.last_notified_cp_trail_index,
            watch_list_cp: &mut self.watch_list_cp,
            propagator_queue: &mut self.propagator_queue,
            event_drain: &mut self.event_drain,
            backtrack_event_drain: &mut self.backtrack_event_drain,
            should_minimise: self.internal_parameters.learning_clause_minimisation,
            proof_log: &mut self.internal_parameters.proof_log,
            is_completing_proof: true,
            unit_nogood_step_ids: &self.unit_nogood_step_ids,
            stateful_assignments: &mut self.stateful_assignments,
        };

        let result = self
            .conflict_resolver
            .resolve_conflict(&mut conflict_analysis_context)
            .expect("Should have a nogood");

        let _ = self
            .internal_parameters
            .proof_log
            .log_learned_clause(result.predicates, &self.variable_names);
    }
}

// methods that offer basic functionality
impl ConstraintSatisfactionSolver {
    pub fn new(solver_options: SatisfactionSolverOptions) -> Self {
        let mut csp_solver: ConstraintSatisfactionSolver = ConstraintSatisfactionSolver {
            last_notified_cp_trail_index: 0,
            state: CSPSolverState::default(),
            assumptions: Vec::default(),
            assignments: Assignments::default(),
            watch_list_cp: WatchListCP::default(),
            propagator_queue: PropagatorQueue::new(5),
            reason_store: ReasonStore::default(),
            event_drain: vec![],
            backtrack_event_drain: vec![],
            restart_strategy: RestartStrategy::new(solver_options.restart_options),
            propagators: PropagatorStore::default(),
            solver_statistics: SolverStatistics::default(),
            variable_names: VariableNames::default(),
            semantic_minimiser: SemanticMinimiser::default(),
            lbd_helper: Lbd::default(),
            unit_nogood_step_ids: Default::default(),
            conflict_resolver: match solver_options.conflict_resolver {
                ConflictResolver::NoLearning => Box::new(NoLearningResolver),
                ConflictResolver::UIP => Box::new(ResolutionResolver::default()),
            },
            internal_parameters: solver_options,
            stateful_assignments: TrailedAssignments::default(),
        };

        // As a convention, the assignments contain a dummy domain_id=0, which represents a 0-1
        // variable that is assigned to one. We use it to represent predicates that are
        // trivially true. We need to adjust other data structures to take this into account.
        csp_solver.watch_list_cp.grow();
        let dummy_id = Predicate::trivially_true().get_domain();

        csp_solver
            .variable_names
            .add_integer(dummy_id, "Dummy".to_owned());

        let _ = csp_solver.add_propagator(
            NogoodPropagator::with_options(csp_solver.internal_parameters.learning_options),
            None,
        );

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

        self.solver_statistics
            .engine_statistics
            .time_spent_in_solver += start_time.elapsed().as_millis() as u64;

        result
    }

    pub fn get_state(&self) -> &CSPSolverState {
        &self.state
    }

    pub fn get_random_generator(&mut self) -> &mut impl Random {
        &mut self.internal_parameters.random_generator
    }

    pub fn log_statistics(&self) {
        // We first check whether the statistics will/should be logged to prevent unnecessarily
        // going through all the propagators
        if should_log_statistics() {
            self.solver_statistics.log(StatisticLogger::default());
            for (index, propagator) in self.propagators.iter_propagators().enumerate() {
                propagator.log_statistics(StatisticLogger::new([
                    propagator.name(),
                    "number",
                    index.to_string().as_str(),
                ]));
            }
        }
    }

    pub fn create_new_literal(&mut self, name: Option<String>) -> Literal {
        let domain_id = self.create_new_integer_variable(0, 1, name);
        Literal::new(domain_id)
    }

    pub fn create_new_literal_for_predicate(
        &mut self,
        predicate: Predicate,
        name: Option<String>,
    ) -> Literal {
        let literal = self.create_new_literal(name);

        // If literal --> predicate
        let _ = self.add_clause(vec![!literal.get_true_predicate(), predicate]);

        // If !literal --> !predicate
        let _ = self.add_clause(vec![!literal.get_false_predicate(), !predicate]);

        literal
    }

    pub fn link_literal_to_predicate(&mut self, literal: Literal, predicate: Predicate) {
        // If literal --> predicate
        let _ = self.add_clause(vec![!literal.get_true_predicate(), predicate]);

        // If !literal --> !predicate
        let _ = self.add_clause(vec![!literal.get_false_predicate(), !predicate]);
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
        values: Vec<i32>,
        name: Option<String>,
    ) -> DomainId {
        let domain_id = self.assignments.create_new_integer_variable_sparse(values);

        self.watch_list_cp.grow();

        if let Some(name) = name {
            self.variable_names.add_integer(domain_id, name);
        }

        domain_id
    }

    /// Returns an unsatisfiable core or an [`Err`] if the provided assumptions were conflicting
    /// with one another ([`Err`] then contain the [`Literal`] which was conflicting).
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
    /// # use pumpkin_solver::Solver;
    /// # use pumpkin_solver::termination::Indefinite;
    /// # use pumpkin_solver::results::SatisfactionResultUnderAssumptions;
    /// let mut solver = Solver::default();
    /// let x = vec![
    ///     solver.new_literal().get_true_predicate(),
    ///     solver.new_literal().get_true_predicate(),
    ///     solver.new_literal().get_true_predicate(),
    /// ];
    ///
    /// solver.add_clause([x[0], x[1], x[2]]);
    /// solver.add_clause([x[0], !x[1], x[2]]);
    ///
    /// let assumptions = [!x[0], x[1], !x[2]];
    /// let mut termination = Indefinite;
    /// let mut brancher = solver.default_brancher();
    /// let result = solver.satisfy_under_assumptions(&mut brancher, &mut termination, &assumptions);
    ///
    /// if let SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(mut unsatisfiable) =
    ///     result
    /// {
    ///     {
    ///         let core = unsatisfiable.extract_core();
    ///
    ///         // The order of the literals in the core is undefined, so we check for unordered
    ///         // equality.
    ///         assert_eq!(
    ///             core.len(),
    ///             assumptions.len(),
    ///             "The core has the length of the number of assumptions"
    ///         );
    ///         assert!(
    ///             core.iter().all(|&lit| assumptions.contains(&lit)),
    ///             "All literals in the core are assumptions"
    ///         );
    ///     }
    /// }
    /// ```
    pub fn extract_clausal_core(&mut self, brancher: &mut impl Brancher) -> CoreExtractionResult {
        if self.state.is_infeasible() {
            return CoreExtractionResult::Core(vec![]);
        }

        self.assumptions
            .iter()
            .enumerate()
            .find(|(index, assumption)| {
                self.assumptions
                    .iter()
                    .skip(index + 1)
                    .any(|other_assumptiion| {
                        assumption.is_mutually_exclusive_with(*other_assumptiion)
                    })
            })
            .map(|(_, conflicting_assumption)| {
                CoreExtractionResult::ConflictingAssumption(*conflicting_assumption)
            })
            .unwrap_or_else(|| {
                let mut conflict_analysis_context = ConflictAnalysisContext {
                    assignments: &mut self.assignments,
                    counters: &mut self.solver_statistics,
                    solver_state: &mut self.state,
                    reason_store: &mut self.reason_store,
                    brancher,
                    semantic_minimiser: &mut self.semantic_minimiser,
                    propagators: &mut self.propagators,
                    last_notified_cp_trail_index: &mut self.last_notified_cp_trail_index,
                    watch_list_cp: &mut self.watch_list_cp,
                    propagator_queue: &mut self.propagator_queue,
                    event_drain: &mut self.event_drain,
                    backtrack_event_drain: &mut self.backtrack_event_drain,
                    should_minimise: self.internal_parameters.learning_clause_minimisation,
                    proof_log: &mut self.internal_parameters.proof_log,
                    is_completing_proof: false,
                    unit_nogood_step_ids: &self.unit_nogood_step_ids,
                    stateful_assignments: &mut self.stateful_assignments,
                };

                let mut resolver = ResolutionResolver::with_mode(AnalysisMode::AllDecision);
                let learned_nogood = resolver
                    .resolve_conflict(&mut conflict_analysis_context)
                    .expect("Expected core extraction to be able to extract a core");

                CoreExtractionResult::Core(learned_nogood.predicates.clone())
            })
    }

    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        let literal_is_true = self
            .assignments
            .is_predicate_satisfied(literal.get_true_predicate());
        let opposite_literal_is_true = self
            .assignments
            .is_predicate_satisfied((!literal).get_true_predicate());

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

    pub fn restore_state_at_root(&mut self, brancher: &mut impl Brancher) {
        if self.assignments.get_decision_level() != 0 {
            ConstraintSatisfactionSolver::backtrack(
                &mut self.assignments,
                &mut self.last_notified_cp_trail_index,
                &mut self.reason_store,
                &mut self.propagator_queue,
                &mut self.watch_list_cp,
                &mut self.propagators,
                &mut self.event_drain,
                &mut self.backtrack_event_drain,
                0,
                brancher,
                &mut self.stateful_assignments,
            );
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

            self.propagate();

            if self.state.no_conflict() {
                // Restarts should only occur after a new decision level has been declared to
                // account for the fact that all assumptions should be assigned when restarts take
                // place. Since one assumption is posted per decision level, all assumptions are
                // assigned when the decision level is strictly larger than the number of
                // assumptions.
                if self.get_decision_level() > self.assumptions.len()
                    && self.restart_strategy.should_restart()
                {
                    self.restart_during_search(brancher);
                }

                let branching_result = self.make_next_decision(brancher);

                if let Err(flag) = branching_result {
                    return flag;
                }
            } else {
                if self.get_decision_level() == 0 {
                    if self.assumptions.is_empty() {
                        // Only complete the proof when _not_ solving under assumptions. It is
                        // unclear what a proof would look like with assumptions, as there is extra
                        // state to consider. It also means that the learned clause could be
                        // non-empty, messing with all kinds of asserts.
                        self.complete_proof();
                    }

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
        match self.propagators[Self::get_nogood_propagator_id()].downcast_mut::<NogoodPropagator>()
        {
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
            self.declare_new_decision_level();

            return self
                .assignments
                .post_predicate(assumption_literal, None)
                .map_err(|_| {
                    self.state
                        .declare_infeasible_under_assumptions(assumption_literal);
                    CSPSolverExecutionFlag::Infeasible
                });
        }

        // Otherwise proceed with standard branching.
        let context = &mut SelectionContext::new(
            &self.assignments,
            &mut self.internal_parameters.random_generator,
        );

        // If there is a next decision, make the decision.
        let Some(decision_predicate) = brancher.next_decision(context) else {
            // Otherwise there are no more decisions to be made,
            // all predicates have been applied without a conflict,
            // meaning the problem is feasible.
            self.state.declare_solution_found();
            return Err(CSPSolverExecutionFlag::Feasible);
        };

        self.declare_new_decision_level();

        // Note: This also checks that the decision predicate is not already true. That is a
        // stronger check than the `.expect(...)` used later on when handling the result of
        // `Assignments::post_predicate`.
        pumpkin_assert_moderate!(
            !self.assignments.is_predicate_satisfied(decision_predicate),
            "Decision should not already be assigned; double check the brancher"
        );

        self.solver_statistics.engine_statistics.num_decisions += 1;
        self.assignments
            .post_predicate(decision_predicate, None)
            .expect("Decisions are expected not to fail.");

        Ok(())
    }

    pub(crate) fn declare_new_decision_level(&mut self) {
        self.assignments.increase_decision_level();
        self.stateful_assignments.increase_decision_level();
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

        let current_decision_level = self.get_decision_level();

        let mut conflict_analysis_context = ConflictAnalysisContext {
            assignments: &mut self.assignments,
            counters: &mut self.solver_statistics,
            solver_state: &mut self.state,
            reason_store: &mut self.reason_store,
            brancher,
            semantic_minimiser: &mut self.semantic_minimiser,
            propagators: &mut self.propagators,
            last_notified_cp_trail_index: &mut self.last_notified_cp_trail_index,
            watch_list_cp: &mut self.watch_list_cp,
            propagator_queue: &mut self.propagator_queue,
            event_drain: &mut self.event_drain,
            backtrack_event_drain: &mut self.backtrack_event_drain,
            should_minimise: self.internal_parameters.learning_clause_minimisation,
            proof_log: &mut self.internal_parameters.proof_log,
            is_completing_proof: false,
            unit_nogood_step_ids: &self.unit_nogood_step_ids,
            stateful_assignments: &mut self.stateful_assignments,
        };

        let learned_nogood = self
            .conflict_resolver
            .resolve_conflict(&mut conflict_analysis_context);

        // important to notify about the conflict _before_ backtracking removes literals from
        // the trail -> although in the current version this does nothing but notify that a
        // conflict happened
        if let Some(learned_nogood) = learned_nogood.as_ref() {
            conflict_analysis_context
                .counters
                .learned_clause_statistics
                .average_backtrack_amount
                .add_term((current_decision_level - learned_nogood.backjump_level) as u64);

            self.restart_strategy.notify_conflict(
                self.lbd_helper.compute_lbd(
                    &learned_nogood.predicates,
                    conflict_analysis_context.assignments,
                ),
                conflict_analysis_context
                    .assignments
                    .get_pruned_value_count(),
            );
        }

        let result = self
            .conflict_resolver
            .process(&mut conflict_analysis_context, &learned_nogood);
        if result.is_err() {
            unreachable!("Cannot resolve nogood and reach error")
        }

        if let Some(learned_nogood) = learned_nogood {
            let learned_clause = learned_nogood
                .predicates
                .iter()
                .map(|&predicate| !predicate);
            let step_id = self
                .internal_parameters
                .proof_log
                .log_learned_clause(learned_clause, &self.variable_names)
                .expect("Failed to write proof log");

            if learned_nogood.predicates.len() == 1 {
                let _ = self
                    .unit_nogood_step_ids
                    .insert(!learned_nogood.predicates[0], step_id);
            }

            self.solver_statistics
                .learned_clause_statistics
                .num_unit_clauses_learned += (learned_nogood.predicates.len() == 1) as u64;

            self.solver_statistics
                .learned_clause_statistics
                .average_learned_clause_length
                .add_term(learned_nogood.predicates.len() as u64);

            self.add_learned_nogood(learned_nogood);
        }

        self.state.declare_solving();
    }

    fn add_learned_nogood(&mut self, learned_nogood: LearnedNogood) {
        let mut context = PropagationContextMut::new(
            &mut self.stateful_assignments,
            &mut self.assignments,
            &mut self.reason_store,
            &mut self.semantic_minimiser,
            Self::get_nogood_propagator_id(),
        );

        ConstraintSatisfactionSolver::add_asserting_nogood_to_nogood_propagator(
            &mut self.propagators[Self::get_nogood_propagator_id()],
            learned_nogood.predicates,
            &mut context,
            &mut self.solver_statistics,
        )
    }

    pub(crate) fn add_asserting_nogood_to_nogood_propagator(
        nogood_propagator: &mut dyn Propagator,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
        statistics: &mut SolverStatistics,
    ) {
        match nogood_propagator.downcast_mut::<NogoodPropagator>() {
            Some(nogood_propagator) => {
                nogood_propagator.add_asserting_nogood(nogood, context, statistics)
            }
            None => panic!("Provided propagator should be the nogood propagator"),
        }
    }

    /// Performs a restart during the search process; it is only called when it has been determined
    /// to be necessary by the [`ConstraintSatisfactionSolver::restart_strategy`]. A 'restart'
    /// differs from backtracking to level zero in that a restart backtracks to decision level
    /// zero and then performs additional operations, e.g., clean up learned clauses, adjust
    /// restart frequency, etc.
    ///
    /// This method will also increase the decision level after backtracking.
    ///
    /// Returns true if a restart took place and false otherwise.
    fn restart_during_search(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(
            self.get_decision_level() > self.assumptions.len(),
            "Sanity check: restarts should not trigger whilst assigning assumptions"
        );

        // no point backtracking past the assumption level
        if self.get_decision_level() <= self.assumptions.len() {
            return;
        }

        if brancher.is_restart_pointless() {
            // If the brancher is static then there is no point in restarting as it would make the
            // exact same decision
            return;
        }

        self.solver_statistics.engine_statistics.num_restarts += 1;

        ConstraintSatisfactionSolver::backtrack(
            &mut self.assignments,
            &mut self.last_notified_cp_trail_index,
            &mut self.reason_store,
            &mut self.propagator_queue,
            &mut self.watch_list_cp,
            &mut self.propagators,
            &mut self.event_drain,
            &mut self.backtrack_event_drain,
            0,
            brancher,
            &mut self.stateful_assignments,
        );

        self.restart_strategy.notify_restart();
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "This method requires this many arguments, though a backtracking context could be considered; for now this function needs to be used by conflict analysis"
    )]
    pub(crate) fn backtrack<BrancherType: Brancher + ?Sized>(
        assignments: &mut Assignments,
        last_notified_cp_trail_index: &mut usize,
        reason_store: &mut ReasonStore,
        propagator_queue: &mut PropagatorQueue,
        watch_list_cp: &mut WatchListCP,
        propagators: &mut PropagatorStore,
        event_drain: &mut Vec<(IntDomainEvent, DomainId)>,
        backtrack_event_drain: &mut Vec<(IntDomainEvent, DomainId)>,
        backtrack_level: usize,
        brancher: &mut BrancherType,
        stateful_assignments: &mut TrailedAssignments,
    ) {
        pumpkin_assert_simple!(backtrack_level < assignments.get_decision_level());

        brancher.on_backtrack();

        assignments
            .synchronise(
                backtrack_level,
                *last_notified_cp_trail_index,
                watch_list_cp.is_watching_any_backtrack_events(),
            )
            .iter()
            .for_each(|(domain_id, previous_value)| {
                brancher.on_unassign_integer(*domain_id, *previous_value)
            });

        stateful_assignments.synchronise(backtrack_level);

        *last_notified_cp_trail_index = assignments.num_trail_entries();

        reason_store.synchronise(backtrack_level);
        propagator_queue.clear();
        // For now all propagators are called to synchronise, in the future this will be improved in
        // two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator in propagators.iter_propagators_mut() {
            let context = PropagationContext::new(assignments);
            propagator.synchronise(context);
        }

        brancher.synchronise(assignments);

        let _ = ConstraintSatisfactionSolver::process_backtrack_events(
            watch_list_cp,
            backtrack_event_drain,
            assignments,
            propagators,
        );

        event_drain.clear();
    }

    pub(crate) fn compute_reason_for_empty_domain(
        assignments: &mut Assignments,
        reason_store: &mut ReasonStore,
        propagators: &mut PropagatorStore,
    ) -> PropositionalConjunction {
        // The empty domain happened after posting the last predicate on the trail.
        // The reason for this empty domain is computed as the reason for the bounds before the last
        // trail predicate was posted, plus the reason for the last trail predicate.

        // The last predicate on the trail reveals the domain id that has resulted
        // in an empty domain.
        let entry = assignments.get_last_entry_on_trail();
        assert!(
            entry.reason.is_some(),
            "Cannot cause an empty domain using a decision."
        );
        let conflict_domain = entry.predicate.get_domain();
        assert!(
            entry.old_lower_bound != assignments.get_lower_bound(conflict_domain)
                || entry.old_upper_bound != assignments.get_upper_bound(conflict_domain),
            "One of the two bounds had to change."
        );

        // Look up the reason for the bound that changed.
        // The reason for changing the bound cannot be a decision, so we can safely unwrap.
        let mut empty_domain_reason: Vec<Predicate> = vec![
            predicate!(conflict_domain >= entry.old_lower_bound),
            predicate!(conflict_domain <= entry.old_upper_bound),
        ];

        let _ = reason_store.get_or_compute(
            entry.reason.unwrap(),
            ExplanationContext::from(&*assignments),
            propagators,
            &mut empty_domain_reason,
        );

        empty_domain_reason.into()
    }

    /// Main propagation loop.
    pub(crate) fn propagate(&mut self) {
        // Record the number of predicates on the trail for statistics purposes.
        let num_assigned_variables_old = self.assignments.num_trail_entries();
        // The initial domain events are due to the decision predicate.
        self.notify_propagators_about_domain_events();
        // Keep propagating until there are unprocessed propagators, or a conflict is detected.
        while let Some(propagator_id) = self.propagator_queue.pop() {
            let tag = self.propagators.get_tag(propagator_id);
            let num_trail_entries_before = self.assignments.num_trail_entries();

            let propagation_status = {
                let propagator = &mut self.propagators[propagator_id];
                let context = PropagationContextMut::new(
                    &mut self.stateful_assignments,
                    &mut self.assignments,
                    &mut self.reason_store,
                    &mut self.semantic_minimiser,
                    propagator_id,
                );
                propagator.propagate(context)
            };
            if self.assignments.get_decision_level() == 0
                && self.internal_parameters.proof_log.is_logging_inferences()
            {
                self.log_root_propagation_to_proof(num_trail_entries_before, tag);
            }
            match propagation_status {
                Ok(_) => {
                    // Notify other propagators of the propagations and continue.
                    self.notify_propagators_about_domain_events();
                }
                Err(inconsistency) => match inconsistency {
                    // A propagator did a change that resulted in an empty domain.
                    Inconsistency::EmptyDomain => {
                        let empty_domain_reason =
                            ConstraintSatisfactionSolver::compute_reason_for_empty_domain(
                                &mut self.assignments,
                                &mut self.reason_store,
                                &mut self.propagators,
                            );

                        // TODO: As a temporary solution, we remove the last trail element.
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
                    Inconsistency::Conflict(conflict_nogood) => {
                        pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                            &self.stateful_assignments,
                            &self.assignments,
                            &conflict_nogood,
                            &self.propagators[propagator_id],
                            propagator_id,
                        ));

                        let stored_conflict_info = StoredConflictInfo::Propagator {
                            conflict_nogood,
                            propagator_id,
                        };
                        self.state.declare_conflict(stored_conflict_info);
                        break;
                    }
                },
            }
            pumpkin_assert_extreme!(
                DebugHelper::debug_check_propagations(
                    num_trail_entries_before,
                    propagator_id,
                    &self.stateful_assignments,
                    &self.assignments,
                    &mut self.reason_store,
                    &mut self.propagators
                ),
                "Checking the propagations performed by the propagator led to inconsistencies!"
            );
        }
        // Record statistics.
        self.solver_statistics.engine_statistics.num_conflicts +=
            self.state.is_conflicting() as u64;
        self.solver_statistics.engine_statistics.num_propagations +=
            self.assignments.num_trail_entries() as u64 - num_assigned_variables_old as u64;
        // Only check fixed point propagation if there was no reported conflict,
        // since otherwise the state may be inconsistent.
        pumpkin_assert_extreme!(
            self.state.is_conflicting()
                || DebugHelper::debug_fixed_point_propagation(
                    &self.stateful_assignments,
                    &self.assignments,
                    &self.propagators,
                )
        );
    }

    /// Introduces any root-level propagations to the proof by introducing them as
    /// nogoods.
    ///
    /// The inference `R -> l` is logged to the proof as follows:
    /// 1. Infernce `R /\ ~l -> false`
    /// 2. Nogood (clause) `l`
    fn log_root_propagation_to_proof(
        &mut self,
        start_trail_index: usize,
        tag: Option<NonZero<u32>>,
    ) {
        for trail_idx in start_trail_index..self.assignments.num_trail_entries() {
            let entry = self.assignments.get_trail_entry(trail_idx);
            let reason_ref = entry
                .reason
                .expect("Added by a propagator and must therefore have a reason");

            // Get the conjunction of predicates explaining the propagation.
            let mut reason = vec![];
            let _ = self.reason_store.get_or_compute(
                reason_ref,
                ExplanationContext::new(&self.assignments, CurrentNogood::empty()),
                &mut self.propagators,
                &mut reason,
            );

            let propagated = entry.predicate;

            // The proof inference for the propagation `R -> l` is `R /\ ~l -> false`.
            let inference_premises = reason.iter().copied().chain(std::iter::once(!propagated));
            let _ = self
                .internal_parameters
                .proof_log
                .log_inference(tag, inference_premises, None);

            // Since inference steps are only related to the nogood they directly precede,
            // facts derived at the root are also logged as nogoods so they can be used in the
            // derivation of other nogoods.
            //
            // In case we are logging hints, we must therefore identify what proof steps contribute
            // to the derivation of the current nogood, and therefore are in the premise of the
            // previously logged inference. These proof steps are necessarily unit nogoods, and
            // therefore we recursively look up which unit nogoods are involved in the premise of
            // the inference.

            let mut to_explain: VecDeque<Predicate> = reason.iter().copied().collect();

            while let Some(premise) = to_explain.pop_front() {
                pumpkin_assert_simple!(self.assignments.is_predicate_satisfied(premise));

                let index = self
                    .assignments
                    .get_trail_position(&premise)
                    .expect("Expected premise to be true");
                let trail_entry = self.assignments.get_trail_entry(index);

                if self.assignments.is_initial_bound(trail_entry.predicate) {
                    continue;
                }

                if let Some(step_id) = self.unit_nogood_step_ids.get(&trail_entry.predicate) {
                    self.internal_parameters.proof_log.add_propagation(*step_id);
                } else {
                    unreachable!()
                }
            }

            // Log the nogood which adds the root-level knowledge to the proof.
            let nogood_step_id = self
                .internal_parameters
                .proof_log
                .log_learned_clause([propagated], &self.variable_names);

            if let Ok(nogood_step_id) = nogood_step_id {
                let _ = self.unit_nogood_step_ids.insert(propagated, nogood_step_id);
            }
        }
    }

    fn peek_next_assumption_predicate(&self) -> Option<Predicate> {
        // The convention is that at decision level i, the (i-1)th assumption is posted.
        // Note that decisions start being posted start at 1, hence the minus one.
        let next_assumption_index = self.get_decision_level();
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
    pub(crate) fn add_propagator(
        &mut self,
        propagator_to_add: impl Propagator + 'static,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        if self.state.is_inconsistent() {
            return Err(ConstraintOperationError::InfeasiblePropagator);
        }

        pumpkin_assert_simple!(
            propagator_to_add.priority() <= 3,
            "The propagator priority exceeds 3.
             Currently we only support values up to 3,
             but this can easily be changed if there is a good reason."
        );

        let new_propagator_id = self.propagators.alloc(Box::new(propagator_to_add), tag);

        let new_propagator = &mut self.propagators[new_propagator_id];

        let mut initialisation_context = PropagatorInitialisationContext::new(
            &mut self.watch_list_cp,
            &mut self.stateful_assignments,
            new_propagator_id,
            &mut self.assignments,
        );

        let initialisation_status = new_propagator.initialise_at_root(&mut initialisation_context);

        if let Err(conflict_explanation) = initialisation_status {
            self.state.declare_conflict(StoredConflictInfo::Propagator {
                conflict_nogood: conflict_explanation,
                propagator_id: new_propagator_id,
            });
            self.complete_proof();
            let _ = self.conclude_proof_unsat();
            self.state.declare_infeasible();
            Err(ConstraintOperationError::InfeasiblePropagator)
        } else {
            self.propagator_queue
                .enqueue_propagator(new_propagator_id, new_propagator.priority());

            self.propagate();

            if self.state.no_conflict() {
                Ok(())
            } else {
                self.complete_proof();
                let _ = self.conclude_proof_unsat();
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
            &mut self.stateful_assignments,
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
        nogood_propagator: &mut dyn Propagator,
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

        // We can simply negate the clause and retrieve a nogood, e.g. if we have the
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

        if predicates.len() == 1 {
            let _ = self
                .internal_parameters
                .proof_log
                .log_inference(None, [predicates[0]], None);
            let step_id = self
                .internal_parameters
                .proof_log
                .log_learned_clause([!predicates[0]], &self.variable_names)
                .expect("Expected to be able to write proof");
            let _ = self.unit_nogood_step_ids.insert(!predicates[0], step_id);
        }

        if let Err(constraint_operation_error) = self.add_nogood(predicates) {
            self.state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    constraint_operation_error,
                ));
            return Err(constraint_operation_error);
        }
        Ok(())
    }

    pub(crate) fn get_decision_level(&self) -> usize {
        self.assignments.get_decision_level()
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

    pub(crate) fn get_conflict_info(&self) -> StoredConflictInfo {
        match &self.internal_state {
            CSPSolverStateInternal::Conflict { conflict_info } => conflict_info.clone(),
            CSPSolverStateInternal::InfeasibleUnderAssumptions {
                violated_assumption,
            } => StoredConflictInfo::EmptyDomain {
                conflict_nogood: vec![*violated_assumption, !(*violated_assumption)].into(),
            },
            _ => {
                panic!("Cannot extract conflict clause if solver is not in a conflict.");
            }
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

struct DummyBrancher;
impl Brancher for DummyBrancher {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> Option<Predicate> {
        todo!()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::ConstraintSatisfactionSolver;
    use super::CoreExtractionResult;
    use crate::basic_types::CSPSolverExecutionFlag;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::propagators::linear_not_equal::LinearNotEqualPropagator;
    use crate::termination::Indefinite;
    use crate::variables::TransformableVariable;
    use crate::DefaultBrancher;

    fn is_same_core(core1: &[Predicate], core2: &[Predicate]) -> bool {
        core1.len() == core2.len() && core2.iter().all(|lit| core1.contains(lit))
    }

    fn is_result_the_same(res1: &CoreExtractionResult, res2: &CoreExtractionResult) -> bool {
        match (res1, res2) {
            (
                CoreExtractionResult::ConflictingAssumption(assumption1),
                CoreExtractionResult::ConflictingAssumption(assumption2),
            ) => assumption1 == assumption2,
            (CoreExtractionResult::Core(core1), CoreExtractionResult::Core(core2)) => {
                is_same_core(core1, core2)
            }
            _ => false,
        }
    }

    fn run_test(
        mut solver: ConstraintSatisfactionSolver,
        assumptions: Vec<Predicate>,
        expected_flag: CSPSolverExecutionFlag,
        expected_result: CoreExtractionResult,
    ) {
        let mut brancher = DefaultBrancher::default_over_all_variables(&solver.assignments);
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

    fn create_instance1() -> (ConstraintSatisfactionSolver, Vec<Predicate>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let lit2 = solver.create_new_literal(None).get_true_predicate();

        let _ = solver.add_clause([lit1, lit2]);
        let _ = solver.add_clause([lit1, !lit2]);
        let _ = solver.add_clause([!lit1, lit2]);
        (solver, vec![lit1, lit2])
    }

    #[test]
    fn core_extraction_unit_core() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let _ = solver.add_clause(vec![lit1]);

        run_test(
            solver,
            vec![!lit1],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![!lit1]),
        )
    }

    #[test]
    fn simple_core_extraction_1_1() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[0], !lits[1]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![!lits[0]]),
        )
    }

    #[test]
    fn simple_core_extraction_1_2() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[1], !lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![!lits[1]]),
        );
    }

    #[test]
    fn simple_core_extraction_1_infeasible() {
        let (mut solver, lits) = create_instance1();
        let _ = solver.add_clause([!lits[0], !lits[1]]);
        run_test(
            solver,
            vec![!lits[1], !lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![]),
        );
    }

    #[test]
    fn simple_core_extraction_1_core_conflicting() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[1], lits[1]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::ConflictingAssumption(!lits[1]),
        );
    }
    fn create_instance2() -> (ConstraintSatisfactionSolver, Vec<Predicate>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let lit2 = solver.create_new_literal(None).get_true_predicate();
        let lit3 = solver.create_new_literal(None).get_true_predicate();

        let _ = solver.add_clause([lit1, lit2, lit3]);
        let _ = solver.add_clause([lit1, !lit2, lit3]);
        (solver, vec![lit1, lit2, lit3])
    }

    #[test]
    fn simple_core_extraction_2_1() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], lits[1], !lits[2]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![!lits[0], lits[1], !lits[2]]),
        );
    }

    #[test]
    fn simple_core_extraction_2_long_assumptions_with_inconsistency_at_the_end() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], lits[1], !lits[2], lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::ConflictingAssumption(!lits[0]),
        );
    }

    #[test]
    fn simple_core_extraction_2_inconsistent_long_assumptions() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], !lits[0], !lits[1], !lits[1], lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::ConflictingAssumption(!lits[0]),
        );
    }
    fn create_instance3() -> (ConstraintSatisfactionSolver, Vec<Predicate>) {
        let mut solver = ConstraintSatisfactionSolver::default();

        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let lit2 = solver.create_new_literal(None).get_true_predicate();
        let lit3 = solver.create_new_literal(None).get_true_predicate();

        let _ = solver.add_clause([lit1, lit2, lit3]);
        (solver, vec![lit1, lit2, lit3])
    }

    #[test]
    fn simple_core_extraction_3_1() {
        let (solver, lits) = create_instance3();
        run_test(
            solver,
            vec![!lits[0], !lits[1], !lits[2]],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![!lits[0], !lits[1], !lits[2]]),
        );
    }

    #[test]
    fn simple_core_extraction_3_2() {
        let (solver, lits) = create_instance3();
        run_test(
            solver,
            vec![!lits[0], !lits[1]],
            CSPSolverExecutionFlag::Feasible,
            CoreExtractionResult::Core(vec![]), // will be ignored in the test
        );
    }

    #[test]
    fn core_extraction_equality_assumption() {
        let mut solver = ConstraintSatisfactionSolver::default();

        let x = solver.create_new_integer_variable(0, 10, None);
        let y = solver.create_new_integer_variable(0, 10, None);
        let z = solver.create_new_integer_variable(0, 10, None);

        let result = solver.add_propagator(
            LinearNotEqualPropagator::new([x.scaled(1), y.scaled(-1)].into(), 0),
            None,
        );
        assert!(result.is_ok());
        run_test(
            solver,
            vec![
                predicate!(x >= 5),
                predicate!(z != 10),
                predicate!(y == 5),
                predicate!(x <= 5),
            ],
            CSPSolverExecutionFlag::Infeasible,
            CoreExtractionResult::Core(vec![predicate!(x == 5), predicate!(y == 5)]),
        )
    }

    #[test]
    fn new_domain_with_negative_lower_bound() {
        let lb = -2;
        let ub = 2;

        let mut solver = ConstraintSatisfactionSolver::default();
        let domain_id = solver.create_new_integer_variable(lb, ub, None);

        assert_eq!(lb, solver.assignments.get_lower_bound(domain_id));

        assert_eq!(ub, solver.assignments.get_upper_bound(domain_id));

        assert!(!solver
            .assignments
            .is_predicate_satisfied(predicate![domain_id == lb]));

        for value in (lb + 1)..ub {
            let predicate = predicate![domain_id >= value];

            assert!(!solver.assignments.is_predicate_satisfied(predicate));

            assert!(!solver
                .assignments
                .is_predicate_satisfied(predicate![domain_id == value]));
        }

        assert!(!solver
            .assignments
            .is_predicate_satisfied(predicate![domain_id == ub]));
    }

    #[test]
    fn check_can_compute_1uip_with_propagator_initialisation_conflict() {
        let mut solver = ConstraintSatisfactionSolver::default();

        let x = solver.create_new_integer_variable(1, 1, None);
        let y = solver.create_new_integer_variable(2, 2, None);

        let propagator = LinearNotEqualPropagator::new(Box::new([x, y]), 3);
        let result = solver.add_propagator(propagator, None);
        assert!(result.is_err());
    }
}
