//! Houses the solver which attempts to find a solution to a Constraint Satisfaction Problem (CSP)
//! using a Lazy Clause Generation approach.
use std::cmp::max;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::sync::Arc;

#[allow(
    clippy::disallowed_types,
    reason = "any rand generator is a valid implementation of Random"
)]
use rand::SeedableRng;
use rand::rngs::SmallRng;

use super::ResolutionResolver;
use super::conflict_analysis::AnalysisMode;
use super::conflict_analysis::ConflictAnalysisContext;
use super::conflict_analysis::NoLearningResolver;
use super::conflict_analysis::SemanticMinimiser;
use super::solver_statistics::SolverStatistics;
use super::termination::TerminationCondition;
use super::variables::IntegerVariable;
use super::variables::Literal;
#[cfg(doc)]
use crate::Solver;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::PredicateId;
use crate::basic_types::Random;
use crate::basic_types::SolutionReference;
use crate::basic_types::StoredConflictInfo;
use crate::basic_types::time::Instant;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::containers::HashMap;
use crate::declare_inference_label;
use crate::engine::Assignments;
use crate::engine::RestartOptions;
use crate::engine::RestartStrategy;
use crate::engine::State;
use crate::engine::conflict_analysis::ConflictResolver as Resolver;
use crate::engine::predicates::predicate::Predicate;
use crate::options::LearningOptions;
use crate::proof::ConstraintTag;
use crate::proof::FinalizingContext;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::proof::RootExplanationContext;
use crate::proof::explain_root_assignment;
use crate::proof::finalize_proof;
use crate::propagation::PropagatorConstructor;
use crate::propagation::store::PropagatorHandle;
use crate::propagators::nogoods::NogoodPropagator;
use crate::propagators::nogoods::NogoodPropagatorConstructor;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_ne_moderate;
use crate::pumpkin_assert_simple;
use crate::statistics::StatisticLogger;
use crate::statistics::statistic_logging::should_log_statistics;
use crate::variables::DomainId;

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
    pub(crate) solver_state: CSPSolverState,
    state: State,
    nogood_propagator_handle: PropagatorHandle<NogoodPropagator>,

    /// Tracks information about the restarts. Occassionally the solver will undo all its decisions
    /// and start the search from the root note. Note that learned clauses and other state
    /// information is kept after a restart.
    restart_strategy: RestartStrategy,
    /// Holds the assumptions when the solver is queried to solve under assumptions.
    assumptions: Vec<Predicate>,
    semantic_minimiser: SemanticMinimiser,
    /// A set of counters updated during the search.
    solver_statistics: SolverStatistics,
    /// Miscellaneous constant parameters used by the solver.
    internal_parameters: SatisfactionSolverOptions,
    /// A map from predicates that are propagated at the root to inference codes in the proof.
    unit_nogood_inference_codes: HashMap<Predicate, InferenceCode>,
    /// The resolver which is used upon a conflict.
    conflict_resolver: Box<dyn Resolver>,
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
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum ConflictResolver {
    NoLearning,
    #[default]
    UIP,
    ExtendedUIP,
    HalfExtendedUIP,
    BoundsExtendedUIP,
}

#[allow(clippy::from_over_into, reason = "Easier with imports")]
impl Into<AnalysisMode> for ConflictResolver {
    fn into(self) -> AnalysisMode {
        match self {
            ConflictResolver::NoLearning => {
                // Does not matter since it will not be used anyways
                AnalysisMode::OneUIP
            }
            ConflictResolver::UIP => AnalysisMode::OneUIP,
            ConflictResolver::ExtendedUIP => AnalysisMode::ExtendedUIP,
            ConflictResolver::HalfExtendedUIP => AnalysisMode::HalfExtendedUIP,
            ConflictResolver::BoundsExtendedUIP => AnalysisMode::BoundsExtendedUIP,
        }
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
    /// The resolver used for conflict analysis
    pub conflict_resolver: ConflictResolver,
    /// The options which influence the learning of the solver.
    pub learning_options: LearningOptions,
    /// The number of MBs which are preallocated by the nogood propagator.
    pub memory_preallocated: usize,
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
            memory_preallocated: 1000,
        }
    }
}

impl ConstraintSatisfactionSolver {
    pub(crate) fn assignments(&self) -> &Assignments {
        &self.state.assignments
    }

    /// This is a temporary accessor to help refactoring.
    pub fn get_solution_reference(&self) -> SolutionReference<'_> {
        self.state.get_solution_reference()
    }

    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) -> std::io::Result<()> {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        proof.unsat(self.state.variable_names())
    }

    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Predicate) -> std::io::Result<()> {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        proof.optimal(bound, self.state.variable_names())
    }

    fn complete_proof(&mut self) {
        struct DummyBrancher;

        impl Brancher for DummyBrancher {
            fn next_decision(&mut self, _: &mut SelectionContext) -> Option<Predicate> {
                unreachable!()
            }

            fn subscribe_to_events(&self) -> Vec<crate::branching::BrancherEvent> {
                unreachable!()
            }
        }

        let mut conflict_analysis_context = ConflictAnalysisContext {
            counters: &mut self.solver_statistics,
            solver_state: &mut self.solver_state,
            brancher: &mut DummyBrancher,
            semantic_minimiser: &mut self.semantic_minimiser,
            should_minimise: self.internal_parameters.learning_clause_minimisation,
            proof_log: &mut self.internal_parameters.proof_log,
            unit_nogood_inference_codes: &mut self.unit_nogood_inference_codes,
            rng: &mut self.internal_parameters.random_generator,
            restart_strategy: &mut self.restart_strategy,

            state: &mut self.state,
        };

        let conflict = conflict_analysis_context.get_conflict_nogood();

        let context = FinalizingContext {
            conflict: conflict.into(),
            proof_log: &mut self.internal_parameters.proof_log,
            unit_nogood_inference_codes: &self.unit_nogood_inference_codes,
            state: &mut self.state,
        };

        finalize_proof(context);
    }

    pub(crate) fn is_logging_proof(&self) -> bool {
        self.internal_parameters.proof_log.is_logging_proof()
    }
}

// methods that offer basic functionality
impl ConstraintSatisfactionSolver {
    pub fn new(solver_options: SatisfactionSolverOptions) -> Self {
        let mut state = State::default();
        let handle = state.add_propagator(NogoodPropagatorConstructor::new(
            (solver_options.memory_preallocated * 1_000_000) / size_of::<PredicateId>(),
            solver_options.learning_options,
            solver_options.conflict_resolver.into(),
        ));

        ConstraintSatisfactionSolver {
            solver_state: CSPSolverState::default(),
            assumptions: Vec::default(),
            restart_strategy: RestartStrategy::new(solver_options.restart_options),
            nogood_propagator_handle: handle,
            solver_statistics: SolverStatistics::default(),
            semantic_minimiser: SemanticMinimiser::default(),
            unit_nogood_inference_codes: Default::default(),
            conflict_resolver: match solver_options.conflict_resolver {
                ConflictResolver::NoLearning => Box::new(NoLearningResolver),
                ConflictResolver::UIP => {
                    Box::new(ResolutionResolver::new(handle, AnalysisMode::OneUIP))
                }
                ConflictResolver::ExtendedUIP => {
                    Box::new(ResolutionResolver::new(handle, AnalysisMode::ExtendedUIP))
                }
                ConflictResolver::HalfExtendedUIP => Box::new(ResolutionResolver::new(
                    handle,
                    AnalysisMode::HalfExtendedUIP,
                )),
                ConflictResolver::BoundsExtendedUIP => Box::new(ResolutionResolver::new(
                    handle,
                    AnalysisMode::BoundsExtendedUIP,
                )),
            },
            internal_parameters: solver_options,
            state,
        }
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
        if self.solver_state.is_inconsistent() {
            return CSPSolverExecutionFlag::Infeasible;
        }

        let start_time = Instant::now();

        self.initialise(assumptions);
        let result = self.solve_internal(termination, brancher);

        self.solver_statistics
            .engine_statistics
            .time_spent_in_solver += start_time.elapsed();

        result
    }

    pub fn get_state(&self) -> &CSPSolverState {
        &self.solver_state
    }

    pub fn get_random_generator(&mut self) -> &mut impl Random {
        &mut self.internal_parameters.random_generator
    }

    pub fn log_statistics(&self, verbose: bool) {
        // We first check whether the statistics will/should be logged to prevent unnecessarily
        // going through all the propagators
        if !should_log_statistics() {
            return;
        }

        self.solver_statistics
            .log(StatisticLogger::default(), verbose);
        self.state.log_statistics(verbose);
    }

    /// Create a new [`ConstraintTag`].
    pub fn new_constraint_tag(&mut self) -> ConstraintTag {
        self.state.new_constraint_tag()
    }

    pub fn create_new_literal(&mut self, name: Option<Arc<str>>) -> Literal {
        self.state.new_literal(name)
    }

    pub fn create_new_literal_for_predicate(
        &mut self,
        predicate: Predicate,
        name: Option<Arc<str>>,
        constraint_tag: ConstraintTag,
    ) -> Literal {
        let literal = self.state.new_literal(name);

        self.internal_parameters
            .proof_log
            .reify_predicate(literal, predicate);

        // If literal --> predicate
        let _ = self.add_clause(
            vec![!literal.get_true_predicate(), predicate],
            constraint_tag,
        );

        // If !literal --> !predicate
        let _ = self.add_clause(
            vec![!literal.get_false_predicate(), !predicate],
            constraint_tag,
        );

        literal
    }

    /// Create a new integer variable. Its domain will have the given lower and upper bounds.
    pub fn create_new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: Option<Arc<str>>,
    ) -> DomainId {
        assert!(
            !self.solver_state.is_inconsistent(),
            "Variables cannot be created in an inconsistent state"
        );

        self.state
            .new_interval_variable(lower_bound, upper_bound, name)
    }

    /// Creates an integer variable with a domain containing only the values in `values`
    pub fn create_new_integer_variable_sparse(
        &mut self,
        values: Vec<i32>,
        name: Option<String>,
    ) -> DomainId {
        self.state.new_sparse_variable(values, name)
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
    /// # use pumpkin_core::Solver;
    /// # use pumpkin_core::termination::Indefinite;
    /// # use pumpkin_core::results::SatisfactionResultUnderAssumptions;
    /// let mut solver = Solver::default();
    ///
    /// // We use a dummy constraint tag for this example.
    /// let constraint_tag = solver.new_constraint_tag();
    ///
    /// let x = vec![
    ///     solver.new_literal().get_true_predicate(),
    ///     solver.new_literal().get_true_predicate(),
    ///     solver.new_literal().get_true_predicate(),
    /// ];
    ///
    /// solver.add_clause([x[0], x[1], x[2]], constraint_tag);
    /// solver.add_clause([x[0], !x[1], x[2]], constraint_tag);
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
        if self.solver_state.is_infeasible() {
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
                    counters: &mut self.solver_statistics,
                    solver_state: &mut self.solver_state,
                    brancher,
                    semantic_minimiser: &mut self.semantic_minimiser,
                    should_minimise: self.internal_parameters.learning_clause_minimisation,
                    proof_log: &mut self.internal_parameters.proof_log,
                    unit_nogood_inference_codes: &mut self.unit_nogood_inference_codes,
                    rng: &mut self.internal_parameters.random_generator,
                    restart_strategy: &mut self.restart_strategy,
                    state: &mut self.state,
                };

                let mut resolver = ResolutionResolver::new(
                    self.nogood_propagator_handle,
                    AnalysisMode::AllDecision,
                );

                let learned_nogood = resolver.learn_nogood(&mut conflict_analysis_context);

                CoreExtractionResult::Core(learned_nogood.predicates.clone())
            })
    }

    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        self.state.get_literal_value(literal)
    }

    /// Get the lower bound for the given variable.
    pub fn get_lower_bound(&self, variable: &impl IntegerVariable) -> i32 {
        self.state.lower_bound(variable.clone())
    }

    /// Get the upper bound for the given variable.
    pub fn get_upper_bound(&self, variable: &impl IntegerVariable) -> i32 {
        self.state.upper_bound(variable.clone())
    }

    /// Determine whether `value` is in the domain of `variable`.
    pub fn integer_variable_contains(&self, variable: &impl IntegerVariable, value: i32) -> bool {
        self.state.contains(variable.clone(), value)
    }

    /// Get the assigned integer for the given variable. If it is not assigned, `None` is returned.
    pub fn get_assigned_integer_value(&self, variable: &impl IntegerVariable) -> Option<i32> {
        self.state.fixed_value(variable.clone())
    }

    pub fn restore_state_at_root(&mut self, brancher: &mut impl Brancher) {
        if self.state.get_checkpoint() != 0 {
            ConstraintSatisfactionSolver::backtrack(
                &mut self.state,
                0,
                brancher,
                &mut self.internal_parameters.random_generator,
            );
            self.solver_state.declare_ready();
        } else if self.solver_state.internal_state == CSPSolverStateInternal::ContainsSolution {
            self.solver_state.declare_ready();
        }
    }
}

// methods that serve as the main building blocks
impl ConstraintSatisfactionSolver {
    fn initialise(&mut self, assumptions: &[Predicate]) {
        pumpkin_assert_simple!(
            !self.solver_state.is_infeasible_under_assumptions(),
            "Solver is not expected to be in the infeasible under assumptions state when initialising.
             Missed extracting the core?"
        );
        self.solver_state.declare_solving();
        assumptions.clone_into(&mut self.assumptions);
    }

    fn solve_internal(
        &mut self,
        termination: &mut impl TerminationCondition,
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        loop {
            if termination.should_stop() {
                self.solver_state.declare_timeout();
                return CSPSolverExecutionFlag::Timeout;
            }

            self.propagate();

            if self.solver_state.no_conflict() {
                // Restarts should only occur after a new decision level has been declared to
                // account for the fact that all assumptions should be assigned when restarts take
                // place. Since one assumption is posted per decision level, all assumptions are
                // assigned when the decision level is strictly larger than the number of
                // assumptions.
                if self.get_checkpoint() > self.assumptions.len()
                    && self.restart_strategy.should_restart()
                {
                    self.restart_during_search(brancher);
                }

                let branching_result = self.make_next_decision(brancher);

                self.solver_statistics.engine_statistics.peak_depth = max(
                    self.solver_statistics.engine_statistics.peak_depth,
                    self.state.get_checkpoint() as u64,
                );

                match branching_result {
                    Err(CSPSolverExecutionFlag::Infeasible) => {
                        // Can happen when the branching decision was an assumption
                        // that is inconsistent with the current assignment. We do not
                        // have to declare a new state, as it will be done inside the
                        // `make_next_decision` function.
                        pumpkin_assert_simple!(self.solver_state.is_infeasible_under_assumptions());

                        self.complete_proof();
                        return CSPSolverExecutionFlag::Infeasible;
                    }

                    Err(flag) => return flag,
                    Ok(()) => {}
                }

                if let Err(flag) = branching_result {
                    return flag;
                }
            } else {
                if self.get_checkpoint() == 0 {
                    self.complete_proof();
                    self.solver_state.declare_infeasible();

                    return CSPSolverExecutionFlag::Infeasible;
                }

                self.resolve_conflict_with_nogood(brancher);

                brancher.on_conflict();
                self.decay_nogood_activities();
            }
        }
    }

    fn decay_nogood_activities(&mut self) {
        match self.state.get_propagator_mut(self.nogood_propagator_handle) {
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
            self.new_checkpoint();

            let _ = self.state.post(assumption_literal).map_err(|_| {
                self.solver_state
                    .declare_infeasible_under_assumptions(assumption_literal);
                CSPSolverExecutionFlag::Infeasible
            })?;

            return Ok(());
        }

        // Otherwise proceed with standard branching.
        let context = &mut SelectionContext::new(
            &self.state.assignments,
            &mut self.internal_parameters.random_generator,
        );

        // If there is a next decision, make the decision.
        let Some(decision_predicate) = brancher.next_decision(context) else {
            // Otherwise there are no more decisions to be made,
            // all predicates have been applied without a conflict,
            // meaning the problem is feasible.
            self.solver_state.declare_solution_found();
            return Err(CSPSolverExecutionFlag::Feasible);
        };

        self.new_checkpoint();

        // Note: This also checks that the decision predicate is not already true. That is a
        // stronger check than the `.expect(...)` used later on when handling the result of
        // `Assignments::post_predicate`.
        pumpkin_assert_ne_moderate!(
            self.state.truth_value(decision_predicate),
            Some(true),
            "Decision should not already be assigned; double check the brancher"
        );

        self.solver_statistics.engine_statistics.num_decisions += 1;
        let update_occurred = self
            .state
            .post(decision_predicate)
            .expect("Decisions are expected not to fail.");
        pumpkin_assert_simple!(update_occurred);

        Ok(())
    }

    pub(crate) fn new_checkpoint(&mut self) {
        self.state.new_checkpoint();
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
        pumpkin_assert_moderate!(self.solver_state.is_conflicting());

        let mut conflict_analysis_context = ConflictAnalysisContext {
            counters: &mut self.solver_statistics,
            solver_state: &mut self.solver_state,
            brancher,
            semantic_minimiser: &mut self.semantic_minimiser,
            should_minimise: self.internal_parameters.learning_clause_minimisation,
            proof_log: &mut self.internal_parameters.proof_log,
            unit_nogood_inference_codes: &mut self.unit_nogood_inference_codes,
            rng: &mut self.internal_parameters.random_generator,
            restart_strategy: &mut self.restart_strategy,
            state: &mut self.state,
        };

        self.conflict_resolver
            .resolve_conflict(&mut conflict_analysis_context);

        self.solver_state.declare_solving();
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
            self.get_checkpoint() > self.assumptions.len(),
            "Sanity check: restarts should not trigger whilst assigning assumptions"
        );

        // no point backtracking past the assumption level
        if self.get_checkpoint() <= self.assumptions.len() {
            return;
        }

        if brancher.is_restart_pointless() {
            // If the brancher is static then there is no point in restarting as it would make the
            // exact same decision
            return;
        }

        self.solver_statistics.engine_statistics.num_restarts += 1;

        ConstraintSatisfactionSolver::backtrack(
            &mut self.state,
            0,
            brancher,
            &mut self.internal_parameters.random_generator,
        );

        self.restart_strategy.notify_restart();
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "This method requires this many arguments, though a backtracking context could be considered; for now this function needs to be used by conflict analysis"
    )]
    pub(crate) fn backtrack<BrancherType: Brancher + ?Sized>(
        state: &mut State,
        backtrack_level: usize,
        brancher: &mut BrancherType,
        rng: &mut dyn Random,
    ) {
        pumpkin_assert_simple!(backtrack_level < state.get_checkpoint());

        brancher.on_backtrack();

        state
            .restore_to(backtrack_level)
            .into_iter()
            .for_each(|(domain_id, previous_value)| {
                brancher.on_unassign_integer(domain_id, previous_value)
            });

        brancher.synchronise(&mut SelectionContext::new(&state.assignments, rng));
    }

    /// Main propagation loop.
    pub(crate) fn propagate(&mut self) {
        let num_trail_entries_prev = self.state.trail_len();

        let result = self.state.propagate_to_fixed_point();

        if self.state.get_checkpoint() == 0 {
            self.handle_root_propagation(num_trail_entries_prev);
        }

        if let Err(conflict) = result {
            self.solver_state.declare_conflict(conflict.into());
        }
    }

    /// Introduces any root-level propagations to the proof by introducing them as
    /// nogoods.
    ///
    /// The inference `R -> l` is logged to the proof as follows:
    /// 1. Infernce `R /\ ~l -> false`
    /// 2. Nogood (clause) `l`
    fn handle_root_propagation(&mut self, start_trail_index: usize) {
        pumpkin_assert_eq_simple!(self.get_checkpoint(), 0);

        for trail_idx in start_trail_index..self.state.trail_len() {
            let entry = self.state.trail_entry(trail_idx);
            let (_, inference_code) = entry
                .reason
                .expect("Added by a propagator and must therefore have a reason");

            if !self.internal_parameters.proof_log.is_logging_inferences() {
                // In case we are not logging inferences, we only need to keep track
                // of the root-level inferences to allow us to correctly finalize the
                // proof.
                let _ = self
                    .unit_nogood_inference_codes
                    .insert(entry.predicate, inference_code);
                continue;
            }

            // Get the conjunction of predicates explaining the propagation.
            let mut reason = vec![];
            self.state
                .get_propagation_reason_trail_entry(trail_idx, &mut reason);

            let propagated = entry.predicate;

            // The proof inference for the propagation `R -> l` is `R /\ ~l -> false`.
            let inference_premises = reason.iter().copied().chain(std::iter::once(!propagated));
            let _ = self.internal_parameters.proof_log.log_inference(
                &self.state.inference_codes,
                &mut self.state.constraint_tags,
                inference_code,
                inference_premises,
                None,
                &self.state.variable_names,
            );

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
                pumpkin_assert_simple!(
                    self.state
                        .truth_value(premise)
                        .expect("Expected predicate to hold")
                );

                let mut context = RootExplanationContext {
                    proof_log: &mut self.internal_parameters.proof_log,
                    unit_nogood_inference_codes: &self.unit_nogood_inference_codes,
                    state: &mut self.state,
                };

                explain_root_assignment(&mut context, premise);
            }

            // Log the nogood which adds the root-level knowledge to the proof.
            let constraint_tag = self.internal_parameters.proof_log.log_deduction(
                [!propagated],
                &self.state.variable_names,
                &mut self.state.constraint_tags,
            );

            if let Ok(constraint_tag) = constraint_tag {
                let inference_code = self
                    .state
                    .create_inference_code(constraint_tag, NogoodLabel);

                let _ = self
                    .unit_nogood_inference_codes
                    .insert(propagated, inference_code);
            }
        }
    }

    fn peek_next_assumption_predicate(&self) -> Option<Predicate> {
        // The convention is that at decision level i, the (i-1)th assumption is posted.
        // Note that decisions start being posted start at 1, hence the minus one.
        let next_assumption_index = self.get_checkpoint();
        self.assumptions.get(next_assumption_index).copied()
    }
}

/// Methods for adding constraints (propagators and clauses)
impl ConstraintSatisfactionSolver {
    /// See [`crate::Solver::add_propagator`] for documentation.
    pub(crate) fn add_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> Result<PropagatorHandle<Constructor::PropagatorImpl>, ConstraintOperationError>
    where
        Constructor: PropagatorConstructor,
        Constructor::PropagatorImpl: 'static,
    {
        if self.solver_state.is_inconsistent() {
            return Err(ConstraintOperationError::InfeasiblePropagator);
        }

        let handle = self.state.add_propagator(constructor);
        let result = self.state.propagate_to_fixed_point();

        if let Err(conflict) = result {
            self.solver_state.declare_conflict(conflict.into());
        }

        if self.solver_state.no_conflict() {
            Ok(handle)
        } else {
            self.complete_proof();
            let _ = self.conclude_proof_unsat();
            Err(ConstraintOperationError::InfeasiblePropagator)
        }
    }

    pub fn post_predicate(&mut self, predicate: Predicate) -> Result<(), ConstraintOperationError> {
        assert!(
            self.get_checkpoint() == 0,
            "Can only post predicates at the root level."
        );

        if self.solver_state.is_infeasible() {
            Err(ConstraintOperationError::InfeasibleState)
        } else {
            match self.state.post(predicate) {
                Ok(_) => Ok(()),
                Err(_) => Err(ConstraintOperationError::InfeasibleNogood),
            }
        }
    }

    fn add_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        inference_code: InferenceCode,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_eq_simple!(self.get_checkpoint(), 0);
        let num_trail_entries = self.state.trail_len();

        let (nogood_propagator, mut context) = self
            .state
            .get_propagator_mut_with_context(self.nogood_propagator_handle);

        let nogood_propagator =
            nogood_propagator.expect("Nogood propagator handle should refer to nogood propagator");

        let addition_status = nogood_propagator.add_nogood(nogood, inference_code, &mut context);

        if addition_status.is_err() || self.solver_state.is_conflicting() {
            if let Err(conflict) = addition_status {
                self.solver_state.declare_conflict(conflict.into());
            }

            self.handle_root_propagation(num_trail_entries);
            self.complete_proof();
            return Err(ConstraintOperationError::InfeasibleNogood);
        }

        self.handle_root_propagation(num_trail_entries);

        #[allow(deprecated, reason = "Will be refactored")]
        self.state.enqueue_propagator(self.nogood_propagator_handle);
        let result = self.state.propagate_to_fixed_point();
        if let Err(conflict) = result {
            self.solver_state.declare_conflict(conflict.into());
        }

        self.handle_root_propagation(num_trail_entries);

        if self.solver_state.is_infeasible() {
            self.complete_proof();
            Err(ConstraintOperationError::InfeasibleState)
        } else {
            Ok(())
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
        constraint_tag: ConstraintTag,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_simple!(
            self.get_checkpoint() == 0,
            "Clauses can only be added in the root"
        );

        if self.solver_state.is_inconsistent() {
            return Err(ConstraintOperationError::InfeasiblePropagator);
        }

        // We can simply negate the clause and retrieve a nogood, e.g. if we have the
        // clause `[x1 >= 5] \/ [x2 != 3] \/ [x3 <= 5]`, then it **cannot** be the case that `[x1 <
        // 5] /\ [x2 = 3] /\ [x3 > 5]`

        let mut are_all_falsified_at_root = true;
        let predicates = predicates
            .into_iter()
            .map(|predicate| {
                are_all_falsified_at_root &= self.state.truth_value(predicate) == Some(false);
                !predicate
            })
            .collect::<Vec<_>>();

        if predicates.is_empty() {
            // This breaks the proof. If it occurs, we should fix up the proof logging.
            // The main issue is that nogoods are not tagged. In the proof that is problematic.
            self.solver_state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    ConstraintOperationError::InfeasibleClause,
                ));
            return Err(ConstraintOperationError::InfeasibleClause);
        }

        if are_all_falsified_at_root {
            finalize_proof(FinalizingContext {
                conflict: predicates.into(),
                proof_log: &mut self.internal_parameters.proof_log,
                unit_nogood_inference_codes: &self.unit_nogood_inference_codes,
                state: &mut self.state,
            });
            self.solver_state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    ConstraintOperationError::InfeasibleClause,
                ));
            return Err(ConstraintOperationError::InfeasibleClause);
        }

        let inference_code = self
            .state
            .create_inference_code(constraint_tag, NogoodLabel);
        if let Err(constraint_operation_error) = self.add_nogood(predicates, inference_code) {
            let _ = self.conclude_proof_unsat();

            self.solver_state
                .declare_conflict(StoredConflictInfo::RootLevelConflict(
                    constraint_operation_error,
                ));
            return Err(constraint_operation_error);
        }
        Ok(())
    }

    pub(crate) fn get_checkpoint(&self) -> usize {
        self.state.get_checkpoint()
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
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
            } => StoredConflictInfo::InconsistentAssumptions(*violated_assumption),
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

    pub fn declare_infeasible(&mut self) {
        self.internal_state = CSPSolverStateInternal::Infeasible;
    }

    pub(crate) fn declare_conflict(&mut self, conflict_info: StoredConflictInfo) {
        self.internal_state = CSPSolverStateInternal::Conflict { conflict_info };
    }

    pub fn declare_solution_found(&mut self) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::ContainsSolution;
    }

    pub fn declare_timeout(&mut self) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::Timeout;
    }

    pub fn declare_infeasible_under_assumptions(&mut self, violated_assumption: Predicate) {
        pumpkin_assert_simple!(!self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::InfeasibleUnderAssumptions {
            violated_assumption,
        }
    }
}

declare_inference_label!(pub(crate) NogoodLabel, "nogood");

#[cfg(test)]
mod tests {
    use super::ConstraintSatisfactionSolver;
    use super::CoreExtractionResult;
    use crate::DefaultBrancher;
    use crate::basic_types::CSPSolverExecutionFlag;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::propagators::linear_not_equal::LinearNotEqualPropagatorArgs;
    use crate::termination::Indefinite;
    use crate::variables::TransformableVariable;

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
        let mut brancher = DefaultBrancher::default_over_all_variables(&solver.state.assignments);
        let flag = solver.solve_under_assumptions(&assumptions, &mut Indefinite, &mut brancher);
        assert_eq!(flag, expected_flag, "The flags do not match.");

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
        let constraint_tag = solver.new_constraint_tag();
        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let lit2 = solver.create_new_literal(None).get_true_predicate();

        let _ = solver.add_clause([lit1, lit2], constraint_tag);
        let _ = solver.add_clause([lit1, !lit2], constraint_tag);
        let _ = solver.add_clause([!lit1, lit2], constraint_tag);
        (solver, vec![lit1, lit2])
    }

    #[test]
    fn core_extraction_unit_core() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let constraint_tag = solver.new_constraint_tag();
        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let _ = solver.add_clause(vec![lit1], constraint_tag);

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
        let constraint_tag = solver.new_constraint_tag();
        let _ = solver.add_clause([!lits[0], !lits[1]], constraint_tag);
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
        let constraint_tag = solver.new_constraint_tag();
        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let lit2 = solver.create_new_literal(None).get_true_predicate();
        let lit3 = solver.create_new_literal(None).get_true_predicate();

        let _ = solver.add_clause([lit1, lit2, lit3], constraint_tag);
        let _ = solver.add_clause([lit1, !lit2, lit3], constraint_tag);
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
        let constraint_tag = solver.new_constraint_tag();

        let lit1 = solver.create_new_literal(None).get_true_predicate();
        let lit2 = solver.create_new_literal(None).get_true_predicate();
        let lit3 = solver.create_new_literal(None).get_true_predicate();

        let _ = solver.add_clause([lit1, lit2, lit3], constraint_tag);
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

        let constraint_tag = solver.new_constraint_tag();

        let result = solver.add_propagator(LinearNotEqualPropagatorArgs {
            terms: [x.scaled(1), y.scaled(-1)].into(),
            rhs: 0,
            constraint_tag,
        });
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

        assert_eq!(lb, solver.state.assignments.get_lower_bound(domain_id));

        assert_eq!(ub, solver.state.assignments.get_upper_bound(domain_id));

        assert!(
            !solver
                .state
                .assignments
                .is_predicate_satisfied(predicate![domain_id == lb])
        );

        for value in (lb + 1)..ub {
            let predicate = predicate![domain_id >= value];

            assert!(!solver.state.assignments.is_predicate_satisfied(predicate));

            assert!(
                !solver
                    .state
                    .assignments
                    .is_predicate_satisfied(predicate![domain_id == value])
            );
        }

        assert!(
            !solver
                .state
                .assignments
                .is_predicate_satisfied(predicate![domain_id == ub])
        );
    }

    #[test]
    fn check_can_compute_1uip_with_propagator_initialisation_conflict() {
        let mut solver = ConstraintSatisfactionSolver::default();

        let x = solver.create_new_integer_variable(1, 1, None);
        let y = solver.create_new_integer_variable(2, 2, None);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = LinearNotEqualPropagatorArgs {
            terms: vec![x, y].into(),
            rhs: 3,
            constraint_tag,
        };
        let result = solver.add_propagator(propagator);
        assert!(result.is_err());
    }
}
