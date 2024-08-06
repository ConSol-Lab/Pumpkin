//! Houses the solver which attempts to find a solution to a Constraint Satisfaction Problem (CSP)
//! using a Lazy Clause Generation approach.

use std::cmp::min;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::time::Instant;

use log::warn;
use rand::rngs::SmallRng;
use rand::SeedableRng;

use super::clause_allocators::ClauseAllocatorInterface;
use super::clause_allocators::ClauseInterface;
use super::conflict_analysis::AnalysisStep;
use super::conflict_analysis::ConflictAnalysisResult;
use super::conflict_analysis::ResolutionConflictAnalyser;
use super::core::Core;
use super::termination::TerminationCondition;
use super::variables::IntegerVariable;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::statistic_logging::statistic_logger::log_statistic;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ClauseReference;
use crate::basic_types::ConflictInfo;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::ConstraintReference;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusOneStepCP;
use crate::basic_types::Random;
use crate::basic_types::SolutionReference;
use crate::basic_types::StoredConflictInfo;
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::branching::Brancher;
use crate::branching::PhaseSaving;
use crate::branching::SelectionContext;
use crate::branching::SolutionGuidedValueSelector;
use crate::branching::Vsids;
use crate::engine::clause_allocators::ClauseAllocatorBasic;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::cp::PropagatorQueue;
use crate::engine::cp::WatchListCP;
use crate::engine::cp::WatchListPropositional;
use crate::engine::debug_helper::DebugDyn;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::proof::ProofLog;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::PropagatorId;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::BooleanDomainEvent;
use crate::engine::DebugHelper;
use crate::engine::EmptyDomain;
use crate::engine::ExplanationClauseManager;
use crate::engine::IntDomainEvent;
use crate::engine::LearnedClauseManager;
use crate::engine::LearningOptions;
use crate::engine::RestartOptions;
use crate::engine::RestartStrategy;
use crate::engine::VariableLiteralMappings;
use crate::propagators::clausal::BasicClausalPropagator;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variable_names::VariableNames;
use crate::DefaultBrancher;
#[cfg(doc)]
use crate::Solver;

pub(crate) type ClausalPropagatorType = BasicClausalPropagator;
pub(crate) type ClauseAllocator = ClauseAllocatorBasic;

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
/// influence the behaviour of the solver; see for example the [`SatisfactionSolverOptions`] and the
/// [`LearningOptions`].
///
/// The solver switches between making decisions using implementations of the [`Brancher`] (which
/// are passed to the [`ConstraintSatisfactionSolver::solve`] method) and propagation (use
/// [`ConstraintSatisfactionSolver::add_propagator`] to add a propagator). If a conflict is found by
/// any of the propagators (including the clausal one) then the solver will analyse the conflict
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
pub struct ConstraintSatisfactionSolver {
    /// The solver continuously changes states during the search.
    /// The state helps track additional information and contributes to making the code clearer.
    pub(crate) state: CSPSolverState,
    /// Tracks information related to the assignments of propositional variables.
    pub(crate) assignments_propositional: AssignmentsPropositional,
    /// Responsible for clausal propagation based on the two-watched scheme.
    /// Although technically just another propagator, we treat the clausal propagator in a special
    /// way due to efficiency and conflict analysis.
    clausal_propagator: ClausalPropagatorType,
    /// The list of propagators. Propagators live here and are queried when events (domain changes)
    /// happen. The list is only traversed during synchronisation for now.
    cp_propagators: Vec<Box<dyn Propagator>>,
    /// Tracks information about all allocated clauses. All clause allocaton goes exclusively
    /// through the clause allocator. There are two notable exceptions:
    /// - Unit clauses are stored directly on the trail.
    /// - Binary clauses may be inlined in the watch lists of the clausal propagator.
    pub(crate) clause_allocator: ClauseAllocator,
    /// Tracks information about all learned clauses, with the exception of
    /// unit clauses which are directly stored on the trail.
    learned_clause_manager: LearnedClauseManager,
    /// Tracks information about the restarts. Occassionally the solver will undo all its decisions
    /// and start the search from the root note. Note that learned clauses and other state
    /// information is kept after a restart.
    restart_strategy: RestartStrategy,
    /// Holds the assumptions when the solver is queried to solve under assumptions.
    assumptions: Vec<Literal>,
    /// Performs conflict analysis, core extraction, and minimisation.
    conflict_analyser: ResolutionConflictAnalyser,
    /// Tracks information related to the assignments of integer variables.
    pub(crate) assignments_integer: AssignmentsInteger,
    /// Contains information on which propagator to notify upon
    /// integer events, e.g., lower or upper bound change of a variable.
    watch_list_cp: WatchListCP,
    /// Contains information on which propagator to notify upon
    /// literal assignment. Not to be confused with the watch list
    /// of the clausal propagator.
    watch_list_propositional: WatchListPropositional,
    /// Used in combination with the propositional watch list
    /// Indicates the next literal on the propositional trail that need to be inspected to notify
    /// subscribed propagators.
    propositional_trail_index: usize,
    /// Dictates the order in which propagators will be called to propagate.
    propagator_queue: PropagatorQueue,
    /// Handles storing information about propagation reasons, which are used later to construct
    /// explanations during conflict analysis
    pub(crate) reason_store: ReasonStore,
    /// Contains events that need to be processed to notify propagators of [`IntDomainEvent`]
    /// occurrences.
    event_drain: Vec<(IntDomainEvent, DomainId)>,
    /// Contains events that need to be processed to notify propagators of backtrack
    /// [`IntDomainEvent`] occurrences (i.e. [`IntDomainEvent`]s being undone).
    backtrack_event_drain: Vec<(IntDomainEvent, DomainId)>,
    /// Holds information needed to map atomic constraints (e.g., [x >= 5]) to literals
    pub(crate) variable_literal_mappings: VariableLiteralMappings,
    /// Used during synchronisation of the propositional and integer trail.
    /// [`AssignmentsInteger::trail`][`cp_trail_synced_position`] is the next entry
    /// that needs to be synchronised with [`AssignmentsPropositional::trail`].
    cp_trail_synced_position: usize,
    /// This is the SAT equivalent of the above, i.e., [`AssignmentsPropositional::trail`]
    /// [[`sat_trail_synced_position`]] is the next
    /// [`Literal`] on the trail that needs to be synchronised with [`AssignmentsInteger::trail`].
    sat_trail_synced_position: usize,
    /// Holds information about explanations during conflict analysis.
    explanation_clause_manager: ExplanationClauseManager,
    /// Convenience literals used in special cases.
    true_literal: Literal,
    false_literal: Literal,
    /// A set of counters updated during the search.
    counters: Counters,
    /// Used to store the learned clause.
    analysis_result: ConflictAnalysisResult,
    /// Miscellaneous constant parameters used by the solver.
    internal_parameters: SatisfactionSolverOptions,
    /// The names of the variables in the solver.
    variable_names: VariableNames,
}

impl Debug for ConstraintSatisfactionSolver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let cp_propagators: Vec<_> = self
            .cp_propagators
            .iter()
            .map(|_| DebugDyn::from("Propagator"))
            .collect();
        f.debug_struct("ConstraintSatisfactionSolver")
            .field("state", &self.state)
            .field("assumptions", &self.assumptions)
            .field("clausal_allocator", &self.clause_allocator)
            .field("assignments_propositional", &self.assignments_propositional)
            .field("clausal_propagator", &self.clausal_propagator)
            .field("learned_clause_manager", &self.learned_clause_manager)
            .field("restart_strategy", &self.restart_strategy)
            .field("cp_propagators", &cp_propagators)
            .field("counters", &self.counters)
            .field("internal_parameters", &self.internal_parameters)
            .field("analysis_result", &self.analysis_result)
            .finish()
    }
}

impl Default for ConstraintSatisfactionSolver {
    fn default() -> Self {
        ConstraintSatisfactionSolver::new(
            LearningOptions::default(),
            SatisfactionSolverOptions::default(),
        )
    }
}

/// Options for the [`Solver`] which determine how it behaves.
#[derive(Debug)]
pub struct SatisfactionSolverOptions {
    /// The options used by the restart strategy.
    pub restart_options: RestartOptions,
    /// Whether learned clause minimisation should take place
    pub learning_clause_minimisation: bool,

    /// The proof log.
    pub proof_log: ProofLog,

    /// A random generator which is used by the [`Solver`], passing it as an
    /// argument allows seeding of the randomization.
    pub random_generator: SmallRng,
}

impl Default for SatisfactionSolverOptions {
    fn default() -> Self {
        SatisfactionSolverOptions {
            restart_options: RestartOptions::default(),
            proof_log: ProofLog::default(),
            learning_clause_minimisation: true,
            random_generator: SmallRng::seed_from_u64(42),
        }
    }
}

impl ConstraintSatisfactionSolver {
    fn process_backtrack_events(&mut self) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self.watch_list_cp.is_watching_any_backtrack_events() {
            self.backtrack_event_drain
                .extend(self.assignments_integer.drain_backtrack_domain_events());

            if self.backtrack_event_drain.is_empty() {
                return false;
            }

            for (event, domain) in self.backtrack_event_drain.drain(..) {
                for propagator_var in self
                    .watch_list_cp
                    .get_backtrack_affected_propagators(event, domain)
                {
                    let propagator = &mut self.cp_propagators[propagator_var.propagator.0 as usize];
                    let context = PropagationContext::new(
                        &self.assignments_integer,
                        &self.assignments_propositional,
                    );

                    propagator.notify_backtrack(&context, propagator_var.variable, event.into())
                }
            }
        }
        true
    }

    /// Process the stored domain events. If no events were present, this returns false. Otherwise,
    /// true is returned.
    fn process_domain_events(&mut self) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self.watch_list_cp.is_watching_anything() {
            self.event_drain
                .extend(self.assignments_integer.drain_domain_events());

            if self.event_drain.is_empty()
                && self.propositional_trail_index
                    == self.assignments_propositional.num_trail_entries()
            {
                return false;
            }

            for (event, domain) in self.event_drain.drain(..) {
                for propagator_var in self.watch_list_cp.get_affected_propagators(event, domain) {
                    let propagator = &mut self.cp_propagators[propagator_var.propagator.0 as usize];
                    let context = PropagationContext::new(
                        &self.assignments_integer,
                        &self.assignments_propositional,
                    );

                    let enqueue_decision =
                        propagator.notify(context, propagator_var.variable, event.into());

                    if enqueue_decision == EnqueueDecision::Enqueue {
                        self.propagator_queue
                            .enqueue_propagator(propagator_var.propagator, propagator.priority());
                    }
                }
            }
        }
        // If there are no literals being watched then there is no reason to perform these
        // operations
        if self.watch_list_propositional.is_watching_anything() {
            for i in
                self.propositional_trail_index..self.assignments_propositional.num_trail_entries()
            {
                let literal = self.assignments_propositional.get_trail_entry(i);
                for (event, affected_literal) in BooleanDomainEvent::get_iterator(literal) {
                    for propagator_var in self
                        .watch_list_propositional
                        .get_affected_propagators(event, affected_literal)
                    {
                        let propagator =
                            &mut self.cp_propagators[propagator_var.propagator.0 as usize];
                        let context = PropagationContext::new(
                            &self.assignments_integer,
                            &self.assignments_propositional,
                        );

                        let enqueue_decision =
                            propagator.notify_literal(context, propagator_var.variable, event);

                        if enqueue_decision == EnqueueDecision::Enqueue {
                            self.propagator_queue.enqueue_propagator(
                                propagator_var.propagator,
                                propagator.priority(),
                            );
                        }
                    }
                }
            }
            self.propositional_trail_index = self.assignments_propositional.num_trail_entries();
        }

        true
    }

    /// Given a predicate, returns the corresponding literal.
    pub fn get_literal(&self, predicate: Predicate) -> Literal {
        match predicate {
            Predicate::IntegerPredicate(integer_predicate) => {
                self.variable_literal_mappings.get_literal(
                    integer_predicate,
                    &self.assignments_propositional,
                    &self.assignments_integer,
                )
            }
            bool_predicate => bool_predicate
                .get_literal_of_bool_predicate(self.assignments_propositional.true_literal)
                .unwrap(),
        }
    }

    /// This is a temporary accessor to help refactoring.
    pub fn get_solution_reference(&self) -> SolutionReference<'_> {
        SolutionReference::new(&self.assignments_propositional, &self.assignments_integer)
    }

    pub(crate) fn is_conflicting(&self) -> bool {
        self.state.conflicting()
    }

    pub(crate) fn declare_ready(&mut self) {
        self.state.declare_ready()
    }

    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) -> std::io::Result<()> {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        proof.unsat(&self.variable_names, &self.variable_literal_mappings)
    }

    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Literal) -> std::io::Result<()> {
        let proof = std::mem::take(&mut self.internal_parameters.proof_log);
        proof.optimal(bound, &self.variable_names, &self.variable_literal_mappings)
    }

    // fn debug_check_consistency(&self, cp_data_structures: &CPEngineDataStructures) -> bool {
    // pumpkin_assert_simple!(
    // assignments_integer.num_domains() as usize
    // == self.mapping_domain_to_lower_bound_literals.len()
    // );
    // pumpkin_assert_simple!(
    // assignments_integer.num_domains() as usize
    // == self.mapping_domain_to_equality_literals.len()
    // );
    // pumpkin_assert_simple!(
    // assignments_integer.num_domains() == cp_data_structures.watch_list_cp.num_domains()
    // );
    // true
    // }
}

// methods that offer basic functionality
impl ConstraintSatisfactionSolver {
    pub fn new(
        learning_options: LearningOptions,
        solver_options: SatisfactionSolverOptions,
    ) -> ConstraintSatisfactionSolver {
        let dummy_literal = Literal::new(PropositionalVariable::new(0), true);

        let mut csp_solver = ConstraintSatisfactionSolver {
            state: CSPSolverState::default(),
            assumptions: Vec::default(),
            assignments_propositional: AssignmentsPropositional::default(),
            clause_allocator: ClauseAllocator::default(),
            assignments_integer: AssignmentsInteger::default(),
            watch_list_cp: WatchListCP::default(),
            watch_list_propositional: WatchListPropositional::default(),
            propagator_queue: PropagatorQueue::new(5),
            reason_store: ReasonStore::default(),
            propositional_trail_index: 0,
            event_drain: vec![],
            backtrack_event_drain: vec![],
            variable_literal_mappings: VariableLiteralMappings::default(),
            cp_trail_synced_position: 0,
            sat_trail_synced_position: 0,
            explanation_clause_manager: ExplanationClauseManager::default(),
            true_literal: dummy_literal,
            false_literal: !dummy_literal,
            conflict_analyser: ResolutionConflictAnalyser::default(),
            clausal_propagator: ClausalPropagatorType::default(),
            learned_clause_manager: LearnedClauseManager::new(learning_options),
            restart_strategy: RestartStrategy::new(solver_options.restart_options),
            cp_propagators: vec![],
            counters: Counters::default(),
            internal_parameters: solver_options,
            analysis_result: ConflictAnalysisResult::default(),
            variable_names: VariableNames::default(),
        };

        // we introduce a dummy variable set to true at the root level
        //  this is useful for convenience when a fact needs to be expressed that is always true
        //  e.g., this makes writing propagator explanations easier for corner cases
        let root_variable = csp_solver.create_new_propositional_variable(Some("true".to_owned()));
        let true_literal = Literal::new(root_variable, true);

        csp_solver.assignments_propositional.true_literal = true_literal;
        csp_solver.assignments_propositional.false_literal = !true_literal;

        csp_solver.true_literal = true_literal;
        csp_solver.false_literal = !true_literal;

        let result = csp_solver.add_clause([true_literal]);
        pumpkin_assert_simple!(result.is_ok());

        csp_solver
    }

    pub fn solve(
        &mut self,
        termination: &mut impl TerminationCondition,
        brancher: &mut impl Brancher,
    ) -> CSPSolverExecutionFlag {
        let dummy_assumptions: Vec<Literal> = vec![];
        self.solve_under_assumptions(&dummy_assumptions, termination, brancher)
    }

    pub fn solve_under_assumptions(
        &mut self,
        assumptions: &[Literal],
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

    pub fn default_brancher_over_all_propositional_variables(&self) -> DefaultBrancher {
        #[allow(deprecated)]
        let variables = self
            .get_propositional_assignments()
            .get_propositional_variables()
            .collect::<Vec<_>>();

        IndependentVariableValueBrancher {
            variable_selector: Vsids::new(&variables),
            value_selector: SolutionGuidedValueSelector::new(
                &variables,
                Vec::new(),
                PhaseSaving::new(&variables),
            ),
            variable_type: PhantomData,
        }
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

        let domain = self.variable_literal_mappings.create_new_domain(
            lower_bound,
            upper_bound,
            &mut self.assignments_integer,
            &mut self.watch_list_cp,
            &mut self.watch_list_propositional,
            &mut self.clausal_propagator,
            &mut self.assignments_propositional,
            &mut self.clause_allocator,
        );

        if let Some(name) = name {
            self.variable_names.add_integer(domain, name);
        }

        domain
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
                self.assignments_integer
                    .remove_initial_value_from_domain(domain_id, value, None)
                    .expect("the domain should not be empty");
                self.assignments_propositional.enqueue_decision_literal(
                    self.variable_literal_mappings.get_inequality_literal(
                        domain_id,
                        value,
                        &self.assignments_propositional,
                        &self.assignments_integer,
                    ),
                )
            }
        }
        pumpkin_assert_simple!(
            next_idx == values.len(),
            "Expected all values to have been processed"
        );

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
    ///             "The core has the length of the number of assumptions"
    ///         );
    ///         assert!(
    ///             core.get_negated_assumption_literals()
    ///                 .into_iter()
    ///                 .all(|lit| assumptions.contains(&!lit)),
    ///             "All literals in the core are negated assumptions"
    ///         );
    ///     }
    /// }
    /// ```
    pub fn extract_clausal_core(&mut self, brancher: &mut impl Brancher) -> Result<Core, Literal> {
        let mut conflict_analysis_context = ConflictAnalysisContext {
            assumptions: &self.assumptions,
            clausal_propagator: &self.clausal_propagator,
            variable_literal_mappings: &self.variable_literal_mappings,
            assignments_integer: &self.assignments_integer,
            assignments_propositional: &self.assignments_propositional,
            internal_parameters: &self.internal_parameters,
            solver_state: &mut self.state,
            brancher,
            clause_allocator: &mut self.clause_allocator,
            explanation_clause_manager: &mut self.explanation_clause_manager,
            reason_store: &mut self.reason_store,
            counters: &mut self.counters,
            learned_clause_manager: &mut self.learned_clause_manager,
        };

        let core = self
            .conflict_analyser
            .compute_clausal_core(&mut conflict_analysis_context);

        if !self.state.is_infeasible() {
            self.restore_state_at_root(brancher);
        }

        core
    }

    #[allow(unused)]
    pub(crate) fn get_conflict_reasons(
        &mut self,
        brancher: &mut impl Brancher,
        on_analysis_step: impl FnMut(AnalysisStep),
    ) {
        let mut conflict_analysis_context = ConflictAnalysisContext {
            assumptions: &self.assumptions,
            clausal_propagator: &self.clausal_propagator,
            variable_literal_mappings: &self.variable_literal_mappings,
            assignments_integer: &self.assignments_integer,
            assignments_propositional: &self.assignments_propositional,
            internal_parameters: &self.internal_parameters,
            solver_state: &mut self.state,
            brancher,
            clause_allocator: &mut self.clause_allocator,
            explanation_clause_manager: &mut self.explanation_clause_manager,
            reason_store: &mut self.reason_store,
            counters: &mut self.counters,
            learned_clause_manager: &mut self.learned_clause_manager,
        };

        self.conflict_analyser
            .get_conflict_reasons(&mut conflict_analysis_context, on_analysis_step);
    }

    /// Returns an infinite iterator of positive literals of new variables. The new variables will
    /// be unnamed.
    ///
    /// Note that this method captures the lifetime of the immutable reference to `self`.
    pub fn new_literals(&mut self) -> impl Iterator<Item = Literal> + '_ {
        std::iter::from_fn(|| Some(self.create_new_propositional_variable(None)))
            .map(|var| Literal::new(var, true))
    }

    pub fn create_new_propositional_variable(
        &mut self,
        name: Option<String>,
    ) -> PropositionalVariable {
        let variable = self
            .variable_literal_mappings
            .create_new_propositional_variable(
                &mut self.watch_list_propositional,
                &mut self.clausal_propagator,
                &mut self.assignments_propositional,
            );

        if let Some(name) = name {
            self.variable_names.add_propositional(variable, name);
        }

        variable
    }

    /// Get a literal which is globally true.
    pub fn get_true_literal(&self) -> Literal {
        self.assignments_propositional.true_literal
    }

    /// Get a literal which is globally false.
    pub fn get_false_literal(&self) -> Literal {
        self.assignments_propositional.false_literal
    }

    /// Get the lower bound for the given variable.
    pub fn get_lower_bound(&self, variable: &impl IntegerVariable) -> i32 {
        variable.lower_bound(&self.assignments_integer)
    }

    /// Get the upper bound for the given variable.
    pub fn get_upper_bound(&self, variable: &impl IntegerVariable) -> i32 {
        variable.upper_bound(&self.assignments_integer)
    }

    /// Determine whether `value` is in the domain of `variable`.
    pub fn integer_variable_contains(&self, variable: &impl IntegerVariable, value: i32) -> bool {
        variable.contains(&self.assignments_integer, value)
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

    /// Get the value of the given literal, which could be unassigned.
    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        if self.assignments_propositional.is_literal_assigned(literal) {
            Some(
                self.assignments_propositional
                    .is_literal_assigned_true(literal),
            )
        } else {
            None
        }
    }

    #[deprecated = "users of the solvers should not have to access solver fields"]
    pub(crate) fn get_propositional_assignments(&self) -> &AssignmentsPropositional {
        &self.assignments_propositional
    }

    pub fn restore_state_at_root(&mut self, brancher: &mut impl Brancher) {
        if !self.assignments_propositional.is_at_the_root_level() {
            self.backtrack(0, brancher);
            self.state.declare_ready();
        }
    }

    fn synchronise_propositional_trail_based_on_integer_trail(&mut self) -> Option<ConflictInfo> {
        // for each entry on the integer trail, we now add the equivalent propositional
        // representation on the propositional trail  note that only one literal per
        // predicate will be stored      since the clausal propagator will propagate other
        // literals to ensure that the meaning of the literal is respected          e.g.,
        // placing [x >= 5] will prompt the clausal propagator to set [x >= 4] [x >= 3] ... [x >= 1]
        // to true
        for cp_trail_pos in
            self.cp_trail_synced_position..self.assignments_integer.num_trail_entries()
        {
            let entry = self.assignments_integer.get_trail_entry(cp_trail_pos);

            // It could be the case that the reason is `None`
            // due to a SAT propagation being put on the trail during
            // `synchronise_integer_trail_based_on_propositional_trail` In that case we
            // do not synchronise since we assume that the SAT trail is already aware of the
            // information
            if let Some(reason_ref) = entry.reason {
                let literal = self.variable_literal_mappings.get_literal(
                    entry.predicate,
                    &self.assignments_propositional,
                    &self.assignments_integer,
                );

                let constraint_reference = ConstraintReference::create_reason_reference(reason_ref);

                let conflict_info = self
                    .assignments_propositional
                    .enqueue_propagated_literal(literal, constraint_reference);

                if conflict_info.is_some() {
                    self.cp_trail_synced_position = cp_trail_pos + 1;
                    return conflict_info;
                }

                // It could occur that one of these propagations caused a conflict in which case the
                // SAT-view and the CP-view are unsynchronised We need to ensure
                // that the views are synchronised up to the CP trail entry which caused the
                // conflict
                if let Err(e) = self.clausal_propagator.propagate(
                    &mut self.assignments_propositional,
                    &mut self.clause_allocator,
                ) {
                    self.cp_trail_synced_position = cp_trail_pos + 1;
                    return Some(e);
                }
            }
        }
        self.cp_trail_synced_position = self.assignments_integer.num_trail_entries();
        None
    }

    fn synchronise_integer_trail_based_on_propositional_trail(
        &mut self,
    ) -> Result<(), EmptyDomain> {
        pumpkin_assert_moderate!(
            self.cp_trail_synced_position == self.assignments_integer.num_trail_entries(),
            "We can only sychronise the propositional trail if the integer trail is already
             sychronised."
        );

        // this could possibly be improved if it shows up as a performance hotspot
        //  in some cases when we push e.g., [x >= a] on the stack, then we could also add the
        // literals to the propositional stack  and update the next_domain_trail_position
        // pointer to go pass the entries that surely are not going to lead to any changes
        //  this would only work if the next_domain_trail pointer is already at the end of the
        // stack, think about this, could be useful for propagators      and might be useful
        // for a custom domain propagator  this would also simplify the code below, no
        // additional checks would be needed? Not sure.

        if self.assignments_integer.num_domains() == 0 {
            self.sat_trail_synced_position = self.assignments_propositional.num_trail_entries();
            return Ok(());
        }

        for sat_trail_pos in
            self.sat_trail_synced_position..self.assignments_propositional.num_trail_entries()
        {
            let literal = self
                .assignments_propositional
                .get_trail_entry(sat_trail_pos);
            self.synchronise_literal(literal)?;
        }
        self.sat_trail_synced_position = self.assignments_propositional.num_trail_entries();
        // the newly added entries to the trail do not need to be synchronise with the propositional
        // trail  this is because the integer trail was already synchronise when this method
        // was called  and the newly added entries are already present on the propositional
        // trail
        self.cp_trail_synced_position = self.assignments_integer.num_trail_entries();

        let _ = self.process_domain_events();

        Ok(())
    }

    fn synchronise_literal(&mut self, literal: Literal) -> Result<(), EmptyDomain> {
        // recall that a literal may be linked to multiple predicates
        //  e.g., this may happen when in preprocessing two literals are detected to be equal
        //  so now we loop for each predicate and make necessary updates
        //  (although currently we do not have any serious preprocessing!)
        for j in 0..self.variable_literal_mappings.literal_to_predicates[literal].len() {
            let predicate = self.variable_literal_mappings.literal_to_predicates[literal][j];
            self.assignments_integer
                .apply_integer_predicate(predicate, None)?;
        }
        Ok(())
    }

    fn synchronise_assignments(&mut self) {
        pumpkin_assert_simple!(
            self.sat_trail_synced_position >= self.assignments_propositional.num_trail_entries()
        );
        pumpkin_assert_simple!(
            self.cp_trail_synced_position >= self.assignments_integer.num_trail_entries()
        );
        self.cp_trail_synced_position = self.assignments_integer.num_trail_entries();
        self.sat_trail_synced_position = self.assignments_propositional.num_trail_entries();
    }
}

// methods that serve as the main building blocks
impl ConstraintSatisfactionSolver {
    fn initialise(&mut self, assumptions: &[Literal]) {
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

            self.learned_clause_manager
                .shrink_learned_clause_database_if_needed(
                    &self.assignments_propositional,
                    &mut self.clause_allocator,
                    &mut self.clausal_propagator,
                );

            self.propagate_enqueued();

            if self.state.no_conflict() {
                self.declare_new_decision_level();

                // Restarts should only occur after a new decision level has been declared to
                // account for the fact that all assumptions should be assigned when restarts take
                // place. Since one assumption is posted per decision level, all assumptions are
                // assigned when the decision level is strictly larger than the number of
                // assumptions.
                if self.restart_strategy.should_restart() {
                    self.restart_during_search(brancher);
                    self.declare_new_decision_level();
                }

                let branching_result = self.enqueue_next_decision(brancher);
                if let Err(flag) = branching_result {
                    return flag;
                }
            }
            // conflict
            else {
                if self.assignments_propositional.is_at_the_root_level() {
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
        if let Some(assumption_literal) = self.peek_next_assumption_literal() {
            let success = self.enqueue_assumption_literal(assumption_literal);
            if !success {
                return Err(CSPSolverExecutionFlag::Infeasible);
            }
            Ok(())
        } else {
            let decided_predicate = brancher.next_decision(&mut SelectionContext::new(
                &self.assignments_integer,
                &self.assignments_propositional,
                &mut self.internal_parameters.random_generator,
            ));
            if let Some(predicate) = decided_predicate {
                self.counters.num_decisions += 1;
                self.assignments_propositional
                    .enqueue_decision_literal(match predicate {
                        Predicate::IntegerPredicate(integer_predicate) => {
                            self.variable_literal_mappings.get_literal(
                                integer_predicate,
                                &self.assignments_propositional,
                                &self.assignments_integer,
                            )
                        }
                        bool_predicate => bool_predicate
                            .get_literal_of_bool_predicate(
                                self.assignments_propositional.true_literal,
                            )
                            .unwrap(),
                    });
                Ok(())
            } else {
                self.state.declare_solution_found();
                Err(CSPSolverExecutionFlag::Feasible)
            }
        }
    }

    /// Returns true if the assumption was successfully enqueued, and false otherwise
    pub(crate) fn enqueue_assumption_literal(&mut self, assumption_literal: Literal) -> bool {
        // Case 1: the assumption is unassigned, assign it
        if self
            .assignments_propositional
            .is_literal_unassigned(assumption_literal)
        {
            self.assignments_propositional
                .enqueue_decision_literal(assumption_literal);
            true
        // Case 2: the assumption has already been set to true
        //  this happens when other assumptions propagated the literal
        //  or the assumption is already set to true at the root level
        } else if self
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

    pub(crate) fn declare_new_decision_level(&mut self) {
        self.assignments_propositional.increase_decision_level();
        self.assignments_integer.increase_decision_level();
        self.reason_store.increase_decision_level();
    }

    /// Changes the state based on the conflict analysis result (stored in
    /// [`ConstraintSatisfactionSolver::analysis_result`]). It performs the following:
    /// - Adds the learned clause to the database
    /// - Performs backtracking
    /// - Enqueues the propagated [`Literal`] of the learned clause
    /// - Updates the internal data structures (e.g. for the restart strategy or the learned clause
    ///   manager)
    ///
    /// # Note
    /// This method performs no propagation, this is left up to the solver afterwards
    fn resolve_conflict(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_moderate!(self.state.conflicting());

        self.analysis_result = self.compute_learned_clause(brancher);

        self.process_learned_clause(brancher);

        self.state.declare_solving();
    }

    fn compute_learned_clause(&mut self, brancher: &mut impl Brancher) -> ConflictAnalysisResult {
        let mut conflict_analysis_context = ConflictAnalysisContext {
            assumptions: &self.assumptions,
            clausal_propagator: &self.clausal_propagator,
            variable_literal_mappings: &self.variable_literal_mappings,
            assignments_integer: &self.assignments_integer,
            assignments_propositional: &self.assignments_propositional,
            internal_parameters: &self.internal_parameters,
            solver_state: &mut self.state,
            brancher,
            clause_allocator: &mut self.clause_allocator,
            explanation_clause_manager: &mut self.explanation_clause_manager,
            reason_store: &mut self.reason_store,
            counters: &mut self.counters,
            learned_clause_manager: &mut self.learned_clause_manager,
        };
        self.conflict_analyser
            .compute_1uip(&mut conflict_analysis_context)
    }

    fn process_learned_clause(&mut self, brancher: &mut impl Brancher) {
        if let Err(write_error) = self
            .internal_parameters
            .proof_log
            .log_learned_clause(self.analysis_result.learned_literals.iter().copied())
        {
            warn!(
                "Failed to update the certificate file, error message: {}",
                write_error
            );
        }

        // unit clauses are treated in a special way: they are added as root level decisions
        if self.analysis_result.learned_literals.len() == 1 {
            // important to notify about the conflict _before_ backtracking removes literals from
            // the trail
            self.restart_strategy
                .notify_conflict(1, self.assignments_propositional.num_trail_entries());

            self.backtrack(0, brancher);

            let unit_clause = self.analysis_result.learned_literals[0];

            self.assignments_propositional
                .enqueue_decision_literal(unit_clause);

            self.counters.num_unit_clauses_learned +=
                (self.analysis_result.learned_literals.len() == 1) as u64;
        } else {
            self.counters
                .average_learned_clause_length
                .add_term(self.analysis_result.learned_literals.len() as u64);

            // important to get trail length before the backtrack
            let num_variables_assigned_before_conflict =
                &self.assignments_propositional.num_trail_entries();

            self.counters
                .average_backtrack_amount
                .add_term((self.get_decision_level() - self.analysis_result.backjump_level) as u64);
            self.backtrack(self.analysis_result.backjump_level, brancher);

            self.learned_clause_manager.add_learned_clause(
                self.analysis_result.learned_literals.clone(), // todo not ideal with clone
                &mut self.clausal_propagator,
                &mut self.assignments_propositional,
                &mut self.clause_allocator,
            );

            let lbd = self.learned_clause_manager.compute_lbd_for_literals(
                &self.analysis_result.learned_literals,
                &self.assignments_propositional,
            );

            self.restart_strategy
                .notify_conflict(lbd, *num_variables_assigned_before_conflict);
        }
    }
    /// Performs a restart during the search process; it is only called when it has been determined
    /// to be necessary by the [`ConstraintSatisfactionSolver::restart_strategy`]. A 'restart'
    /// differs from backtracking to level zero in that a restart backtracks to decision level
    /// zero and then performs additional operations, e.g., clean up learned clauses, adjust
    /// restart frequency, etc.
    fn restart_during_search(&mut self, brancher: &mut impl Brancher) {
        pumpkin_assert_simple!(
            self.are_all_assumptions_assigned(),
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

        let unassigned_literals = self.assignments_propositional.synchronise(backtrack_level);

        unassigned_literals.for_each(|literal| {
            brancher.on_unassign_literal(literal);
            // TODO: We should also backtrack on the integer variables here
        });

        self.clausal_propagator
            .synchronise(self.assignments_propositional.num_trail_entries());

        pumpkin_assert_simple!(
            self.assignments_propositional.get_decision_level()
                < self.assignments_integer.get_decision_level(),
            "assignments_propositional must be backtracked _before_ CPEngineDataStructures"
        );
        self.propositional_trail_index = min(
            self.propositional_trail_index,
            self.assignments_propositional.num_trail_entries(),
        );
        self.assignments_integer
            .synchronise(
                backtrack_level,
                self.watch_list_cp.is_watching_any_backtrack_events(),
            )
            .iter()
            .for_each(|(domain_id, previous_value)| {
                brancher.on_unassign_integer(*domain_id, *previous_value)
            });

        self.reason_store.synchronise(backtrack_level);
        self.propagator_queue.clear();
        //  note that variable_literal_mappings sync should be called after the sat/cp data
        // structures backtrack
        self.synchronise_assignments();
        // for now all propagators are called to synchronise
        //  in the future this will be improved in two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator_id in 0..self.cp_propagators.len() {
            let context =
                PropagationContext::new(&self.assignments_integer, &self.assignments_propositional);
            self.cp_propagators[propagator_id].synchronise(&context);
        }

        let _ = self.process_backtrack_events();
    }

    /// Main propagation loop.
    pub(crate) fn propagate_enqueued(&mut self) {
        let num_assigned_variables_old = self.assignments_integer.num_trail_entries();

        loop {
            let conflict_info = self.synchronise_propositional_trail_based_on_integer_trail();

            if let Some(conflict_info) = conflict_info {
                // The previous propagation triggered an empty domain.
                self.state
                    .declare_conflict(conflict_info.try_into().unwrap());
                break;
            }

            let clausal_propagation_status = self.clausal_propagator.propagate(
                &mut self.assignments_propositional,
                &mut self.clause_allocator,
            );

            if let Err(conflict_info) = clausal_propagation_status {
                self.state
                    .declare_conflict(conflict_info.try_into().unwrap());
                break;
            }

            self.synchronise_integer_trail_based_on_propositional_trail()
                .expect("should not be an error");

            // ask propagators to propagate
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
                    let result = self.synchronise_propositional_trail_based_on_integer_trail();

                    // If the clausal propagator found a conflict during synchronisation then we
                    // want to use that conflict; if we do not use that conflict then it could be
                    // the case that there are literals in the conflict_info found by the CP
                    // propagator which are not assigned in the SAT-view (which leads to an error
                    // during conflict analysis)
                    self.state.declare_conflict(
                        result
                            .map(|ci| {
                                ci.try_into()
                                    .expect("this is not a ConflictInfo::Explanation")
                            })
                            .unwrap_or(conflict_info),
                    );
                    break;
                }
            } // end match
        }

        self.counters.num_conflicts += self.state.conflicting() as u64;

        self.counters.num_propagations +=
            self.assignments_integer.num_trail_entries() as u64 - num_assigned_variables_old as u64;

        // Only check fixed point propagation if there was no reported conflict.
        pumpkin_assert_extreme!(
            self.state.conflicting()
                || DebugHelper::debug_fixed_point_propagation(
                    &self.clausal_propagator,
                    &self.assignments_integer,
                    &self.assignments_propositional,
                    &self.clause_allocator,
                    &self.cp_propagators,
                )
        );
    }

    /// Performs propagation using propagators, stops after a propagator propagates at least one
    /// domain change. The idea is to go to the clausal propagator first before proceeding with
    /// other propagators, in line with the idea of propagating simpler propagators before more
    /// complex ones.
    fn propagate_cp_one_step(&mut self) -> PropagationStatusOneStepCP {
        if self.propagator_queue.is_empty() {
            return PropagationStatusOneStepCP::FixedPoint;
        }

        let propagator_id = self.propagator_queue.pop();
        let propagator = &mut self.cp_propagators[propagator_id.0 as usize];
        let context = PropagationContextMut::new(
            &mut self.assignments_integer,
            &mut self.reason_store,
            &mut self.assignments_propositional,
            propagator_id,
        );

        match propagator.propagate(context) {
            // An empty domain conflict will be caught by the clausal propagator.
            Err(Inconsistency::EmptyDomain) => PropagationStatusOneStepCP::PropagationHappened,

            // A propagator-specific reason for the current conflict.
            Err(Inconsistency::Other(conflict_info)) => {
                if let ConflictInfo::Explanation(ref propositional_conjunction) = conflict_info {
                    pumpkin_assert_advanced!(DebugHelper::debug_reported_failure(
                        &self.assignments_integer,
                        &self.assignments_propositional,
                        &self.variable_literal_mappings,
                        propositional_conjunction,
                        propagator.as_ref(),
                        propagator_id,
                    ));
                }

                PropagationStatusOneStepCP::ConflictDetected {
                    conflict_info: conflict_info.into_stored(propagator_id),
                }
            }

            Ok(()) => {
                let _ = self.process_domain_events();
                PropagationStatusOneStepCP::PropagationHappened
            }
        }
    }

    fn are_all_assumptions_assigned(&self) -> bool {
        self.assignments_propositional.get_decision_level() > self.assumptions.len()
    }

    fn peek_next_assumption_literal(&self) -> Option<Literal> {
        if self.are_all_assumptions_assigned() {
            None
        } else {
            // the convention is that at decision level i, the (i-1)th assumption is set
            //  note that the decision level is increased before calling branching hence the minus
            // one
            Some(self.assumptions[self.assignments_propositional.get_decision_level() - 1])
        }
    }
}

// methods for adding constraints (propagators and clauses)
impl ConstraintSatisfactionSolver {
    /// Add a clause (of at least length 2) which could later be deleted. Be mindful of the effect
    /// of this on learned clauses etc. if a solve call were to be invoked after adding a clause
    /// through this function.
    ///
    /// The clause is marked as 'learned'.
    pub(crate) fn add_allocated_deletable_clause(
        &mut self,
        clause: Vec<Literal>,
    ) -> ClauseReference {
        self.clausal_propagator
            .add_clause_unchecked(clause, true, &mut self.clause_allocator)
            .unwrap()
    }

    /// Delete an allocated clause. Users of this method must ensure the state of the solver stays
    /// well-defined. In particular, if there are learned clauses derived through this clause, and
    /// it is removed, those learned clauses may no-longer be valid.
    pub(crate) fn delete_allocated_clause(&mut self, reference: ClauseReference) -> Vec<Literal> {
        let clause = self.clause_allocator[reference]
            .get_literal_slice()
            .to_vec();

        self.clausal_propagator
            .remove_clause_from_consideration(&clause, reference);
        self.clause_allocator.delete_clause(reference);

        clause
    }

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

        let new_propagator_id = PropagatorId(self.cp_propagators.len() as u32);
        let mut constructor_context = PropagatorConstructorContext::new(
            &mut self.watch_list_cp,
            &mut self.watch_list_propositional,
            new_propagator_id,
        );

        let propagator_to_add = constructor.create_boxed(&mut constructor_context);

        pumpkin_assert_simple!(
            propagator_to_add.priority() <= 3,
            "The propagator priority exceeds 3.
             Currently we only support values up to 3,
             but this can easily be changed if there is a good reason."
        );

        self.cp_propagators.push(propagator_to_add);

        let new_propagator = &mut self.cp_propagators[new_propagator_id];
        let initialisation_status = new_propagator.initialise_at_root(PropagationContext::new(
            &self.assignments_integer,
            &self.assignments_propositional,
        ));

        if initialisation_status.is_err() {
            self.state.declare_infeasible();
            Err(ConstraintOperationError::InfeasiblePropagator)
        } else {
            self.propagator_queue
                .enqueue_propagator(new_propagator_id, new_propagator.priority());

            self.propagate_enqueued();

            if self.state.no_conflict() {
                Ok(())
            } else {
                Err(ConstraintOperationError::InfeasiblePropagator)
            }
        }
    }

    /// Creates a clause from `literals` and adds it to the current formula.
    ///
    /// If the formula becomes trivially unsatisfiable, a [`ConstraintOperationError`] will be
    /// returned. Subsequent calls to this method will always return an error, and no
    /// modification of the solver will take place.
    pub fn add_clause(
        &mut self,
        literals: impl IntoIterator<Item = Literal>,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_moderate!(!self.state.is_infeasible_under_assumptions());
        pumpkin_assert_moderate!(self.is_propagation_complete());

        if self.state.is_infeasible() {
            return Err(ConstraintOperationError::InfeasibleState);
        }

        let literals: Vec<Literal> = literals.into_iter().collect();

        let result = self.clausal_propagator.add_permanent_clause(
            literals,
            &mut self.assignments_propositional,
            &mut self.clause_allocator,
        );

        if result.is_err() {
            self.state.declare_infeasible();
            return Err(ConstraintOperationError::InfeasibleClause);
        }

        self.propagate_enqueued();

        if self.state.is_infeasible() {
            self.state.declare_infeasible();
            return Err(ConstraintOperationError::InfeasibleClause);
        }

        Ok(())
    }
}

// methods for getting simple info out of the solver
impl ConstraintSatisfactionSolver {
    pub fn is_propagation_complete(&self) -> bool {
        self.clausal_propagator
            .is_propagation_complete(self.assignments_propositional.num_trail_entries())
            && self.propagator_queue.is_empty()
    }

    pub(crate) fn get_decision_level(&self) -> usize {
        pumpkin_assert_moderate!(
            self.assignments_propositional.get_decision_level()
                == self.assignments_integer.get_decision_level()
        );
        self.assignments_propositional.get_decision_level()
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
    pub(crate) average_number_of_removed_literals_recursive: CumulativeMovingAverage,
    pub(crate) average_number_of_removed_literals_semantic: CumulativeMovingAverage,
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
        log_statistic(
            "averageNumberOfRemovedLiteralsRecursive",
            self.average_number_of_removed_literals_recursive.value(),
        );
        log_statistic(
            "averageNumberOfRemovedLiteralsSemantic",
            self.average_number_of_removed_literals_semantic.value(),
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

    /// Determines whether the current state is inconsistent; i.e. whether it is conflicting,
    /// infeasible or infeasible under assumptions
    pub fn is_inconsistent(&self) -> bool {
        self.conflicting() || self.is_infeasible() || self.is_infeasible_under_assumptions()
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
        pumpkin_assert_simple!((self.is_ready() || self.conflicting()) && !self.is_infeasible());
        self.internal_state = CSPSolverStateInternal::Solving;
    }

    fn declare_infeasible(&mut self) {
        self.internal_state = CSPSolverStateInternal::Infeasible;
    }

    fn declare_conflict(&mut self, conflict_info: StoredConflictInfo) {
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

#[cfg(test)]
mod tests {
    use super::ConstraintSatisfactionSolver;
    use crate::basic_types::CSPSolverExecutionFlag;
    use crate::basic_types::PropagationStatusCP;
    use crate::basic_types::PropositionalConjunction;
    use crate::basic_types::StoredConflictInfo;
    use crate::conjunction;
    use crate::engine::constraint_satisfaction_solver::CSPSolverStateInternal;
    use crate::engine::predicates::integer_predicate::IntegerPredicate;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::propagation::propagation_context::HasAssignments;
    use crate::engine::propagation::LocalId;
    use crate::engine::propagation::PropagationContextMut;
    use crate::engine::propagation::Propagator;
    use crate::engine::propagation::PropagatorConstructor;
    use crate::engine::propagation::PropagatorConstructorContext;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::reason::ReasonRef;
    use crate::engine::termination::indefinite::Indefinite;
    use crate::engine::variables::DomainId;
    use crate::engine::variables::Literal;
    use crate::engine::Core;
    use crate::engine::DomainEvents;
    use crate::predicate;

    /// Constructor for the [`TestPropagator`], it takes as input a list of propagations and their
    /// explanations (in the forms of tuples of [`Predicate`]s and [`PropositionalConjunction`]s),
    /// and conflicts (also in the form of [`PropositionalConjunction`]s).
    struct TestPropagatorConstructor {
        propagations: Vec<(Predicate, PropositionalConjunction)>,
        conflicts: Vec<PropositionalConjunction>,
    }

    impl PropagatorConstructor for TestPropagatorConstructor {
        type Propagator = TestPropagator;

        fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
            let propagations: Vec<(DomainId, Predicate, PropositionalConjunction)> = self
                .propagations
                .iter()
                .enumerate()
                .rev()
                .map(|(index, variable)| {
                    (
                        context.register(
                            variable.0.get_domain().unwrap(),
                            DomainEvents::BOUNDS,
                            LocalId::from(index as u32),
                        ),
                        variable.0,
                        variable.1.clone(),
                    )
                })
                .collect::<Vec<_>>();
            TestPropagator {
                original_propagations: propagations.clone(),
                propagations,
                conflicts: self.conflicts.into_iter().rev().collect::<Vec<_>>(),
                is_in_root: true,
            }
        }
    }

    /// A test propagator which propagates the stored propagations and then reports one of the
    /// stored conflicts. If multiple conflicts are stored then the next time it is called, it will
    /// return the next conflict.
    ///
    /// It is assumed that the propagations do not lead to conflict, if the propagations do lead to
    /// a conflict then this method will panic.
    struct TestPropagator {
        original_propagations: Vec<(DomainId, Predicate, PropositionalConjunction)>,
        propagations: Vec<(DomainId, Predicate, PropositionalConjunction)>,
        conflicts: Vec<PropositionalConjunction>,
        is_in_root: bool,
    }

    impl Propagator for TestPropagator {
        fn name(&self) -> &str {
            "TestPropagator"
        }

        fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
            if self.is_in_root {
                self.is_in_root = false;
                return Ok(());
            }

            while let Some(propagation) = self.propagations.pop() {
                match propagation.1 {
                    Predicate::IntegerPredicate(integer_predicate) => match integer_predicate {
                        IntegerPredicate::LowerBound {
                            domain_id: _,
                            lower_bound,
                        } => {
                            let result =
                                context.set_lower_bound(&propagation.0, lower_bound, propagation.2);
                            assert!(result.is_ok())
                        }
                        IntegerPredicate::UpperBound {
                            domain_id: _,
                            upper_bound,
                        } => {
                            let result =
                                context.set_upper_bound(&propagation.0, upper_bound, propagation.2);
                            assert!(result.is_ok())
                        }
                        IntegerPredicate::NotEqual {
                            domain_id: _,
                            not_equal_constant,
                        } => {
                            let result =
                                context.remove(&propagation.0, not_equal_constant, propagation.2);
                            assert!(result.is_ok())
                        }
                        IntegerPredicate::Equal {
                            domain_id: _,
                            equality_constant: _,
                        } => todo!(),
                    },
                    _ => todo!(),
                }
            }

            if let Some(conflict) = self.conflicts.pop() {
                return Err(conflict.into());
            }

            Ok(())
        }

        fn debug_propagate_from_scratch(
            &self,
            context: PropagationContextMut,
        ) -> PropagationStatusCP {
            // This method detects when a debug propagation method is called and it attempts to
            // return the correct result in this case
            if self.conflicts.is_empty()
                && self.original_propagations.iter().all(|propagation| {
                    context.assignments_integer().does_integer_predicate_hold(
                        propagation
                            .1
                            .try_into()
                            .expect("Expected provided predicate to be integer"),
                    )
                })
            {
                Err(crate::basic_types::Inconsistency::EmptyDomain)
            } else {
                Ok(())
            }
        }
    }

    fn is_same_core(core1: &[Literal], core2: &[Literal]) -> bool {
        core1.len() == core2.len() && core2.iter().all(|lit| core1.contains(lit))
    }

    fn is_result_the_same(res1: &Result<Core, Literal>, res2: &Result<Core, Literal>) -> bool {
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
            is_same_core(
                &res1.clone().unwrap().get_core(),
                &res2.clone().unwrap().get_core(),
            )
        }
    }

    fn run_test(
        mut solver: ConstraintSatisfactionSolver,
        assumptions: Vec<Literal>,
        expected_flag: CSPSolverExecutionFlag,
        expected_result: Result<Core, Literal>,
    ) {
        let mut brancher = solver.default_brancher_over_all_propositional_variables();
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

    fn create_instance1() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
        let lit2 = Literal::new(solver.create_new_propositional_variable(None), true);

        let _ = solver.add_clause([lit1, lit2]);
        let _ = solver.add_clause([lit1, !lit2]);
        let _ = solver.add_clause([!lit1, lit2]);
        (solver, vec![lit1, lit2])
    }

    /// Warning: This test is potentially flaky due to its dependence on the propagation order of
    /// the clausal propagator, please treat with caution.
    #[test]
    fn test_synchronisation_view() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let variable = solver.create_new_integer_variable(1, 5, None);
        let other_variable = solver.create_new_integer_variable(3, 5, None);

        // We create a test solver which propagates such that there is a jump across a hole in the
        // domain and then we report a conflict based on the assigned values.
        let propagator_constructor = TestPropagatorConstructor {
            propagations: vec![
                (predicate!(variable != 2), conjunction!()),
                (predicate!(variable <= 3), conjunction!()),
                (predicate!(variable != 3), conjunction!()),
                (predicate!(other_variable != 4), conjunction!()),
                (predicate!(other_variable <= 4), conjunction!()),
            ],
            conflicts: vec![conjunction!([other_variable == 3] & [variable == 1])],
        };

        let result = solver.add_propagator(propagator_constructor);
        assert!(result.is_ok());

        // We add the clause that will lead to the conflict in the SAT-solver
        let result = solver.add_clause([
            solver.get_literal(predicate![variable == 2]),
            solver.get_literal(predicate![variable == 3]),
            solver.get_literal(predicate![variable == 4]),
            solver.get_literal(predicate![variable == 5]),
        ]);
        assert!(result.is_ok());

        solver.declare_new_decision_level();

        // We manually enqueue the propagator to mimic solver behaviour
        solver
            .propagator_queue
            .enqueue_propagator(PropagatorId(0), 0);

        // After propagating we expect that both the CP propagators and the SAT solver have found a
        // conflict, however, the CP conflict explanation contains variables which are not assigned
        // in the SAT view due to it finding a conflict. We expect the conflict info in the solver
        // to contain the conflict found by the clausal propagator.
        solver.propagate_enqueued();

        assert!(solver.state.is_inconsistent());
        assert_eq!(solver.get_assigned_integer_value(&variable), Some(1));

        // We check whether the conflict which is returned is the conflict found by the SAT-solver
        // rather than the one found by the CP solver.
        assert!(matches!(
            solver.state.internal_state,
            CSPSolverStateInternal::Conflict {
                conflict_info: StoredConflictInfo::Propagation {
                    reference: _,
                    literal: _,
                },
            }
        ));
    }

    #[test]
    fn core_extraction_unit_core() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
        let _ = solver.add_clause(vec![lit1]);

        run_test(
            solver,
            vec![!lit1],
            CSPSolverExecutionFlag::Infeasible,
            Ok(Core::RootLevel {
                root_level_assumption_literal: !lit1,
            }),
        )
    }

    #[test]
    fn simple_core_extraction_1_1() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[0], !lits[1]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(Core::RootLevel {
                root_level_assumption_literal: !lits[0],
            }),
        )
    }

    #[test]
    fn simple_core_extraction_1_2() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[1], !lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(Core::RootLevel {
                root_level_assumption_literal: !lits[1],
            }),
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
            Ok(Core::Empty),
        );
    }

    #[test]
    fn simple_core_extraction_1_core_before_inconsistency() {
        let (solver, lits) = create_instance1();
        run_test(
            solver,
            vec![!lits[1], lits[1]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(Core::Standard {
                negated_assumption_literals: vec![lits[1]],
            }), // The core gets computed before inconsistency is detected
        );
    }

    fn create_instance2() -> (ConstraintSatisfactionSolver, Vec<Literal>) {
        let mut solver = ConstraintSatisfactionSolver::default();
        let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
        let lit2 = Literal::new(solver.create_new_propositional_variable(None), true);
        let lit3 = Literal::new(solver.create_new_propositional_variable(None), true);

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
            Ok(Core::Standard {
                negated_assumption_literals: vec![lits[0], !lits[1], lits[2]],
            }),
        );
    }

    #[test]
    fn simple_core_extraction_2_long_assumptions_with_inconsistency_at_the_end() {
        let (solver, lits) = create_instance2();
        run_test(
            solver,
            vec![!lits[0], lits[1], !lits[2], lits[0]],
            CSPSolverExecutionFlag::Infeasible,
            Ok(Core::Standard {
                negated_assumption_literals: vec![lits[0], !lits[1], lits[2]],
            }), /* could return inconsistent assumptions,
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
        let lit1 = Literal::new(solver.create_new_propositional_variable(None), true);
        let lit2 = Literal::new(solver.create_new_propositional_variable(None), true);
        let lit3 = Literal::new(solver.create_new_propositional_variable(None), true);
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
            Ok(Core::Standard {
                negated_assumption_literals: vec![lits[0], lits[1], lits[2]],
            }),
        );
    }

    #[test]
    fn simple_core_extraction_3_2() {
        let (solver, lits) = create_instance3();
        run_test(
            solver,
            vec![!lits[0], !lits[1]],
            CSPSolverExecutionFlag::Feasible,
            Ok(Core::Empty), // will be ignored in the test
        );
    }

    #[test]
    fn negative_upper_bound() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let domain_id = solver.create_new_integer_variable(0, 10, None);

        let result = solver.get_literal(predicate![domain_id <= -2]);
        assert_eq!(result, solver.assignments_propositional.false_literal);
    }

    #[test]
    fn lower_bound_literal_lower_than_lower_bound_should_be_true_literal() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let domain_id = solver.create_new_integer_variable(0, 10, None);
        let result = solver.get_literal(predicate![domain_id >= -2]);
        assert_eq!(result, solver.assignments_propositional.true_literal);
    }

    #[test]
    fn new_domain_with_negative_lower_bound() {
        let lb = -2;
        let ub = 2;

        let mut solver = ConstraintSatisfactionSolver::default();
        let domain_id = solver.create_new_integer_variable(lb, ub, None);

        assert_eq!(lb, solver.assignments_integer.get_lower_bound(domain_id));

        assert_eq!(ub, solver.assignments_integer.get_upper_bound(domain_id));

        assert_eq!(
            solver.assignments_propositional.true_literal,
            solver.get_literal(predicate![domain_id >= lb])
        );

        assert_eq!(
            solver.assignments_propositional.false_literal,
            solver.get_literal(predicate![domain_id <= lb - 1])
        );

        assert!(solver
            .assignments_propositional
            .is_literal_unassigned(solver.get_literal(predicate![domain_id == lb])));

        assert_eq!(
            solver.assignments_propositional.false_literal,
            solver.get_literal(predicate![domain_id == lb - 1])
        );

        for value in (lb + 1)..ub {
            let literal = solver.get_literal(predicate![domain_id >= value]);

            assert!(solver
                .assignments_propositional
                .is_literal_unassigned(literal));

            assert!(solver
                .assignments_propositional
                .is_literal_unassigned(solver.get_literal(predicate![domain_id == value])));
        }

        assert_eq!(
            solver.assignments_propositional.false_literal,
            solver.get_literal(predicate![domain_id >= ub + 1])
        );
        assert_eq!(
            solver.assignments_propositional.true_literal,
            solver.get_literal(predicate![domain_id <= ub])
        );
        assert!(solver
            .assignments_propositional
            .is_literal_unassigned(solver.get_literal(predicate![domain_id == ub])));
        assert_eq!(
            solver.assignments_propositional.false_literal,
            solver.get_literal(predicate![domain_id == ub + 1])
        );
    }

    #[test]
    fn clausal_propagation_is_synced_until_right_before_conflict() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let domain_id = solver.create_new_integer_variable(0, 10, None);
        let dummy_reason = ReasonRef(0);

        let result =
            solver
                .assignments_integer
                .tighten_lower_bound(domain_id, 2, Some(dummy_reason));
        assert!(result.is_ok());
        assert_eq!(solver.assignments_integer.get_lower_bound(domain_id), 2);

        let result =
            solver
                .assignments_integer
                .tighten_lower_bound(domain_id, 8, Some(dummy_reason));
        assert!(result.is_ok());
        assert_eq!(solver.assignments_integer.get_lower_bound(domain_id), 8);

        let result =
            solver
                .assignments_integer
                .tighten_lower_bound(domain_id, 12, Some(dummy_reason));
        assert!(result.is_err());
        assert_eq!(solver.assignments_integer.get_lower_bound(domain_id), 12);

        let _ = solver.synchronise_propositional_trail_based_on_integer_trail();

        for lower_bound in 0..=8 {
            let literal = solver.get_literal(predicate!(domain_id >= lower_bound));
            assert!(
                solver
                    .assignments_propositional
                    .is_literal_assigned_true(literal),
                "Literal for lower-bound {lower_bound} is not assigned"
            );
        }
    }

    #[test]
    fn check_correspondence_predicates_creating_new_int_domain() {
        let mut solver = ConstraintSatisfactionSolver::default();

        let lower_bound = 0;
        let upper_bound = 10;
        let domain_id = solver.create_new_integer_variable(lower_bound, upper_bound, None);

        for bound in lower_bound + 1..upper_bound {
            let lower_bound_predicate = predicate![domain_id >= bound];
            let equality_predicate = predicate![domain_id == bound];
            for predicate in [lower_bound_predicate, equality_predicate] {
                let literal = solver.get_literal(predicate);
                assert!(
                    solver.variable_literal_mappings.literal_to_predicates[literal]
                        .contains(&predicate.try_into().unwrap())
                )
            }
        }
    }
}
