use super::results::OptimisationResult;
use super::results::SatisfactionResult;
use super::results::SatisfactionResultUnderAssumptions;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashSet;
use crate::basic_types::Solution;
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
#[cfg(doc)]
use crate::branching::value_selection::ValueSelector;
#[cfg(doc)]
use crate::branching::variable_selection::VariableSelector;
use crate::branching::Brancher;
use crate::branching::PhaseSaving;
use crate::branching::SolutionGuidedValueSelector;
use crate::branching::Vsids;
use crate::constraints::ConstraintPoster;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::termination::TerminationCondition;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::options::LearningOptions;
use crate::options::SolverOptions;
use crate::pumpkin_assert_simple;
use crate::results::solution_iterator::SolutionIterator;
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;
use crate::statistics::log_statistic;
use crate::statistics::log_statistic_postfix;
use crate::variables::PropositionalVariable;

/// The main interaction point which allows the creation of variables, the addition of constraints,
/// and solving problems.
///
///
/// # Creating Variables
/// As stated in [`crate::variables`], we can create two types of variables: propositional variables
/// and integer variables.
///
/// ```rust
/// # use pumpkin_lib::Solver;
/// # use crate::pumpkin_lib::variables::TransformableVariable;
/// let mut solver = Solver::default();
///
/// // Integer Variables
///
/// // We can create an integer variable with a domain in the range [0, 10]
/// let integer_between_bounds = solver.new_bounded_integer(0, 10);
///
/// // We can also create such a variable with a name
/// let named_integer_between_bounds = solver.new_named_bounded_integer(0, 10, "x");
///
/// // We can also create an integer variable with a non-continuous domain in the follow way
/// let mut sparse_integer = solver.new_sparse_integer(vec![0, 3, 5]);
///
/// // We can also create such a variable with a name
/// let named_sparse_integer = solver.new_named_sparse_integer(vec![0, 3, 5], "y");
///
/// // Additionally, we can also create an affine view over a variable with both a scale and an offset (or either)
/// let view_over_integer = integer_between_bounds.scaled(-1).offset(15);
///
///
/// // Propositional Variable
///
/// // We can create a literal
/// let literal = solver.new_literal();
///
/// // We can also create such a variable with a name
/// let named_literal = solver.new_named_literal("z");
///
/// // We can also get the propositional variable from the literal
/// let propositional_variable = literal.get_propositional_variable();
///
/// // We can also create an iterator of new literals and get a number of them at once
/// let list_of_5_literals = solver.new_literals().take(5).collect::<Vec<_>>();
/// assert_eq!(list_of_5_literals.len(), 5);
/// ```
///
/// # Using the Solver
/// For examples on how to use the solver, see the [root-level crate documentation](crate) or [one of these examples](https://github.com/ConSol-Lab/Pumpkin/tree/master/pumpkin-lib/examples).
pub struct Solver {
    /// The internal [`ConstraintSatisfactionSolver`] which is used to solve the problems.
    satisfaction_solver: ConstraintSatisfactionSolver,
    /// The function is called whenever an optimisation function finds a solution; see
    /// [`Solver::with_solution_callback`].
    solution_callback: Box<dyn Fn(&Solution)>,
}

impl Default for Solver {
    fn default() -> Self {
        Self {
            satisfaction_solver: Default::default(),
            solution_callback: create_empty_function(),
        }
    }
}

/// Creates a place-holder empty function which does not do anything when a solution is found.
fn create_empty_function() -> Box<dyn Fn(&Solution)> {
    Box::new(|_| {})
}

impl std::fmt::Debug for Solver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Solver")
            .field("satisfaction_solver", &self.satisfaction_solver)
            .finish()
    }
}

impl Solver {
    /// Creates a solver with the provided [`LearningOptions`] and [`SolverOptions`].
    pub fn with_options(learning_options: LearningOptions, solver_options: SolverOptions) -> Self {
        Solver {
            satisfaction_solver: ConstraintSatisfactionSolver::new(
                learning_options,
                solver_options,
            ),
            solution_callback: create_empty_function(),
        }
    }

    /// Adds a call-back to the [`Solver`] which is called every time that a solution is found when
    /// optimising using [`Solver::maximise`] or [`Solver::minimise`].
    ///
    /// Note that this will also
    /// perform the call-back on the optimal solution which is returned in
    /// [`OptimisationResult::Optimal`].
    pub fn with_solution_callback(&mut self, solution_callback: impl Fn(&Solution) + 'static) {
        self.solution_callback = Box::new(solution_callback);
    }

    /// Logs the statistics currently present in the solver with the provided objective value.
    pub fn log_statistics_with_objective(&self, objective_value: i64) {
        log_statistic("objective", objective_value);
        self.log_statistics();
    }

    /// Logs the statistics currently present in the solver.
    pub fn log_statistics(&self) {
        self.satisfaction_solver.log_statistics();
        log_statistic_postfix();
    }
}

/// Methods to retrieve information about variables
impl Solver {
    /// Get the literal corresponding to the given predicate. As the literal may need to be
    /// created, this possibly mutates the solver.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// # use pumpkin_lib::predicate;
    /// let mut solver = Solver::default();
    ///
    /// let x = solver.new_bounded_integer(0, 10);
    ///
    /// // We can get the literal representing the predicate `[x >= 3]` via the Solver
    /// let literal = solver.get_literal(predicate!(x >= 3));
    ///
    /// // Note that we can also get a literal which is always true
    /// let true_lower_bound_literal = solver.get_literal(predicate!(x >= 0));
    /// assert_eq!(true_lower_bound_literal, solver.get_true_literal());
    /// ```
    pub fn get_literal(&self, predicate: Predicate) -> Literal {
        self.satisfaction_solver.get_literal(predicate)
    }

    /// Get the value of the given [`Literal`] at the root level (after propagation), which could be
    /// unassigned.
    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        self.satisfaction_solver.get_literal_value(literal)
    }

    /// Get a literal which is globally true.
    pub fn get_true_literal(&self) -> Literal {
        self.satisfaction_solver.get_true_literal()
    }

    /// Get a literal which is globally false.
    pub fn get_false_literal(&self) -> Literal {
        self.satisfaction_solver.get_false_literal()
    }

    /// Get the lower-bound of the given [`IntegerVariable`] at the root level (after propagation).
    pub fn lower_bound(&self, variable: &impl IntegerVariable) -> i32 {
        self.satisfaction_solver.get_lower_bound(variable)
    }

    /// Get the upper-bound of the given [`IntegerVariable`] at the root level (after propagation).
    pub fn upper_bound(&self, variable: &impl IntegerVariable) -> i32 {
        self.satisfaction_solver.get_upper_bound(variable)
    }
}

/// Functions to create and retrieve integer and propositional variables.
impl Solver {
    /// Returns an infinite iterator of positive literals of new variables. The new variables will
    /// be unnamed.
    ///
    /// # Example
    /// ```
    /// # use pumpkin_lib::Solver;
    /// # use pumpkin_lib::variables::Literal;
    /// let mut solver = Solver::default();
    /// let literals: Vec<Literal> = solver.new_literals().take(5).collect();
    ///
    /// // `literals` contains 5 positive literals of newly created propositional variables.
    /// assert_eq!(literals.len(), 5);
    /// ```
    ///
    /// Note that this method captures the lifetime of the immutable reference to `self`.
    pub fn new_literals(&mut self) -> impl Iterator<Item = Literal> + '_ {
        std::iter::from_fn(|| Some(self.new_literal()))
    }

    /// Create a fresh propositional variable and return the literal with positive polarity.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can create a literal
    /// let literal = solver.new_literal();
    /// ```
    pub fn new_literal(&mut self) -> Literal {
        Literal::new(
            self.satisfaction_solver
                .create_new_propositional_variable(None),
            true,
        )
    }

    /// Create a fresh propositional variable with a given name and return the literal with positive
    /// polarity.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can also create such a variable with a name
    /// let named_literal = solver.new_named_literal("z");
    /// ```
    pub fn new_named_literal(&mut self, name: impl Into<String>) -> Literal {
        Literal::new(
            self.satisfaction_solver
                .create_new_propositional_variable(Some(name.into())),
            true,
        )
    }

    /// Create a new integer variable with the given bounds.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can create an integer variable with a domain in the range [0, 10]
    /// let integer_between_bounds = solver.new_bounded_integer(0, 10);
    /// ```
    pub fn new_bounded_integer(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        self.satisfaction_solver
            .create_new_integer_variable(lower_bound, upper_bound, None)
    }

    /// Create a new named integer variable with the given bounds.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can also create such a variable with a name
    /// let named_integer_between_bounds = solver.new_named_bounded_integer(0, 10, "x");
    /// ```
    pub fn new_named_bounded_integer(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: impl Into<String>,
    ) -> DomainId {
        self.satisfaction_solver.create_new_integer_variable(
            lower_bound,
            upper_bound,
            Some(name.into()),
        )
    }

    /// Create a new integer variable which has a domain of predefined values. We remove duplicates
    /// by converting to a hash set
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can also create an integer variable with a non-continuous domain in the follow way
    /// let mut sparse_integer = solver.new_sparse_integer(vec![0, 3, 5]);
    /// ```
    pub fn new_sparse_integer(&mut self, values: impl Into<Vec<i32>>) -> DomainId {
        let values: HashSet<i32> = values.into().into_iter().collect();

        self.satisfaction_solver
            .create_new_integer_variable_sparse(values.into_iter().collect(), None)
    }

    /// Create a new named integer variable which has a domain of predefined values.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can also create such a variable with a name
    /// let named_sparse_integer = solver.new_named_sparse_integer(vec![0, 3, 5], "y");
    /// ```
    pub fn new_named_sparse_integer(
        &mut self,
        values: impl Into<Vec<i32>>,
        name: impl Into<String>,
    ) -> DomainId {
        self.satisfaction_solver
            .create_new_integer_variable_sparse(values.into(), Some(name.into()))
    }
}

/// Functions for solving with the constraints that have been added to the [`Solver`].
impl Solver {
    /// Solves the current model in the [`Solver`] until it finds a solution (or is indicated to
    /// terminate by the provided [`TerminationCondition`]) and returns a [`SatisfactionResult`]
    /// which can be used to obtain the found solution or find other solutions.
    pub fn satisfy<B: Brancher, T: TerminationCondition>(
        &mut self,
        brancher: &mut B,
        termination: &mut T,
    ) -> SatisfactionResult {
        match self.satisfaction_solver.solve(termination, brancher) {
            CSPSolverExecutionFlag::Feasible => {
                let solution: Solution = self.satisfaction_solver.get_solution_reference().into();
                self.satisfaction_solver.restore_state_at_root(brancher);
                brancher.on_solution(solution.as_reference());
                SatisfactionResult::Satisfiable(solution)
            }
            CSPSolverExecutionFlag::Infeasible => {
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                SatisfactionResult::Unsatisfiable
            }
            CSPSolverExecutionFlag::Timeout => {
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                SatisfactionResult::Unknown
            }
        }
    }

    pub fn get_solution_iterator<
        'this,
        'brancher,
        'termination,
        B: Brancher,
        T: TerminationCondition,
    >(
        &'this mut self,
        brancher: &'brancher mut B,
        termination: &'termination mut T,
    ) -> SolutionIterator<'this, 'brancher, 'termination, B, T> {
        SolutionIterator::new(&mut self.satisfaction_solver, brancher, termination)
    }

    /// Solves the current model in the [`Solver`] until it finds a solution (or is indicated to
    /// terminate by the provided [`TerminationCondition`]) and returns a [`SatisfactionResult`]
    /// which can be used to obtain the found solution or find other solutions.
    ///
    /// This method takes as input a list of [`Literal`]s which represent so-called assumptions (see
    /// \[1\] for a more detailed explanation). The [`Literal`]s corresponding to [`Predicate`]s
    /// over [`IntegerVariable`]s (e.g. lower-bound predicates) can be retrieved from the [`Solver`]
    /// using [`Solver::get_literal`].
    ///
    /// # Bibliography
    /// \[1\] N. Eén and N. Sörensson, ‘Temporal induction by incremental SAT solving’, Electronic
    /// Notes in Theoretical Computer Science, vol. 89, no. 4, pp. 543–560, 2003.
    pub fn satisfy_under_assumptions<'this, 'brancher, B: Brancher, T: TerminationCondition>(
        &'this mut self,
        brancher: &'brancher mut B,
        termination: &mut T,
        assumptions: &[Literal],
    ) -> SatisfactionResultUnderAssumptions<'this, 'brancher, B> {
        match self
            .satisfaction_solver
            .solve_under_assumptions(assumptions, termination, brancher)
        {
            CSPSolverExecutionFlag::Feasible => {
                let solution: Solution = self.satisfaction_solver.get_solution_reference().into();
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                brancher.on_solution(solution.as_reference());
                SatisfactionResultUnderAssumptions::Satisfiable(solution)
            }
            CSPSolverExecutionFlag::Infeasible => {
                if self
                    .satisfaction_solver
                    .state
                    .is_infeasible_under_assumptions()
                {
                    // The state is automatically reset when we return this result
                    SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(
                        UnsatisfiableUnderAssumptions::new(&mut self.satisfaction_solver, brancher),
                    )
                } else {
                    // Reset the state whenever we return a result
                    self.satisfaction_solver.restore_state_at_root(brancher);
                    SatisfactionResultUnderAssumptions::Unsatisfiable
                }
            }
            CSPSolverExecutionFlag::Timeout => {
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                SatisfactionResultUnderAssumptions::Unknown
            }
        }
    }

    /// Solves the model currently in the [`Solver`] to optimality where the provided
    /// `objective_variable` is minimised (or is indicated to terminate by the provided
    /// [`TerminationCondition`]).
    ///
    /// It returns an [`OptimisationResult`] which can be used to retrieve the optimal solution if
    /// it exists.
    pub fn minimise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
    ) -> OptimisationResult {
        self.minimise_internal(brancher, termination, objective_variable, false)
    }

    /// Solves the model currently in the [`Solver`] to optimality where the provided
    /// `objective_variable` is maximised (or is indicated to terminate by the provided
    /// [`TerminationCondition`]).
    ///
    /// It returns an [`OptimisationResult`] which can be used to retrieve the optimal solution if
    /// it exists.
    pub fn maximise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
    ) -> OptimisationResult {
        self.minimise_internal(brancher, termination, objective_variable.scaled(-1), true)
    }

    /// The internal method which optimizes the objective function, this function takes an extra
    /// argument (`is_maximising`) as compared to [`Solver::maximise`] and [`Solver::minimise`]
    /// which determines whether the logged objective value should be scaled by `-1` or not.
    ///
    /// This is necessary due to the fact that [`Solver::maximise`] simply calls minimise with
    /// the objective variable scaled with `-1` which would lead to incorrect statistic if not
    /// scaled back.
    fn minimise_internal(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
        is_maximising: bool,
    ) -> OptimisationResult {
        // If we are maximising then when we simply scale the variable by -1, however, this will
        // lead to the printed objective value in the statistics to be multiplied by -1; this
        // objective_multiplier ensures that the objective is correctly logged.
        let objective_multiplier = if is_maximising { -1 } else { 1 };

        let initial_solve = self.satisfaction_solver.solve(termination, brancher);
        match initial_solve {
            CSPSolverExecutionFlag::Feasible => {}
            CSPSolverExecutionFlag::Infeasible => {
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                return OptimisationResult::Unsatisfiable;
            }
            CSPSolverExecutionFlag::Timeout => {
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                return OptimisationResult::Unknown;
            }
        }
        let mut best_objective_value = Default::default();
        let mut best_solution = Solution::default();

        self.process_solution(
            objective_multiplier,
            &objective_variable,
            &mut best_objective_value,
            &mut best_solution,
            brancher,
        );
        loop {
            self.satisfaction_solver.restore_state_at_root(brancher);

            if self
                .strengthen(
                    &objective_variable,
                    best_objective_value * objective_multiplier as i64,
                )
                .is_err()
            {
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
                return OptimisationResult::Optimal(best_solution);
            }

            let solve_result = self.satisfaction_solver.solve(termination, brancher);
            match solve_result {
                CSPSolverExecutionFlag::Feasible => {
                    self.debug_bound_change(
                        &objective_variable,
                        best_objective_value * objective_multiplier as i64,
                    );
                    self.process_solution(
                        objective_multiplier,
                        &objective_variable,
                        &mut best_objective_value,
                        &mut best_solution,
                        brancher,
                    );
                }
                CSPSolverExecutionFlag::Infeasible => {
                    {
                        // Reset the state whenever we return a result
                        self.satisfaction_solver.restore_state_at_root(brancher);
                        return OptimisationResult::Optimal(best_solution);
                    }
                }
                CSPSolverExecutionFlag::Timeout => {
                    // Reset the state whenever we return a result
                    self.satisfaction_solver.restore_state_at_root(brancher);
                    return OptimisationResult::Satisfiable(best_solution);
                }
            }
        }
    }

    /// Processes a solution when it is found, it consists of the following procedure:
    /// - Assigning `best_objective_value` the value assigned to `objective_variable` (multiplied by
    ///   `objective_multiplier`).
    /// - Storing the new best solution in `best_solution`.
    /// - Calling [`Brancher::on_solution`] on the provided `brancher`.
    /// - Logging the statistics using [`Solver::log_statistics_with_objective`].
    /// - Calling the solution callback stored in [`Solver::solution_callback`].
    fn process_solution(
        &self,
        objective_multiplier: i32,
        objective_variable: &impl IntegerVariable,
        best_objective_value: &mut i64,
        best_solution: &mut Solution,
        brancher: &mut impl Brancher,
    ) {
        *best_objective_value = (objective_multiplier
            * self
                .satisfaction_solver
                .get_assigned_integer_value(objective_variable)
                .expect("expected variable to be assigned")) as i64;
        *best_solution = self.satisfaction_solver.get_solution_reference().into();

        self.log_statistics_with_objective(*best_objective_value);
        brancher.on_solution(self.satisfaction_solver.get_solution_reference());
        (self.solution_callback)(best_solution);
    }

    /// Given the current objective value `best_objective_value`, it adds a constraint specifying
    /// that the objective value should be at most `best_objective_value - 1`. Note that it is
    /// assumed that we are always minimising the variable.
    fn strengthen(
        &mut self,
        objective_variable: &impl IntegerVariable,
        best_objective_value: i64,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_clause([self.satisfaction_solver.get_literal(
                objective_variable.upper_bound_predicate((best_objective_value - 1) as i32),
            )])
    }

    fn debug_bound_change(
        &self,
        objective_variable: &impl IntegerVariable,
        best_objective_value: i64,
    ) {
        pumpkin_assert_simple!(
            (self
                .satisfaction_solver
                .get_assigned_integer_value(objective_variable)
                .expect("expected variable to be assigned") as i64)
                < best_objective_value,
            "{}",
            format!(
                "The current bound {} should be smaller than the previous bound {}",
                self.satisfaction_solver
                    .get_assigned_integer_value(objective_variable)
                    .expect("expected variable to be assigned"),
                best_objective_value
            )
        );
    }
}

/// Functions for adding new constraints to the solver.
impl Solver {
    /// Add a constraint to the solver. This returns a [`ConstraintPoster`] which enables control
    /// on whether to add the constraint as-is, or whether to (half) reify it.
    ///
    /// If none of the methods on [`ConstraintPoster`] are used, the constraint _is not_ actually
    /// added to the solver. In this case, a warning is emitted.
    ///
    /// # Example
    /// ```
    /// # use pumpkin_lib::constraints;
    /// # use pumpkin_lib::Solver;
    /// let mut solver = Solver::default();
    ///
    /// let a = solver.new_bounded_integer(0, 3);
    /// let b = solver.new_bounded_integer(0, 3);
    ///
    /// solver.add_constraint(constraints::equals([a, b], 0)).post();
    /// ```
    pub fn add_constraint<Constraint>(
        &mut self,
        constraint: Constraint,
    ) -> ConstraintPoster<'_, Constraint> {
        ConstraintPoster::new(self, constraint)
    }

    /// Creates a clause from `literals` and adds it to the current formula.
    ///
    /// If the formula becomes trivially unsatisfiable, a [`ConstraintOperationError`] will be
    /// returned. Subsequent calls to this method will always return an error, and no
    /// modification of the solver will take place.
    pub fn add_clause(
        &mut self,
        clause: impl IntoIterator<Item = Literal>,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver.add_clause(clause)
    }

    /// Post a new propagator to the solver. If unsatisfiability can be immediately determined
    /// through propagation, this will return a [`ConstraintOperationError`].
    ///
    /// The caller should ensure the solver is in the root state before calling this, either
    /// because no call to [`Self::solve()`] has been made, or because
    /// [`Self::restore_state_at_root()`] was called.
    ///
    /// If the solver is already in a conflicting state, i.e. a previous call to this method
    /// already returned `false`, calling this again will not alter the solver in any way, and
    /// `false` will be returned again.
    pub(crate) fn add_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> Result<(), ConstraintOperationError>
    where
        Constructor: PropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        self.satisfaction_solver.add_propagator(constructor)
    }
}

/// Default brancher implementation
impl Solver {
    /// Creates a default [`IndependentVariableValueBrancher`] which uses [`Vsids`] as
    /// [`VariableSelector`] and [`SolutionGuidedValueSelector`] (with [`PhaseSaving`] as its
    /// back-up selector) as its [`ValueSelector`]; it searches over all
    /// [`PropositionalVariable`]s defined in the provided `solver`.
    pub fn default_brancher_over_all_propositional_variables(&self) -> DefaultBrancher {
        self.satisfaction_solver
            .default_brancher_over_all_propositional_variables()
    }
}

/// Proof logging methods
impl Solver {
    #[doc(hidden)]
    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) -> std::io::Result<()> {
        self.satisfaction_solver.conclude_proof_unsat()
    }

    #[doc(hidden)]
    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Literal) -> std::io::Result<()> {
        self.satisfaction_solver.conclude_proof_optimal(bound)
    }

    pub(crate) fn into_satisfaction_solver(self) -> ConstraintSatisfactionSolver {
        self.satisfaction_solver
    }
}

/// The type of [`Brancher`] which is created by
/// [`Solver::default_brancher_over_all_propositional_variables`]. It consists of the value selector
/// [`Vsids`] in combination with a [`SolutionGuidedValueSelector`] with as backup [`PhaseSaving`].
pub type DefaultBrancher = IndependentVariableValueBrancher<
    PropositionalVariable,
    Vsids<PropositionalVariable>,
    SolutionGuidedValueSelector<
        PropositionalVariable,
        bool,
        PhaseSaving<PropositionalVariable, bool>,
    >,
>;
