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
use crate::engine::predicates::predicate::Predicate;
use crate::engine::termination::TerminationCondition;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::options::LearningOptions;
use crate::options::SolverOptions;
use crate::predicate;
use crate::propagators::absolute_value::AbsoluteValueConstructor;
use crate::propagators::division::DivisionConstructor;
use crate::propagators::element::ElementConstructor;
use crate::propagators::integer_multiplication::IntegerMultiplicationConstructor;
use crate::propagators::linear_less_or_equal::LinearLessOrEqualConstructor;
use crate::propagators::linear_not_equal::LinearNotEqualConstructor;
use crate::propagators::maximum::MaximumConstructor;
use crate::propagators::ArgTask;
use crate::propagators::ReifiedPropagatorConstructor;
use crate::propagators::TimeTableOverInterval;
use crate::pumpkin_assert_simple;
use crate::results::solution_iterator::SolutionIterator;
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;
use crate::statistics::log_statistic;
use crate::statistics::log_statistic_postfix;
use crate::variables::PropositionalVariable;
use crate::variables::TransformableVariable;

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
/// For examples on how to use the solver, see the [root-level crate documentation](crate) or the
/// examples in the repository at `pumpkin-lib/examples`.
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
                let solution_reference = self.satisfaction_solver.get_solution_reference();
                brancher.on_solution(solution_reference);
                SatisfactionResult::Satisfiable(solution_reference.into())
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
                let solution = self.satisfaction_solver.get_solution_reference().into();
                // Reset the state whenever we return a result
                self.satisfaction_solver.restore_state_at_root(brancher);
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

    /// Solver the model currently in the [`Solver`] to optimality where the provided
    /// `objective_variable` is minmised (or is indicated to terminate by the provided
    /// [`TerminationCondition`]). It returns an [`OptimisationResult`] which can be used to
    /// retrieve the optimal solution if it exists.
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
    /// [`TerminationCondition`]). It returns an [`OptimisationResult`] which can be used to
    /// retrieve the optimal solution if it exists.
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

    /// Adds the [element](https://sofdem.github.io/gccat/gccat/Celement.html) constraint which states that `array[index] = rhs`.
    pub fn element<ElementVar: IntegerVariable + 'static>(
        &mut self,
        index: impl IntegerVariable + 'static,
        array: impl Into<Box<[ElementVar]>>,
        rhs: impl IntegerVariable + 'static,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver.add_propagator(ElementConstructor {
            index,
            array: array.into(),
            rhs,
        })
    }

    /// Adds the constraint `\sum terms_i != rhs`.
    pub fn not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(LinearNotEqualConstructor::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif <-> \sum terms_i != rhs`.
    pub fn reified_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: Box<[Var]>,
        rhs: i32,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_not_equals(terms.clone(), rhs, reif)?;
        self.half_reified_equals(terms, rhs, !reif)
    }

    /// Adds the constraint `reif -> \sum terms_i != rhs`.
    pub fn half_reified_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(ReifiedPropagatorConstructor {
                propagator: LinearNotEqualConstructor::new(terms.into(), rhs),
                reification_literal: reif,
            })
    }

    /// Adds the constraint `\sum terms_i <= rhs`.
    pub fn less_than_or_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(LinearLessOrEqualConstructor::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif <-> (\sum terms_i <= rhs)`.
    pub fn reified_less_than_or_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: Box<[Var]>,
        rhs: i32,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_less_than_or_equals(terms.clone(), rhs, reif)?;
        self.half_reified_less_than_or_equals(
            terms.iter().map(|term| term.scaled(-1)).collect::<Vec<_>>(),
            -rhs - 1,
            !reif,
        )
    }

    /// Adds the constraint `reif -> (\sum terms_i <= rhs)`.
    pub fn half_reified_less_than_or_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(ReifiedPropagatorConstructor {
                propagator: LinearLessOrEqualConstructor::new(terms.into(), rhs),
                reification_literal: reif,
            })
    }

    /// Adds the constraint `\sum terms_i = rhs`.
    pub fn equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> Result<(), ConstraintOperationError> {
        let terms = terms.into();

        self.less_than_or_equals(terms.clone(), rhs)?;

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.less_than_or_equals(negated, -rhs)
    }

    /// Adds the constraint `reif <-> (\sum terms_i = rhs)`.
    pub fn reified_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: Box<[Var]>,
        rhs: i32,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_equals(terms.clone(), rhs, reif)?;
        self.half_reified_not_equals(terms, rhs, !reif)
    }

    /// Adds the constraint `reif -> (\sum terms_i = rhs)`.
    pub fn half_reified_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let terms = terms.into();

        self.half_reified_less_than_or_equals(terms.clone(), rhs, reif)?;

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.half_reified_less_than_or_equals(negated, -rhs, reif)
    }

    /// Adds the constraint `lhs != rhs`.
    pub fn binary_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
    ) -> Result<(), ConstraintOperationError> {
        self.not_equals([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif <-> lhs != rhs`.
    pub fn reified_binary_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        a: Var,
        b: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_binary_not_equals(a.clone(), b.clone(), reif)?;
        self.half_reified_binary_equals(a, b, !reif)
    }

    /// Adds the constraint `reif -> lhs != rhs`.
    pub fn half_reified_binary_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_not_equals([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs <= rhs`.
    pub fn binary_less_than_or_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
    ) -> Result<(), ConstraintOperationError> {
        self.less_than_or_equals([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif <-> (lhs <= rhs)`.
    pub fn reified_binary_less_than_or_equals<Var: IntegerVariable + 'static>(
        &mut self,
        a: Var,
        b: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.reified_less_than_or_equals(vec![a.scaled(1), b.scaled(-1)].into(), 0, reif)
    }

    /// Adds the constraint `reif -> (lhs <= rhs)`.
    pub fn half_reified_binary_less_than_or_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_less_than_or_equals([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs < rhs`.
    pub fn binary_less_than<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
    ) -> Result<(), ConstraintOperationError> {
        self.binary_less_than_or_equals(lhs.scaled(1), rhs.offset(-1))
    }

    pub fn reified_binary_less_than<Var: IntegerVariable + 'static>(
        &mut self,
        a: Var,
        b: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.reified_less_than_or_equals(vec![a.scaled(1), b.scaled(-1)].into(), -1, reif)
    }

    /// Adds the constraint `reif -> (lhs < rhs)`.
    pub fn half_reified_binary_less_than<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_binary_less_than_or_equals(lhs.scaled(1), rhs.offset(-1), reif)
    }

    /// Adds the constraint `lhs = rhs`.
    pub fn binary_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
    ) -> Result<(), ConstraintOperationError> {
        self.equals([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif <-> (lhs = rhs)`.
    pub fn reified_binary_equals<Var: IntegerVariable + 'static>(
        &mut self,
        a: Var,
        b: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.reified_equals(vec![a.scaled(1), b.scaled(-1)].into(), 0, reif)
    }

    /// Adds the constraint `reif -> (lhs = rhs)`.
    pub fn half_reified_binary_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.half_reified_equals([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `a + b = c`.
    pub fn plus<Var: IntegerVariable + 'static>(
        &mut self,
        a: Var,
        b: Var,
        c: Var,
    ) -> Result<(), ConstraintOperationError> {
        self.equals([a.scaled(1), b.scaled(1), c.scaled(-1)], 0)
    }

    /// Adds the constraint `a * b = c`.
    pub fn times(
        &mut self,
        a: impl IntegerVariable + 'static,
        b: impl IntegerVariable + 'static,
        c: impl IntegerVariable + 'static,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(IntegerMultiplicationConstructor { a, b, c })
    }

    /// A propagator for maintaining the constraint `numerator / denominator = rhs`; note that this
    /// propagator performs truncating division (i.e. rounding towards 0).
    ///
    /// The propagator assumes that the `denominator` is a non-zero integer.
    ///
    /// The implementation is ported from [OR-tools](https://github.com/google/or-tools/blob/870edf6f7bff6b8ff0d267d936be7e331c5b8c2d/ortools/sat/integer_expr.cc#L1209C1-L1209C19).
    pub fn division(
        &mut self,
        numerator: impl IntegerVariable + 'static,
        denominator: impl IntegerVariable + 'static,
        rhs: impl IntegerVariable + 'static,
    ) -> Result<(), ConstraintOperationError>
    where
        Self: Sized,
    {
        self.satisfaction_solver
            .add_propagator(DivisionConstructor {
                numerator,
                denominator,
                rhs,
            })
    }

    /// Adds the constraint `|signed| = absolute`.
    pub fn absolute(
        &mut self,
        signed: impl IntegerVariable + 'static,
        absolute: impl IntegerVariable + 'static,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(AbsoluteValueConstructor { signed, absolute })
    }

    /// Adds the constraint that all variables must be distinct.
    pub fn all_different<Var: IntegerVariable + 'static>(
        &mut self,
        variables: impl Into<Box<[Var]>>,
    ) -> Result<(), ConstraintOperationError> {
        let variables = variables.into();

        for i in 0..variables.len() {
            for j in i + 1..variables.len() {
                self.binary_not_equals(variables[i].clone(), variables[j].clone())?;
            }
        }

        Ok(())
    }

    /// Posts the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint.
    /// This constraint ensures that at no point in time, the cumulative resource usage of the tasks
    /// exceeds `bound`.
    ///
    /// The implementation uses a form of time-table reasoning (for an example of this type of
    /// reasoning, see \[1], note that it does **not** implement the specific algorithm in the paper
    /// but that the reasoning used is the same).
    ///
    /// The length of `start_times`, `durations` and `resource_requirements` should be the same; if
    /// this is not the case then this method will panic.
    ///
    /// # Example
    /// ```rust
    /// // We construct three tasks for a resource with capacity 2:
    /// // - Task 0: Start times: [0, 5], Processing time: 4, Resource usage: 1
    /// // - Task 1: Start times: [0, 5], Processing time: 2, Resource usage: 1
    /// // - Task 2: Start times: [0, 5], Processing time: 4, Resource usage: 2
    /// // We can infer that Task 0 and Task 1 execute at the same time
    /// // while Task 2 will start after them
    /// # use pumpkin_lib::termination::Indefinite;
    /// # use pumpkin_lib::Solver;
    /// # use pumpkin_lib::results::SatisfactionResult;
    /// # use crate::pumpkin_lib::results::ProblemSolution;
    /// let solver = Solver::default();
    ///
    /// let mut solver = Solver::default();
    ///
    /// let start_0 = solver.new_bounded_integer(0, 4);
    /// let start_1 = solver.new_bounded_integer(0, 4);
    /// let start_2 = solver.new_bounded_integer(0, 5);
    ///
    /// let start_times = [start_0, start_1, start_2];
    /// let durations = [5, 2, 5];
    /// let resource_requirements = [1, 1, 2];
    /// let resource_capacity = 2;
    ///
    /// solver.cumulative(
    ///     &start_times,
    ///     &durations,
    ///     &resource_requirements,
    ///     resource_capacity,
    ///     false,
    /// );
    ///
    /// let mut termination = Indefinite;
    /// let mut brancher = solver.default_brancher_over_all_propositional_variables();
    ///
    /// let result = solver.satisfy(&mut brancher, &mut termination);
    ///
    /// // We check whether the result was feasible
    /// if let SatisfactionResult::Satisfiable(solution) = result {
    ///     let horizon = durations.iter().sum::<i32>();
    ///     let start_times = [start_0, start_1, start_2];
    ///
    ///     // Now we check whether the resource constraint is satisfied at each time-point t
    ///     assert!((0..=horizon).all(|t| {
    ///         // We gather all of the resource usages at the current time t
    ///         let resource_usage_at_t = start_times
    ///             .iter()
    ///             .enumerate()
    ///             .filter_map(|(task_index, start_time)| {
    ///                 if solution.get_integer_value(*start_time) <= t
    ///                     && solution.get_integer_value(*start_time) + durations[task_index] > t
    ///                 {
    ///                     Some(resource_requirements[task_index])
    ///                 } else {
    ///                     None
    ///                 }
    ///             })
    ///             .sum::<i32>();
    ///         // Then we check whether the resource usage at the current time point is lower than
    ///         // the resource capacity
    ///         resource_usage_at_t <= resource_capacity
    ///     }));
    ///
    ///     // Finally we check whether Task 2 starts after Task 0 and Task 1 and that Task 0 and
    ///     // Task 1 overlap
    ///     assert!(
    ///         solution.get_integer_value(start_2)
    ///             >= solution.get_integer_value(start_0) + durations[0]
    ///             && solution.get_integer_value(start_2)
    ///                 >= solution.get_integer_value(start_1) + durations[1]
    ///     );
    ///     assert!(
    ///         solution.get_integer_value(start_0)
    ///             < solution.get_integer_value(start_1) + durations[1]
    ///             && solution.get_integer_value(start_1)
    ///                 < solution.get_integer_value(start_0) + durations[0]
    ///     );
    /// }
    /// ```
    ///
    /// # Bibliography
    /// \[1\] S. Gay, R. Hartert, and P. Schaus, ‘Simple and scalable time-table filtering for the
    /// cumulative constraint’, in Principles and Practice of Constraint Programming: 21st
    /// International Conference, CP 2015, Cork, Ireland, August 31--September 4, 2015, Proceedings
    /// 21, 2015, pp. 149–157.
    pub fn cumulative<Var: IntegerVariable + 'static + std::fmt::Debug + Copy>(
        &mut self,
        start_times: &[Var],
        durations: &[i32],
        resource_requirements: &[i32],
        resource_capacity: i32,
        allow_holes_in_domain: bool,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_simple!(
            start_times.len() == durations.len() && durations.len() == resource_requirements.len(),
            "The number of start variables, durations and resource requirements should be the same!car"
        );
        self.satisfaction_solver
            .add_propagator(TimeTableOverInterval::new(
                start_times
                    .iter()
                    .zip(durations)
                    .zip(resource_requirements)
                    .map(|((start_time, duration), resource_requirement)| ArgTask {
                        start_time: *start_time,
                        processing_time: *duration,
                        resource_usage: *resource_requirement,
                    })
                    .collect(),
                resource_capacity,
                allow_holes_in_domain,
            ))
    }

    /// Posts the constraint `max(array) = m`.
    pub fn maximum<Var: IntegerVariable + 'static>(
        &mut self,
        array: impl Into<Box<[Var]>>,
        rhs: impl IntegerVariable + 'static,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver.add_propagator(MaximumConstructor {
            array: array.into(),
            rhs,
        })
    }

    /// Posts the constraint `min(array) = m`.
    pub fn minimum<Var: IntegerVariable + 'static>(
        &mut self,
        array: impl IntoIterator<Item = Var>,
        rhs: impl IntegerVariable + 'static,
    ) -> Result<(), ConstraintOperationError> {
        let array = array
            .into_iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<_>>();
        self.maximum(array, rhs.scaled(-1))
    }

    /// Posts the constraint `reif <-> \/ clause`
    pub fn reified_clause(
        &mut self,
        clause: impl Into<Vec<Literal>>,
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let mut clause = clause.into();

        // \/clause -> r
        clause
            .iter()
            .try_for_each(|&literal| self.add_clause([!literal, reif]))?;

        // r -> \/clause
        clause.insert(0, !reif);

        self.add_clause(clause)
    }

    /// Posts the constraint `reif <-> /\ literal_i`
    pub fn reified_conjunction(
        &mut self,
        conjunction: &[Literal],
        reif: Literal,
    ) -> Result<(), ConstraintOperationError> {
        // /\conjunction -> r
        let clause: Vec<Literal> = conjunction
            .iter()
            .map(|&literal| !literal)
            .chain(std::iter::once(reif))
            .collect();
        self.add_clause(clause)?;

        // r -> /\conjunction
        conjunction
            .iter()
            .try_for_each(|&literal| self.add_clause([!reif, literal]))?;
        Ok(())
    }

    /// Posts the constraint `\sum weights_i * bools_i <= rhs`.
    pub fn boolean_less_than_or_equals(
        &mut self,
        weights: &[i32],
        bools: &[Literal],
        rhs: i32,
    ) -> Result<(), ConstraintOperationError> {
        let domains = bools
            .iter()
            .enumerate()
            .map(|(index, bool)| {
                let corresponding_domain_id = self.new_bounded_integer(0, 1);
                // bool -> [domain = 1]
                let _ = self.add_clause([
                    !*bool,
                    self.get_literal(predicate![corresponding_domain_id >= 1]),
                ]);
                // !bool -> [domain = 0]
                let _ = self.add_clause([
                    *bool,
                    self.get_literal(predicate![corresponding_domain_id <= 0]),
                ]);
                corresponding_domain_id.scaled(weights[index])
            })
            .collect::<Vec<_>>();
        self.less_than_or_equals(domains, rhs)
    }

    /// Posts the constraint `\sum weights_i * bools_i <= rhs`.
    pub fn boolean_equals(
        &mut self,
        weights: &[i32],
        bools: &[Literal],
        rhs: DomainId,
    ) -> Result<(), ConstraintOperationError> {
        let domains = bools
            .iter()
            .enumerate()
            .map(|(index, bool)| {
                let corresponding_domain_id = self.new_bounded_integer(0, 1);
                // bool -> [domain = 1]
                let _ = self.add_clause([
                    !*bool,
                    self.get_literal(predicate![corresponding_domain_id >= 1]),
                ]);
                // !bool -> [domain = 0]
                let _ = self.add_clause([
                    *bool,
                    self.get_literal(predicate![corresponding_domain_id <= 0]),
                ]);
                corresponding_domain_id.scaled(weights[index])
            })
            .chain(std::iter::once(rhs.scaled(-1)))
            .collect::<Vec<_>>();
        self.equals(domains, 0)
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
