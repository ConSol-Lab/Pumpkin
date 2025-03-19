use std::num::NonZero;

use super::outputs::SolutionReference;
use super::results::OptimisationResult;
use super::results::SatisfactionResult;
use super::results::SatisfactionResultUnderAssumptions;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashSet;
use crate::basic_types::Solution;
use crate::branching::branchers::autonomous_search::AutonomousSearch;
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::branching::value_selection::RandomSplitter;
#[cfg(doc)]
use crate::branching::value_selection::ValueSelector;
use crate::branching::variable_selection::RandomSelector;
#[cfg(doc)]
use crate::branching::variable_selection::VariableSelector;
use crate::branching::Brancher;
use crate::constraints::ConstraintPoster;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::Propagator;
use crate::engine::termination::TerminationCondition;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::optimisation::linear_sat_unsat::LinearSatUnsat;
#[cfg(doc)]
use crate::optimisation::linear_unsat_sat::LinearUnsatSat;
use crate::optimisation::OptimisationProcedure;
use crate::options::SolverOptions;
#[cfg(doc)]
use crate::predicates;
use crate::results::solution_iterator::SolutionIterator;
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;
use crate::statistics::log_statistic;
use crate::statistics::log_statistic_postfix;

/// The main interaction point which allows the creation of variables, the addition of constraints,
/// and solving problems.
///
///
/// # Creating Variables
/// As stated in [`crate::variables`], we can create two types of variables: propositional variables
/// and integer variables.
///
/// ```rust
/// # use pumpkin_solver::Solver;
/// # use crate::pumpkin_solver::variables::TransformableVariable;
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
/// // We can also get the predicate from the literal
/// let true_predicate = literal.get_true_predicate();
///
/// // We can also create an iterator of new literals and get a number of them at once
/// let list_of_5_literals = solver.new_literals().take(5).collect::<Vec<_>>();
/// assert_eq!(list_of_5_literals.len(), 5);
/// ```
///
/// # Using the Solver
/// For examples on how to use the solver, see the [root-level crate documentation](crate) or [one of these examples](https://github.com/ConSol-Lab/Pumpkin/tree/master/pumpkin-lib/examples).
#[derive(Debug)]
pub struct Solver {
    /// The internal [`ConstraintSatisfactionSolver`] which is used to solve the problems.
    pub(crate) satisfaction_solver: ConstraintSatisfactionSolver,
    true_literal: Literal,
}

impl Default for Solver {
    fn default() -> Self {
        let satisfaction_solver = ConstraintSatisfactionSolver::default();
        let true_literal = Literal::new(Predicate::trivially_true().get_domain());
        Self {
            satisfaction_solver,
            true_literal,
        }
    }
}

impl Solver {
    /// Creates a solver with the provided [`SolverOptions`].
    pub fn with_options(solver_options: SolverOptions) -> Self {
        let satisfaction_solver = ConstraintSatisfactionSolver::new(solver_options);
        let true_literal = Literal::new(Predicate::trivially_true().get_domain());
        Self {
            satisfaction_solver,
            true_literal,
        }
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

    pub fn get_solution_reference(&self) -> SolutionReference {
        self.satisfaction_solver.get_solution_reference()
    }
}

/// Methods to retrieve information about variables
impl Solver {
    /// Get the value of the given [`Literal`] at the root level (after propagation), which could be
    /// unassigned.
    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        self.satisfaction_solver.get_literal_value(literal)
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
    /// # use pumpkin_solver::Solver;
    /// # use pumpkin_solver::variables::Literal;
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
    /// # use pumpkin_solver::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can create a literal
    /// let literal = solver.new_literal();
    /// ```
    pub fn new_literal(&mut self) -> Literal {
        self.satisfaction_solver.create_new_literal(None)
    }

    pub fn new_literal_for_predicate(&mut self, predicate: Predicate) -> Literal {
        self.satisfaction_solver
            .create_new_literal_for_predicate(predicate, None)
    }

    /// Create a fresh propositional variable with a given name and return the literal with positive
    /// polarity.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_solver::Solver;
    /// let mut solver = Solver::default();
    ///
    /// // We can also create such a variable with a name
    /// let named_literal = solver.new_named_literal("z");
    /// ```
    pub fn new_named_literal(&mut self, name: impl Into<String>) -> Literal {
        self.satisfaction_solver
            .create_new_literal(Some(name.into()))
    }

    /// Get a literal which is always true.
    pub fn get_true_literal(&self) -> Literal {
        self.true_literal
    }

    /// Get a literal which is always false.
    pub fn get_false_literal(&self) -> Literal {
        !self.true_literal
    }

    /// Create a new integer variable with the given bounds.
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_solver::Solver;
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
    /// # use pumpkin_solver::Solver;
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
    /// # use pumpkin_solver::Solver;
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
    /// # use pumpkin_solver::Solver;
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
                let _ = self.satisfaction_solver.conclude_proof_unsat();

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
        SolutionIterator::new(self, brancher, termination)
    }

    /// Solves the current model in the [`Solver`] until it finds a solution (or is indicated to
    /// terminate by the provided [`TerminationCondition`]) and returns a [`SatisfactionResult`]
    /// which can be used to obtain the found solution or find other solutions.
    ///
    /// This method takes as input a list of [`Predicate`]s which represent so-called assumptions
    /// (see \[1\] for a more detailed explanation). See the [`predicates`] documentation for how
    /// to construct these predicates.
    ///
    /// # Bibliography
    /// \[1\] N. Eén and N. Sörensson, ‘Temporal induction by incremental SAT solving’, Electronic
    /// Notes in Theoretical Computer Science, vol. 89, no. 4, pp. 543–560, 2003.
    pub fn satisfy_under_assumptions<'this, 'brancher, B: Brancher, T: TerminationCondition>(
        &'this mut self,
        brancher: &'brancher mut B,
        termination: &mut T,
        assumptions: &[Predicate],
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
    /// `objective_variable` is optimised as indicated by the `direction` (or is indicated to
    /// terminate by the provided [`TerminationCondition`]). Uses a search strategy based on the
    /// provided [`OptimisationProcedure`], currently [`LinearSatUnsat`] and
    /// [`LinearUnsatSat`] are supported.
    ///
    /// It returns an [`OptimisationResult`] which can be used to retrieve the optimal solution if
    /// it exists.
    pub fn optimise<Var: IntegerVariable, Callback: Fn(&Solver, SolutionReference)>(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        mut optimisation_procedure: impl OptimisationProcedure<Var, Callback>,
    ) -> OptimisationResult {
        optimisation_procedure.optimise(brancher, termination, self)
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
    /// # use pumpkin_solver::constraints;
    /// # use pumpkin_solver::Solver;
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
        clause: impl IntoIterator<Item = Predicate>,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver.add_clause(clause)
    }

    /// Adds a propagator with a tag, which is used to identify inferences made by this propagator
    /// in the proof log.
    pub(crate) fn add_tagged_propagator(
        &mut self,
        propagator: impl Propagator + 'static,
        tag: NonZero<u32>,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_propagator(propagator, Some(tag))
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
    pub(crate) fn add_propagator(
        &mut self,
        propagator: impl Propagator + 'static,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver.add_propagator(propagator, None)
    }
}

/// Default brancher implementation
impl Solver {
    /// Creates an instance of the [`DefaultBrancher`].
    pub fn default_brancher(&self) -> DefaultBrancher {
        DefaultBrancher::default_over_all_variables(&self.satisfaction_solver.assignments)
    }
}

/// Proof logging methods
impl Solver {
    #[doc(hidden)]
    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) {
        let _ = self.satisfaction_solver.conclude_proof_unsat();
    }

    #[doc(hidden)]
    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Literal) {
        let _ = self
            .satisfaction_solver
            .conclude_proof_optimal(bound.get_true_predicate());
    }
}

/// A brancher which makes use of VSIDS \[1\] and solution-based phase saving (both adapted for CP).
///
/// If VSIDS does not contain any (unfixed) predicates then it will default to the
/// [`IndependentVariableValueBrancher`] using [`RandomSelector`] for variable selection
/// (over the variables in the order in which they were defined) and [`RandomSplitter`] for
/// value selection.
///
/// # Bibliography
/// \[1\] M. W. Moskewicz, C. F. Madigan, Y. Zhao, L. Zhang, and S. Malik, ‘Chaff: Engineering an
/// efficient SAT solver’, in Proceedings of the 38th annual Design Automation Conference, 2001.
///
/// \[2\] E. Demirović, G. Chu, and P. J. Stuckey, ‘Solution-based phase saving for CP: A
/// value-selection heuristic to simulate local search behavior in complete solvers’, in the
/// proceedings of the Principles and Practice of Constraint Programming (CP 2018).
pub type DefaultBrancher =
    AutonomousSearch<IndependentVariableValueBrancher<DomainId, RandomSelector, RandomSplitter>>;
