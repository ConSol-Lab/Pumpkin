//! Contains the representation of a satisfiable solution for satisfaction problems.

use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::SolutionReference;
use crate::branching::Brancher;
use crate::engine::ConstraintSatisfactionSolver;
use crate::results::Solution;
use crate::termination::TerminationCondition;
use crate::variables::Literal;

/// A struct which allows either the retrieval of the current solution using
/// [`Satisfiable::as_solution`] or allows iterating over solutions using the [`SolutionIterator`]
/// provided by [`Satisfiable::iterate_solutions`].
///
/// Note that when this struct is dropped (using [`Drop`]) then the [`Solver`] is reset.
#[derive(Debug)]
pub struct Satisfiable<'solver, 'brancher, 'termination, B, T> {
    solver: &'solver mut ConstraintSatisfactionSolver,
    brancher: &'brancher mut B,
    termination: &'termination mut T,
}

impl<'solver, 'brancher, 'termination, B: Brancher, T: TerminationCondition>
    Satisfiable<'solver, 'brancher, 'termination, B, T>
{
    pub fn new(
        solver: &'solver mut ConstraintSatisfactionSolver,
        brancher: &'brancher mut B,
        termination: &'termination mut T,
    ) -> Self {
        Satisfiable {
            solver,
            brancher,
            termination,
        }
    }

    /// Get the solution the solver obtained.
    pub fn as_solution<'this>(&'this self) -> SolutionReference<'solver>
    where
        'this: 'solver,
    {
        self.solver.get_solution_reference()
    }

    /// Continue finding solutions within the current time budget.
    ///
    /// Iterating solutions is done by blocking previously found solutions. This means that the
    /// solver may learn clauses taking those blocked solutions into account. Consequently, after
    /// this [`Satisfiable`] is dropped, the solutions which were obtained will remain blocked.
    ///
    /// There are two effects of this design choice that are worth noting:
    ///   - When the termination of the current search is triggered, a new search can be started
    ///   with a fresh termination. The new search will not find the previously found solutions
    ///   again.
    ///   - When the solver concludes no more solutions exist, the solver is now in an inconstent
    ///   state at the root and no more variables/constraints can be added.
    pub fn iterate_solutions(self) -> SolutionIterator<'solver, 'brancher, 'termination, B, T> {
        self.brancher
            .on_solution(self.solver.get_solution_reference());
        SolutionIterator::new(self.solver, self.brancher, self.termination)
    }
}

/// A struct which allows the retrieval of multiple solutions to a satisfaction problem.
#[derive(Debug)]
pub struct SolutionIterator<'solver, 'brancher, 'termination, B, T> {
    solver: &'solver mut ConstraintSatisfactionSolver,
    brancher: &'brancher mut B,
    termination: &'termination mut T,
}

impl<'solver, 'brancher, 'termination, B: Brancher, T: TerminationCondition>
    SolutionIterator<'solver, 'brancher, 'termination, B, T>
{
    pub fn new(
        solver: &'solver mut ConstraintSatisfactionSolver,
        brancher: &'brancher mut B,
        termination: &'termination mut T,
    ) -> Self {
        SolutionIterator {
            solver,
            brancher,
            termination,
        }
    }

    /// Find a new solution by blocking the previous solution from being found. Also calls the
    /// [`Brancher::on_solution`] method from the [`Brancher`] used to run the initial solve.
    pub fn next_solution(&mut self) -> IteratedSolution {
        if SolutionIterator::<B, T>::add_blocking_clause(self.solver, self.brancher) {
            match self.solver.solve(self.termination, self.brancher) {
                CSPSolverExecutionFlag::Feasible => {
                    self.brancher
                        .on_solution(self.solver.get_solution_reference());
                    IteratedSolution::Solution(self.solver.get_solution_reference().into())
                }
                CSPSolverExecutionFlag::Infeasible => IteratedSolution::Finished,
                CSPSolverExecutionFlag::Timeout => IteratedSolution::Unknown,
            }
        } else {
            return IteratedSolution::Finished;
        }
    }

    /// Creates a clause which prevents the current solution from occurring again by going over the
    /// defined output variables and creating a clause which prevents those values from
    /// being assigned. This method is used when attempting to find multiple solutions. It restores
    /// the state of the passed [`ConstraintSatisfactionSolver`] to the root (using
    /// [`ConstraintSatisfactionSolver::restore_state_at_root`]) and returns true if adding the
    /// clause was successful (i.e. it is possible that there could be another solution) and
    /// returns false otherwise (i.e. if adding a clause led to a conflict which indicates that
    /// there are no more solutions).
    fn add_blocking_clause(
        solver: &mut ConstraintSatisfactionSolver,
        brancher: &mut impl Brancher,
    ) -> bool {
        #[allow(deprecated)]
        let clause = solver
            .get_propositional_assignments()
            .get_propositional_variables()
            .filter(|propositional_variable| {
                solver
                    .get_propositional_assignments()
                    .is_variable_assigned(*propositional_variable)
            })
            .map(|propositional_variable| {
                !Literal::new(
                    propositional_variable,
                    solver
                        .get_propositional_assignments()
                        .is_variable_assigned_true(propositional_variable),
                )
            })
            .collect::<Vec<_>>();
        solver.restore_state_at_root(brancher);
        if clause.is_empty() {
            return false;
        }
        solver.add_clause(clause).is_ok()
    }
}

/// Enum which specifies the status of the call to [`SolutionIterator::next_solution`].
#[derive(Debug)]
pub enum IteratedSolution {
    /// A new solution was identified.
    Solution(Solution),

    /// No more solutions exist.
    Finished,

    /// The solver was terminated during search.
    Unknown,
}
