use self::unsatisfiable::UnsatisfiableUnderAssumptions;
pub use crate::basic_types::ProblemSolution;
use crate::basic_types::Solution;
pub use crate::basic_types::SolutionReference;
use crate::Solver;
pub mod solution_iterator;
pub mod unsatisfiable;
use crate::branching::Brancher;
#[cfg(doc)]
use crate::termination::TerminationCondition;

/// The result of a call to [`Solver::satisfy`].
#[derive(Debug)]
pub enum SatisfactionResult<'solver, 'brancher, B: Brancher> {
    /// Indicates that a solution was found and provides the corresponding [`Solution`].
    Satisfiable(Satisfiable<'solver, 'brancher, B>),
    /// Indicates that there is no solution to the satisfaction problem.
    Unsatisfiable(&'solver Solver),
    /// Indicates that it is not known whether a solution exists. This is likely due to a
    /// [`TerminationCondition`] triggering.
    Unknown(&'solver Solver),
}

/// The result of a call to [`Solver::satisfy_under_assumptions`].
#[derive(Debug)]
pub enum SatisfactionResultUnderAssumptions<'solver, 'brancher, B: Brancher> {
    /// Indicates that a solution was found and provides the corresponding [`Solution`] via
    /// [`Satisfiable::solution`].
    Satisfiable(Satisfiable<'solver, 'brancher, B>),
    /// Indicates that there is no solution to the satisfaction problem due to the provided
    /// assumptions. It returns an [`UnsatisfiableUnderAssumptions`] which can be used to retrieve
    /// an unsatisfiable core.
    UnsatisfiableUnderAssumptions(UnsatisfiableUnderAssumptions<'solver, 'brancher, B>),
    /// Indicates that there is no solution to the satisfaction problem.
    Unsatisfiable(&'solver Solver),
    /// Indicates that it is not known whether a solution exists. This is likely due to a
    /// [`TerminationCondition`] triggering.
    Unknown(&'solver Solver),
}

/// The result of a call to [`Solver::optimise`].
#[derive(Debug)]
pub enum OptimisationResult {
    /// Indicates that an optimal solution has been found and proven to be optimal. It provides an
    /// instance of [`Solution`] which contains the optimal solution.
    Optimal(Solution),
    /// Indicates that a solution was found and provides an instance of [`Solution`] which contains
    /// best known solution by the solver.
    Satisfiable(Solution),
    /// Indicates that there is no solution to the problem.
    Unsatisfiable,
    /// Indicates that it is not known whether a solution exists. This is likely due to a
    /// [`TerminationCondition`] triggering.
    Unknown,
}

#[derive(Debug)]
pub struct Satisfiable<'solver, 'brancher, B: Brancher> {
    solver: &'solver mut Solver,
    brancher: &'brancher mut B,
}

impl<'solver, 'brancher, B: Brancher> Satisfiable<'solver, 'brancher, B> {
    pub(crate) fn new(solver: &'solver mut Solver, brancher: &'brancher mut B) -> Self {
        Satisfiable { solver, brancher }
    }

    /// Get the solution that was discovered.
    pub fn solution(&self) -> SolutionReference<'_> {
        self.solver.get_solution_reference()
    }

    /// Get the solver instance.
    pub fn solver(&self) -> &Solver {
        self.solver
    }

    /// Get the brancher.
    pub fn brancher(&self) -> &B {
        self.brancher
    }
}

impl<B: Brancher> Drop for Satisfiable<'_, '_, B> {
    fn drop(&mut self) {
        self.solver
            .satisfaction_solver
            .restore_state_at_root(self.brancher);
    }
}
