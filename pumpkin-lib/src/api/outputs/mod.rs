use self::unsatisfiable::UnsatisfiableUnderAssumptions;
pub use crate::basic_types::ProblemSolution;
use crate::basic_types::Solution;
pub use crate::basic_types::SolutionReference;
pub mod solution_iterator;
pub mod unsatisfiable;
use crate::branching::Brancher;
#[cfg(doc)]
use crate::termination::TerminationCondition;
#[cfg(doc)]
use crate::Solver;

/// The result of a call to [`Solver::satisfy`].
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum SatisfactionResult {
    /// Indicates that a solution was found and provides an instance of [`Satisfiable`] which
    /// allows the retrieval of the solution and allows iterating over multiple solutions.
    Satisfiable(Solution),
    /// Indicates that there is no solution to the satisfaction problem.
    Unsatisfiable,
    /// Indicates that it is not known whether a solution exists. This is likely due to a
    /// [`TerminationCondition`] triggering.
    Unknown,
}

/// The result of a call to [`Solver::satisfy_under_assumptions`].
#[derive(Debug)]
pub enum SatisfactionResultUnderAssumptions<'solver, 'brancher, B: Brancher> {
    /// Indicates that a solution was found and provides an instance of [`Satisfiable`] which
    /// allows the retrieval of the solution and allows iterating over multiple solutions.
    Satisfiable(Solution),
    /// Indicates that there is no solution to the satisfaction problem due to the provided
    /// assumptions.
    UnsatisfiableUnderAssumptions(UnsatisfiableUnderAssumptions<'solver, 'brancher, B>),
    /// Indicates that there is no solution to the satisfaction problem.
    Unsatisfiable,
    /// Indicates that it is not known whether a solution exists. This is likely due to a
    /// [`TerminationCondition`] triggering.
    Unknown,
}

/// The result of a call to [`Solver::maximise`] or [`Solver::minimise`].
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
