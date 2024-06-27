use self::satisfiable::Satisfiable;
use self::unsatisfiable::UnsatisfiableUnderAssumptions;
pub use crate::basic_types::ProblemSolution;
use crate::basic_types::Solution;
pub use crate::basic_types::SolutionReference;

pub mod satisfiable;
pub mod unsatisfiable;

#[derive(Debug)]
pub enum SatisfactionResult<'solver, 'brancher, 'termination, B, T> {
    Satisfiable(Satisfiable<'solver, 'brancher, 'termination, B, T>),
    Unsatisfiable,
    Unknown,
}

#[derive(Debug)]
pub enum SatisfactionResultUnderAssumptions<'solver, 'brancher, 'termination, B, T> {
    Satisfiable(Satisfiable<'solver, 'brancher, 'termination, B, T>),
    UnsatisfiableUnderAssumptions(UnsatisfiableUnderAssumptions<'solver, 'brancher, B>),
    Unsatisfiable,
    Unknown,
}

#[derive(Debug)]
pub enum OptimisationResult {
    Optimal(Solution),
    Satisfiable(Solution),
    Unsatisfiable,
    Unknown,
}
