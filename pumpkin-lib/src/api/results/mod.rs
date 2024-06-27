pub use crate::basic_types::ProblemSolution;
use crate::basic_types::Solution;
pub use crate::basic_types::SolutionReference;

use self::satisfiable::Satisfiable;
use self::unsatisfiable::UnsatisfiableUnderAssumptions;

pub mod satisfiable;
pub mod unsatisfiable;

#[derive(Debug)]
pub enum SatisfactionResult<'a> {
    Satisfiable(Satisfiable<'a>),
    Unsatisfiable,
    Unknown,
}

#[derive(Debug)]
pub enum SatisfactionResultUnderAssumptions<'solver, 'brancher, B> {
    Satisfiable(Satisfiable<'solver>),
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
