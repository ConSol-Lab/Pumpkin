//! Contains the representation of a unsatisfiable solution.

use crate::branching::Brancher;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::Solver;

/// A struct which allows the retrieval of an unsatisfiable core consisting of the provided
/// assumptions passed to the initial [`Solver::satisfy_under_assumptions`].
#[derive(Debug)]
pub struct UnsatisfiableUnderAssumptions<'solver, 'brancher, B> {
    solver: &'solver mut ConstraintSatisfactionSolver,
    brancher: &'brancher mut B,
}

impl<'solver, 'brancher, B: Brancher> UnsatisfiableUnderAssumptions<'solver, 'brancher, B> {
    pub fn new(
        solver: &'solver mut ConstraintSatisfactionSolver,
        brancher: &'brancher mut B,
    ) -> Self {
        UnsatisfiableUnderAssumptions { solver, brancher }
    }

    /// Extract the unsatisfiable core in terms of the assumptions.
    pub fn extract_core(&mut self) -> Box<[Literal]> {
        self.solver
            .extract_clausal_core(self.brancher)
            .expect("expected consistent assumptions")
            .into()
    }
}
