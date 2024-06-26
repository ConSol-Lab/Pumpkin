use crate::{basic_types::SolutionReference, engine::ConstraintSatisfactionSolver};

#[derive(Debug)]
#[allow(unused)]
pub struct Satisfiable<'a> {
    solver: &'a mut ConstraintSatisfactionSolver,
}

impl<'solver> Satisfiable<'solver> {
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
    pub fn iterate_solutions(self) -> SolutionIterator<'solver> {
        // Note: this takes ownership of `self` and converts to an iterator over solutions.
        todo!()
    }
}

#[derive(Debug)]
#[allow(unused)]
pub struct SolutionIterator<'a> {
    solver: &'a mut ConstraintSatisfactionSolver,
}

impl<'a> SolutionIterator<'a> {
    /// Find a new solution.
    pub fn next_solution(&mut self) -> IteratedSolution<'a> {
        todo!()
    }
}

#[derive(Debug)]
#[allow(unused)]
pub enum IteratedSolution<'a> {
    /// A new solution was identified.
    Solution(SolutionReference<'a>),

    /// No more solutions exist.
    Finished,

    /// The solver was terminated during search.
    Unknown,
}
