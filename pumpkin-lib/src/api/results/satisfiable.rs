use crate::{basic_types::SolutionReference, engine::ConstraintSatisfactionSolver};

#[derive(Debug)]
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
pub struct SolutionIterator<'a> {
    solver: &'a mut ConstraintSatisfactionSolver,
}

impl<'a> SolutionIterator<'a> {
    /// Find a new solution.
    pub fn next_solution(&mut self) -> IteratedSolution<'a> {
        todo!()
    }

    // /// Creates a clause which prevents the current solution from occurring again by going over the
    // /// defined output variables and creating a clause which prevents those values from being assigned.
    // ///
    // /// This method is used when attempting to find multiple solutions. It restores the state of the
    // /// passed [`ConstraintSatisfactionSolver`] to the root (using
    // /// [`ConstraintSatisfactionSolver::restore_state_at_root`]) and returns true if adding the clause
    // /// was successful (i.e. it is possible that there could be another solution) and returns false
    // /// otherwise (i.e. if adding a clause led to a conflict which indicates that there are no more
    // /// solutions).
    // fn add_blocking_clause(
    //     solver: &mut Solver,
    //     outputs: &[Output],
    //     brancher: &mut impl Brancher,
    // ) -> bool {
    //     #[allow(deprecated)]
    //     let solution = solver.get_solution_reference();

    //     let clause = outputs
    //         .iter()
    //         .flat_map(|output| match output {
    //             Output::Bool(bool) => {
    //                 let literal = *bool.get_variable();

    //                 let literal = if solution.get_literal_value(literal) {
    //                     literal
    //                 } else {
    //                     !literal
    //                 };

    //                 Box::new(std::iter::once(literal))
    //             }

    //             Output::Int(int) => {
    //                 let domain = *int.get_variable();
    //                 let value = solution.get_integer_value(domain);
    //                 Box::new(std::iter::once(
    //                     solver.get_literal(predicate![domain == value]),
    //                 ))
    //             }

    //             #[allow(trivial_casts)]
    //             Output::ArrayOfBool(array_of_bool) => {
    //                 Box::new(array_of_bool.get_contents().map(|&literal| {
    //                     if solution.get_literal_value(literal) {
    //                         literal
    //                     } else {
    //                         !literal
    //                     }
    //                 })) as Box<dyn Iterator<Item = Literal>>
    //             }

    //             #[allow(trivial_casts)]
    //             Output::ArrayOfInt(array_of_ints) => {
    //                 Box::new(array_of_ints.get_contents().map(|&domain| {
    //                     let value = solution.get_integer_value(domain);
    //                     solver.get_literal(predicate![domain == value])
    //                 })) as Box<dyn Iterator<Item = Literal>>
    //             }
    //         })
    //         .map(|literal| !literal)
    //         .collect::<Vec<_>>();
    //     solver.restore_state_at_root(brancher);
    //     if clause.is_empty() {
    //         return false;
    //     }
    //     solver.add_clause(clause).is_ok()
    // }
}

#[derive(Debug)]
pub enum IteratedSolution<'a> {
    /// A new solution was identified.
    Solution(SolutionReference<'a>),

    /// No more solutions exist.
    Finished,

    /// The solver was terminated during search.
    Unknown,
}
