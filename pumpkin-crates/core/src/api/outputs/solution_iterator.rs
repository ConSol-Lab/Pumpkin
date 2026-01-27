//! Contains the structures corresponding to solution iterations.

use std::fmt::Debug;

use super::SatisfactionResult::Satisfiable;
use super::SatisfactionResult::Unknown;
use super::SatisfactionResult::Unsatisfiable;
use super::SolutionReference;
use crate::Solver;
use crate::branching::Brancher;
use crate::conflict_resolving::ConflictResolver;
use crate::predicate;
use crate::predicates::Predicate;
use crate::results::ProblemSolution;
use crate::results::Solution;
use crate::termination::TerminationCondition;

/// A struct which allows the retrieval of multiple solutions to a satisfaction problem.
#[derive(Debug)]
pub struct SolutionIterator<'solver, 'brancher, 'termination, 'resolver, B, T, R> {
    solver: &'solver mut Solver,
    brancher: &'brancher mut B,
    termination: &'termination mut T,
    resolver: &'resolver mut R,

    next_blocking_clause: Option<Vec<Predicate>>,
    has_solution: bool,
}

impl<
    'solver,
    'brancher,
    'termination,
    'resolver,
    B: Brancher,
    T: TerminationCondition,
    R: ConflictResolver,
> SolutionIterator<'solver, 'brancher, 'termination, 'resolver, B, T, R>
{
    pub(crate) fn new(
        solver: &'solver mut Solver,
        brancher: &'brancher mut B,
        termination: &'termination mut T,
        resolver: &'resolver mut R,
    ) -> Self {
        SolutionIterator {
            solver,
            brancher,
            termination,
            resolver,
            next_blocking_clause: None,
            has_solution: false,
        }
    }

    /// Find a new solution by blocking the previous solution from being found. Also calls the
    /// [`Brancher::on_solution`] method from the [`Brancher`] used to run the initial solve.
    pub fn next_solution(&mut self) -> IteratedSolution<'_, B, R> {
        if let Some(blocking_clause) = self.next_blocking_clause.take() {
            // We do not care much about this tag, as the proof is nonsensical for
            // solution enumeration anyways.
            let constraint_tag = self.solver.new_constraint_tag();
            if self
                .solver
                .add_clause(blocking_clause, constraint_tag)
                .is_err()
            {
                return IteratedSolution::Finished;
            }
        }

        let result = match self
            .solver
            .satisfy(self.brancher, self.termination, self.resolver)
        {
            Satisfiable(satisfiable) => {
                let solution: Solution = satisfiable.solution().into();
                self.has_solution = true;
                self.next_blocking_clause = Some(get_blocking_clause(solution.as_reference()));
                IterationResult::Solution(solution)
            }
            Unsatisfiable(_, _, _) => {
                if self.has_solution {
                    IterationResult::Finished
                } else {
                    IterationResult::Unsatisfiable
                }
            }
            Unknown(_, _, _) => IterationResult::Unknown,
        };

        match result {
            IterationResult::Solution(solution) => {
                IteratedSolution::Solution(solution, self.solver, self.brancher, self.resolver)
            }
            IterationResult::Finished => IteratedSolution::Finished,
            IterationResult::Unsatisfiable => IteratedSolution::Unsatisfiable,
            IterationResult::Unknown => IteratedSolution::Unknown,
        }
    }
}

/// The different results we can get from the next solution call. We need this type because
/// [`IteratedSolution`] takes a reference to [`Solver`], which, at the time where
/// [`IterationResult::Solution`] is created, cannot be given as there is an exclusive borrow of the
/// solver alive as well.
enum IterationResult {
    Solution(Solution),
    Finished,
    Unsatisfiable,
    Unknown,
}

/// Creates a clause which prevents the current solution from occurring again by going over the
/// defined output variables and creating a clause which prevents those values from
/// being assigned.
///
/// This method is used when attempting to find multiple solutions.
fn get_blocking_clause(solution: SolutionReference) -> Vec<Predicate> {
    solution
        .get_domains()
        .map(|variable| predicate!(variable != solution.get_integer_value(variable)))
        .collect::<Vec<_>>()
}
/// Enum which specifies the status of the call to [`SolutionIterator::next_solution`].
#[allow(
    clippy::large_enum_variant,
    reason = "these will not be stored in bulk, so this is not an issue"
)]
#[derive(Debug)]
pub enum IteratedSolution<'a, B, R> {
    /// A new solution was identified.
    Solution(Solution, &'a Solver, &'a B, &'a R),

    /// No more solutions exist.
    Finished,

    /// The solver was terminated during search.
    Unknown,

    /// There exists no solution
    Unsatisfiable,
}
