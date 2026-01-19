use crate::Solver;
use crate::branching::Brancher;
use crate::conflict_resolving::ConflictResolver;
use crate::results::SolutionReference;

pub trait SolutionCallback<B: Brancher, R: ConflictResolver> {
    fn on_solution_callback(
        &self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
        resolver: &R,
    );
}

impl<T: Fn(&Solver, SolutionReference, &B, &R), B: Brancher, R: ConflictResolver>
    SolutionCallback<B, R> for T
{
    fn on_solution_callback(
        &self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
        resolver: &R,
    ) {
        (self)(solver, solution, brancher, resolver)
    }
}

impl<T: SolutionCallback<B, R>, B: Brancher, R: ConflictResolver> SolutionCallback<B, R>
    for Option<T>
{
    fn on_solution_callback(
        &self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
        resolver: &R,
    ) {
        if let Some(callback) = self {
            callback.on_solution_callback(solver, solution, brancher, resolver)
        }
    }
}
