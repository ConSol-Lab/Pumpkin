use crate::branching::Brancher;
use crate::results::SolutionReference;
use crate::Solver;

pub trait SolutionCallback<B: Brancher> {
    fn on_solution_callback(&self, solver: &Solver, solution: SolutionReference, brancher: &B);
}

impl<T: Fn(&Solver, SolutionReference, &B), B: Brancher> SolutionCallback<B> for T {
    fn on_solution_callback(&self, solver: &Solver, solution: SolutionReference, brancher: &B) {
        (self)(solver, solution, brancher)
    }
}

impl<T: SolutionCallback<B>, B: Brancher> SolutionCallback<B> for Option<T> {
    fn on_solution_callback(&self, solver: &Solver, solution: SolutionReference, brancher: &B) {
        if let Some(callback) = self {
            callback.on_solution_callback(solver, solution, brancher)
        }
    }
}
