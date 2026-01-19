use std::ops::ControlFlow;

use crate::Solver;
use crate::branching::Brancher;
use crate::results::SolutionReference;

pub trait SolutionCallback<B: Brancher> {
    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
    ) -> ControlFlow<(), ()>;
}

impl<T, B> SolutionCallback<B> for T
where
    T: FnMut(&Solver, SolutionReference, &B) -> ControlFlow<(), ()>,
    B: Brancher,
{
    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
    ) -> ControlFlow<(), ()> {
        (self)(solver, solution, brancher)
    }
}

impl<T: SolutionCallback<B>, B: Brancher> SolutionCallback<B> for Option<T> {
    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
    ) -> ControlFlow<(), ()> {
        if let Some(callback) = self {
            return callback.on_solution_callback(solver, solution, brancher);
        }

        ControlFlow::Continue(())
    }
}
