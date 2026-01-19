use std::ops::ControlFlow;

use crate::Solver;
use crate::branching::Brancher;
use crate::results::SolutionReference;

/// Called during optimisation with every encountered solution.
///
/// The callback can determine whether to proceed optimising or whether to stop by returning a
/// [`ControlFlow`] value. When [`ControlFlow::Break`] is returned, a value of
/// [`SolutionCallback::Stop`] can be supplied that will be forwarded to the result of the
/// optimisation call.
pub trait SolutionCallback<B: Brancher> {
    /// The type of value to return if optimisation should stop.
    type Stop;

    /// Called when a solution is encountered.
    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
    ) -> ControlFlow<Self::Stop>;
}

impl<T, B, R> SolutionCallback<B> for T
where
    T: FnMut(&Solver, SolutionReference, &B) -> ControlFlow<R>,
    B: Brancher,
{
    type Stop = R;

    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
    ) -> ControlFlow<Self::Stop> {
        (self)(solver, solution, brancher)
    }
}

impl<T, R, B> SolutionCallback<B> for Option<T>
where
    T: SolutionCallback<B, Stop = R>,
    B: Brancher,
{
    type Stop = R;

    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
    ) -> ControlFlow<Self::Stop> {
        if let Some(callback) = self {
            return callback.on_solution_callback(solver, solution, brancher);
        }

        ControlFlow::Continue(())
    }
}
