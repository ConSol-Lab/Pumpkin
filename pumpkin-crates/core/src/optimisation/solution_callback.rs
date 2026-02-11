use std::ops::ControlFlow;

use crate::Solver;
use crate::branching::Brancher;
use crate::conflict_resolving::ConflictResolver;
use crate::results::SolutionReference;

/// Called during optimisation with every encountered solution.
///
/// The callback can determine whether to proceed optimising or whether to stop by returning a
/// [`ControlFlow`] value. When [`ControlFlow::Break`] is returned, a value of
/// [`SolutionCallback::Stop`] can be supplied that will be forwarded to the result of the
/// optimisation call.
pub trait SolutionCallback<B: Brancher, R: ConflictResolver> {
    /// The type of value to return if optimisation should stop.
    type Stop;

    /// Called when a solution is encountered.
    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
        resolver: &R,
    ) -> ControlFlow<Self::Stop>;
}

impl<T, B, R, StopData> SolutionCallback<B, R> for T
where
    T: FnMut(&Solver, SolutionReference, &B, &R) -> ControlFlow<StopData>,
    B: Brancher,
    R: ConflictResolver,
{
    type Stop = StopData;

    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
        resolver: &R,
    ) -> ControlFlow<Self::Stop> {
        (self)(solver, solution, brancher, resolver)
    }
}

impl<T, StopData, B, R> SolutionCallback<B, R> for Option<T>
where
    T: SolutionCallback<B, R, Stop = StopData>,
    B: Brancher,
    R: ConflictResolver,
{
    type Stop = StopData;

    fn on_solution_callback(
        &mut self,
        solver: &Solver,
        solution: SolutionReference,
        brancher: &B,
        resolver: &R,
    ) -> ControlFlow<Self::Stop> {
        if let Some(callback) = self {
            return callback.on_solution_callback(solver, solution, brancher, resolver);
        }

        ControlFlow::Continue(())
    }
}
