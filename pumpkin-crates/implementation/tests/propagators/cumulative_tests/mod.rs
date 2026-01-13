#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
mod cumulative_checker_tests;
mod cumulative_conflict_tests;
mod cumulative_propagation_tests;

use implementation::propagators::cumulative::CumulativeConstructor;
use pumpkin_core::TestSolver;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorId;
use pumpkin_core::variables::DomainId;

fn set_up_cumulative_state(
    task_info: &[((i32, i32), u32, u32)],
    capacity: u32,
    conflict_detection_only: bool,
) -> (TestSolver, Result<PropagatorId, Conflict>, Vec<DomainId>) {
    let mut solver = TestSolver::default();

    let mut start_times = Vec::default();
    let mut durations = Vec::default();
    let mut resource_usages = Vec::default();

    for ((lb, ub), duration, resource_usage) in task_info {
        start_times.push(solver.new_variable(*lb, *ub));
        durations.push(*duration);
        resource_usages.push(*resource_usage);
    }
    let constraint_tag = solver.new_constraint_tag();

    let result = solver.new_propagator(CumulativeConstructor {
        start_times: start_times.clone().into(),
        durations: durations.into(),
        resource_usages: resource_usages.into(),
        capacity,
        constraint_tag,
        conflict_detection_only,
    });
    (solver, result, start_times)
}
