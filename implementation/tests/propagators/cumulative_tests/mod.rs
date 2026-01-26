#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
mod cumulative_checker_tests;
mod cumulative_conflict_tests;
mod cumulative_propagation_tests;

use implementation::propagators::cumulative::CumulativeConstructor;
use implementation::propagators::cumulative::Task;
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
    let mut tasks = Vec::default();

    for ((lb, ub), duration, resource_usage) in task_info {
        let start_time = solver.new_variable(*lb, *ub);
        start_times.push(start_time);
        tasks.push(Task {
            start_time,
            duration: *duration,
            resource_usage: *resource_usage,
        })
    }
    let constraint_tag = solver.new_constraint_tag();

    let result = solver.new_propagator(CumulativeConstructor {
        tasks: tasks.into(),
        capacity,
        constraint_tag,
        conflict_detection_only,
    });
    (solver, result, start_times)
}
