//! Contains the propagators for the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html)
//! constraint, currently it contains solely time-tabling propagators (see
//! [`crate::propagators::cumulative::time_table`] for an explanation).
//!
//! # Theoretical
//!
//! The cumulative constraint reasons over a set of [`Task`]s over a single resource
//! with a capacity. Each [`Task`] consists of the following parameters:
//! - A variable `s_i` representing the start time of the [`Task`]
//! - The duration of the [`Task`] `p_i` (which is the same for all resources) which cannot be
//!   interruped
//! - The constant resource usage `r_i` of the [`Task`] (which can differ for different resources)
//!
//! Oftentimes the following notation is used to denote certain significant time points:
//! - `EST_i` - The earliest starting time, equal to `lb(s_i)`
//! - `ECT_i` - The earliest completion time, equal to `lb(s_i) + p_i`
//! - `LST_i` - The latest start time, equal to `ub(s_i)`
//! - `LCT_i` - The latest completion time, equal to `ub(s_i) + p_i`
//!
//! A [`Task`] is said to execute at time point *t* if it holds that `s_i <= t < s_i + p_i`. The
//! constraint then ensures that at no time point *t* in the horizon (the latest time at which
//! any [`Task`] can execute) there is an overflow of the resource capacity by the cumulative
//! resource usage of the [`Task`]s which are being processed at point *t*.
//!
//! A common problem which makes use of the Cumulative constraint is the [RCPSP](https://www.projectmanagement.ugent.be/research/project_scheduling/rcpsp)
//! problem. Which uses a combination of [Precedence](https://sofdem.github.io/gccat/gccat/Cprecedence.html)
//! and Cumulative constraints.
//!
//! # Practical
//!
//! The following example shows how one of the propagators for the Cumulative constraint
//! ([`TimeTablePerPointPropagator`]) can be used:
//!
//! ```rust
//! // We construct three tasks for a resource with capacity 2:
//! // - Task 1: Start times: [0, 5], Processing time: 4, Resource usage: 1
//! // - Task 2: Start times: [0, 5], Processing time: 2, Resource usage: 1
//! // - Task 3: Start times: [0, 5], Processing time: 4, Resource usage: 2
//! // We can infer that Task 1 and Task 2 execute at the same time
//! // while Task 3 will start after them
//! use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
//! use pumpkin_lib::constraints::ConstraintsExt;
//! use pumpkin_lib::engine::ConstraintSatisfactionSolver;
//! use pumpkin_lib::propagators::ArgTask;
//! use pumpkin_lib::propagators::TimeTablePerPoint;
//! let solver = ConstraintSatisfactionSolver::default();
//!
//! let mut solver = ConstraintSatisfactionSolver::default();
//! let start_1 = solver.create_new_integer_variable(0, 4);
//! let task_1 = ArgTask {
//!     start_time: start_1,
//!     processing_time: 5,
//!     resource_usage: 1,
//! };
//!
//! let start_2 = solver.create_new_integer_variable(0, 4);
//! let task_2 = ArgTask {
//!     start_time: start_2,
//!     processing_time: 2,
//!     resource_usage: 1,
//! };
//!
//! let start_3 = solver.create_new_integer_variable(0, 5);
//! let task_3 = ArgTask {
//!     start_time: start_3,
//!     processing_time: 5,
//!     resource_usage: 2,
//! };
//!
//! let tasks = [task_1.clone(), task_2.clone(), task_3.clone()];
//! let resource_capacity = 2;
//!
//! solver.cumulative(&tasks, resource_capacity);
//!
//! let result = solver.solve(i64::MAX);
//!
//! // We check whether the result was feasible
//! assert_eq!(CSPSolverExecutionFlag::Feasible, result);
//!
//! let horizon = tasks.iter().map(|task| task.processing_time).sum::<i32>();
//! let start_times = [start_1, start_2, start_3];
//! let assignments = solver.get_integer_assignments();
//!
//! // Now we check whether the resource constraint is satisfied at each time-point t
//! assert!((0..=horizon).all(|t| {
//!     // We gather all of the resource usages at the current time t
//!     let resource_usage_at_t = start_times
//!         .iter()
//!         .enumerate()
//!         .filter_map(|(task_index, start_time)| {
//!             if assignments.get_assigned_value(*start_time) <= t
//!                 && assignments.get_assigned_value(*start_time)
//!                     + tasks[task_index].processing_time
//!                     > t
//!             {
//!                 Some(tasks[task_index].resource_usage)
//!             } else {
//!                 None
//!             }
//!         })
//!         .sum::<i32>();
//!     // Then we check whether the resource usage at the current time point is lower than the
//!     // resource capacity
//!     resource_usage_at_t <= resource_capacity
//! }));
//!
//! // Finally we check whether Task 3 starts after Task 1 and Task 2 and that Task 1 and Task 2
//! // overlap
//! assert!(
//!     assignments.get_assigned_value(start_3)
//!         >= assignments.get_assigned_value(start_1) + task_1.processing_time
//!         && assignments.get_assigned_value(start_3)
//!             >= assignments.get_assigned_value(start_2) + task_2.processing_time
//! );
//! assert!(
//!     assignments.get_assigned_value(start_1)
//!         < assignments.get_assigned_value(start_2) + task_2.processing_time
//!         && assignments.get_assigned_value(start_2)
//!             < assignments.get_assigned_value(start_1) + task_1.processing_time
//! );
//! ```
mod time_table;
pub use time_table::*;

mod utils;
pub use utils::*;
