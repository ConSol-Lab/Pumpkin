//! Contains the propagators for the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html)
//! constraint.
//!
//! Currently it contains solely time-tabling propagators (see
//! [`crate::cumulative::time_table`] for an explanation).
//!
//! # Theoretical
//!
//! The cumulative constraint reasons over a set of tasks over a single resource
//! with a capacity. Each task consists of the following parameters:
//! - A variable `s_i` representing the start time of the task
//! - The duration of the task `p_i` (which is the same for all resources) which cannot be
//!   interruped
//! - The constant resource usage `r_i` of the task (which can differ for different resources)
//!
//! Oftentimes the following notation is used to denote certain significant time points:
//! - `EST_i` - The earliest starting time, equal to `lb(s_i)`
//! - `ECT_i` - The earliest completion time, equal to `lb(s_i) + p_i`
//! - `LST_i` - The latest start time, equal to `ub(s_i)`
//! - `LCT_i` - The latest completion time, equal to `ub(s_i) + p_i`
//!
//! A task is said to execute at time point *t* if it holds that `s_i <= t < s_i + p_i`. The
//! constraint then ensures that at no time point *t* in the horizon (the latest time at which
//! any task can execute) there is an overflow of the resource capacity by the cumulative
//! resource usage of the tasks which are being processed at point *t*.
//!
//! A common problem which makes use of the Cumulative constraint is the [RCPSP](https://www.projectmanagement.ugent.be/research/project_scheduling/rcpsp)
//! problem. Which uses a combination of [Precedence](https://sofdem.github.io/gccat/gccat/Cprecedence.html)
//! and Cumulative constraints.
//!
//! # Practical
//!
//! The following example shows how one of the propagators for the Cumulative constraint can be
//! used:
//!
//! ```rust
//! // We construct three tasks for a resource with capacity 2:
//! // - Task 0: Start times: [0, 5], Processing time: 4, Resource usage: 1
//! // - Task 1: Start times: [0, 5], Processing time: 2, Resource usage: 1
//! // - Task 2: Start times: [0, 5], Processing time: 4, Resource usage: 2
//! // We can infer that Task 0 and Task 1 execute at the same time
//! // while Task 2 will start after them
//! # use pumpkin_core::termination::Indefinite;
//! # use pumpkin_core::Solver;
//! # use pumpkin_core::results::SatisfactionResult;
//! # use pumpkin_core::constraints;
//! # use pumpkin_core::constraints::Constraint;
//! # use pumpkin_core::results::ProblemSolution;
//! # use pumpkin_propagators::cumulative::ArgTask;
//! # use pumpkin_propagators::cumulative::options::CumulativePropagatorOptions;
//! # use pumpkin_propagators::cumulative::time_table::TimeTablePerPointPropagator;
//! let mut solver = Solver::default();
//!
//! let tasks = [
//!     ArgTask {
//!         start_time: solver.new_bounded_integer(0, 4),
//!         processing_time: 5,
//!         resource_usage: 1,
//!     },
//!     ArgTask {
//!         start_time: solver.new_bounded_integer(0, 4),
//!         processing_time: 2,
//!         resource_usage: 1,
//!     },
//!     ArgTask {
//!         start_time: solver.new_bounded_integer(0, 4),
//!         processing_time: 5,
//!         resource_usage: 2,
//!     },
//! ];
//!
//! let resource_capacity = 2;
//!
//! let tag = solver.new_constraint_tag();
//! let result = solver.add_propagator(TimeTablePerPointPropagator::new(
//!     &tasks,
//!     resource_capacity,
//!     CumulativePropagatorOptions::default(),
//!     tag,
//! ));
//!
//! // ...
//! ```
pub mod options;
pub mod time_table;
pub use utils::ArgTask;

mod utils;
pub(crate) use utils::*;
