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
//! // - Task 0: Start times: [0, 5], Processing time: 4, Resource usage: 1
//! // - Task 1: Start times: [0, 5], Processing time: 2, Resource usage: 1
//! // - Task 2: Start times: [0, 5], Processing time: 4, Resource usage: 2
//! // We can infer that Task 0 and Task 1 execute at the same time
//! // while Task 2 will start after them
//! # use pumpkin_solver::termination::Indefinite;
//! # use pumpkin_solver::Solver;
//! # use pumpkin_solver::results::SatisfactionResult;
//! # use pumpkin_solver::constraints;
//! # use pumpkin_solver::constraints::Constraint;
//! # use crate::pumpkin_solver::results::ProblemSolution;
//! let mut solver = Solver::default();
//!
//! let start_0 = solver.new_bounded_integer(0, 4);
//! let start_1 = solver.new_bounded_integer(0, 4);
//! let start_2 = solver.new_bounded_integer(0, 5);
//!
//! let start_times = [start_0, start_1, start_2];
//! let durations = [5, 2, 5];
//! let resource_requirements = [1, 1, 2];
//! let resource_capacity = 2;
//!
//! solver
//!     .add_constraint(constraints::cumulative(
//!         start_times.clone(),
//!         durations.clone(),
//!         resource_requirements.clone(),
//!         resource_capacity,
//!     ))
//!     .post();
//!
//! let mut termination = Indefinite;
//! let mut brancher = solver.default_brancher();
//!
//! let result = solver.satisfy(&mut brancher, &mut termination);
//!
//! // We check whether the result was feasible
//! if let SatisfactionResult::Satisfiable(solution) = result {
//!     let horizon = durations.iter().sum::<i32>();
//!     let start_times = [start_0, start_1, start_2];
//!
//!     // Now we check whether the resource constraint is satisfied at each time-point t
//!     assert!((0..=horizon).all(|t| {
//!         // We gather all of the resource usages at the current time t
//!         let resource_usage_at_t = start_times
//!             .iter()
//!             .enumerate()
//!             .filter_map(|(task_index, start_time)| {
//!                 if solution.get_integer_value(*start_time) <= t
//!                     && solution.get_integer_value(*start_time) + durations[task_index] > t
//!                 {
//!                     Some(resource_requirements[task_index])
//!                 } else {
//!                     None
//!                 }
//!             })
//!             .sum::<i32>();
//!         // Then we check whether the resource usage at the current time point is lower than
//!         // the resource capacity
//!         resource_usage_at_t <= resource_capacity
//!     }));
//!
//!     // Finally we check whether Task 2 starts after Task 0 and Task 1 and that Task 0 and
//!     // Task 1 overlap
//!     assert!(
//!         solution.get_integer_value(start_2)
//!             >= solution.get_integer_value(start_0) + durations[0]
//!             && solution.get_integer_value(start_2)
//!                 >= solution.get_integer_value(start_1) + durations[1]
//!     );
//!     assert!(
//!         solution.get_integer_value(start_0)
//!             < solution.get_integer_value(start_1) + durations[1]
//!             && solution.get_integer_value(start_1)
//!                 < solution.get_integer_value(start_0) + durations[0]
//!     );
//! }
//! ```
mod time_table;
pub use time_table::CumulativeExplanationType;
pub(crate) use time_table::*;
mod options;
pub use options::*;

mod utils;
pub(crate) use utils::*;
