use std::fmt::Debug;

use super::Constraint;
use crate::propagators::ArgTask;
use crate::propagators::TimeTableOverIntervalIncremental;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Creates the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) [`Constraint`].
/// This constraint ensures that at no point in time, the cumulative resource usage of the tasks
/// exceeds `bound`.
///
/// The implementation uses a form of time-table reasoning (for an example of this type of
/// reasoning, see \[1], note that it does **not** implement the specific algorithm in the paper
/// but that the reasoning used is the same).
///
/// The length of `start_times`, `durations` and `resource_requirements` should be the same; if
/// this is not the case then this method will panic.
///
/// # Example
/// ```rust
/// // We construct three tasks for a resource with capacity 2:
/// // - Task 0: Start times: [0, 5], Processing time: 4, Resource usage: 1
/// // - Task 1: Start times: [0, 5], Processing time: 2, Resource usage: 1
/// // - Task 2: Start times: [0, 5], Processing time: 4, Resource usage: 2
/// // We can infer that Task 0 and Task 1 execute at the same time
/// // while Task 2 will start after them
/// # use pumpkin_lib::termination::Indefinite;
/// # use pumpkin_lib::Solver;
/// # use pumpkin_lib::results::SatisfactionResult;
/// # use pumpkin_lib::constraints;
/// # use pumpkin_lib::constraints::Constraint;
/// # use crate::pumpkin_lib::results::ProblemSolution;
/// let solver = Solver::default();
///
/// let mut solver = Solver::default();
///
/// let start_0 = solver.new_bounded_integer(0, 4);
/// let start_1 = solver.new_bounded_integer(0, 4);
/// let start_2 = solver.new_bounded_integer(0, 5);
///
/// let start_times = [start_0, start_1, start_2];
/// let durations = [5, 2, 5];
/// let resource_requirements = [1, 1, 2];
/// let resource_capacity = 2;
///
/// solver
///     .add_constraint(constraints::cumulative(
///         &start_times,
///         &durations,
///         &resource_requirements,
///         resource_capacity,
///         false,
///     ))
///     .post();
///
/// let mut termination = Indefinite;
/// let mut brancher = solver.default_brancher_over_all_propositional_variables();
///
/// let result = solver.satisfy(&mut brancher, &mut termination);
///
/// // We check whether the result was feasible
/// if let SatisfactionResult::Satisfiable(solution) = result {
///     let horizon = durations.iter().sum::<i32>();
///     let start_times = [start_0, start_1, start_2];
///
///     // Now we check whether the resource constraint is satisfied at each time-point t
///     assert!((0..=horizon).all(|t| {
///         // We gather all of the resource usages at the current time t
///         let resource_usage_at_t = start_times
///             .iter()
///             .enumerate()
///             .filter_map(|(task_index, start_time)| {
///                 if solution.get_integer_value(*start_time) <= t
///                     && solution.get_integer_value(*start_time) + durations[task_index] > t
///                 {
///                     Some(resource_requirements[task_index])
///                 } else {
///                     None
///                 }
///             })
///             .sum::<i32>();
///         // Then we check whether the resource usage at the current time point is lower than
///         // the resource capacity
///         resource_usage_at_t <= resource_capacity
///     }));
///
///     // Finally we check whether Task 2 starts after Task 0 and Task 1 and that Task 0 and
///     // Task 1 overlap
///     assert!(
///         solution.get_integer_value(start_2)
///             >= solution.get_integer_value(start_0) + durations[0]
///             && solution.get_integer_value(start_2)
///                 >= solution.get_integer_value(start_1) + durations[1]
///     );
///     assert!(
///         solution.get_integer_value(start_0)
///             < solution.get_integer_value(start_1) + durations[1]
///             && solution.get_integer_value(start_1)
///                 < solution.get_integer_value(start_0) + durations[0]
///     );
/// }
/// ```
///
/// # Bibliography
/// \[1\] S. Gay, R. Hartert, and P. Schaus, ‘Simple and scalable time-table filtering for the
/// cumulative constraint’, in Principles and Practice of Constraint Programming: 21st
/// International Conference, CP 2015, Cork, Ireland, August 31--September 4, 2015, Proceedings
/// 21, 2015, pp. 149–157.
pub fn cumulative<Var: IntegerVariable + 'static + Debug>(
    start_times: &[Var],
    durations: &[i32],
    resource_requirements: &[i32],
    resource_capacity: i32,
    allow_holes_in_domain: bool,
) -> impl Constraint {
    pumpkin_assert_simple!(
        start_times.len() == durations.len() && durations.len() == resource_requirements.len(),
        "The number of start variables, durations and resource requirements should be the
same!car"
    );

    TimeTableOverIntervalIncremental::new(
        start_times
            .iter()
            .zip(durations)
            .zip(resource_requirements)
            .map(|((start_time, duration), resource_requirement)| ArgTask {
                start_time: start_time.clone(),
                processing_time: *duration,
                resource_usage: *resource_requirement,
            })
            .collect(),
        resource_capacity,
        allow_holes_in_domain,
    )
}
