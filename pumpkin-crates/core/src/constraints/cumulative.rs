use std::fmt::Debug;

use super::Constraint;
use crate::options::CumulativePropagationMethod;
use crate::proof::ConstraintTag;
use crate::propagators::ArgTask;
use crate::propagators::CumulativeOptions;
use crate::propagators::TimeTableOverIntervalIncrementalPropagator;
use crate::propagators::TimeTableOverIntervalPropagator;
use crate::propagators::TimeTablePerPointIncrementalPropagator;
use crate::propagators::TimeTablePerPointPropagator;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// Creates the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) [`Constraint`].
///
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
/// It is possible to specify certain options for the cumulative (such as whether to allow holes in
/// the domain or the type of explanation) using [`cumulative_with_options`].
///
/// # Example
/// ```rust
/// // We construct three tasks for a resource with capacity 2:
/// // - Task 0: Start times: [0, 5], Processing time: 4, Resource usage: 1
/// // - Task 1: Start times: [0, 5], Processing time: 2, Resource usage: 1
/// // - Task 2: Start times: [0, 5], Processing time: 4, Resource usage: 2
/// // We can infer that Task 0 and Task 1 execute at the same time
/// // while Task 2 will start after them
/// # use pumpkin_solver::termination::Indefinite;
/// # use pumpkin_solver::Solver;
/// # use pumpkin_solver::results::SatisfactionResult;
/// # use pumpkin_solver::constraints;
/// # use pumpkin_solver::constraints::Constraint;
/// # use crate::pumpkin_solver::results::ProblemSolution;
/// let solver = Solver::default();
///
/// let mut solver = Solver::default();
///
/// let start_0 = solver.new_bounded_integer(0, 4);
/// let start_1 = solver.new_bounded_integer(0, 4);
/// let start_2 = solver.new_bounded_integer(0, 5);
///
/// let constraint_tag = solver.new_constraint_tag();
///
/// let start_times = [start_0, start_1, start_2];
/// let durations = [5, 2, 5];
/// let resource_requirements = [1, 1, 2];
/// let resource_capacity = 2;
///
/// solver
///     .add_constraint(constraints::cumulative(
///         start_times.clone(),
///         durations.clone(),
///         resource_requirements.clone(),
///         resource_capacity,
///         constraint_tag,
///     ))
///     .post();
///
/// let mut termination = Indefinite;
/// let mut brancher = solver.default_brancher();
///
/// let result = solver.satisfy(&mut brancher, &mut termination);
///
/// // We check whether the result was feasible
/// if let SatisfactionResult::Satisfiable(satisfiable) = result {
///     let solution = satisfiable.solution();
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
pub fn cumulative<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    tasks: impl IntoIterator<Item = ArgTask<Var, PVar, RVar>>,
    resource_capacity: CVar,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    cumulative_with_options(
        tasks,
        resource_capacity,
        CumulativeOptions::default(),
        constraint_tag,
    )
}

/// Creates the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// with the provided [`CumulativeOptions`].
///
/// See the documentation of [`cumulative`] for more information about the constraint.
pub fn cumulative_with_options<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    tasks: impl IntoIterator<Item = ArgTask<Var, PVar, RVar>>,
    resource_capacity: CVar,
    options: CumulativeOptions,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    CumulativeConstraint::<Var, PVar, RVar, CVar>::new(
        &tasks.into_iter().collect::<Vec<_>>(),
        resource_capacity,
        options,
        constraint_tag,
    )
}

struct CumulativeConstraint<Var, PVar, RVar, CVar> {
    tasks: Vec<ArgTask<Var, PVar, RVar>>,
    resource_capacity: CVar,
    options: CumulativeOptions,
    constraint_tag: ConstraintTag,
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
        CVar: IntegerVariable + 'static,
    > CumulativeConstraint<Var, PVar, RVar, CVar>
{
    fn new(
        tasks: &[ArgTask<Var, PVar, RVar>],
        resource_capacity: CVar,
        options: CumulativeOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self {
            tasks: tasks.into(),
            resource_capacity,

            options,
            constraint_tag,
        }
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
        CVar: IntegerVariable + 'static,
    > Constraint for CumulativeConstraint<Var, PVar, RVar, CVar>
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        match self.options.propagation_method {
            CumulativePropagationMethod::TimeTablePerPoint => TimeTablePerPointPropagator::new(
                &self.tasks,
                self.resource_capacity,
                self.options.propagator_options,
                self.constraint_tag,
            )
            .post(solver),

            CumulativePropagationMethod::TimeTablePerPointIncremental => {
                TimeTablePerPointIncrementalPropagator::<Var, false>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .post(solver)
            }
            CumulativePropagationMethod::TimeTablePerPointIncrementalSynchronised => {
                TimeTablePerPointIncrementalPropagator::<Var, true>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .post(solver)
            }
            CumulativePropagationMethod::TimeTableOverInterval => {
                TimeTableOverIntervalPropagator::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .post(solver)
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncremental => {
                TimeTableOverIntervalIncrementalPropagator::<Var, false>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .post(solver)
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncrementalSynchronised => {
                TimeTableOverIntervalIncrementalPropagator::<Var, true>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .post(solver)
            }
        }
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        match self.options.propagation_method {
            CumulativePropagationMethod::TimeTablePerPoint => TimeTablePerPointPropagator::new(
                &self.tasks,
                self.resource_capacity,
                self.options.propagator_options,
                self.constraint_tag,
            )
            .implied_by(solver, reification_literal),
            CumulativePropagationMethod::TimeTablePerPointIncremental => {
                TimeTablePerPointIncrementalPropagator::<Var, false>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .implied_by(solver, reification_literal)
            }
            CumulativePropagationMethod::TimeTablePerPointIncrementalSynchronised => {
                TimeTablePerPointIncrementalPropagator::<Var, true>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .implied_by(solver, reification_literal)
            }
            CumulativePropagationMethod::TimeTableOverInterval => {
                TimeTableOverIntervalPropagator::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .implied_by(solver, reification_literal)
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncremental => {
                TimeTableOverIntervalIncrementalPropagator::<Var, false>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .implied_by(solver, reification_literal)
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncrementalSynchronised => {
                TimeTableOverIntervalIncrementalPropagator::<Var, true>::new(
                    &self.tasks,
                    self.resource_capacity,
                    self.options.propagator_options,
                    self.constraint_tag,
                )
                .implied_by(solver, reification_literal)
            }
        }
    }
}
