use std::fmt::Debug;

use pumpkin_core::ConstraintOperationError;
use pumpkin_core::Solver;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;
use pumpkin_propagators::cumulative::ArgTask;
use pumpkin_propagators::cumulative::options::CumulativeOptions;
use pumpkin_propagators::cumulative::options::CumulativePropagationMethod;
use pumpkin_propagators::cumulative::time_table::TimeTableOverIntervalIncrementalPropagator;
use pumpkin_propagators::cumulative::time_table::TimeTableOverIntervalPropagator;
use pumpkin_propagators::cumulative::time_table::TimeTablePerPointIncrementalPropagator;
use pumpkin_propagators::cumulative::time_table::TimeTablePerPointPropagator;

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
/// # use pumpkin_core::termination::Indefinite;
/// # use pumpkin_core::Solver;
/// # use pumpkin_core::results::SatisfactionResult;
/// # use pumpkin_core::constraints;
/// # use pumpkin_core::constraints::Constraint;
/// # use pumpkin_core::results::ProblemSolution;
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
///     .add_constraint(pumpkin_constraints::cumulative(
///         start_times.clone(),
///         durations.clone(),
///         resource_requirements.clone(),
///         resource_capacity,
///         constraint_tag,
///     ))
///     .post();
///
/// // ...
/// ```
///
/// # Bibliography
/// \[1\] S. Gay, R. Hartert, and P. Schaus, ‘Simple and scalable time-table filtering for the
/// cumulative constraint’, in Principles and Practice of Constraint Programming: 21st
/// International Conference, CP 2015, Cork, Ireland, August 31--September 4, 2015, Proceedings
/// 21, 2015, pp. 149–157.
pub fn cumulative<StartTimes, Durations, ResourceRequirements>(
    start_times: StartTimes,
    durations: Durations,
    resource_requirements: ResourceRequirements,
    resource_capacity: i32,
    constraint_tag: ConstraintTag,
) -> impl Constraint
where
    StartTimes: IntoIterator,
    StartTimes::Item: IntegerVariable + Debug + 'static,
    StartTimes::IntoIter: ExactSizeIterator,
    Durations: IntoIterator<Item = i32>,
    Durations::IntoIter: ExactSizeIterator,
    ResourceRequirements: IntoIterator<Item = i32>,
    ResourceRequirements::IntoIter: ExactSizeIterator,
{
    cumulative_with_options(
        start_times,
        durations,
        resource_requirements,
        resource_capacity,
        CumulativeOptions::default(),
        constraint_tag,
    )
}

/// Creates the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) [`Constraint`]
/// with the provided [`CumulativeOptions`].
///
/// See the documentation of [`cumulative`] for more information about the constraint.
pub fn cumulative_with_options<StartTimes, Durations, ResourceRequirements>(
    start_times: StartTimes,
    durations: Durations,
    resource_requirements: ResourceRequirements,
    resource_capacity: i32,
    options: CumulativeOptions,
    constraint_tag: ConstraintTag,
) -> impl Constraint
where
    StartTimes: IntoIterator,
    StartTimes::Item: IntegerVariable + Debug + 'static,
    StartTimes::IntoIter: ExactSizeIterator,
    Durations: IntoIterator<Item = i32>,
    Durations::IntoIter: ExactSizeIterator,
    ResourceRequirements: IntoIterator<Item = i32>,
    ResourceRequirements::IntoIter: ExactSizeIterator,
{
    let start_times = start_times.into_iter();
    let durations = durations.into_iter();
    let resource_requirements = resource_requirements.into_iter();

    pumpkin_assert_simple!(
        start_times.len() == durations.len() && durations.len() == resource_requirements.len(),
        "The number of start variables, durations and resource requirements should be the same!"
    );

    CumulativeConstraint::new(
        &start_times
            .zip(durations)
            .zip(resource_requirements)
            .map(|((start_time, duration), resource_requirement)| ArgTask {
                start_time,
                processing_time: duration,
                resource_usage: resource_requirement,
            })
            .collect::<Vec<_>>(),
        resource_capacity,
        options,
        constraint_tag,
    )
}

struct CumulativeConstraint<Var> {
    tasks: Vec<ArgTask<Var>>,
    resource_capacity: i32,
    options: CumulativeOptions,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> CumulativeConstraint<Var> {
    fn new(
        tasks: &[ArgTask<Var>],
        resource_capacity: i32,
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

impl<Var: IntegerVariable + 'static + Debug> Constraint for CumulativeConstraint<Var> {
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
