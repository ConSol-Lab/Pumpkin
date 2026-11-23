//! [`Propagator`] for the Cumulative constraint; it
//! reasons over individual time-points instead of intervals. See [`TimeTablePerPointPropagator`]
//! for more information.

use std::collections::BTreeMap;
use std::rc::Rc;

use pumpkin_core::asserts::pumpkin_assert_extreme;
use pumpkin_core::conjunction;
use pumpkin_core::constraints::ReifiablePropagatorConstructor;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::ImpliedBy;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotReified;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::REIFICATION_LOCAL_ID;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::Reification;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::state::propagator_conflict;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;

use super::TimeTable;
use super::time_table_util::propagate_based_on_timetable;
use super::time_table_util::should_enqueue;
use crate::cumulative::ArgTask;
use crate::cumulative::CumulativeParameters;
use crate::cumulative::ResourceProfile;
use crate::cumulative::UpdatableStructures;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::CheckerTask;
use crate::cumulative::time_table::TimeTableChecker;
use crate::cumulative::util::create_tasks;
use crate::cumulative::util::register_tasks;
use crate::cumulative::util::update_bounds_task;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;

/// [`Propagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a resource profile per time point rather than
/// creating one over an interval (hence the name). Furthermore, the [`TimeTablePerPointPropagator`]
/// has a generic argument which represents the type of variable used for modelling the start
/// variables, this will be an implementation of [`IntegerVariable`].
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug, Clone)]
pub struct TimeTablePerPointPropagator<Var, R = NotReified> {
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
    /// Stores structures which change during the search; used to store the bounds
    updatable_structures: UpdatableStructures<Var>,

    // TODO: Update with proapgator constructor.
    constraint_tag: ConstraintTag,
    inference_code: Option<InferenceCode>,

    /// Whether the propagator is half-reified.
    reification: R,
}

/// The type of the time-table used by propagators which use time-table reasoning per time-point;
/// using a [`ResourceProfile`] is more complex than necessary (as [`ResourceProfile::start`] =
/// [`ResourceProfile::end`]) but it allows for a more unified implementation of methods.
///
/// The key t (representing a time-point) holds the mandatory resource consumption of tasks at
/// that time (stored in a [`ResourceProfile`]); the [ResourceProfile]s are sorted based on
/// start time and they are non-overlapping
pub(crate) type PerPointTimeTableType<Var> = BTreeMap<u32, ResourceProfile<Var>>;

impl<Var: IntegerVariable + 'static> TimeTablePerPointPropagator<Var> {
    pub fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> TimeTablePerPointPropagator<Var> {
        let tasks = create_tasks(arg_tasks);
        let parameters = CumulativeParameters::new(tasks, capacity, cumulative_options);
        let updatable_structures = UpdatableStructures::new(&parameters);

        TimeTablePerPointPropagator {
            parameters,
            updatable_structures,
            constraint_tag,
            inference_code: None,
            reification: NotReified,
        }
    }
}

impl<Var: IntegerVariable + 'static> ReifiablePropagatorConstructor
    for TimeTablePerPointPropagator<Var>
{
    type ImpliedBy = TimeTablePerPointPropagator<Var, ImpliedBy>;

    fn into_implied_by(self, reification_literal: Literal) -> Self::ImpliedBy {
        TimeTablePerPointPropagator {
            parameters: self.parameters,
            updatable_structures: self.updatable_structures,
            constraint_tag: self.constraint_tag,
            inference_code: self.inference_code,
            reification: ImpliedBy(reification_literal),
        }
    }
}

impl<Var: IntegerVariable + 'static, R: Reification> PropagatorConstructor
    for TimeTablePerPointPropagator<Var, R>
{
    type PropagatorImpl = Self;

    fn create(
        mut self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        self.updatable_structures
            .initialise_bounds_and_remove_fixed(context.domains(), &self.parameters);
        let mut registration = register_tasks(&self.parameters.tasks, context.reborrow(), false);
        self.reification.register(&mut registration);

        let mut checkers = RuntimeCheckers::builder();
        self.inference_code = Some(
            checkers.add_inference_checker(
                self.constraint_tag,
                TimeTable,
                TimeTableChecker {
                    tasks: self
                        .parameters
                        .tasks
                        .iter()
                        .map(|task| CheckerTask {
                            start_time: task.start_variable.clone(),
                            processing_time: task.processing_time,
                            resource_usage: task.resource_usage,
                        })
                        .collect(),
                    capacity: self.parameters.capacity,
                },
            ),
        );

        PropagatorSpec {
            registration,
            checkers: checkers.build().reified(self.reification),
            propagator: self,
        }
    }
}

impl<Var: IntegerVariable + 'static, R: Reification> Propagator
    for TimeTablePerPointPropagator<Var, R>
{
    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if !self.reification.should_propagate(&mut context, |_| None)? {
            return Ok(());
        }

        let reification = self.reification;
        reification.add_to_conflict(self.propagate_enforced(context))
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if !self.reification.should_propagate(&mut context, |_| None)? {
            return Ok(());
        }

        self.reification
            .add_to_conflict(self.propagate_from_scratch_enforced(context))
    }

    fn synchronise(&mut self, mut context: NotificationContext<'_>) {
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters)
    }

    fn notify(
        &mut self,
        mut context: NotificationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if local_id == REIFICATION_LOCAL_ID {
            return EnqueueDecision::Enqueue;
        }

        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);

        let result = should_enqueue(
            &self.updatable_structures,
            &updated_task,
            context.domains(),
            &self.parameters,
        );

        // Note that the non-incremental proapgator does not make use of `result.updated` since it
        // propagates from scratch anyways
        update_bounds_task(
            context.domains(),
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if matches!(
            updated_task.start_variable.unpack_event(event),
            DomainEvent::Assign
        ) {
            self.updatable_structures.fix_task(&updated_task)
        }

        self.reification
            .filter_enqueue_decision(&mut context, result.decision, |_| None)
    }

    fn priority(&self) -> Priority {
        Priority::VeryLow
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }
}

impl<Var: IntegerVariable + 'static, R: Reification> TimeTablePerPointPropagator<Var, R> {
    fn propagate_enforced(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if self.parameters.is_infeasible {
            return propagator_conflict(conjunction!(), self.inference_code.as_ref().unwrap());
        }

        let time_table = create_time_table_per_point_from_scratch(
            context.domains(),
            self.inference_code.as_ref().unwrap(),
            &self.parameters,
        )?;
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(
            &mut context,
            self.inference_code.as_ref().unwrap(),
            time_table.values(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn propagate_from_scratch_enforced(
        &self,
        mut context: PropagationContext,
    ) -> PropagationStatusCP {
        propagate_from_scratch_time_table_point(
            &mut context,
            self.inference_code.as_ref().unwrap(),
            &self.parameters,
            &self.updatable_structures,
        )
    }
}

/// Creates a time-table consisting of [`ResourceProfile`]s which represent rectangles with a
/// start and end (both inclusive) consisting of tasks with a cumulative height Assumptions:
/// The time-table is sorted based on start time and none of the profiles overlap - generally,
/// it is assumed that the calculated [`ResourceProfile`]s are maximal
///
/// The result of this method is either the time-table of type
/// [`PerPointTimeTableType`] or the tasks responsible for the
/// conflict in the form of an [`PropagatorConflict`].
pub(crate) fn create_time_table_per_point_from_scratch<
    Var: IntegerVariable + 'static,
    Context: ReadDomains,
>(
    context: Context,
    inference_code: &InferenceCode,
    parameters: &CumulativeParameters<Var>,
) -> Result<PerPointTimeTableType<Var>, PropagatorConflict> {
    let mut time_table: PerPointTimeTableType<Var> = PerPointTimeTableType::new();
    // First we go over all tasks and determine their mandatory parts
    for task in parameters.tasks.iter() {
        let upper_bound = context.upper_bound(&task.start_variable);
        let lower_bound = context.lower_bound(&task.start_variable);

        if upper_bound < lower_bound + task.processing_time {
            // There is a mandatory part
            for i in upper_bound..(lower_bound + task.processing_time) {
                // For every time-point of the mandatory part,
                //  add the resource usage of the current task to the ResourceProfile and add it
                // to the profile tasks of the resource
                let current_profile: &mut ResourceProfile<Var> = time_table
                    .entry(i as u32)
                    .or_insert(ResourceProfile::default(i));
                current_profile.height += task.resource_usage;
                current_profile.profile_tasks.push(Rc::clone(task));

                if current_profile.height > parameters.capacity {
                    // The addition of the current task to the resource profile has caused an
                    // overflow
                    return Err(create_conflict_explanation(
                        context,
                        inference_code,
                        current_profile,
                        parameters.options.explanation_type,
                        parameters.capacity,
                    ));
                }
            }
        }
    }
    pumpkin_assert_extreme!(
        time_table
            .values()
            .all(|profile| profile.start == profile.end),
        "The TimeTablePerPointPropagator method should only create profiles where `start == end`"
    );
    Ok(time_table)
}

pub(crate) fn propagate_from_scratch_time_table_point<Var: IntegerVariable + 'static>(
    context: &mut PropagationContext,
    inference_code: &InferenceCode,
    parameters: &CumulativeParameters<Var>,
    updatable_structures: &UpdatableStructures<Var>,
) -> PropagationStatusCP {
    // We first create a time-table per point and return an error if there was
    // an overflow of the resource capacity while building the time-table
    let time_table =
        create_time_table_per_point_from_scratch(context.domains(), inference_code, parameters)?;
    // Then we check whether propagation can take place
    let mut updatable_structures_clone =
        updatable_structures.recreate_from_context(context.domains(), parameters);
    propagate_based_on_timetable(
        context,
        inference_code,
        time_table.values(),
        parameters,
        &mut updatable_structures_clone,
    )
}
