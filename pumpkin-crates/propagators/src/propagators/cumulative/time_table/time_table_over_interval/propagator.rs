use std::rc::Rc;

use pumpkin_core::asserts::pumpkin_assert_extreme;
use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::state::propagator_conflict;
use pumpkin_core::variables::IntegerVariable;

use crate::cumulative::ArgTask;
use crate::cumulative::CumulativeParameters;
use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::cumulative::UpdatableStructures;
use crate::cumulative::options::CumulativePropagatorOptions;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTablePerPointPropagator;
use crate::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::cumulative::time_table::time_table_util::should_enqueue;
use crate::cumulative::util::create_tasks;
use crate::cumulative::util::update_bounds_task;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;

/// An event storing the start and end of mandatory parts used for creating the time-table
#[derive(Debug)]
pub(crate) struct Event<Var> {
    /// The time-point at which the [`Event`] took place
    time_stamp: i32,
    /// Change in resource usage at [time_stamp][Event::time_stamp], positive if it is the start of
    /// a mandatory part and negative otherwise
    change_in_resource_usage: i32,
    /// The [`Task`] which has caused the event to take place
    task: Rc<Task<Var>>,
}

/// [`Propagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a resource profile over an interval rather than
/// creating one per time-point (as is done in [`TimeTablePerPointPropagator`]).
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug, Clone)]
pub struct TimeTableOverIntervalPropagator<Var> {
    /// Stores the input parameters to the cumulative constraint
    pub(super) parameters: CumulativeParameters<Var>,
    /// Stores structures which change during the search; used to store the bounds
    pub(super) updatable_structures: UpdatableStructures<Var>,

    // TODO: Update with propagator constructor.
    pub(super) constraint_tag: ConstraintTag,
    pub(super) inference_code: Option<InferenceCode>,
}

/// The type of the time-table used by propagators which use time-table reasoning over intervals.
///
/// The [ResourceProfile]s are sorted based on start time and they are non-overlapping; each entry
/// in the [`Vec`] represents the mandatory resource usage across an interval.
pub(crate) type OverIntervalTimeTableType<Var> = Vec<ResourceProfile<Var>>;
impl<Var: IntegerVariable + 'static> TimeTableOverIntervalPropagator<Var> {
    pub fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> TimeTableOverIntervalPropagator<Var> {
        let tasks = create_tasks(arg_tasks);
        let parameters = CumulativeParameters::new(tasks, capacity, cumulative_options);
        let updatable_structures = UpdatableStructures::new(&parameters);

        TimeTableOverIntervalPropagator {
            parameters,
            updatable_structures,
            constraint_tag,
            inference_code: None,
        }
    }
}

impl<Var: IntegerVariable + 'static> Propagator for TimeTableOverIntervalPropagator<Var> {
    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if self.parameters.is_infeasible {
            return propagator_conflict(conjunction!(), self.inference_code.as_ref().unwrap());
        }

        let time_table = create_time_table_over_interval_from_scratch(
            context.domains(),
            &self.parameters,
            self.inference_code.as_ref().unwrap(),
        )?;
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(
            &mut context,
            self.inference_code.as_ref().unwrap(),
            time_table.iter(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn synchronise(&mut self, mut context: NotificationContext<'_>) {
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters);
    }

    fn notify(
        &mut self,
        mut context: NotificationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);

        let result = should_enqueue(
            &self.updatable_structures,
            &updated_task,
            context.domains(),
            &self.parameters,
        );

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

        result.decision
    }

    fn priority(&self) -> Priority {
        Priority::VeryLow
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverInterval"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        propagate_from_scratch_time_table_interval(
            &mut context,
            &self.parameters,
            &self.updatable_structures,
            self.inference_code.as_ref().unwrap(),
        )
    }
}

/// Creates a time-table consisting of [`ResourceProfile`]s which represent rectangles with a
/// start and end (both inclusive) consisting of tasks with a cumulative height.
///
/// **Assumptions:**
/// The time-table is sorted based on start time and none of the profiles overlap - it is
/// assumed that the calculated [`ResourceProfile`]s are maximal
///
/// The result of this method is either the time-table of type
/// [`OverIntervalTimeTableType`] or the tasks responsible for the
/// conflict in the form of an [`PropagatorConflict`].
pub(crate) fn create_time_table_over_interval_from_scratch<Var: IntegerVariable + 'static>(
    mut context: Domains,
    parameters: &CumulativeParameters<Var>,
    inference_code: &InferenceCode,
) -> Result<OverIntervalTimeTableType<Var>, PropagatorConflict> {
    // First we create a list of all the events (i.e. start and ends of mandatory parts)
    let events = create_events(context.reborrow(), parameters);

    // Then we create a time-table using these events
    create_time_table_from_events(events, context, inference_code, parameters)
}

/// Creates a list of all the events (for the starts and ends of mandatory parts) of all the
/// tasks defined in `parameters`.
///
/// The events are returned in chonological order, if a tie between time points occurs then this
/// is resolved by placing the events which signify the ends of mandatory parts first (if the
/// tie is between events of the same type then the tie-breaking is done on the id in
/// non-decreasing order).
fn create_events<Var: IntegerVariable + 'static, Context: ReadDomains>(
    context: Context,
    parameters: &CumulativeParameters<Var>,
) -> Vec<Event<Var>> {
    // First we create a list of events with which we will create the time-table
    let mut events: Vec<Event<Var>> = Vec::new();
    // Then we go over every task
    for task in parameters.tasks.iter() {
        let upper_bound = context.upper_bound(&task.start_variable);
        let lower_bound = context.lower_bound(&task.start_variable);
        if upper_bound < lower_bound + task.processing_time {
            // The task has a mandatory part, we need to add the appropriate events to the
            // events list

            // Thus we first add an event for the start of a mandatory part (with positive
            // resource usage)
            events.push(Event {
                time_stamp: upper_bound,
                change_in_resource_usage: task.resource_usage,
                task: Rc::clone(task),
            });

            // Then we create an event for the end of a mandatory part (with negative resource
            // usage)
            events.push(Event {
                time_stamp: lower_bound + task.processing_time,
                change_in_resource_usage: -task.resource_usage,
                task: Rc::clone(task),
            });
        }
    }
    // We will go over the events in chronological order (non-decreasing time_stamp);
    // this allows us to build the time-table in a single pass
    events.sort_by(|a, b| {
        match a.time_stamp.cmp(&b.time_stamp) {
            // If the time_stamps are equal then we first go through the ends of the mandatory
            // parts. This allows us to build smaller explanations by ensuring
            // that we report an error as soon as it can be found
            std::cmp::Ordering::Equal => {
                if a.change_in_resource_usage.signum() != b.change_in_resource_usage.signum() {
                    // If `a` is the start (end) of a mandatory part and `b` is the end (start)
                    // of a mandatory part then we need to ensure that
                    // we go through the end of the mandatory part first
                    a.change_in_resource_usage.cmp(&b.change_in_resource_usage)
                } else {
                    // If both events are starts or both events are ends then we sort on the
                    // task id for easier reproducibility
                    a.task.id.unpack().cmp(&b.task.id.unpack())
                }
            }
            other_ordering => other_ordering,
        }
    });

    events
}

/// Creates a time-table based on the provided `events` (which are assumed to be sorted
/// chronologically, with tie-breaking performed in such a way that the ends of mandatory parts
/// are before the starts of mandatory parts).
fn create_time_table_from_events<Var: IntegerVariable + 'static, Context: ReadDomains>(
    events: Vec<Event<Var>>,
    context: Context,
    inference_code: &InferenceCode,
    parameters: &CumulativeParameters<Var>,
) -> Result<OverIntervalTimeTableType<Var>, PropagatorConflict> {
    pumpkin_assert_extreme!(
        events.is_empty()
            || (0..events.len() - 1)
                .all(|index| events[index].time_stamp <= events[index + 1].time_stamp),
        "Events that were passed were not sorted chronologically"
    );
    pumpkin_assert_extreme!(
        events.is_empty()
            || (0..events.len() - 1).all(|index| events[index].time_stamp
                != events[index + 1].time_stamp
                || events[index].change_in_resource_usage.signum()
                    <= events[index + 1].change_in_resource_usage.signum()),
        "Events were not ordered in such a way that the ends of mandatory parts occurred first"
    );

    let mut time_table: OverIntervalTimeTableType<Var> = Default::default();
    // The tasks which are contributing to the current profile under consideration
    let mut current_profile_tasks: Vec<Rc<Task<Var>>> = Vec::new();
    // The cumulative resource usage of the tasks which are contributing to the current profile
    // under consideration
    let mut current_resource_usage: i32 = 0;
    // The beginning of the current interval under consideration
    let mut start_of_interval: i32 = -1;
    // Determines whether a conflict has occurred
    let mut is_conflicting = false;

    // We go over all the events and create the time-table
    for event in events {
        if start_of_interval == -1 {
            // A new profile needs to be started
            pumpkin_assert_moderate!(
                check_starting_new_profile_invariants(
                    &event,
                    current_resource_usage,
                    &current_profile_tasks
                ),
                "The invariants for creating a new profile did not hold"
            );

            // We thus assign the start of the interval to the time_stamp of the event, add its
            // resource usage and add it to the contributing tasks
            start_of_interval = event.time_stamp;
            current_resource_usage = event.change_in_resource_usage;
            current_profile_tasks.push(event.task);
        } else {
            // A profile is currently being created

            // We have first traversed all of the ends of mandatory parts, meaning that any
            // overflow will persist after processing all events at this time-point
            if current_resource_usage > parameters.capacity {
                is_conflicting = true;
            }

            // Potentially we need to end the current profile and start a new one due to the
            // addition/removal of the current task
            if start_of_interval != event.time_stamp {
                let new_profile = ResourceProfile {
                    start: start_of_interval,
                    end: event.time_stamp - 1,
                    profile_tasks: current_profile_tasks.clone(),
                    height: current_resource_usage,
                };
                if is_conflicting {
                    // We have found a conflict and the profile has been ended, we can report the
                    // conflict using the current profile
                    return Err(create_conflict_explanation(
                        context,
                        inference_code,
                        &new_profile,
                        parameters.options.explanation_type,
                        parameters.capacity,
                    ));
                } else {
                    // We end the current profile, creating a profile from [start_of_interval,
                    // time_stamp)
                    time_table.push(new_profile);
                }
            }
            // Process the current event, note that `change_in_resource_usage` can be negative
            pumpkin_assert_simple!(
                event.change_in_resource_usage > 0
                    || current_resource_usage >= event.change_in_resource_usage,
                "Processing this task would have caused negative resource usage which should not be possible"
            );
            current_resource_usage += event.change_in_resource_usage;
            if current_resource_usage == 0 {
                // No tasks have an active mandatory at the current `time_stamp`
                // We can thus reset the start of the interval and remove all profile tasks
                start_of_interval = -1;
                current_profile_tasks.clear();
            } else {
                // There are still tasks which have a mandatory part at the current time-stamp
                // We thus need to start a new profile
                start_of_interval = event.time_stamp;
                if event.change_in_resource_usage < 0 {
                    pumpkin_assert_simple!(!is_conflicting);
                    // The mandatory part of a task has ended, we should thus remove it from the
                    // contributing tasks
                    let _ = current_profile_tasks.remove(
                        current_profile_tasks
                            .iter()
                            .position(|current_task| {
                                current_task.id.unpack() == event.task.id.unpack()
                            })
                            .expect("Task should have been found in `current_profile`"),
                    );
                } else {
                    // The mandatory part of a task has started, we should thus add it to the
                    // set of contributing tasks
                    pumpkin_assert_extreme!(
                        !current_profile_tasks.contains(&event.task),
                        "Task is being added to the profile while it is already part of the contributing tasks"
                    );
                    // We add the task even if we are already conflicting so that we can pick the
                    // tasks which are used for the conflict explanation
                    current_profile_tasks.push(event.task);
                }
            }
        }
    }
    pumpkin_assert_simple!(!is_conflicting);
    Ok(time_table)
}

fn check_starting_new_profile_invariants<Var: IntegerVariable + 'static>(
    event: &Event<Var>,
    current_resource_usage: i32,
    current_profile_tasks: &[Rc<Task<Var>>],
) -> bool {
    if event.change_in_resource_usage <= 0 {
        eprintln!(
            "The resource usage of an event which causes a new profile to be started should never be negative"
        )
    }
    if current_resource_usage != 0 {
        eprintln!("The resource usage should be 0 when a new profile is started")
    }
    if !current_profile_tasks.is_empty() {
        eprintln!("There should be no contributing tasks when a new profile is started")
    }
    event.change_in_resource_usage > 0
        && current_resource_usage == 0
        && current_profile_tasks.is_empty()
}

pub(crate) fn propagate_from_scratch_time_table_interval<Var: IntegerVariable + 'static>(
    context: &mut PropagationContext,
    parameters: &CumulativeParameters<Var>,
    updatable_structures: &UpdatableStructures<Var>,
    inference_code: &InferenceCode,
) -> PropagationStatusCP {
    // We first create a time-table over interval and return an error if there was
    // an overflow of the resource capacity while building the time-table
    let time_table = create_time_table_over_interval_from_scratch(
        context.domains(),
        parameters,
        inference_code,
    )?;
    // Then we check whether propagation can take place
    let mut updatable_structures_clone =
        updatable_structures.recreate_from_context(context.domains(), parameters);
    propagate_based_on_timetable(
        context,
        inference_code,
        time_table.iter(),
        parameters,
        &mut updatable_structures_clone,
    )
}
