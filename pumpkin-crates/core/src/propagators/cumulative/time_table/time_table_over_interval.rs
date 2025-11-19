use std::rc::Rc;

use super::TimeTable;
use super::time_table_util::propagate_based_on_timetable;
use super::time_table_util::should_enqueue;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::conjunction;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagators::ArgTask;
use crate::propagators::CumulativeParameters;
use crate::propagators::CumulativePropagatorOptions;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
#[cfg(doc)]
use crate::propagators::TimeTablePerPointPropagator;
use crate::propagators::UpdatableStructures;
use crate::propagators::cumulative::time_table::propagation_handler::create_explanation_profile_height;
use crate::propagators::util::create_tasks;
use crate::propagators::util::register_tasks;
use crate::propagators::util::update_bounds_task;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// An event storing the start and end of mandatory parts used for creating the time-table
#[derive(Debug)]
pub(crate) struct Event<Var, PVar, RVar> {
    /// The time-point at which the [`Event`] took place
    time_stamp: i32,
    /// Change in resource usage at [time_stamp][Event::time_stamp], positive if it is the start of
    /// a mandatory part and negative otherwise
    change_in_resource_usage: i32,
    /// The [`Task`] which has caused the event to take place
    task: Rc<Task<Var, PVar, RVar>>,
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
#[derive(Debug)]
pub(crate) struct TimeTableOverIntervalPropagator<Var, PVar, RVar, CVar> {
    /// Stores whether the time-table is empty
    is_time_table_empty: bool,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var, PVar, RVar, CVar>,
    /// Stores structures which change during the search; used to store the bounds
    updatable_structures: UpdatableStructures<Var, PVar, RVar>,

    inference_code: InferenceCode,
}

pub(crate) struct TimeTableOverIntervalConstructor<Var, PVar, RVar, CVar> {
    tasks: Vec<ArgTask<Var, PVar, RVar>>,
    capacity: CVar,
    cumulative_options: CumulativePropagatorOptions,
    constraint_tag: ConstraintTag,
}

impl<Var, PVar, RVar, CVar> TimeTableOverIntervalConstructor<Var, PVar, RVar, CVar> {
    pub(crate) fn new(
        arg_tasks: Vec<ArgTask<Var, PVar, RVar>>,
        capacity: CVar,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self {
            tasks: arg_tasks,
            capacity,
            cumulative_options,
            constraint_tag,
        }
    }
}

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
> PropagatorConstructor for TimeTableOverIntervalConstructor<Var, PVar, RVar, CVar>
{
    type PropagatorImpl = TimeTableOverIntervalPropagator<Var, PVar, RVar, CVar>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let inference_code = context.create_inference_code(self.constraint_tag, TimeTable);

        TimeTableOverIntervalPropagator::new(
            context,
            &self.tasks,
            self.capacity,
            self.cumulative_options,
            inference_code,
        )
    }
}

/// The type of the time-table used by propagators which use time-table reasoning over intervals.
///
/// The [ResourceProfile]s are sorted based on start time and they are non-overlapping; each entry
/// in the [`Vec`] represents the mandatory resource usage across an interval.
pub(crate) type OverIntervalTimeTableType<Var, PVar, RVar> = Vec<ResourceProfile<Var, PVar, RVar>>;

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
> TimeTableOverIntervalPropagator<Var, PVar, RVar, CVar>
{
    fn new(
        mut context: PropagatorConstructorContext,
        arg_tasks: &[ArgTask<Var, PVar, RVar>],
        capacity: CVar,
        cumulative_options: CumulativePropagatorOptions,
        inference_code: InferenceCode,
    ) -> TimeTableOverIntervalPropagator<Var, PVar, RVar, CVar> {
        let tasks = create_tasks(context.as_readonly(), arg_tasks);

        let parameters =
            CumulativeParameters::new(context.as_readonly(), tasks, capacity, cumulative_options);
        register_tasks(
            &parameters.tasks,
            &parameters.capacity,
            context.reborrow(),
            false,
        );

        let mut updatable_structures = UpdatableStructures::new(&parameters);
        updatable_structures.initialise_bounds_and_remove_fixed(context.as_readonly(), &parameters);

        TimeTableOverIntervalPropagator {
            is_time_table_empty: true,
            parameters,
            updatable_structures,
            inference_code,
        }
    }
}

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
> Propagator for TimeTableOverIntervalPropagator<Var, PVar, RVar, CVar>
{
    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        if self.parameters.is_infeasible {
            return Err(Inconsistency::Conflict(PropagatorConflict {
                conjunction: conjunction!(),
                inference_code: self.inference_code,
            }));
        }

        let time_table = create_time_table_over_interval_from_scratch(
            &mut context,
            &self.parameters,
            self.inference_code,
        )?;
        self.is_time_table_empty = time_table.is_empty();
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(
            &mut context,
            self.inference_code,
            time_table.iter(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn synchronise(&mut self, context: PropagationContext) {
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context, &self.parameters);
    }

    fn notify(
        &mut self,
        context: PropagationContextWithTrailedValues,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if local_id.unpack() as usize >= self.parameters.tasks.len() {
            // The upper-bound of the capacity has been updated; we should enqueue
            return EnqueueDecision::Enqueue;
        }

        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        // Note that it could be the case that `is_time_table_empty` is inaccurate here since it
        // wasn't updated in `synchronise`; however, `synchronise` will only remove profiles
        // meaning that `is_time_table_empty` will always return `false` when it is not
        // empty and it might return `false` even when the time-table is not empty *but* it
        // will never return `true` when the time-table is not empty.
        let result = should_enqueue(
            &self.parameters,
            &self.updatable_structures,
            &updated_task,
            context.as_readonly(),
            self.is_time_table_empty,
        );

        update_bounds_task(
            context.as_readonly(),
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if context.is_fixed(&updated_task.start_variable)
            && context.is_fixed(&updated_task.processing_time)
            && context.is_fixed(&updated_task.resource_usage)
        {
            self.updatable_structures.fix_task(&updated_task)
        }

        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverInterval"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        debug_propagate_from_scratch_time_table_interval(
            &mut context,
            &self.parameters,
            &self.updatable_structures,
            self.inference_code,
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
/// conflict in the form of an [`Inconsistency`].
pub(crate) fn create_time_table_over_interval_from_scratch<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    context: &mut PropagationContextMut,
    parameters: &CumulativeParameters<Var, PVar, RVar, CVar>,
    inference_code: InferenceCode,
) -> Result<OverIntervalTimeTableType<Var, PVar, RVar>, Inconsistency> {
    // First we create a list of all the events (i.e. start and ends of mandatory parts)
    let events = create_events(context.as_readonly(), parameters);

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
fn create_events<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
    Context: ReadDomains + Copy,
>(
    context: Context,
    parameters: &CumulativeParameters<Var, PVar, RVar, CVar>,
) -> Vec<Event<Var, PVar, RVar>> {
    // First we create a list of events with which we will create the time-table
    let mut events: Vec<Event<Var, PVar, RVar>> = Vec::new();
    // Then we go over every task
    for task in parameters.tasks.iter() {
        let upper_bound = context.upper_bound(&task.start_variable);
        let lower_bound = context.lower_bound(&task.start_variable);
        if upper_bound < lower_bound + context.lower_bound(&task.processing_time) {
            // The task has a mandatory part, we need to add the appropriate events to the
            // events list

            // Thus we first add an event for the start of a mandatory part (with positive
            // resource usage)
            events.push(Event {
                time_stamp: upper_bound,
                change_in_resource_usage: context.lower_bound(&task.resource_usage),
                task: Rc::clone(task),
            });

            // Then we create an event for the end of a mandatory part (with negative resource
            // usage)
            events.push(Event {
                time_stamp: lower_bound + context.lower_bound(&task.processing_time),
                change_in_resource_usage: -context.lower_bound(&task.resource_usage),
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
fn create_time_table_from_events<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    events: Vec<Event<Var, PVar, RVar>>,
    context: &mut PropagationContextMut,
    inference_code: InferenceCode,
    parameters: &CumulativeParameters<Var, PVar, RVar, CVar>,
) -> Result<OverIntervalTimeTableType<Var, PVar, RVar>, Inconsistency> {
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

    let mut time_table: OverIntervalTimeTableType<Var, PVar, RVar> = Default::default();
    // The tasks which are contributing to the current profile under consideration
    let mut current_profile_tasks: Vec<Rc<Task<Var, PVar, RVar>>> = Vec::new();
    // The cumulative resource usage of the tasks which are contributing to the current profile
    // under consideration
    let mut current_resource_usage: i32 = 0;
    // The beginning of the current interval under consideration
    let mut start_of_interval: i32 = -1;

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

            // Potentially we need to end the current profile and start a new one due to the
            // addition/removal of the current task
            if start_of_interval != event.time_stamp {
                let new_profile = ResourceProfile {
                    start: start_of_interval,
                    end: event.time_stamp - 1,
                    profile_tasks: current_profile_tasks.clone(),
                    height: current_resource_usage,
                };

                // Now we propagate the lower-bound of the capacity
                if new_profile.height > context.lower_bound(&parameters.capacity) {
                    context.post(
                        predicate!(parameters.capacity >= new_profile.height),
                        create_explanation_profile_height(
                            context.as_readonly(),
                            inference_code,
                            &new_profile,
                            parameters.options.explanation_type,
                        )
                        .conjunction,
                        inference_code,
                    )?;
                }
                // We end the current profile, creating a profile from [start_of_interval,
                // time_stamp)
                time_table.push(new_profile);
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
                    // If the profile is already conflicting then we shouldn't add more tasks.
                    // This could be changed in the future so that we can pick the tasks which
                    // are used for the conflict explanation
                    current_profile_tasks.push(event.task);
                }
            }
        }
    }
    Ok(time_table)
}

fn check_starting_new_profile_invariants<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    event: &Event<Var, PVar, RVar>,
    current_resource_usage: i32,
    current_profile_tasks: &[Rc<Task<Var, PVar, RVar>>],
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

pub(crate) fn debug_propagate_from_scratch_time_table_interval<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    context: &mut PropagationContextMut,
    parameters: &CumulativeParameters<Var, PVar, RVar, CVar>,
    updatable_structures: &UpdatableStructures<Var, PVar, RVar>,
    inference_code: InferenceCode,
) -> PropagationStatusCP {
    // We first create a time-table over interval and return an error if there was
    // an overflow of the resource capacity while building the time-table
    let time_table =
        create_time_table_over_interval_from_scratch(context, parameters, inference_code)?;
    // Then we check whether propagation can take place
    propagate_based_on_timetable(
        context,
        inference_code,
        time_table.iter(),
        parameters,
        &mut updatable_structures.recreate_from_context(context.as_readonly(), parameters),
    )
}

#[cfg(test)]
mod tests {
    use crate::conjunction;
    use crate::constraint_arguments::CumulativeExplanationType;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::propagators::ArgTask;
    use crate::propagators::CumulativePropagatorOptions;
    use crate::propagators::TimeTableOverIntervalConstructor;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);
    }

    #[test]
    fn propagator_detects_conflict() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 1);
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(TimeTableOverIntervalConstructor::new(
            [
                ArgTask {
                    start_time: s1,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2,
                    processing_time: 4,
                    resource_usage: 1,
                },
            ]
            .into_iter()
            .collect::<Vec<_>>(),
            1,
            CumulativePropagatorOptions {
                explanation_type: CumulativeExplanationType::Naive,
                ..Default::default()
            },
            constraint_tag,
        ));

        assert!(result.is_err());
        let reason = solver.get_reason_int(Predicate::trivially_false());
        let expected = [
            predicate!(s1 <= 1),
            predicate!(s1 >= 1),
            predicate!(s2 >= 1),
            predicate!(s2 <= 1),
        ];
        assert!(
            expected
                .iter()
                .all(|y| { reason.iter().collect::<Vec<&Predicate>>().contains(&y) })
                && reason.iter().all(|y| expected.contains(y))
        );
    }

    #[test]
    fn propagator_propagates_nothing() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(0, 6);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 6);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(2, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b = solver.new_variable(2, 3);
        let a = solver.new_variable(0, 1);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 5,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        let notification_status = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 7);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 5);
        assert_eq!(solver.upper_bound(s1), 6);
    }

    #[test]
    fn propagator_propagates_end_time() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(6, 6);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason_int(predicate!(s2 <= 3));
        assert_eq!(conjunction!([s2 <= 8] & [s1 >= 6] & [s1 <= 6]), reason);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt_after_update() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(0, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b = solver.new_variable(2, 3);
        let a = solver.new_variable(0, 1);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 4,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(a), 0);
        assert_eq!(solver.upper_bound(a), 1);
        assert_eq!(solver.lower_bound(b), 2);
        assert_eq!(solver.upper_bound(b), 3);
        assert_eq!(solver.lower_bound(c), 8);
        assert_eq!(solver.upper_bound(c), 9);
        assert_eq!(solver.lower_bound(d), 0);
        assert_eq!(solver.upper_bound(d), 2);
        assert_eq!(solver.lower_bound(e), 0);
        assert_eq!(solver.upper_bound(e), 4);
        assert_eq!(solver.lower_bound(f), 0);
        assert_eq!(solver.upper_bound(f), 14);

        let notification_status = solver.increase_lower_bound_and_notify(propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt_multiple_profiles() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(0, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b2 = solver.new_variable(5, 5);
        let b1 = solver.new_variable(3, 3);
        let a = solver.new_variable(0, 1);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b1,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: b2,
                        processing_time: 3,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 4,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(a), 0);
        assert_eq!(solver.upper_bound(a), 1);
        assert_eq!(solver.lower_bound(c), 8);
        assert_eq!(solver.upper_bound(c), 9);
        assert_eq!(solver.lower_bound(d), 0);
        assert_eq!(solver.upper_bound(d), 2);
        assert_eq!(solver.lower_bound(e), 0);
        assert_eq!(solver.upper_bound(e), 4);
        assert_eq!(solver.lower_bound(f), 0);
        assert_eq!(solver.upper_bound(f), 14);

        let notification_status = solver.increase_lower_bound_and_notify(propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason_int(predicate!(s2 >= 5));
        assert_eq!(conjunction!([s2 >= 1] & [s1 >= 1] & [s1 <= 1]), reason);
    }

    #[test]
    fn propagator_propagates_generic_bounds() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(3, 3);
        let s2 = solver.new_variable(5, 5);
        let s3 = solver.new_variable(1, 15);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason_int(predicate!(s3 >= 7));
        assert_eq!(conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 5]), reason);
    }

    #[test]
    fn propagator_propagates_with_holes() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(4, 4);
        let s2 = solver.new_variable(0, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalConstructor::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    allow_holes_in_domain: true,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 4);
        assert_eq!(solver.upper_bound(s1), 4);

        for removed in 2..8 {
            assert!(!solver.contains(s2, removed));
            let reason = solver.get_reason_int(predicate!(s2 != removed));
            assert_eq!(conjunction!([s1 <= 4] & [s1 >= 4]), reason);
        }
    }
}
