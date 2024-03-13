use std::rc::Rc;

use super::time_table_util::propagate_based_on_timetable;
use super::time_table_util::should_enqueue;
use super::time_table_util::ResourceProfile;
use crate::basic_types::variables::IntVar;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::EnqueueDecision;
use crate::engine::PropagationContext;
use crate::engine::PropagationContextMut;
use crate::engine::PropagatorConstructorContext;
use crate::engine::ReadDomains;
use crate::propagators::create_inconsistency;
use crate::propagators::create_tasks;
use crate::propagators::reset_bounds_clear_updated;
use crate::propagators::update_bounds_task;
use crate::propagators::CumulativeArgs;
use crate::propagators::CumulativeParameters;
use crate::propagators::Task;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;

/// An event storing the start and end of mandatory parts used for creating the time-table
#[derive(Debug)]
pub struct Event<Var> {
    /// The time-point at which the [`Event`] took place
    time_stamp: i32,
    /// Change in resource usage at [time_stamp][Event::time_stamp], positive if it is the start of
    /// a mandatory part and negative otherwise
    change_in_resource_usage: i32,
    /// The [`Task`] which has caused the event to take place
    task: Rc<Task<Var>>,
}

/// [`ConstraintProgrammingPropagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a [`ResourceProfile`] over an interval rather than
/// creating one per time-point (hence the name). Furthermore, the
/// [`TimeTableOverIntervalPropagator`] has a generic argument which represents the type of variable
/// used for modelling the start variables, this will be an implementation of [`IntVar`].
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug)]
pub struct TimeTableOverIntervalPropagator<Var> {
    /// Stores whether the time-table is empty
    is_time_table_empty: bool,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
}

/// The type of the time-table used by propagators which use time-table reasoning over intervals.
///
/// The [ResourceProfile]s are sorted based on start time and they are non-overlapping; each entry
/// in the [`Vec`] represents the mandatory resource usage across an interval.
pub(crate) type OverIntervalTimeTableType<Var> = Vec<ResourceProfile<Var>>;

impl<Var> CPPropagatorConstructor for CumulativeArgs<Var, TimeTableOverIntervalPropagator<Var>>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Propagator = TimeTableOverIntervalPropagator<Var>;

    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let tasks = create_tasks(&self.tasks, context);
        TimeTableOverIntervalPropagator::new(CumulativeParameters::new(tasks, self.capacity))
    }
}

impl<Var: IntVar + 'static> TimeTableOverIntervalPropagator<Var> {
    pub fn new(parameters: CumulativeParameters<Var>) -> TimeTableOverIntervalPropagator<Var> {
        TimeTableOverIntervalPropagator {
            is_time_table_empty: true,
            parameters,
        }
    }

    pub(crate) fn debug_propagate_from_scratch_time_table_interval(
        context: &mut PropagationContextMut,
        parameters: &CumulativeParameters<Var>,
    ) -> PropagationStatusCP {
        // We first create a time-table over interval and return an error if there was
        // an overflow of the resource capacity while building the time-table
        let time_table =
            TimeTableOverIntervalPropagator::create_time_table_over_interval_from_scratch(
                context, parameters,
            )?;
        // Then we check whether propagation can take place
        propagate_based_on_timetable(context, time_table.iter(), parameters)
    }

    /// Creates a time-table consisting of [`ResourceProfile`]s which represent rectangles with a
    /// start and end (both inclusive) consisting of tasks with a cumulative height Assumptions:
    /// The time-table is sorted based on start time and none of the profiles overlap - generally,
    /// it is assumed that the calculated [`ResourceProfile`]s are maximal
    ///
    /// The result of this method is either the time-table of type
    /// [`OverIntervalTimeTableType`] or the tasks responsible for the
    /// conflict in the form of an [`Inconsistency`].
    pub(crate) fn create_time_table_over_interval_from_scratch(
        context: &PropagationContextMut,
        parameters: &CumulativeParameters<Var>,
    ) -> Result<OverIntervalTimeTableType<Var>, Inconsistency> {
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
                // parts This allows us to build smaller explanations by ensuring
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

        let mut time_table: OverIntervalTimeTableType<Var> = Default::default();
        // The tasks which are contributing to the current profile under consideration
        let mut current_profile_tasks: Vec<Rc<Task<Var>>> = Vec::new();
        // The cumulative resource usage of the tasks which are contributing to the current profile
        // under consideration
        let mut current_resource_usage: i32 = 0;
        // The beginning of the current interval under consideration
        let mut start_of_interval: i32 = -1;

        // We go over all the events and create the time-table
        for Event {
            time_stamp,
            change_in_resource_usage,
            task,
        } in events
        {
            if start_of_interval == -1 {
                // A new profile needs to be started
                pumpkin_assert_simple!(
                    change_in_resource_usage > 0,
                    "The resource usage of an event which causes a new profile to be started should never be negative"
                );
                pumpkin_assert_simple!(
                    current_resource_usage == 0,
                    "The resource usage should be 0 when a new profile is started"
                );
                pumpkin_assert_simple!(
                    current_profile_tasks.is_empty(),
                    "There should be no contributing tasks when a new profile is started"
                );

                // We thus assign the start of the interval to the time_stamp of the event, add its
                // resource usage and add it to the contributing tasks
                start_of_interval = time_stamp;
                current_resource_usage = change_in_resource_usage;
                current_profile_tasks.push(task);
            } else {
                // A profile is currently being created

                // We have first traversed all of the ends of mandatory parts, meaning that any
                // overflow will persist after processing all events at this time-point
                if current_resource_usage > parameters.capacity {
                    // An overflow has occurred due to mandatory parts
                    return Err(create_inconsistency(context, &current_profile_tasks));
                }

                // Potentially we need to end the current profile and start a new one due to the
                // addition/removal of the current task
                if start_of_interval != time_stamp {
                    // We end the current profile, creating a profile from [start_of_interval,
                    // time_stamp)
                    time_table.push(ResourceProfile {
                        start: start_of_interval,
                        end: time_stamp - 1,
                        profile_tasks: current_profile_tasks.clone(),
                        height: current_resource_usage,
                    });
                }
                // Process the current event, note that `change_in_resource_usage` can be negative
                pumpkin_assert_simple!(
                    change_in_resource_usage > 0
                        || current_resource_usage >= change_in_resource_usage,
                    "Processing this task would have caused negative resource usage which should not be possible"
                );
                current_resource_usage += change_in_resource_usage;
                if current_resource_usage == 0 {
                    // No tasks have an active mandatory at the current `time_stamp`
                    // We can thus reset the start of the interval and remove all profile tasks
                    start_of_interval = -1;
                    current_profile_tasks.clear();
                } else {
                    // There are still tasks which have a mandatory part at the current time-stamp
                    // We thus need to start a new profile
                    start_of_interval = time_stamp;
                    if change_in_resource_usage < 0 {
                        // The mandatory part of a task has ended, we should thus remove it from the
                        // contributing tasks
                        let _ = current_profile_tasks.remove(
                            current_profile_tasks
                                .iter()
                                .position(|current_task| {
                                    current_task.id.unpack() == task.id.unpack()
                                })
                                .expect("Task should have been found in `current_profile`"),
                        );
                    } else {
                        // The mandatory part of a task has started, we should thus add it to the
                        // set of contributing tasks
                        pumpkin_assert_extreme!(
                            !current_profile_tasks.contains(&task),
                            "Task is being added to the profile while it is already part of the contributing tasks"
                        );
                        current_profile_tasks.push(task);
                    }
                }
            }
        }
        Ok(time_table)
    }
}

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator
    for TimeTableOverIntervalPropagator<Var>
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        let time_table =
            TimeTableOverIntervalPropagator::create_time_table_over_interval_from_scratch(
                context,
                &self.parameters,
            )?;
        self.is_time_table_empty = time_table.is_empty();
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(context, time_table.iter(), &self.parameters)
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        reset_bounds_clear_updated(
            context,
            &mut self.parameters.updated,
            &mut self.parameters.bounds,
            &self.parameters.tasks,
        );
    }

    fn notify(
        &mut self,
        context: &mut PropagationContextMut,
        local_id: crate::engine::LocalId,
        _event: crate::engine::OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        // Note that it could be the case that `is_time_table_empty` is inaccurate here since it
        // wasn't updated in `synchronise`; however, `synchronise` will only remove profiles
        // meaning that `is_time_table_empty` will always return `false` when it is not
        // empty and it might return `false` even when the time-table is not empty *but* it
        // will never return `true` when the time-table is not empty.
        let result = should_enqueue(
            &self.parameters,
            &updated_task,
            context,
            self.is_time_table_empty,
        );
        update_bounds_task(context, &mut self.parameters.bounds, &updated_task);
        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverInterval"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        for task in self.parameters.tasks.iter() {
            self.parameters.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ))
        }
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        TimeTableOverIntervalPropagator::debug_propagate_from_scratch_time_table_interval(
            context,
            &self.parameters,
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::ConflictInfo;
    use crate::basic_types::Inconsistency;
    use crate::basic_types::Predicate;
    use crate::basic_types::PredicateConstructor;
    use crate::basic_types::PropositionalConjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::engine::EnqueueDecision;
    use crate::propagators::ArgTask;
    use crate::propagators::TimeTableOverInterval;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let _ = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                1,
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

        let result = solver.new_propagator(TimeTableOverInterval::new(
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
            .collect(),
            1,
        ));
        assert!(match result {
            Err(Inconsistency::Other(ConflictInfo::Explanation(x))) => {
                let expected = [
                    s1.upper_bound_predicate(1),
                    s1.lower_bound_predicate(1),
                    s2.upper_bound_predicate(1),
                    s2.lower_bound_predicate(1),
                ];
                expected
                    .iter()
                    .all(|y| x.iter().collect::<Vec<&Predicate>>().contains(&y))
                    && x.iter().all(|y| expected.contains(y))
            }
            _ => false,
        });
    }

    #[test]
    fn propagator_propagates_nothing() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(0, 6);

        let _ = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                1,
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

        let _ = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                5,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);

        let mut propagator = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        let notification_status = solver.increase_lower_bound_and_notify(&mut propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        let result = solver.propagate(&mut propagator);
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

        let _ = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason_int(s2.upper_bound_predicate(3)).clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(9),
                s1.lower_bound_predicate(6),
                s1.upper_bound_predicate(6),
            ]),
            reason
        );
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

        let mut propagator = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                5,
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

        let notification_status = solver.increase_lower_bound_and_notify(&mut propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(&mut propagator);
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

        let mut propagator = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                5,
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

        let notification_status = solver.increase_lower_bound_and_notify(&mut propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let _ = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason_int(s2.lower_bound_predicate(5)).clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.lower_bound_predicate(0),
                s1.lower_bound_predicate(1),
                s1.upper_bound_predicate(1),
            ]),
            reason
        );
    }

    #[test]
    fn propagator_propagates_generic_bounds() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(3, 3);
        let s2 = solver.new_variable(5, 5);
        let s3 = solver.new_variable(1, 15);

        let _ = solver
            .new_propagator(TimeTableOverInterval::new(
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
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason_int(s3.lower_bound_predicate(7)).clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(5),
                s2.lower_bound_predicate(5),
                s3.lower_bound_predicate(2),
            ]),
            reason
        );
    }
}
