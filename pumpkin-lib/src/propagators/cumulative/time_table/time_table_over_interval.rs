use std::{collections::HashMap, rc::Rc};

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PropagationStatusCP, PropositionalConjunction,
    },
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, EnqueueDecision,
        PropagationContext, PropagatorConstructorContext,
    },
    propagators::{
        CumulativeArgs, CumulativeParameters, PropagationStatusWithExplanation, Task, Util,
    },
    pumpkin_assert_extreme, pumpkin_assert_simple,
};

use super::time_table_propagator::{
    check_for_updates, should_enqueue, ResourceProfile, TimeTablePropagator,
};

/// An event storing the start and end of mandatory parts used for creating the time-table
pub(crate) struct Event<Var> {
    /// The time-point at which the event took place
    pub time_stamp: i32,
    /// Change in resource usage at [time_stamp][Event::time_stamp], positive if it is the start of a mandatory part and negative otherwise
    pub change_in_resource_usage: i32,
    /// The task which has caused the event to take place
    pub task: Rc<Task<Var>>,
}

/// Propagator responsible for using time-table reasoning to propagate the [Cumulative] constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at different time-points -
/// This method creates a [ResourceProfile] over intervals rather than creating per time-point (hence the name).
///
/// See ["Improving Scheduling by Learning - Andreas Schutt (2011)"](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
/// Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 for more information about time-table reasoning.
pub struct TimeTableOverIntervalProp<Var> {
    /// Each elements holds the mandatory resource consumption of 1 or more tasks over an interval (stored in a [ResourceProfile]);
    /// the [ResourceProfile]s are sorted based on start time and they are assumed to be non-overlapping
    time_table: OverIntervalTimeTableType<Var>,
    /// For each variable, eagerly maps the explanation of the lower-bound change;
    /// For a task i (representing a map in the [Vec]), \[bound\] stores the explanation for \[s_i >= bound\]
    reasons_for_propagation_lower_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// For each variable, eagerly maps the explanation of the upper-bound change
    /// For a task i (representing a map in the [Vec]), \[bound\] stores the explanation for \[s_i <= bound\]
    reasons_for_propagation_upper_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
}

/// The type of the time-table used by propagators which use time-table reasoning over intervals
pub(crate) type OverIntervalTimeTableType<Var> = Vec<ResourceProfile<Var>>;

/// The type of the time-table iterator used by propagators which use time-table reasoning over intervals
pub(crate) type OverIntervalIteratorType<'a, Var> = std::slice::Iter<'a, ResourceProfile<Var>>;

impl<Var> CPPropagatorConstructor for CumulativeArgs<Var, TimeTableOverIntervalProp<Var>>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Propagator = TimeTableOverIntervalProp<Var>;

    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let (tasks, horizon) = Util::create_tasks(&self.tasks, context);
        TimeTableOverIntervalProp::new(CumulativeParameters::new(tasks, self.capacity, horizon))
    }
}

impl<Var: IntVar + 'static> TimeTableOverIntervalProp<Var> {
    pub fn new(parameters: CumulativeParameters<Var>) -> TimeTableOverIntervalProp<Var> {
        let reasons_for_propagation: Vec<HashMap<i32, PropositionalConjunction>> =
            vec![HashMap::new(); parameters.tasks.len()];
        TimeTableOverIntervalProp {
            time_table: Default::default(),
            reasons_for_propagation_lower_bound: reasons_for_propagation.to_vec(),
            reasons_for_propagation_upper_bound: reasons_for_propagation,
            parameters,
        }
    }

    pub(crate) fn debug_propagate_from_scratch_time_table_interval(
        context: &mut PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) -> PropagationStatusCP {
        if let Ok(time_table) = TimeTableOverIntervalProp::create_time_table(context, parameters) {
            if check_for_updates(context, time_table.iter(), parameters)
                .status
                .is_ok()
            {
                Ok(())
            } else {
                Err(Inconsistency::EmptyDomain)
            }
        } else {
            //There was a conflict while creating the time-table, we need to return an error
            Err(Inconsistency::from(PropositionalConjunction::default()))
        }
    }

    pub(crate) fn create_time_table_over_interval_from_scratch(
        context: &PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) -> Result<OverIntervalTimeTableType<Var>, Vec<Rc<Task<Var>>>> {
        // First we create a list of events with which we will create the time-table
        let mut events: Vec<Event<Var>> = Vec::new();
        // Then we go over every task
        for task in parameters.tasks.iter() {
            let upper_bound = context.upper_bound(&task.start_variable);
            let lower_bound = context.lower_bound(&task.start_variable);
            if upper_bound < lower_bound + task.processing_time {
                // The task has a mandatory part, we need to add the appropriate events to the events list

                // Thus we first add an event for the start of a mandatory part (with positive resource usage)
                events.push(Event {
                    time_stamp: upper_bound,
                    change_in_resource_usage: task.resource_usage,
                    task: Rc::clone(task),
                });

                // Then we create an event for the end of a mandatory part (with negative resource usage)
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
                // If the time_stamps are equal then we first go through the ends of the mandatory parts
                // This allows us to build smaller explanations by ensuring that we report an error as soon as it can be found
                std::cmp::Ordering::Equal => {
                    if a.change_in_resource_usage.signum() != b.change_in_resource_usage.signum() {
                        // If a is the start (end) of a mandatory part and b is the end (start) of a mandatory part then we need to ensure that we go through the end of the mandatory part first
                        a.change_in_resource_usage.cmp(&b.change_in_resource_usage)
                    } else {
                        // If both events are starts or both events are ends then we sort on the task id for easier reproducibility
                        a.task.id.unpack().cmp(&b.task.id.unpack())
                    }
                }
                other_ordering => other_ordering,
            }
        });

        let mut time_table: OverIntervalTimeTableType<Var> = Default::default();
        // The tasks which are contributing to the current profile under consideration
        let mut current_profile_tasks: Vec<Rc<Task<Var>>> = Vec::new();
        // The cumulative resource usage of the tasks which are contributing to the current profile under consideration
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

                // We thus assign the start of the interval to the time_stamp of the event, add its resource usage and add it to the contributing tasks
                start_of_interval = time_stamp;
                current_resource_usage = change_in_resource_usage;
                current_profile_tasks.push(task);
            } else {
                // A profile is currently being created

                // We have first traversed all of the ends of mandatory parts, meaning that any overflow will persist after processing all events at this time-point
                if current_resource_usage > parameters.capacity {
                    // An overflow has occurred due to mandatory parts
                    return Err(current_profile_tasks);
                }

                // Potentially we need to end the current profile and start a new one due to the addition/removal of the current task
                if start_of_interval != time_stamp {
                    // We end the current profile, creating a profile from [start_of_interval, time_stamp)
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
                        // The mandatory part of a task has ended, we should thus remove it from the contributing tasks
                        current_profile_tasks.remove(
                            current_profile_tasks
                                .iter()
                                .position(|current_task| {
                                    current_task.id.unpack() == task.id.unpack()
                                })
                                .expect("Task should have been found in `current_profile`"),
                        );
                    } else {
                        // The mandatory part of a task has started, we should thus add it to the set of contributing tasks
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

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator for TimeTableOverIntervalProp<Var> {
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        let PropagationStatusWithExplanation {
            status,
            explanations,
        } = TimeTablePropagator::propagate_from_scratch(self, context);

        Util::store_explanations(
            explanations,
            &mut self.reasons_for_propagation_lower_bound,
            &mut self.reasons_for_propagation_upper_bound,
        );
        status
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        Util::reset_bounds_clear_updated(
            context,
            &mut self.parameters.updated,
            &mut self.parameters.bounds,
            &self.parameters.tasks,
        );
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        Util::get_reason_for_propagation(
            delta,
            &self.reasons_for_propagation_lower_bound,
            &self.reasons_for_propagation_upper_bound,
            &self.parameters.tasks,
        )
    }

    fn notify(
        &mut self,
        context: &mut PropagationContext,
        local_id: crate::engine::LocalId,
        _event: crate::engine::OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        should_enqueue(
            &mut self.parameters,
            updated_task,
            context,
            self.time_table.is_empty(),
        )
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverInterval"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        Util::initialise_at_root(true, &mut self.parameters, context);
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        TimeTableOverIntervalProp::debug_propagate_from_scratch_time_table_interval(
            context,
            self.get_parameters(),
        )
    }
}

impl<Var: IntVar + 'static> TimeTablePropagator<Var> for TimeTableOverIntervalProp<Var> {
    type TimeTableType = OverIntervalTimeTableType<Var>;
    type TimeTableIteratorType<'a> = OverIntervalIteratorType<'a, Var>;

    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
    ) -> Result<(), Vec<Rc<Task<Var>>>> {
        match <TimeTableOverIntervalProp<Var> as TimeTablePropagator<Var>>::create_time_table(
            context,
            self.get_parameters(),
        ) {
            Ok(result) => {
                self.time_table = result;
                Ok(())
            }
            Err(explanation) => Err(explanation),
        }
    }

    fn create_time_table(
        context: &PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) -> Result<Self::TimeTableType, Vec<Rc<Task<Var>>>> {
        TimeTableOverIntervalProp::create_time_table_over_interval_from_scratch(context, parameters)
    }

    fn get_parameters(&self) -> &CumulativeParameters<Var> {
        &self.parameters
    }

    fn get_time_table(&self) -> Self::TimeTableIteratorType<'_> {
        self.time_table.iter()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::{
            ConflictInfo, Inconsistency, Predicate, PredicateConstructor, PropositionalConjunction,
        },
        engine::{test_helper::TestSolver, Delta, DomainChange, EnqueueDecision, LocalId},
        propagators::{ArgTask, TimeTableOverInterval},
    };

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        solver
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

        solver
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

        solver
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

        let mut propagator = solver
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

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::UpperBound(3)),
        );
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

        let mut propagator = solver
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

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::LowerBound(5)),
        );
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

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(2), DomainChange::LowerBound(7)),
        );
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
