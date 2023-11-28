use std::{
    cmp::{max, min},
    collections::HashSet,
};

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{DomainChange, EnqueueDecision, PropagationContext, PropagatorVariable},
    propagators::{IncrementalPropagator, Task, Updated},
};

#[derive(Clone)]
pub struct Event {
    pub time_stamp: i32, //time-stamp of event
    pub delta_c: i32, //change in resource usage at this time-point (positive if start of mandatory part, negative otherwise)
    pub task_id: usize, //id of the task responsible for the event
}

#[derive(Clone, Debug)]
pub struct ResourceProfile {
    pub start: i32,                //start time of profile (inclusive)
    pub end: i32,                  //end time of profile (inclusive), end >= start
    pub profile_tasks: Vec<usize>, //the ids of the tasks in this profile
    pub height: i32,               //height of the profile
}

impl ResourceProfile {
    pub fn default(time: i32) -> ResourceProfile {
        ResourceProfile {
            start: time,
            end: time,
            profile_tasks: Vec::new(),
            height: 0,
        }
    }
}

pub trait TimeTablePropagator<Var: IntVar + 'static>: IncrementalPropagator<Var> {
    /// Static method for creating an error clause similar to [create_error_clause][IncrementalPropagator::create_error_clause] consisting of the bounds of the provided arguments
    /// * `conflict_tasks` - A list of indices into the `tasks` parameter which constitute the tasks which have caused the conflict
    fn create_error_clause(
        context: &PropagationContext,
        tasks: &[Task<Var>],
        conflict_tasks: &Vec<usize>,
    ) -> PropagationStatusCP {
        let mut error_clause = Vec::with_capacity(conflict_tasks.len() * 2);
        for task_id in conflict_tasks.iter() {
            let Task {
                start_variable: s,
                processing_time: _,
                resource_usage: _,
                id: _,
            } = &tasks[*task_id];
            error_clause.push(s.upper_bound_predicate(context.upper_bound(s)));
            error_clause.push(s.lower_bound_predicate(context.lower_bound(s)));
        }

        Err(Inconsistency::from(PropositionalConjunction::from(
            error_clause,
        )))
    }

    /// Determines whether the propagator should be enqueued for propagation
    /// * `task` - The task which has been updated
    /// * `bounds` - The bounds before the current update took place - this does not need to be updated in this function as it will be updated in [notify][CumulativePropagator::notify]
    /// * `updated` - A vector of [Updated] objects which represent the updated tasks since the last iteration of propagations
    fn should_propagate(
        &self,
        task: &Task<Var>,
        context: &PropagationContext,
        bounds: &[(i32, i32)],
        updated: &mut Vec<Updated>,
    ) -> EnqueueDecision {
        if context.lower_bound(&task.start_variable) <= bounds[task.id.get_value()].0
            && context.upper_bound(&task.start_variable) >= bounds[task.id.get_value()].1
        {
            //No update to the bounds of the variables has taken place, enqueue if the list of updated is not empty, skip otherwise
            return if !updated.is_empty() {
                EnqueueDecision::Enqueue
            } else {
                EnqueueDecision::Skip
            };
        }
        let (old_lower_bound, old_upper_bound) = bounds[task.id.get_value()];
        if context.upper_bound(&task.start_variable)
            < context.lower_bound(&task.start_variable) + task.processing_time
        //If this is the case then a mandatory part exists for the updated task
        {
            if context.lower_bound(&task.start_variable) > old_lower_bound
                || context.upper_bound(&task.start_variable) < old_upper_bound
            //Check whether there has either been an update to the lower-bound or the upper-bound (or both)
            {
                updated.push(Updated {
                    task_id: task.id.get_value(),
                    old_lower_bound,
                    old_upper_bound,
                    new_lower_bound: context.lower_bound(&task.start_variable),
                    new_upper_bound: context.upper_bound(&task.start_variable),
                });
            }
            //There is a new mandatory part, Enqueue
            return EnqueueDecision::Enqueue;
        }
        EnqueueDecision::Enqueue
    }

    /// Creates a time-table consisting of [ResourceProfile]s which represent rectangles with a start and end (both inclusive) consisting of tasks with a cumulative height
    /// Assumptions: The time-table is sorted based on start time and none of the profiles overlap - generally, it is assumed that the calculated [ResourceProfile]s are maximal
    fn create_time_table(
        &mut self,
        context: &PropagationContext,
        tasks: &[Task<Var>],
        capacity: i32,
        reversed: bool,
    ) -> (bool, Vec<usize>);

    /// Resets the data structures after backtracking/backjumping - generally this means recreating the time-table from scratch
    fn reset_structures(
        &mut self,
        context: &PropagationContext,
        tasks: &[Task<Var>],
        _horizon: i32,
        capacity: i32,
    ) {
        self.create_time_table(context, tasks, capacity, false);
    }

    /// Returns the current time-table
    fn get_time_table(&self) -> &Vec<ResourceProfile>;

    /// Eagerly store the reason for a propagation of a value in the appropriate datastructure
    fn store_explanation(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
        explanation: PropositionalConjunction,
        lower_bound: bool,
    );

    /// Determines the maximum bound for a given profile (i.e. given that a task profile propagated due to a profile, what is the best bound we can find based on other profiles)
    /// * `lower_bound` - Whether the propagation is on the lower-bound or the upper-bound
    /// * `profiles` - The [ResourceProfile]s with together with their indices in the time-table
    fn find_maximum_bound_and_profiles(
        &self,
        lower_bound: bool,
        propagating_index: usize,
        profiles: &[ResourceProfile],
        propagating_task: &Task<Var>,
        capacity: i32,
        (tasks, bounds): (&[Task<Var>], &[(i32, i32)]),
    ) -> (i32, Vec<usize>) {
        let mut current_profile = &profiles[propagating_index]; //The profile which caused the original propagation
        let mut new_profile_tasks = HashSet::new(); //The profile tasks responsible for the maximum calculated bound update
        new_profile_tasks.extend(current_profile.profile_tasks.clone());
        let mut bound = if lower_bound {
            current_profile.end + 1 //We are updating the lower-bound so the original update will place the lower-bound after the end of the propagating profile
        } else {
            current_profile.start - propagating_task.processing_time //We are updating the lower-bound so the original update will place the upper-bound such that it does not overlap with the current profile
        };
        if lower_bound {
            //If the propagating_index is the last profile then no further propagations are able to take place based on the current one
            if propagating_index < profiles.len() - 1 {
                for next_profile in profiles.iter().skip(propagating_index + 1) {
                    //Go through all profiles starting from the propagating one
                    //Find all profiles after the current one which would propagate for the current task based on that profiles[propagating_index] propagated
                    //(i.e. find all profiles of which the task is not a part with less than propagating_task.processing_time between the profiles)
                    if next_profile.height + propagating_task.resource_usage > capacity
                        && !self.has_mandatory_part_in_interval(
                            propagating_task.id.get_value(),
                            next_profile.start,
                            next_profile.end,
                            tasks,
                            bounds,
                        ) //If the updated task has a mandatory part in the current interval then it cannot be propagated by the current profile
                        && (next_profile.start - current_profile.end + 1) < propagating_task.processing_time
                    //The updated task necessarily overlaps with the next profile
                    {
                        bound = next_profile.end + 1;
                        new_profile_tasks.extend(next_profile.profile_tasks.clone());
                        current_profile = next_profile;
                    } else if (current_profile.start - next_profile.end + 1)
                        >= propagating_task.processing_time
                    {
                        //The distance between the current and the next profile is too great to propagate, since the ResourceProfiles are non-overlapping, we know that we can break from the loop
                        break;
                    }
                }
            }
        } else if propagating_index > 0 {
            //An upper-bound update and we check whether the propagating profile is not the first profile
            for next_profile in profiles.iter().take(propagating_index).rev() {
                //Find all profiles before the current one which would propagate for the current task based on that profiles[propagating_index] propagated
                //(i.e. find all profiles of which the task is not a part with less than propagating_task.processing_time between the profiles)
                if next_profile.height + propagating_task.resource_usage > capacity
                    && !self.has_mandatory_part_in_interval(
                        propagating_task.id.get_value(),
                        next_profile.start,
                        next_profile.end,
                        tasks,
                        bounds,
                    ) //If the updated task has a mandatory part in the current interval then it cannot be propagated by the current profile
                    && (current_profile.start - next_profile.end + 1) < propagating_task.processing_time
                //The updated task necessarily overlaps with the previous profile after the update
                {
                    bound = next_profile.start - propagating_task.processing_time;
                    new_profile_tasks.extend(next_profile.profile_tasks.clone());
                    current_profile = next_profile;
                } else if (current_profile.start - next_profile.end + 1)
                    >= propagating_task.processing_time
                {
                    //The distance between the current and the next profile is too great to propagate, since the ResourceProfiles are non-overlapping, we know that we can break from the loop
                    break;
                }
            }
        }
        (bound, new_profile_tasks.into_iter().collect::<Vec<_>>())
    }

    /// Checks whether a specific task (indicated by id) has a mandatory part which overlaps with the interval [start, end]
    fn has_mandatory_part_in_interval(
        &self,
        id: usize,
        start: i32,
        end: i32,
        tasks: &[Task<Var>],
        bounds: &[(i32, i32)],
    ) -> bool {
        let task: &Task<Var> = &tasks[id];
        let (lower_bound, upper_bound) = bounds[task.id.get_value()];
        (upper_bound < (lower_bound + task.processing_time)) //There exists a mandatory part
                && (upper_bound <= end) //The start of the mandatory part is before the end of the interval
                && (start <= (lower_bound + task.processing_time)) //The start of the interval is before the end of the mandatory part
    }

    /// Checks whether the lower and upper bound of a variable overlap with the provided interval
    fn var_has_overlap_with_interval(
        &self,
        context: &mut PropagationContext,
        p: i32,
        var: &PropagatorVariable<Var>,
        start: i32,
        end: i32,
    ) -> bool {
        let (lower_bound, upper_bound) = (context.lower_bound(var), context.upper_bound(var) + p); //The release time of the task and the deadline
        (start <= upper_bound) && (lower_bound <= end) //Check whether there is any overlap between the two intervals (i.e. there exists a C such that start <= C <= end /\ lower_bound <= C <= upper_bound)
    }

    /// Checks whether a propagation should occur based on the current state of the time-table
    /// * `to_check` - The profiles which should be checked
    fn check_for_updates(
        &self,
        context: &mut PropagationContext,
        to_check: &[ResourceProfile],
        tasks: &[Task<Var>],
        bounds: &mut Vec<(i32, i32)>,
        capacity: i32,
    ) -> (
        PropagationStatusCP,
        Vec<(bool, usize, i32, PropositionalConjunction)>,
    ) {
        let mut explanations: Vec<(bool, usize, i32, PropositionalConjunction)> = Vec::new();
        //We go over all profiles
        for (
            index,
            ResourceProfile {
                start,
                end,
                profile_tasks: _,
                height,
            },
        ) in to_check.iter().enumerate()
        {
            //Then we go over all the different tasks
            for task in tasks.iter() {
                if context.is_fixed(&task.start_variable) {
                    //If the variable is already fixed then we do not need to check for any updated
                    continue;
                }
                let Task {
                    start_variable: s,
                    processing_time: p,
                    resource_usage: c,
                    id,
                } = &task;
                if height + c <= capacity {
                    // The tasks are sorted by capacity, if this task doesn't overload then none will
                    break;
                } else if self.has_mandatory_part_in_interval(
                    id.get_value(),
                    *start,
                    *end,
                    tasks,
                    bounds,
                ) {
                    // The current task is part of the current profile, which means that it can't be propagated by it
                    continue;
                } else if self.var_has_overlap_with_interval(context, *p, s, *start, *end) {
                    //The current task has an overlap with the current resource profile (i.e. it could be propagated by the current profile)
                    if (start - p) < context.lower_bound(s) && *end >= context.lower_bound(s) {
                        //The current task necessarily overlap with the current ResourceProfile (i.e. lb(s) + p >= start /\ lb(s) <= end)
                        //Based on this propagation, find the profile which now propagates to the highest lower bound for this task
                        let (lower_bound, new_profile_tasks) = self
                            .find_maximum_bound_and_profiles(
                                true,
                                index,
                                to_check,
                                task,
                                capacity,
                                (tasks, bounds),
                            );
                        let (conflict_found, explanation) = self.propagate_and_explain(
                            context,
                            DomainChange::LowerBound(min(
                                context.lower_bound(s),
                                lower_bound - p + 1,
                            )), //Use the minimum bound which would have propagated the profile at index
                            (s, lower_bound),
                            tasks,
                            &new_profile_tasks,
                        );
                        explanations.push((true, id.get_value(), lower_bound, explanation));

                        if conflict_found {
                            //We have propagated a task which led to an empty domain, return the explanations of the propagations and the inconsistency
                            return (Err(Inconsistency::EmptyDomain), explanations);
                        }
                    }
                    if end > &context.upper_bound(s) && *start - p < context.upper_bound(s) {
                        //The current task has overlap with the current resource profile (i.e. end > ub(s) /\ start - p < ub(s)); this means that if the task starts at its latest starting time it would overlap with this ResourceProfile
                        //Based on this propagation, find the profile which now propagates to the lowest upper bound for this task
                        let (upper_bound, new_profile_tasks) = self
                            .find_maximum_bound_and_profiles(
                                false,
                                index,
                                to_check,
                                task,
                                capacity,
                                (tasks, bounds),
                            );
                        let (conflict_found, explanation) = self.propagate_and_explain(
                            context,
                            DomainChange::UpperBound(max(context.upper_bound(s), *end)),
                            (s, upper_bound),
                            tasks,
                            &new_profile_tasks,
                        );
                        explanations.push((false, id.get_value(), upper_bound, explanation));
                        if conflict_found {
                            //We have propagated a task which led to an empty domain, return the explanations of the propagations and the inconsistency
                            return (Err(Inconsistency::EmptyDomain), explanations);
                        }
                    }
                }
            }
        }
        (Ok(()), explanations)
    }

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
        tasks: &[Task<Var>],
        bounds: &mut Vec<(i32, i32)>,
        _horizon: i32,
        capacity: i32,
    ) -> (
        PropagationStatusCP,
        Vec<(bool, usize, i32, PropositionalConjunction)>,
    ) {
        //Clear the current known bounds and recalculate them from the current bounds
        bounds.clear();
        for task in tasks.iter() {
            bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ))
        }
        {
            //Create the time-table
            let (unsat, conflict_profile) = self.create_time_table(context, tasks, capacity, false);
            if unsat {
                //We have found a ResourceProfile which overloads the resource capacity, create an error clause using the responsible profiles
                return (
                    self.create_error_clause(context, tasks, &conflict_profile),
                    Vec::new(),
                );
            }
        }
        let time_table = self.get_time_table();
        //Check for updates (i.e. go over all profiles and all tasks and check whether an update can take place)
        self.check_for_updates(context, time_table, tasks, bounds, capacity)
    }
}
