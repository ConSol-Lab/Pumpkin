use std::{
    cmp::max,
    collections::{BTreeMap, HashSet},
    rc::Rc,
};

use crate::{
    basic_types::{variables::IntVar, Inconsistency},
    engine::{DomainChange, EnqueueDecision, PropagationContext, PropagatorVariable},
    propagators::{CumulativePropagationResult, Explanation, IncrementalPropagator, Task, Updated},
    pumpkin_assert_simple,
};

#[derive(Clone)]
///Structure used for storing events for calculating the time-table (e.g. in [TimeTablePerPoint][TimeTablePerPoint::time_table]) by keeping track of when a mandatory part has started or ended
/// * `time_stamp` - The time-stamp of the event
/// * `change_resource_usage` - Change in the resource usage at the given time-point; a positive value indicates the start of a mandatory part and a negative value indicates the end of a mandatory part
/// * `task_id` - The id of the task responsible for the event
pub struct Event {
    pub time_stamp: i32,
    pub change_resource_usage: i32,
    pub task_id: usize,
}

#[derive(Clone, Debug)]
///Structures used for storing the data related to resource profiles; a [ResourceProfile] represents a rectangle where the height is the cumulative mandatory resource usage of the [profile tasks][ResourceProfile::profile_tasks]
/// * `start` - The start time of the [ResourceProfile] (inclusive)
/// * `end` - The end time of the [ResourceProfile] (inclusive)
/// * `profile_tasks` - The IDs of the tasks which are part of the profile
/// * `height` - The amount of cumulative resource usage of all [profile tasks][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
pub struct ResourceProfile {
    pub start: i32,
    pub end: i32,
    pub profile_tasks: Vec<usize>,
    pub height: i32,
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

///A generic propagator which stores certain parts of the common behaviour for different time-table methods (i.e. a propagator which stores [ResourceProfile]s per time-point and a propagator which stores [ResourceProfile]s over an interval)
pub trait TimeTablePropagator<Var: IntVar + 'static>: IncrementalPropagator<Var> {
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
        let (old_lower_bound, old_upper_bound) = bounds[task.id.get_value()];
        if context.lower_bound(&task.start_variable) <= old_lower_bound
            && context.upper_bound(&task.start_variable) >= old_upper_bound
        {
            //No update to the bounds of the variables has taken place, enqueue if the list of updated is not empty, skip otherwise
            return if !updated.is_empty() {
                EnqueueDecision::Enqueue
            } else {
                EnqueueDecision::Skip
            };
        }

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

    ///See method [create_time_table][TimeTablePropagator::create_time_table]; creates and assigns the time-table
    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
        tasks: &[Rc<Task<Var>>],
        capacity: i32,
        reversed: bool,
    ) -> Result<BTreeMap<u32, ResourceProfile>, Vec<usize>>;

    /// Creates a time-table consisting of [ResourceProfile]s which represent rectangles with a start and end (both inclusive) consisting of tasks with a cumulative height
    /// Assumptions: The time-table is sorted based on start time and none of the profiles overlap - generally, it is assumed that the calculated [ResourceProfile]s are maximal
    fn create_time_table(
        &self,
        context: &PropagationContext,
        tasks: &[Rc<Task<Var>>],
        capacity: i32,
        reversed: bool,
    ) -> Result<BTreeMap<u32, ResourceProfile>, Vec<usize>>;

    /// Resets the data structures after backtracking/backjumping - generally this means recreating the time-table from scratch
    fn reset_structures(
        &mut self,
        context: &PropagationContext,
        tasks: &[Rc<Task<Var>>],
        _horizon: i32,
        capacity: i32,
    ) {
        let result = self.create_time_table_and_assign(context, tasks, capacity, false);
        pumpkin_assert_simple!(
            result.is_ok(),
            "Found error while backtracking, this indicates that a conflict was not reported by the current propagator"
        );
    }

    fn find_maximum_bound_and_profiles_lower_bound<'a>(
        &self,
        propagating_index: usize,
        mut profiles: impl Iterator<Item = &'a ResourceProfile> + DoubleEndedIterator,
        propagating_task: &Task<Var>,
        capacity: i32,
        tasks: &[Rc<Task<Var>>],
        bounds: &[(i32, i32)],
    ) -> (i32, Vec<Rc<Task<Var>>>) {
        let mut current_profile = profiles
            .nth(propagating_index)
            .expect("Propagating profile did not exist within the provided iterator");
        let mut new_profile_tasks = HashSet::new();
        new_profile_tasks.extend(
            current_profile
                .profile_tasks
                .clone()
                .iter()
                .map(|current| Rc::clone(&tasks[*current])),
        );
        let mut bound = current_profile.end + 1; //We are updating the lower-bound so the original update will place the lower-bound after the end of the propagating profile
                                                 //If the propagating_index is the last profile then no further propagations are able to take place based on the current one
        for next_profile in profiles {
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
                new_profile_tasks.extend(
                    next_profile
                        .profile_tasks
                        .iter()
                        .map(|current| tasks[*current].clone()),
                );
                current_profile = next_profile;
            } else if (current_profile.start - next_profile.end + 1)
                >= propagating_task.processing_time
            {
                //The distance between the current and the next profile is too great to propagate, since the ResourceProfiles are non-overlapping, we know that we can break from the loop
                break;
            }
        }
        (bound, new_profile_tasks.into_iter().collect::<Vec<_>>())
    }

    /// Determines the maximum bound for a given profile (i.e. given that a task profile propagated due to a profile, what is the best bound we can find based on other profiles)
    /// * `lower_bound` - Whether the propagation is on the lower-bound or the upper-bound
    /// * `profiles` - The [ResourceProfile]s in the time-table
    fn find_maximum_bound_and_profiles_upper_bound<'a>(
        &self,
        num_profiles: usize,
        propagating_index: usize,
        mut profiles: impl Iterator<Item = &'a ResourceProfile> + DoubleEndedIterator,
        propagating_task: &Task<Var>,
        capacity: i32,
        tasks: &[Rc<Task<Var>>],
        bounds: &[(i32, i32)],
    ) -> (i32, Vec<Rc<Task<Var>>>) {
        //We need to find the current profile and then proceed in reverse order
        //If we have the list [0, 1, 2, 3, 4, 5, 6] with propagating_index = 4 then we need to find the 2nd profile from the back
        //This is equal to taking the (num_profiles - propagating_index - 1)th profile (in the example the 7 - 4 - 1 = 2nd profile)
        let mut current_profile = profiles
            .nth_back(num_profiles - propagating_index - 1)
            .expect("Propagating profile did not exist within the provided iterator");

        let mut new_profile_tasks = HashSet::new();
        new_profile_tasks.extend(
            current_profile
                .profile_tasks
                .clone()
                .iter()
                .map(|current| Rc::clone(&tasks[*current])),
        );
        let mut bound = current_profile.start - propagating_task.processing_time; //We are updating the lower-bound so the original update will place the upper-bound such that it does not overlap with the current profile;
        if propagating_index > 0 {
            //An upper-bound update and we check whether the propagating profile is not the first profile
            for next_profile in profiles.rev() {
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
                    new_profile_tasks.extend(
                        next_profile
                            .profile_tasks
                            .iter()
                            .map(|current| tasks[*current].clone()),
                    );
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
        tasks: &[Rc<Task<Var>>],
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
    fn check_for_updates<'a>(
        &self,
        context: &mut PropagationContext,
        to_check: impl Iterator<Item = &'a ResourceProfile> + Clone + DoubleEndedIterator,
        to_check_len: usize,
        tasks: &[Rc<Task<Var>>],
        bounds: &mut Vec<(i32, i32)>,
        capacity: i32,
    ) -> CumulativePropagationResult {
        let mut explanations: Vec<Explanation> = Vec::new();
        //We go over all profiles
        for (
            index,
            ResourceProfile {
                start,
                end,
                profile_tasks: _,
                height,
            },
        ) in to_check.clone().enumerate()
        {
            //Then we go over all the different tasks
            for task in tasks.iter() {
                if context.is_fixed(&task.start_variable) {
                    //If the variable is already fixed then we do not need to check for any updated
                    continue;
                }
                if height + task.resource_usage <= capacity {
                    // The tasks are sorted by capacity, if this task doesn't overload then none will
                    break;
                } else if self.has_mandatory_part_in_interval(
                    task.id.get_value(),
                    *start,
                    *end,
                    tasks,
                    bounds,
                ) {
                    // The current task is part of the current profile, which means that it can't be propagated by it
                    continue;
                } else if self.var_has_overlap_with_interval(
                    context,
                    task.processing_time,
                    &task.start_variable,
                    *start,
                    *end,
                ) {
                    //The current task has an overlap with the current resource profile (i.e. it could be propagated by the current profile)
                    if (start - task.processing_time) < context.lower_bound(&task.start_variable)
                        && *end >= context.lower_bound(&task.start_variable)
                    {
                        //The current task necessarily overlap with the current ResourceProfile (i.e. lb(s) + p >= start /\ lb(s) <= end)
                        //Based on this propagation, find the profile which now propagates to the highest lower bound for this task
                        let (lower_bound, new_profile_tasks) = self
                            .find_maximum_bound_and_profiles_lower_bound(
                                index,
                                to_check.clone(),
                                task,
                                capacity,
                                tasks,
                                bounds,
                            );
                        match self.propagate_and_explain(
                            context,
                            DomainChange::LowerBound(max(0, start - task.processing_time + 1)), //Use the minimum bound which would have propagated the profile at index
                            &task.start_variable,
                            lower_bound,
                            &new_profile_tasks,
                        ) {
                            Ok(explanation) => {
                                explanations.push(Explanation::new(
                                    DomainChange::LowerBound(lower_bound),
                                    task.id.get_value(),
                                    explanation,
                                ));
                            }
                            Err(explanation) => {
                                explanations.push(Explanation::new(
                                    DomainChange::LowerBound(lower_bound),
                                    task.id.get_value(),
                                    explanation,
                                ));
                                //We have propagated a task which led to an empty domain, return the explanations of the propagations and the inconsistency
                                return CumulativePropagationResult::new(
                                    Err(Inconsistency::EmptyDomain),
                                    Some(explanations),
                                );
                            }
                        }
                    }
                    if end > &context.upper_bound(&task.start_variable)
                        && *start - task.processing_time < context.upper_bound(&task.start_variable)
                    {
                        //The current task has overlap with the current resource profile (i.e. end > ub(s) /\ start - p < ub(s)); this means that if the task starts at its latest starting time it would overlap with this ResourceProfile
                        //Based on this propagation, find the profile which now propagates to the lowest upper bound for this task
                        let (upper_bound, new_profile_tasks) = self
                            .find_maximum_bound_and_profiles_upper_bound(
                                to_check_len,
                                index,
                                to_check.clone(),
                                task,
                                capacity,
                                tasks,
                                bounds,
                            );
                        match self.propagate_and_explain(
                            context,
                            DomainChange::UpperBound(max(
                                context.upper_bound(&task.start_variable),
                                *end,
                            )),
                            &task.start_variable,
                            upper_bound,
                            &new_profile_tasks,
                        ) {
                            Ok(explanation) => {
                                explanations.push(Explanation::new(
                                    DomainChange::UpperBound(upper_bound),
                                    task.id.get_value(),
                                    explanation,
                                ));
                            }
                            Err(explanation) => {
                                explanations.push(Explanation::new(
                                    DomainChange::UpperBound(upper_bound),
                                    task.id.get_value(),
                                    explanation,
                                ));
                                //We have propagated a task which led to an empty domain, return the explanations of the propagations and the inconsistency
                                return CumulativePropagationResult::new(
                                    Err(Inconsistency::EmptyDomain),
                                    Some(explanations),
                                );
                            }
                        }
                    }
                }
            }
        }
        CumulativePropagationResult::new(Ok(()), Some(explanations))
    }

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
        tasks: &[Rc<Task<Var>>],
        bounds: &mut Vec<(i32, i32)>,
        _horizon: i32,
        capacity: i32,
    ) -> CumulativePropagationResult {
        //Clear the current known bounds and recalculate them from the current bounds
        bounds.clear();
        for task in tasks.iter() {
            bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ))
        }

        match self.create_time_table_and_assign(context, tasks, capacity, false) {
            Ok(time_table) => {
                //Check for updates (i.e. go over all profiles and all tasks and check whether an update can take place)
                self.check_for_updates(
                    context,
                    time_table.values(),
                    time_table.len(),
                    tasks,
                    bounds,
                    capacity,
                )
            }
            Err(conflict_profile) => {
                //We have found a ResourceProfile which overloads the resource capacity, create an error clause using the responsible profiles
                CumulativePropagationResult::new(
                    self.create_error_clause(context, tasks, &conflict_profile),
                    None,
                )
            }
        }
    }
}
