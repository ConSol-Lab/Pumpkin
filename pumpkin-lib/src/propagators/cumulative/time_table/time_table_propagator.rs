use std::{cmp::max, collections::HashSet, rc::Rc};

use crate::{
    basic_types::{variables::IntVar, Inconsistency},
    engine::{DomainChange, PropagationContext},
    propagators::{
        CumulativeParameters, CumulativePropagationResult, Explanation, Task, Util,
    },
    pumpkin_assert_simple,
};

#[derive(Clone, Debug)]
/// Structures used for storing the data related to resource profiles; a [ResourceProfile] represents a rectangle where the height is the cumulative mandatory resource usage of the [profile tasks][ResourceProfile::profile_tasks]
pub struct ResourceProfile<Var> {
    /// * `start` - The start time of the [ResourceProfile] (inclusive)
    pub start: i32,
    /// * `end` - The end time of the [ResourceProfile] (inclusive)
    pub end: i32,
    /// * `profile_tasks` - The IDs of the tasks which are part of the profile
    pub profile_tasks: Vec<Rc<Task<Var>>>,
    /// * `height` - The amount of cumulative resource usage of all [profile tasks][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    pub height: i32,
}

impl<Var: IntVar + 'static> ResourceProfile<Var> {
    pub fn default(time: i32) -> ResourceProfile<Var> {
        ResourceProfile {
            start: time,
            end: time,
            profile_tasks: Vec::new(),
            height: 0,
        }
    }
}

/// A generic propagator which stores certain parts of the common behaviour for different time-table methods (i.e. a propagator which stores [ResourceProfile]s per time-point and a propagator which stores [ResourceProfile]s over an interval)
pub trait TimeTablePropagator<Var: IntVar + 'static> {
    ///Type of the generic iterator for iterating over the time-table without assuming the type of [TimeTableType][TimeTablePropagator::TimeTableType]
    type TimeTableIterator<'b>: Iterator<Item = &'b ResourceProfile<Var>>
        + Clone
        + DoubleEndedIterator
    where
        Self: 'b;
    ///Type of the time-table which is returned when performing [TimeTablePropagator::create_time_table]
    type TimeTableType;

    ///Returns a [TimeTableIterator][TimeTablePropagator::TimeTableIterator] and the number of profiles contained in the iterator
    fn get_time_table_and_length(&self) -> (Self::TimeTableIterator<'_>, usize);

    /// See method [create_time_table][TimeTablePropagator::create_time_table]; creates and assigns the time-table, optionally returning the profile responsible for the conflict (if such a conflict occurred)
    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
    ) -> Option<Vec<Rc<Task<Var>>>>;

    /// Creates a time-table consisting of [ResourceProfile]s which represent rectangles with a start and end (both inclusive) consisting of tasks with a cumulative height
    /// Assumptions: The time-table is sorted based on start time and none of the profiles overlap - generally, it is assumed that the calculated [ResourceProfile]s are maximal
    ///
    /// The result of this method is either the time-table of type [TimeTableType][TimeTablePropagator::TimeTableType] or the tasks responsible for the conflict
    fn create_time_table(
        context: &PropagationContext,
        params: &CumulativeParameters<Var>,
    ) -> Result<Self::TimeTableType, Vec<Rc<Task<Var>>>>;

    /// Resets the data structures after backtracking/backjumping - generally this means recreating the time-table from scratch
    fn reset_structures(&mut self, context: &PropagationContext) {
        let result = self.create_time_table_and_assign(context);
        pumpkin_assert_simple!(
            result.is_none(),
            "Found error while backtracking, this indicates that a conflict was not reported by the current propagator"
        );
    }

    fn get_parameters(&self) -> &CumulativeParameters<Var>;

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
    ) -> CumulativePropagationResult<Var> {
        if let Some(conflict_profile) = self.create_time_table_and_assign(context) {
            //We have found a ResourceProfile which overloads the resource capacity, create an error clause using the responsible profiles
            CumulativePropagationResult::new(
                Util::create_error_clause(context, &conflict_profile),
                None,
            )
        } else {
            //Check for updates (i.e. go over all profiles and all tasks and check whether an update can take place)
            let (time_table, time_table_len) = self.get_time_table_and_length();
            check_for_updates(context, time_table, time_table_len, self.get_parameters())
        }
    }
}

/// Determines the maximum bound for a given profile (i.e. given that a task profile propagated due to a profile, what is the best bound we can find based on other profiles)
/// This method assumes that the lower-bound has been propagated due to `profiles.nth(propagating_index)`
/// * `profiles` - The [ResourceProfile]s in the time-table
fn find_maximum_bound_and_profiles_lower_bound<'a, Var: IntVar + 'static>(
    context: &PropagationContext,
    propagating_index: usize,
    mut profiles: impl Iterator<Item = &'a ResourceProfile<Var>> + DoubleEndedIterator,
    propagating_task: &Rc<Task<Var>>,
    capacity: i32,
) -> (i32, Vec<Rc<Task<Var>>>) {
    let mut current_profile = profiles
        .nth(propagating_index)
        .expect("Propagating profile did not exist within the provided iterator");
    let mut new_profile_tasks: HashSet<&Rc<Task<Var>>> = HashSet::new();
    new_profile_tasks.extend(current_profile.profile_tasks.iter());
    let mut bound = current_profile.end + 1; //We are updating the lower-bound so the original update will place the lower-bound after the end of the propagating profile
    for next_profile in profiles {
        //Go through all profiles starting from the propagating one
        //Find all profiles after the current one which would propagate for the current task based on that profiles[propagating_index] propagated
        //(i.e. find all profiles of which the task is not a part with less than propagating_task.processing_time between the profiles)
        if next_profile.height + propagating_task.resource_usage > capacity
                && !has_mandatory_part_in_interval(
                    context,
                    propagating_task,
                    next_profile.start,
                    next_profile.end,
                ) //If the updated task has a mandatory part in the current interval then it cannot be propagated by the current profile
                && (next_profile.start - current_profile.end + 1) < propagating_task.processing_time
        //The updated task necessarily overlaps with the next profile
        {
            bound = next_profile.end + 1;
            new_profile_tasks.extend(next_profile.profile_tasks.iter());
            current_profile = next_profile;
        } else if (current_profile.start - next_profile.end + 1) >= propagating_task.processing_time
        {
            //The distance between the current and the next profile is too great to propagate, since the ResourceProfiles are non-overlapping, we know that we can break from the loop
            break;
        }
    }
    (
        bound,
        new_profile_tasks
            .into_iter()
            .map(Rc::clone)
            .collect::<Vec<_>>(),
    )
}

/// Determines the maximum bound for a given profile (i.e. given that a task profile propagated due to a profile, what is the best bound we can find based on other profiles)
/// This method assumes that the upper-bound has been propagated due to `profiles.nth(propagating_index)`
/// * `profiles` - The [ResourceProfile]s in the time-table
fn find_maximum_bound_and_profiles_upper_bound<'a, Var: IntVar + 'static>(
    context: &PropagationContext,
    num_profiles: usize,
    propagating_index: usize,
    mut profiles: impl Iterator<Item = &'a ResourceProfile<Var>> + DoubleEndedIterator,
    propagating_task: &Rc<Task<Var>>,
    capacity: i32,
) -> (i32, Vec<Rc<Task<Var>>>) {
    //We need to find the current profile and then proceed in reverse order
    //If we have the list [0, 1, 2, 3, 4, 5, 6] with propagating_index = 4 then we need to find the 2nd profile from the back
    //This is equal to taking the (num_profiles - propagating_index - 1)th profile (in the example the 7 - 4 - 1 = 2nd profile)
    let mut current_profile = profiles
        .nth_back(num_profiles - propagating_index - 1)
        .expect("Propagating profile did not exist within the provided iterator");

    let mut new_profile_tasks = HashSet::new();
    new_profile_tasks.extend(current_profile.profile_tasks.iter());
    let mut bound = current_profile.start - propagating_task.processing_time; //We are updating the lower-bound so the original update will place the upper-bound such that it does not overlap with the current profile;
    for next_profile in profiles.rev() {
        //Find all profiles before the current one which would propagate for the current task based on that profiles[propagating_index] propagated
        //(i.e. find all profiles of which the task is not a part with less than propagating_task.processing_time between the profiles)
        if next_profile.height + propagating_task.resource_usage > capacity
                && !has_mandatory_part_in_interval(
                    context,
                    propagating_task,
                    next_profile.start,
                    next_profile.end,
                ) //If the updated task has a mandatory part in the current interval then it cannot be propagated by the current profile
                && (current_profile.start - next_profile.end + 1) < propagating_task.processing_time
        //The updated task necessarily overlaps with the previous profile after the update
        {
            bound = next_profile.start - propagating_task.processing_time;
            new_profile_tasks.extend(next_profile.profile_tasks.iter());
            current_profile = next_profile;
        } else if (current_profile.start - next_profile.end + 1) >= propagating_task.processing_time
        {
            //The distance between the current and the next profile is too great to propagate, since the ResourceProfiles are non-overlapping, we know that we can break from the loop
            break;
        }
    }
    (
        bound,
        new_profile_tasks
            .into_iter()
            .map(Rc::clone)
            .collect::<Vec<_>>(),
    )
}

/// Checks whether a specific task (indicated by id) has a mandatory part which overlaps with the interval [start, end]
pub fn has_mandatory_part_in_interval<Var: IntVar + 'static>(
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    start: i32,
    end: i32,
) -> bool {
    let (lower_bound, upper_bound) = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable),
    );
    (upper_bound < (lower_bound + task.processing_time)) //There exists a mandatory part
                && (upper_bound <= end) //The start of the mandatory part is before the end of the interval
                && (start <= (lower_bound + task.processing_time)) //The start of the interval is before the end of the mandatory part
}

/// Checks whether the lower and upper bound of a variable overlap with the provided interval
pub fn var_has_overlap_with_interval<Var: IntVar + 'static>(
    context: &mut PropagationContext,
    task: &Rc<Task<Var>>,
    start: i32,
    end: i32,
) -> bool {
    let (lower_bound, upper_bound) = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable) + task.processing_time,
    ); //The release time of the task and the deadline
    (start <= upper_bound) && (lower_bound <= end) //Check whether there is any overlap between the two intervals (i.e. there exists a C such that start <= C <= end /\ lower_bound <= C <= upper_bound)
}

/// Checks whether a propagation should occur based on the current state of the time-table
/// * `to_check` - The profiles which should be checked
fn check_for_updates<'a, Var: IntVar + 'static>(
    context: &mut PropagationContext,
    to_check: impl Iterator<Item = &'a ResourceProfile<Var>> + Clone + DoubleEndedIterator,
    to_check_len: usize,
    params: &CumulativeParameters<Var>,
) -> CumulativePropagationResult<Var> {
    let mut explanations: Vec<Explanation<Var>> = Vec::new();
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
        for task in params.tasks.iter() {
            if context.is_fixed(&task.start_variable) {
                //If the variable is already fixed then we do not need to check for any updated
                continue;
            }
            if height + task.resource_usage <= params.capacity {
                // The tasks are sorted by capacity, if this task doesn't overload then none will
                break;
            } else if has_mandatory_part_in_interval(context, task, *start, *end) {
                // The current task is part of the current profile, which means that it can't be propagated by it
                continue;
            } else if var_has_overlap_with_interval(context, task, *start, *end) {
                //The current task has an overlap with the current resource profile (i.e. it could be propagated by the current profile)
                if (start - task.processing_time) < context.lower_bound(&task.start_variable)
                    && *end >= context.lower_bound(&task.start_variable)
                {
                    //The current task necessarily overlap with the current ResourceProfile (i.e. lb(s) + p >= start /\ lb(s) <= end)
                    //Based on this propagation, find the profile which now propagates to the highest lower bound for this task
                    let (lower_bound, new_profile_tasks) =
                        find_maximum_bound_and_profiles_lower_bound(
                            context,
                            index,
                            to_check.clone(),
                            task,
                            params.capacity,
                        );
                    match Util::propagate_and_explain(
                        context,
                        DomainChange::LowerBound(max(0, start - task.processing_time + 1)), //Use the minimum bound which would have propagated the profile at index
                        task,
                        lower_bound,
                        &new_profile_tasks,
                    ) {
                        Ok(explanation) => {
                            explanations.push(Explanation::new(
                                DomainChange::LowerBound(lower_bound),
                                Rc::clone(task),
                                explanation,
                            ));
                        }
                        Err(explanation) => {
                            explanations.push(Explanation::new(
                                DomainChange::LowerBound(lower_bound),
                                Rc::clone(task),
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
                    let (upper_bound, new_profile_tasks) =
                        find_maximum_bound_and_profiles_upper_bound(
                            context,
                            to_check_len,
                            index,
                            to_check.clone(),
                            task,
                            params.capacity,
                        );
                    match Util::propagate_and_explain(
                        context,
                        DomainChange::UpperBound(max(
                            context.upper_bound(&task.start_variable),
                            *end,
                        )),
                        task,
                        upper_bound,
                        &new_profile_tasks,
                    ) {
                        Ok(explanation) => {
                            explanations.push(Explanation::new(
                                DomainChange::UpperBound(upper_bound),
                                Rc::clone(task),
                                explanation,
                            ));
                        }
                        Err(explanation) => {
                            explanations.push(Explanation::new(
                                DomainChange::UpperBound(upper_bound),
                                Rc::clone(task),
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