use std::{cmp::max, rc::Rc};

use crate::basic_types::PropagationStatusCP;
use crate::engine::{EmptyDomain, ReadDomains};
use crate::propagators::ChangeWithBound;
use crate::{
    basic_types::variables::IntVar,
    engine::{EnqueueDecision, PropagationContextMut},
    propagators::{CumulativeParameters, SparseSet, Task, Updated, Util},
    pumpkin_assert_advanced, pumpkin_assert_extreme, pumpkin_assert_moderate,
    pumpkin_assert_simple,
};

#[derive(Clone, Debug)]
/// Structures used for storing the data related to resource profiles;
/// A [ResourceProfile] represents a rectangle where the height is the cumulative mandatory resource usage of the [profile tasks][ResourceProfile::profile_tasks]
pub(crate) struct ResourceProfile<Var> {
    /// The start time of the [ResourceProfile] (inclusive)
    pub start: i32,
    /// The end time of the [ResourceProfile] (inclusive)
    pub end: i32,
    /// The IDs of the tasks which are part of the profile
    pub profile_tasks: Vec<Rc<Task<Var>>>,
    /// The amount of cumulative resource usage of all [profile tasks][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
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

/// A generic propagator which stores certain parts of the common behaviour for different time-table methods
/// (i.e. a propagator which stores [ResourceProfile]s per time-point and a propagator which stores [ResourceProfile]s over an interval)
pub(crate) trait TimeTablePropagator<Var: IntVar + 'static> {
    /// Type of the time-table which is returned when performing [create_time_table][TimeTablePropagator::create_time_table]
    type TimeTableType;

    /// Type of the iterator which is returned by [get_time_table][TimeTablePropagator::get_time_table],
    /// provides a list of non-overlapping [ResourceProfile]s which are sorted in increasing order based on start time
    type TimeTableIteratorType<'a>: Iterator<Item = &'a ResourceProfile<Var>> + Clone
    where
        Self: 'a;

    /// Returns the time-table as a vector (to facilitate O(1) indexing operations)
    ///
    /// Assumptions: The profiles are sorted based on time-point & The profiles are non-overlapping
    fn get_time_table(&self) -> Self::TimeTableIteratorType<'_>;

    /// See method [create_time_table][TimeTablePropagator::create_time_table]; creates and assigns the time-table, if a conflict occurred then it returns an error containing the profile responsible for the conflict
    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContextMut,
    ) -> Result<(), Vec<Rc<Task<Var>>>>;

    /// Creates a time-table consisting of [ResourceProfile]s which represent rectangles with a start and end (both inclusive) consisting of tasks with a cumulative height
    /// Assumptions: The time-table is sorted based on start time and none of the profiles overlap - generally, it is assumed that the calculated [ResourceProfile]s are maximal
    ///
    /// The result of this method is either the time-table of type [TimeTableType][TimeTablePropagator::TimeTableType] or the tasks responsible for the conflict
    fn create_time_table(
        context: &PropagationContextMut,
        parameters: &CumulativeParameters<Var>,
    ) -> Result<Self::TimeTableType, Vec<Rc<Task<Var>>>>;

    /// Resets the data structures after backtracking/backjumping - generally this means recreating the time-table from scratch
    fn reset_structures(&mut self, context: &PropagationContextMut) {
        let result = self.create_time_table_and_assign(context);
        pumpkin_assert_simple!(
            result.is_ok(),
            "Found error while backtracking, this indicates that a conflict was not reported by the current propagator"
        );
    }

    fn get_parameters(&self) -> &CumulativeParameters<Var>;

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        if let Err(conflict_profile) = self.create_time_table_and_assign(context) {
            //We have found a ResourceProfile which overloads the resource capacity, create an error clause using the responsible profiles
            Util::create_error_clause(context, &conflict_profile)
        } else {
            //Check for updates (i.e. go over all profiles and all tasks and check whether an update can take place)
            check_for_updates(context, self.get_time_table(), self.get_parameters())
        }
    }
}

/// Determines whether a time-table propagator should enqueue and updates the appropriate structures for processing during propagation
pub(crate) fn should_enqueue<Var: IntVar + 'static>(
    parameters: &mut CumulativeParameters<Var>,
    updated_task: Rc<Task<Var>>,
    context: &PropagationContextMut,
    empty_time_table: bool,
) -> EnqueueDecision {
    let task = &parameters.tasks[updated_task.id.unpack() as usize];
    pumpkin_assert_extreme!(
        context.lower_bound(&task.start_variable) > parameters.bounds[task.id.unpack() as usize].0
            || parameters.bounds[task.id.unpack() as usize].1
                >= context.upper_bound(&task.start_variable)
        , "Either the stored lower-bound was larger than or equal to the actual lower bound or the upper-bound was smaller than or equal to the actual upper-bound, 
           this either indicates that the propagator subscribed to events other than lower-bound and upper-bound updates 
           or the stored bounds were not managed properly"
    );
    let old_lower_bound = parameters.bounds[task.id.unpack() as usize].0;
    let old_upper_bound = parameters.bounds[task.id.unpack() as usize].1;

    //We check whether a mandatory part was extended/introduced
    if context.upper_bound(&task.start_variable)
        < context.lower_bound(&task.start_variable) + task.processing_time
    {
        parameters.updated.push(Updated {
            task: Rc::clone(task),
            old_lower_bound,
            old_upper_bound,
            new_lower_bound: context.lower_bound(&task.start_variable),
            new_upper_bound: context.upper_bound(&task.start_variable),
        });
    }
    Util::update_bounds_task(context, &mut parameters.bounds, task);

    // If the time-table is empty and we have not received any updates (e.g. no mandatory parts have been introduced since the last propagation)
    // then we can determine that no propagation will take place
    // It is not sufficient to check whether there have been no updates since it could be the case that a task which has been updated can now propagate due to an existing profile
    if !empty_time_table || !parameters.updated.is_empty() {
        EnqueueDecision::Enqueue
    } else {
        EnqueueDecision::Skip
    }
}

/// Checks whether a specific task (indicated by id) has a mandatory part which overlaps with the interval [start, end]
pub(crate) fn has_mandatory_part_in_interval<Var: IntVar + 'static>(
    context: &PropagationContextMut,
    task: &Rc<Task<Var>>,
    start: i32,
    end: i32,
) -> bool {
    let (lower_bound, upper_bound) = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable),
    );
    (upper_bound < (lower_bound + task.processing_time)) //There exists a mandatory part
                && has_overlap_with_interval(upper_bound, lower_bound + task.processing_time, start, end)
    // Determine whether the mandatory part overlaps with the provided bounds
}

/// Checks whether the lower and upper bound of a task overlap with the provided interval
pub(crate) fn task_has_overlap_with_interval<Var: IntVar + 'static>(
    context: &PropagationContextMut,
    task: &Rc<Task<Var>>,
    start: i32,
    end: i32,
) -> bool {
    let (lower_bound, upper_bound) = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable) + task.processing_time,
    ); //The release time of the task and the deadline
    has_overlap_with_interval(lower_bound, upper_bound, start, end)
}

/// Determines whether the interval [lower_bound, upper_bound) overlaps with the interval [start, end]
fn has_overlap_with_interval(lower_bound: i32, upper_bound: i32, start: i32, end: i32) -> bool {
    start < upper_bound && lower_bound <= end
}

/// Checks whether propagations should occur based on the current state of the time-table
///
/// It goes over all profiles and all tasks and determines which ones should be propagated;
/// Note that this method is not idempotent
pub(crate) fn check_for_updates<'a, Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
    time_table: impl Iterator<Item = &'a ResourceProfile<Var>> + Clone,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    pumpkin_assert_extreme!(
        {
            let collected_time_table = time_table.clone().collect::<Vec<_>>();
            collected_time_table.is_empty()
                || (0..collected_time_table.len() - 1).all(|profile_index| {
                    collected_time_table[profile_index].end
                        < collected_time_table[profile_index + 1].start
                })
        },
        "The provided time-table was not ordered according to start/end times"
    );
    pumpkin_assert_extreme!(
        {
            let collected_time_table = time_table.clone().collect::<Vec<_>>();
            collected_time_table.is_empty()
                || (0..collected_time_table.len()).all(|profile_index| {
                    (0..collected_time_table.len()).all(|other_profile_index| {
                        let current_profile = collected_time_table[profile_index];
                        let other_profile = collected_time_table[other_profile_index];
                        profile_index == other_profile_index
                            || !has_overlap_with_interval(
                                current_profile.start,
                                current_profile.end + 1,
                                other_profile.start,
                                other_profile.end,
                            )
                    })
                })
        },
        "There was overlap between profiles in the provided time-table"
    );
    let mut tasks_to_consider = SparseSet::new(parameters.tasks.to_vec(), Task::get_id);
    'profile_loop: for profile in time_table {
        //Then we go over all the different tasks
        let mut task_index = 0;
        while tasks_to_consider.has_next(task_index) {
            let task = Rc::clone(tasks_to_consider.get(task_index));
            if context.is_fixed(&task.start_variable)
                || profile.start > context.upper_bound(&task.start_variable) + task.processing_time
            {
                //Task is fixed or the start of the current profile is necessarily after the latest completion time of the task under consideration
                //The profiles are sorted by start time (and non-overlapping) so we can remove the task from consideration
                tasks_to_consider.remove(&task);
                if tasks_to_consider.is_empty() {
                    //There are no tasks left to consider, we can exit the loop
                    break 'profile_loop;
                }
                continue;
            }
            task_index += 1;
            check_whether_task_can_be_updated_by_profile(context, &task, profile, parameters)?;
        }
    }
    Ok(())
}

/// Determines whether the lower bound of a task can be propagated by a [ResourceProfile] with the provided start time and end time;
/// This method checks the following conditions:
///     * lb(s) + p > start, i.e. the earliest completion time of the task is after the start of the [ResourceProfile]
///     * lb(s) <= end, i.e. the earliest start time is before the end of the [ResourceProfile]
///
/// Note: It is assumed that task.resource_usage + height > capacity (i.e. the task has the potential to overflow the capacity in combination with the profile)
fn lower_bound_can_be_propagated_by_profile<Var: IntVar + 'static>(
    context: &PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    capacity: i32,
) -> bool {
    pumpkin_assert_moderate!(
        profile.height + task.resource_usage > capacity
            && task_has_overlap_with_interval(context, task, profile.start, profile.end)
    , "It is checked whether a task can be propagated while the invariants do not hold - The task should overflow the capacity with the profile");
    (context.lower_bound(&task.start_variable) + task.processing_time) > profile.start
        && context.lower_bound(&task.start_variable) <= profile.end
}

/// Determines whether the upper bound of a task can be propagated by a [ResourceProfile] with the provided start time and end time
/// This method checks the following conditions:
///     * ub(s) + p > start, i.e. the latest completion time is after the start of the [ResourceProfile]
///     * ub(s) <= end, i.e. the latest start time is before the end of the [ResourceProfile]
/// Note: It is assumed that the task is known to overflow the [ResourceProfile]
fn upper_bound_can_be_propagated_by_profile<Var: IntVar + 'static>(
    context: &PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    capacity: i32,
) -> bool {
    pumpkin_assert_moderate!(
        profile.height + task.resource_usage > capacity
    , "It is checked whether a task can be propagated while the invariants do not hold - The task should overflow the capacity with the profile");
    (context.upper_bound(&task.start_variable) + task.processing_time) > profile.start
        && context.upper_bound(&task.start_variable) <= profile.end
}

/// Propagates the lower-bound of the task to avoid overlap with the [ResourceProfile] `profile`
fn propagate_lower_bound_task_by_profile<Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    parameters: &CumulativeParameters<Var>,
    profile: &ResourceProfile<Var>,
) -> Result<(), EmptyDomain> {
    pumpkin_assert_advanced!(
        lower_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity),
        "Lower-bound of task is being propagated by profile while the conditions do not hold"
    );
    //The new lower-bound is set to be after the current profile
    let new_lower_bound = profile.end + 1;
    //Then we perform the actual propagation using this bound and the responsible tasks
    Util::propagate_and_explain(
        context,
        ChangeWithBound::LowerBound(max(0, profile.start - task.processing_time + 1)), //Use the minimum bound which would have propagated the profile at index
        task,
        new_lower_bound,
        &profile.profile_tasks,
    )
}

/// Propagates the upper-bound of the task to avoid overlap with the [ResourceProfile] `profile`
fn propagate_upper_bound_task_by_profile<Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    parameters: &CumulativeParameters<Var>,
    profile: &ResourceProfile<Var>,
) -> Result<(), EmptyDomain> {
    pumpkin_assert_advanced!(
        upper_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity),
        "Upper-bound of task is being propagated by profile while the conditions do not hold"
    );
    //The new upper-bound is set such that if the task is started at its latest starting time, it will never overlap with the profile
    let new_upper_bound = profile.start - task.processing_time;
    //Then we perform the actual propagation using this bound and the responsible tasks
    Util::propagate_and_explain(
        context,
        ChangeWithBound::UpperBound(max(context.upper_bound(&task.start_variable), profile.end)),
        task,
        new_upper_bound,
        &profile.profile_tasks,
    )
}

/// The method checks whether the current task can be propagated by the provided profile and (if appropriate) performs the propagation.
/// It then returns whether any of the propagations led to a conflict or whether all propagations were succesful.
///
/// Note that this method can only find [Inconsistency::EmptyDomain] conflicts which means that we handle that error in the parent function
fn check_whether_task_can_be_updated_by_profile<Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
) -> Result<(), EmptyDomain> {
    if profile.height + task.resource_usage <= parameters.capacity
        || has_mandatory_part_in_interval(context, task, profile.start, profile.end)
    {
        //The task cannot be propagated due to its resource usage being too low or it is part of the interval which means that it cannot be updated at all
        return Ok(());
    } else if task_has_overlap_with_interval(context, task, profile.start, profile.end) {
        //The current task has an overlap with the current resource profile (i.e. it could be propagated by the current profile)
        if lower_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity) {
            propagate_lower_bound_task_by_profile(context, task, parameters, profile)?;
        }
        if upper_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity) {
            propagate_upper_bound_task_by_profile(context, task, parameters, profile)?;
        }
    }
    Ok(())
}
