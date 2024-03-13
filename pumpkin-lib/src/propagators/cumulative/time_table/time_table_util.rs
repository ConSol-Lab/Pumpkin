//! Defines common methods for [`ConstraintProgrammingPropagator`]s which make use of time-table
//! reasoning (see [`crate::propagators::cumulative::time_table`] for more information) such as
//! [`should_enqueue`] or [`propagate_based_on_timetable`].

use std::cmp::max;
use std::ops::Range;
use std::rc::Rc;

use crate::basic_types::variables::IntVar;
#[cfg(doc)]
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
#[cfg(doc)]
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::EmptyDomain;
use crate::engine::EnqueueDecision;
use crate::engine::PropagationContextMut;
use crate::engine::ReadDomains;
use crate::propagators::util::propagate_and_explain;
use crate::propagators::ChangeWithExplanationBound;
use crate::propagators::CumulativeParameters;
use crate::propagators::SparseSet;
use crate::propagators::Task;
use crate::propagators::UpdatedTaskInfo;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;

/// Structures used for storing the data related to resource profiles;
/// A [`ResourceProfile`] represents a rectangle where the height is the cumulative mandatory
/// resource usage of the [`profile tasks`][ResourceProfile::profile_tasks]
#[derive(Clone, Debug)]
pub(crate) struct ResourceProfile<Var> {
    /// The start time of the [`ResourceProfile`] (inclusive)
    pub(crate) start: i32,
    /// The end time of the [`ResourceProfile`] (inclusive)
    pub(crate) end: i32,
    /// The IDs of the tasks which are part of the profile
    pub(crate) profile_tasks: Vec<Rc<Task<Var>>>,
    /// The amount of cumulative resource usage of all [`profile
    /// tasks`][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    pub(crate) height: i32,
}

impl<Var: IntVar + 'static> ResourceProfile<Var> {
    pub(crate) fn default(time: i32) -> ResourceProfile<Var> {
        ResourceProfile {
            start: time,
            end: time,
            profile_tasks: Vec::new(),
            height: 0,
        }
    }
}

/// The result of [`should_enqueue`], contains the [`EnqueueDecision`] whether the propagator should
/// currently be enqueued and potentially the updated [`Task`] (in the form of a
/// [`UpdatedTaskInfo`]) if the mandatory part of this [`Task`] has changed.
pub(crate) struct ShouldEnqueueResult<Var> {
    /// Whether the propagator which called this method should be enqueued
    pub(crate) decision: EnqueueDecision,
    /// If the mandatory part of the task passed to [`should_enqueue`] has changed then this field
    /// will contain the corresponding [`UpdatedTaskInfo`] otherwise it will be [`None`].
    pub(crate) update: Option<UpdatedTaskInfo<Var>>,
}

/// Determines whether a time-table propagator should enqueue and updates the appropriate structures
/// for processing during propagation. This method should be called in the
/// [`ConstraintProgrammingPropagator::notify`] method.
pub(crate) fn should_enqueue<Var: IntVar + 'static>(
    parameters: &CumulativeParameters<Var>,
    updated_task: &Rc<Task<Var>>,
    context: &PropagationContextMut,
    empty_time_table: bool,
) -> ShouldEnqueueResult<Var> {
    pumpkin_assert_extreme!(
        context.lower_bound(&updated_task.start_variable) > parameters.bounds[updated_task.id.unpack() as usize].0
            || parameters.bounds[updated_task.id.unpack() as usize].1
                >= context.upper_bound(&updated_task.start_variable)
        , "Either the stored lower-bound was larger than or equal to the actual lower bound or the upper-bound was smaller than or equal to the actual upper-bound,
           this either indicates that the propagator subscribed to events other than lower-bound and upper-bound updates
           or the stored bounds were not managed properly"
    );

    let mut result = ShouldEnqueueResult {
        decision: EnqueueDecision::Skip,
        update: None,
    };

    let old_lower_bound = parameters.bounds[updated_task.id.unpack() as usize].0;
    let old_upper_bound = parameters.bounds[updated_task.id.unpack() as usize].1;

    // We check whether a mandatory part was extended/introduced
    if context.upper_bound(&updated_task.start_variable)
        < context.lower_bound(&updated_task.start_variable) + updated_task.processing_time
    {
        result.update = Some(UpdatedTaskInfo {
            task: Rc::clone(updated_task),
            old_lower_bound,
            old_upper_bound,
            new_lower_bound: context.lower_bound(&updated_task.start_variable),
            new_upper_bound: context.upper_bound(&updated_task.start_variable),
        });
    }

    // If the time-table is empty and we have not received any updates (e.g. no mandatory parts have
    // been introduced since the last propagation) then we can determine that no propagation will
    // take place. It is not sufficient to check whether there have been no updates since it
    // could be the case that a task which has been updated can now propagate due to an existing
    // profile (this is due to the fact that we only propagate bounds and (currently) do not create
    // holes in the domain!).
    if !empty_time_table || !parameters.updated.is_empty() || result.update.is_some() {
        result.decision = EnqueueDecision::Enqueue;
    }
    result
}

/// An enum which specifies whether a current mandatory part was extended or whether a fully new
/// mandatory part is introduced; see [`generate_update_range`] for more information.
pub(crate) enum AddedMandatoryConsumption {
    /// There was an existing mandatory part but it has been extended by an update; the first
    /// [`Range`] is the added mandatory part due to an update of the upper-bound of the start time
    /// and the second [`Range`] si the added mandatory part due to an update of the lower-bound of
    /// the start time.
    AdditionalMandatoryParts(Range<i32>, Range<i32>),
    /// There was no existing mandatory part before the update but there is one now.
    FullyNewMandatoryPart(Range<i32>),
}

impl Iterator for AddedMandatoryConsumption {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AddedMandatoryConsumption::AdditionalMandatoryParts(
                first_added_part,
                second_added_part,
            ) => first_added_part.next().or(second_added_part.next()),
            AddedMandatoryConsumption::FullyNewMandatoryPart(fully_new_added_part) => {
                fully_new_added_part.next()
            }
        }
    }
}

/// When a [`Task`] is updated (i.e. its release time increased or its deadline decreased), this
/// function determines at which times mandatory parts are added.
/// It returns a tuple consisting of two ranges:
/// - If a fully new mandatory part is added (i.e. there previously was not a mandatory part but
///   after the update there is) then the first [`Range`] will contain the new mandatory part and
///   the second [`Range`] will be empty
/// - If a mandatory part already existed then the new mandatory parts extend the already existing
///   mandatory part either before, after or both. In this case, the first returned [`Range`] will
///   contain the mandatory part introduced by a potential update of the upper-bound of the start
///   time (consisting of [LST', LST] where LST is the previous latest start time and LST' is the
///   updated latest start time) and the second [`Range`] will consist of the mandatory part
///   introduced by a potential update of the lower-bound of the start time (consisting of [EST,
///   EST']).
///
/// Note: It is required that the task has a mandatory part in the current state of the solver.
pub(crate) fn generate_update_range<Var: IntVar + 'static>(
    task: &Task<Var>,
    prev_lower_bound: i32,
    prev_upper_bound: i32,
    new_lower_bound: i32,
    new_upper_bound: i32,
) -> AddedMandatoryConsumption {
    pumpkin_assert_moderate!(
        new_upper_bound < new_lower_bound + task.processing_time,
        "The `generate_update_range` method assumes that the task has a new mandatory part"
    );
    if prev_upper_bound < prev_lower_bound + task.processing_time {
        // A mandatory part existed previously, the current mandatory part has thus been extended
        AddedMandatoryConsumption::AdditionalMandatoryParts(
            new_upper_bound..prev_upper_bound,
            prev_lower_bound + task.processing_time..new_lower_bound + task.processing_time,
        )
    } else {
        // A mandatory part did not exist previously but the task has a mandatory part after the
        // update
        AddedMandatoryConsumption::FullyNewMandatoryPart(
            new_upper_bound..new_lower_bound + task.processing_time,
        )
    }
}

/// Checks whether a specific task (indicated by id) has a mandatory part which overlaps with the
/// interval [start, end]
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
    // There exists a mandatory part
    (upper_bound < (lower_bound + task.processing_time))
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
    ); // The release time of the task and the deadline
    has_overlap_with_interval(lower_bound, upper_bound, start, end)
}

/// Determines whether the interval \[lower_bound, upper_bound\) overlaps with the interval [start,
/// end]
fn has_overlap_with_interval(lower_bound: i32, upper_bound: i32, start: i32, end: i32) -> bool {
    start < upper_bound && lower_bound <= end
}

/// Checks whether propagations should occur based on the current state of the time-table
///
/// It goes over all profiles and all tasks and determines which ones should be propagated;
/// Note that this method is not idempotent and that it assumes that the [`ResourceProfile`]s are
/// sorted in increasing order in terms of [`ResourceProfile::start`] and that the
/// [`ResourceProfile`] is maximal (i.e. the [`ResourceProfile::start`] and [`ResourceProfile::end`]
/// cannot be increased or decreased, respectively).
pub(crate) fn propagate_based_on_timetable<'a, Var: IntVar + 'static>(
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
        // Then we go over all the different tasks
        let mut task_index = 0;
        while task_index < tasks_to_consider.len() {
            let task = Rc::clone(tasks_to_consider.get(task_index));
            if context.is_fixed(&task.start_variable)
                || profile.start > context.upper_bound(&task.start_variable) + task.processing_time
            {
                // Task is fixed or the start of the current profile is necessarily after the latest
                // completion time of the task under consideration The profiles are
                // sorted by start time (and non-overlapping) so we can remove the task from
                // consideration
                tasks_to_consider.remove(&task);
                if tasks_to_consider.is_empty() {
                    // There are no tasks left to consider, we can exit the loop
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

/// Determines whether the lower bound of a task can be propagated by a [`ResourceProfile`] with the
/// provided start time and end time; This method checks the following conditions:
///     * lb(s) + p > start, i.e. the earliest completion time of the task is after the start of the
///       [`ResourceProfile`]
///     * lb(s) <= end, i.e. the earliest start time is before the end of the [`ResourceProfile`]
///
/// Note: It is assumed that task.resource_usage + height > capacity (i.e. the task has the
/// potential to overflow the capacity in combination with the profile)
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

/// Determines whether the upper bound of a task can be propagated by a [`ResourceProfile`] with the
/// provided start time and end time This method checks the following conditions:
///     * ub(s) + p > start, i.e. the latest completion time is after the start of the
///       [`ResourceProfile`]
///     * ub(s) <= end, i.e. the latest start time is before the end of the [`ResourceProfile`]
/// Note: It is assumed that the task is known to overflow the [`ResourceProfile`]
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

/// Propagates the lower-bound of the task to avoid overlap with the [`ResourceProfile`] `profile`
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
    // The new lower-bound is set to be after the current profile
    let new_lower_bound = profile.end + 1;
    // Then we perform the actual propagation using this bound and the responsible tasks
    propagate_and_explain(
        context,
        ChangeWithExplanationBound::LowerBound(max(0, profile.start - task.processing_time + 1)), /* Use the minimum bound which would have propagated the profile at index */
        task,
        new_lower_bound,
        &profile.profile_tasks,
    )
}

/// Propagates the upper-bound of the task to avoid overlap with the [`ResourceProfile`] `profile`
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
    // The new upper-bound is set such that if the task is started at its latest starting time, it
    // will never overlap with the profile
    let new_upper_bound = profile.start - task.processing_time;
    // Then we perform the actual propagation using this bound and the responsible tasks
    propagate_and_explain(
        context,
        ChangeWithExplanationBound::UpperBound(max(
            context.upper_bound(&task.start_variable),
            profile.end,
        )),
        task,
        new_upper_bound,
        &profile.profile_tasks,
    )
}

/// The method checks whether the current task can be propagated by the provided profile and (if
/// appropriate) performs the propagation. It then returns whether any of the propagations led to a
/// conflict or whether all propagations were succesful.
///
/// Note that this method can only find [`Inconsistency::EmptyDomain`] conflicts which means that we
/// handle that error in the parent function
fn check_whether_task_can_be_updated_by_profile<Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
) -> Result<(), EmptyDomain> {
    if profile.height + task.resource_usage <= parameters.capacity
        || has_mandatory_part_in_interval(context, task, profile.start, profile.end)
    {
        // The task cannot be propagated due to its resource usage being too low or it is part of
        // the interval which means that it cannot be updated at all
        return Ok(());
    } else if task_has_overlap_with_interval(context, task, profile.start, profile.end) {
        // The current task has an overlap with the current resource profile (i.e. it could be
        // propagated by the current profile)
        if lower_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity) {
            propagate_lower_bound_task_by_profile(context, task, parameters, profile)?;
        }
        if upper_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity) {
            propagate_upper_bound_task_by_profile(context, task, parameters, profile)?;
        }
    }
    Ok(())
}
