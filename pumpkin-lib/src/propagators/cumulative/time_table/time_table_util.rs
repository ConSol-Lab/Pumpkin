//! Defines common methods for [`Propagator`]s which make use of time-table
//! reasoning (see [`crate::propagators::cumulative::time_table`] for more information) such as
//! [`should_enqueue`] or [`propagate_based_on_timetable`].

use std::cmp::max;
use std::ops::Range;
use std::rc::Rc;

#[cfg(doc)]
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::variables::IntegerVariable;
use crate::propagators::cumulative::time_table::propagation_handler::CumulativePropagationHandler;
use crate::propagators::CumulativeParameters;
use crate::propagators::SparseSet;
use crate::propagators::Task;
use crate::propagators::UpdatedTaskInfo;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

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

impl<Var: IntegerVariable + 'static> ResourceProfile<Var> {
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
    ///
    /// In general, non-incremental propagators will not make use of this field since they will
    /// propagate from scratch anyways.
    pub(crate) update: Option<UpdatedTaskInfo<Var>>,
}

/// Determines whether a time-table propagator should enqueue and returns a structure containing the
/// [`EnqueueDecision`] and the info of the task with the extended mandatory part (or [`None`] if no
/// such task exists). This method should be called in the
/// [`ConstraintProgrammingPropagator::notify`] method.
pub(crate) fn should_enqueue<Var: IntegerVariable + 'static>(
    parameters: &CumulativeParameters<Var>,
    updated_task: &Rc<Task<Var>>,
    context: &PropagationContext,
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

    result.decision = if parameters.options.allow_holes_in_domain {
        // If there are updates then propagations might occur due to new mandatory parts being
        // added. However, if there are no updates then because we allow holes in the domain, no
        // updates can occur so we can skip propagation!
        if !parameters.updated.is_empty() || result.update.is_some() {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    } else {
        // If the time-table is empty and we have not received any updates (e.g. no mandatory parts
        // have been introduced since the last propagation) then we can determine that no
        // propagation will take place. It is not sufficient to check whether there have
        // been no updates since it could be the case that a task which has been updated can
        // now propagate due to an existing profile (this is due to the fact that we only
        // propagate bounds and (currently) do not create holes in the domain!).
        if !empty_time_table || !parameters.updated.is_empty() || result.update.is_some() {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    };
    result
}

/// An enum which specifies whether a current mandatory part was extended or whether a fully new
/// mandatory part is introduced; see [`generate_update_range`] for more information.
pub(crate) enum AddedMandatoryConsumption {
    /// There was an existing mandatory part but it has been extended by an update; the first
    /// [`Range`] is the added mandatory part due to an update of the upper-bound of the start time
    /// and the second [`Range`] is the added mandatory part due to an update of the lower-bound of
    /// the start time.
    AdditionalMandatoryParts(Range<i32>, Range<i32>),
    /// There was no existing mandatory part before the update but there is one now.
    FullyNewMandatoryPart(Range<i32>),
}

impl AddedMandatoryConsumption {
    /// There are two cases:
    /// - In the case of [`AddedMandatoryConsumption::AdditionalMandatoryParts`] - This function
    ///   will return first the range which has been added due to an update of the lower-bound and
    ///   then the range which has bene added due to an update of the uppper-bound.
    /// - In the case of [`AddedMandatoryConsumption::FullyNewMandatoryPart`] - This function will
    ///   return the range consisting of the added mandatory part.
    ///
    /// This function is used by [`TimeTableOverIntervalIncremental::propagate`] since it needs to
    /// traverse the second added mandatory part first due to the way it processes indices.
    pub(crate) fn get_reverse_update_ranges(&self) -> Vec<Range<i32>> {
        match self {
            AddedMandatoryConsumption::AdditionalMandatoryParts(
                first_added_part,
                second_added_part,
            ) => {
                // First return the second part and then return the first part, filtering out the
                // empty mandatory parts
                [second_added_part.clone(), first_added_part.clone()]
                    .into_iter()
                    .filter(|added_part| !added_part.is_empty())
                    .collect()
            }
            AddedMandatoryConsumption::FullyNewMandatoryPart(added_mandatory_part) => {
                pumpkin_assert_simple!(!added_mandatory_part.is_empty());
                vec![added_mandatory_part.clone()]
            }
        }
    }
}

impl Iterator for AddedMandatoryConsumption {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AddedMandatoryConsumption::AdditionalMandatoryParts(
                first_added_part,
                second_added_part,
            ) => first_added_part.next().or_else(|| second_added_part.next()),
            AddedMandatoryConsumption::FullyNewMandatoryPart(fully_new_added_part) => {
                fully_new_added_part.next()
            }
        }
    }
}

/// When a [`Task`] is updated (i.e. its release time increased or its deadline decreased), this
/// function determines at which times mandatory parts are added.
/// It returns an [`AddedMandatoryConsumption`] consisting of two possibilities:
/// - If a fully new mandatory part is added (i.e. there previously was not a mandatory part but
///   after the update there is) then it will return a
///   [`AddedMandatoryConsumption::FullyNewMandatoryPart`] containing the range of time-points which
///   are covered by the new mandatory part.
/// - If a mandatory part already existed then the new mandatory parts extend the already existing
///   mandatory part either before, after or both. In this case, it will return a
///   [`AddedMandatoryConsumption::AdditionalMandatoryParts`]. The first [`Range`] held by this
///   structure will contain the mandatory part introduced by a potential update of the upper-bound
///   of the start time (consisting of [LST', LST] where LST is the previous latest start time and
///   LST' is the updated latest start time) and the second [`Range`] held by this structure will
///   consist of the mandatory part introduced by a potential update of the lower-bound of the start
///   time (consisting of [EST, EST']).
///
/// Note: It is required that the task has a mandatory part in the current state of the solver.
pub(crate) fn generate_update_range<Var: IntegerVariable + 'static>(
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
pub(crate) fn has_mandatory_part_in_interval<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
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
pub(crate) fn task_has_overlap_with_interval<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
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

/// Determines whether the interval \[lower_bound, upper_bound\) overlaps with the interval \[start,
/// end\]
pub(crate) fn has_overlap_with_interval(
    lower_bound: i32,
    upper_bound: i32,
    start: i32,
    end: i32,
) -> bool {
    start < upper_bound && lower_bound <= end
}

/// A method which checks whether the time-table (provided in the form of an iterator) is sorted
/// based on start time and that the profiles are maximal (i.e. the [`ResourceProfile::start`] and
/// [`ResourceProfile::end`] cannot be increased or decreased, respectively). It returns true if
/// both of these invariants hold and false otherwise.
fn debug_check_whether_profiles_are_maximal_and_sorted<'a, Var: IntegerVariable + 'static>(
    time_table: impl Iterator<Item = &'a ResourceProfile<Var>> + Clone,
) -> bool {
    let collected_time_table = time_table.clone().collect::<Vec<_>>();
    let sorted_profiles = collected_time_table.is_empty()
        || (0..collected_time_table.len() - 1).all(|profile_index| {
            collected_time_table[profile_index].end < collected_time_table[profile_index + 1].start
        });
    if !sorted_profiles {
        eprintln!("The provided time-table was not ordered according to start/end times");
    }

    let non_overlapping_profiles = collected_time_table.is_empty()
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
        });
    if !non_overlapping_profiles {
        eprintln!("There was overlap between profiles in the provided time-table");
    }
    sorted_profiles && non_overlapping_profiles
}

/// Checks whether propagations should occur based on the current state of the time-table.
///
/// It goes over all profiles and all tasks and determines which ones should be propagated;
/// Note that this method is not idempotent and that it assumes that the [`ResourceProfile`]s are
/// sorted in increasing order in terms of [`ResourceProfile::start`] and that the
/// [`ResourceProfile`] is maximal (i.e. the [`ResourceProfile::start`] and [`ResourceProfile::end`]
/// cannot be increased or decreased, respectively).
pub(crate) fn propagate_based_on_timetable<'a, Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    time_table: impl Iterator<Item = &'a ResourceProfile<Var>> + Clone,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    pumpkin_assert_extreme!(
        debug_check_whether_profiles_are_maximal_and_sorted(time_table.clone()),
        "The provided time-table did not adhere to the invariants"
    );
    if parameters.options.generate_sequence {
        propagate_sequence_of_profiles(context, time_table, parameters)?;
    } else {
        propagate_single_profiles(context, time_table, parameters)?;
    }

    Ok(())
}

/// For each profile in chronological order, this method goes through the tasks and checks whether
/// the profile can propagate the domain of the task.
///
/// If it can then it will immediately propagate it even if this propagation would cause subsequent
/// propagations by the next profile. For a method which propagates based on a sequence of profiles
/// see [`propagate_sequence_of_profiles`].
///
/// This type of propagation is likely to be less beneficial for the explanation
/// [`CumulativeExplanationType::Pointwise`].
fn propagate_single_profiles<'a, Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    time_table: impl Iterator<Item = &'a ResourceProfile<Var>> + Clone,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // We create the structure responsible for propagations and explanations
    let mut propagation_handler =
        CumulativePropagationHandler::new(parameters.options.explanation_type);

    // We create a structure which keeps track of which tasks are fixed so that we don't traverse
    // them further
    let mut tasks_to_consider = SparseSet::new(parameters.tasks.to_vec(), Task::get_id);

    // Then we go over all of the profiles in the time-table
    'profile_loop: for profile in time_table {
        // We indicate to the propagation handler that we cannot re-use an existing profile
        // explanation
        propagation_handler.next_profile();

        // Then we go over all the different tasks
        let mut task_index = 0;
        while task_index < tasks_to_consider.len() {
            let task = Rc::clone(tasks_to_consider.get(task_index));
            if context.is_fixed(&task.start_variable)
                || profile.start > context.upper_bound(&task.start_variable) + task.processing_time
            {
                // Task is fixed or the start of the current profile is necessarily after the
                // latest completion time of the task under consideration
                // The profiles are sorted by start time (and
                // non-overlapping) so we can remove the task from
                // consideration
                tasks_to_consider.remove(&task);
                if tasks_to_consider.is_empty() {
                    // There are no tasks left to consider, we can exit the loop
                    break 'profile_loop;
                }
                continue;
            }
            task_index += 1;

            // We get the updates which are possible (i.e. a lower-bound update, an upper-bound
            // update or a hole in the domain)
            let possible_updates = find_possible_updates(context, &task, profile, parameters);
            for possible_update in possible_updates {
                // For every possible update we let the propagation handler propagate
                match possible_update {
                    CanUpdate::LowerBound => {
                        propagation_handler
                            .propagate_lower_bound_with_explanations(context, profile, &task)?;
                    }
                    CanUpdate::UpperBound => {
                        propagation_handler
                            .propagate_upper_bound_with_explanations(context, profile, &task)?;
                    }
                    CanUpdate::Holes => {
                        propagation_handler.propagate_holes_in_domain(context, profile, &task)?;
                    }
                }
            }
        }
    }
    Ok(())
}

/// For each task this method goes through the profiles in chronological order to find one which can
/// update the task's bounds.
///
/// If it can find such a profile then it proceeds to generate a sequence of profiles
/// which can propagate the bound of the task and uses these to explain the propagation rather than
/// the individual profiles (for propagating individual profiles see [`propagate_single_profiles`]).
///
/// Especially in the case of [`CumulativeExplanationType::Pointwise`] this is likely to be
/// beneficial.
fn propagate_sequence_of_profiles<'a, Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    time_table: impl Iterator<Item = &'a ResourceProfile<Var>> + Clone,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // We create the structure responsible for propagations and explanations
    let mut propagation_handler =
        CumulativePropagationHandler::new(parameters.options.explanation_type);

    // We collect the time-table since we will need to index into it
    let time_table = time_table.collect::<Vec<_>>();

    // Then we go over all the possible tasks
    for task in parameters.tasks.iter() {
        if context.is_fixed(&task.start_variable) {
            // If the task is fixed then we are not able to propagate it further
            continue;
        }

        // Then we go over all the different profiles
        let mut profile_index = 0;
        'profile_loop: while profile_index < time_table.len() {
            let profile = time_table[profile_index];

            if profile.start > context.upper_bound(&task.start_variable) + task.processing_time {
                // The profiles are sorted, if we cannot update using this one then we cannot update
                // using the subsequent profiles, we can break from the loop
                break 'profile_loop;
            }

            if !can_be_updated_by_profile(
                &context.as_readonly(),
                task,
                profile,
                parameters.capacity,
            ) {
                // The task cannot be propagate by the profile so we move to the next one
                profile_index += 1;
                continue;
            }

            // Keep track of the next profile index to use after we generate the sequence of
            // profiles
            let mut new_profile_index = profile_index;

            // Then we check what propagations can be performed
            if lower_bound_can_be_propagated_by_profile(
                &context.as_readonly(),
                task,
                profile,
                parameters.capacity,
            ) {
                // We find the index (non-inclusive) of the last profile in the chain of lower-bound
                // propagations
                let last_index = find_index_last_profile_which_propagates_lower_bound(
                    profile_index,
                    &time_table,
                    &context.as_readonly(),
                    task,
                    parameters.capacity,
                );

                // Then we provide the propagation handler with the chain of profiles and propagate
                // all of them
                propagation_handler.propagate_chain_of_lower_bounds_with_explanations(
                    context,
                    &time_table[profile_index..last_index],
                    task,
                )?;

                // Then we set the new profile index to the last index, note that this index (since
                // it is non-inclusive) will always be larger than the current profile index
                new_profile_index = last_index;
            }

            if upper_bound_can_be_propagated_by_profile(
                &context.as_readonly(),
                task,
                profile,
                parameters.capacity,
            ) {
                // We find the index (inclusive) of the last profile in the chain of upper-bound
                // propagations (note that the index of this last profile in the chain is `<=
                // profile_index`)
                let first_index = find_index_last_profile_which_propagates_upper_bound(
                    profile_index,
                    &time_table,
                    &context.as_readonly(),
                    task,
                    parameters.capacity,
                );
                // Then we provide the propagation handler with the chain of profiles and propagate
                // all of them
                propagation_handler.propagate_chain_of_upper_bounds_with_explanations(
                    context,
                    &time_table[first_index..=profile_index],
                    task,
                )?;

                // Then we set the new profile index to maximum of the previous value of the new
                // profile index and the next profile index
                new_profile_index = max(new_profile_index, profile_index + 1);
            }

            if parameters.options.allow_holes_in_domain {
                // If we allow the propagation of holes in the domain then we simply let the
                // propagation handler handle it
                propagation_handler.propagate_holes_in_domain(context, profile, task)?;

                // Then we set the new profile index to maximum of the previous value of the new
                // profile index and the next profile index
                new_profile_index = max(new_profile_index, profile_index + 1);
            }

            // Finally, we simply set the profile index to the index of the new profile
            profile_index = new_profile_index;
        }
    }
    Ok(())
}

/// Returns the index of the profile which cannot propagate the lower-bound of the provided task any
/// further based on the propagation of the upper-bound due to `time_table[profile_index]`.
fn find_index_last_profile_which_propagates_lower_bound<Var: IntegerVariable + 'static>(
    profile_index: usize,
    time_table: &[&ResourceProfile<Var>],
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    capacity: i32,
) -> usize {
    let mut last_index = profile_index + 1;
    while last_index < time_table.len() {
        let next_profile = time_table[last_index];
        if next_profile.start - time_table[last_index - 1].end >= task.processing_time
            || !can_be_updated_by_profile(context, task, next_profile, capacity)
            || !lower_bound_can_be_propagated_by_profile(context, task, next_profile, capacity)
        {
            break;
        }
        last_index += 1;
    }
    last_index
}

/// Returns the index of the last profile which could propagate the upper-bound of the task based on
/// the propagation of the upper-bound due to `time_table[profile_index]`.
fn find_index_last_profile_which_propagates_upper_bound<Var: IntegerVariable + 'static>(
    profile_index: usize,
    time_table: &[&ResourceProfile<Var>],
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    capacity: i32,
) -> usize {
    if profile_index == 0 {
        return 0;
    }
    let mut first_index = profile_index - 1;
    loop {
        let previous_profile = time_table[first_index];
        if time_table[first_index + 1].start - previous_profile.end >= task.processing_time
            || !can_be_updated_by_profile(context, task, previous_profile, capacity)
            || !upper_bound_can_be_propagated_by_profile(context, task, previous_profile, capacity)
        {
            first_index += 1;
            break;
        }

        if first_index == 0 {
            break;
        } else {
            first_index -= 1;
        }
    }
    first_index
}

/// Determines whether the lower bound of a task can be propagated by a [`ResourceProfile`] with the
/// provided start time and end time; This method checks the following conditions:
///     * lb(s) + p > start, i.e. the earliest completion time of the task is after the start of the
///       [`ResourceProfile`]
///     * lb(s) <= end, i.e. the earliest start time is before the end of the [`ResourceProfile`]
///
/// Note: It is assumed that task.resource_usage + height > capacity (i.e. the task has the
/// potential to overflow the capacity in combination with the profile)
fn lower_bound_can_be_propagated_by_profile<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
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
fn upper_bound_can_be_propagated_by_profile<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
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

/// Returns whether the provided `task` can be updated by the profile by checking the following:
/// 1. Whether the task and the profile together would overflow the resource capacity
/// 2. Whether the task has a mandatory part in the profile
/// 3. Whether the bounds of the task overlap with the profile
///
/// If the first condition is true, the second false and the third true then this method returns
/// true (otherwise it returns false)
fn can_be_updated_by_profile<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    capacity: i32,
) -> bool {
    profile.height + task.resource_usage > capacity
        && !has_mandatory_part_in_interval(context, task, profile.start, profile.end)
        && task_has_overlap_with_interval(context, task, profile.start, profile.end)
}

/// An enum which represents which values can be updated by a profile
enum CanUpdate {
    LowerBound,
    UpperBound,
    Holes,
}

/// The method checks whether the current task can be propagated by the provided profile and (if
/// appropriate) performs the propagation. It then returns whether any of the propagations led to a
/// conflict or whether all propagations were succesful.
///
/// Note that this method can only find [`Inconsistency::EmptyDomain`] conflicts which means that we
/// handle that error in the parent function
fn find_possible_updates<Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
) -> Vec<CanUpdate> {
    if !can_be_updated_by_profile(&context.as_readonly(), task, profile, parameters.capacity) {
        // If the task cannot be updated by the profile then we simply return the empty list
        vec![]
    } else {
        // The task could be updated by the profile!
        let mut result = vec![];

        if lower_bound_can_be_propagated_by_profile(
            &context.as_readonly(),
            task,
            profile,
            parameters.capacity,
        ) {
            // The lower-bound of the task can be updated by the profile
            result.push(CanUpdate::LowerBound)
        }
        if upper_bound_can_be_propagated_by_profile(
            &context.as_readonly(),
            task,
            profile,
            parameters.capacity,
        ) {
            // The upper-bound of the task can be updated by the profile
            result.push(CanUpdate::UpperBound)
        }
        if parameters.options.allow_holes_in_domain {
            // Holes can be created in the domain of the task by the profile
            result.push(CanUpdate::Holes)
        }
        result
    }
}
