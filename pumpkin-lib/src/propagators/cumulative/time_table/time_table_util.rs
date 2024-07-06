//! Defines common methods for [`Propagator`]s which make use of time-table
//! reasoning (see [`crate::propagators::cumulative::time_table`] for more information) such as
//! [`should_enqueue`] or [`propagate_based_on_timetable`].

use std::cell::OnceCell;
use std::cmp::max;
use std::cmp::min;
use std::ops::Range;
use std::rc::Rc;

#[cfg(doc)]
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::variables::IntegerVariable;
use crate::engine::EmptyDomain;
use crate::predicate;
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

    result.decision = if parameters.allow_holes_in_domain {
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
pub(crate) fn task_has_overlap_with_interval<Var: IntegerVariable + 'static>(
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

/// Determines whether the interval \[lower_bound, upper_bound\) overlaps with the interval \[start,
/// end\]
fn has_overlap_with_interval(lower_bound: i32, upper_bound: i32, start: i32, end: i32) -> bool {
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

/// Checks whether propagations should occur based on the current state of the time-table
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

    let mut tasks_to_consider = SparseSet::new(parameters.tasks.to_vec(), Task::get_id);
    'profile_loop: for profile in time_table {
        // Then we go over all the different tasks
        let mut task_index = 0;
        let mut profile_explanation = OnceCell::new();
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
            check_whether_task_can_be_updated_by_profile(
                context,
                &task,
                profile,
                parameters,
                &mut profile_explanation,
            )?;
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
fn lower_bound_can_be_propagated_by_profile<Var: IntegerVariable + 'static>(
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
fn upper_bound_can_be_propagated_by_profile<Var: IntegerVariable + 'static>(
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
fn propagate_lower_bound_task_by_profile<Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    parameters: &CumulativeParameters<Var>,
    profile: &ResourceProfile<Var>,
    explanation: Rc<Vec<Predicate>>,
) -> Result<(), EmptyDomain> {
    pumpkin_assert_advanced!(
        lower_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity),
        "Lower-bound of task is being propagated by profile while the conditions do not hold"
    );
    // The new value to which the lower-bound of `task` will be propagated
    let new_lower_bound_value = profile.end + 1;

    // Create the predicate which is used in the "lazy" explanation
    // Note that this is not the value to which the lower-bound is propagated but rather the
    // predicate which ensures that `task` has the most general bound possible such that
    // this propagation would have taken place.
    let general_explanation_lower_bound_predicate =
        predicate!(task.start_variable >= max(0, profile.start - task.processing_time + 1));
    // Then we perform the actual propagation using this bound and the responsible tasks
    // Note that this is a semi-lazy explanation, we do not recalculate the entire explanation from
    // scratch but we only clone the underlying structure when it is used in an explanation; this
    // prevents creating (possibly a very similar) explanation for every propagation which we do
    context.set_lower_bound(
        &task.start_variable,
        new_lower_bound_value,
        move |_context: &PropagationContext| {
            // We clone the underlying Vec in the Rc and adjust it to include the appropriate
            // predicate for the type of propagation (using the
            // `general_explanation_lower_bound_predicate` which we constructed earlier)
            let mut reason = (*explanation).clone();
            reason.push(general_explanation_lower_bound_predicate);
            reason.into()
        },
    )
}

/// Propagates the upper-bound of the task to avoid overlap with the [`ResourceProfile`] `profile`
fn propagate_upper_bound_task_by_profile<Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    parameters: &CumulativeParameters<Var>,
    profile: &ResourceProfile<Var>,
    explanation: Rc<Vec<Predicate>>,
) -> Result<(), EmptyDomain> {
    pumpkin_assert_advanced!(
        upper_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity),
        "Upper-bound of task is being propagated by profile while the conditions do not hold"
    );
    // The new value to which the upper-bound of `task` will be propagated.
    // The new upper-bound is set such that if the task is started at its latest starting time, it
    // will never overlap with the profile
    let new_upper_bound_value = profile.start - task.processing_time;

    // Create the predicate which is used in the "lazy" explanation
    // Note that this is not the value to which the upper-bound is propagated but rather the
    // predicate which ensures that `task` has the most general bound possible such that
    // this propagation would have taken place.
    let general_explanation_upper_bound_predicate = predicate!(
        task.start_variable <= max(context.upper_bound(&task.start_variable), profile.end)
    );

    // Then we perform the actual propagation using this bound and the responsible tasks
    // Note that this is a semi-lazy explanation, we do not recalculate the entire explanation from
    // scratch but we only clone the underlying structure when it is used in an explanation; this
    // prevents creating (possibly a very similar) explanation for every propagation which we do
    context.set_upper_bound(
        &task.start_variable,
        new_upper_bound_value,
        move |_context: &PropagationContext| {
            // We clone the underlying Vec in the Rc and adjust it to include the appropriate
            // predicate for the type of propagation (using the
            // `general_explanation_upper_bound_predicate` which we constructed earlier)
            let mut reason = (*explanation).clone();
            reason.push(general_explanation_upper_bound_predicate);
            reason.into()
        },
    )
}

/// The method checks whether the current task can be propagated by the provided profile and (if
/// appropriate) performs the propagation. It then returns whether any of the propagations led to a
/// conflict or whether all propagations were succesful.
///
/// Note that this method can only find [`Inconsistency::EmptyDomain`] conflicts which means that we
/// handle that error in the parent function
fn check_whether_task_can_be_updated_by_profile<Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
    profile_explanation: &mut OnceCell<Rc<Vec<Predicate>>>,
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
            let explanation = create_profile_explanation(context, profile, profile_explanation);
            propagate_lower_bound_task_by_profile(context, task, parameters, profile, explanation)?;
        }
        if upper_bound_can_be_propagated_by_profile(context, task, profile, parameters.capacity) {
            let explanation = create_profile_explanation(context, profile, profile_explanation);
            propagate_upper_bound_task_by_profile(context, task, parameters, profile, explanation)?;
        }
        if parameters.allow_holes_in_domain {
            // We go through all of the time-points which cause `task` to overlap with the resource
            // profile

            // There are two options for determining the lowest value to remove from the domain:
            // - We remove all time-points from `profile.start - duration + 1` (i.e. the earliest
            //   time-point such that `task` necessarily overlaps with the profile).
            // - It could be the case that the lower-bound is larger than the previous earliest
            //   time-point in which case we simply start from the lower-bound of the task.

            let lower_bound_removed_time_points = max(
                context.lower_bound(&task.start_variable),
                profile.start - task.processing_time + 1,
            );

            // There are also two options for determine the highest value to remove from the domain:
            // - We remove all time-points up-and-until the end of the profile (i.e. the latest
            //   time-point which would result in the `task` overlapping with the profile)
            // - It could be the case that the upper-bound is smaller than the previous later
            //   time-point in case we remove all time-points up-and-until the upper-bound of the
            //   task.
            let upper_bound_removed_time_points =
                min(context.upper_bound(&task.start_variable), profile.end);
            for time_point in lower_bound_removed_time_points..=upper_bound_removed_time_points {
                let explanation = create_profile_explanation(context, profile, profile_explanation);

                context.remove(
                    &task.start_variable,
                    time_point,
                    move |_context: &PropagationContext| {
                        // We clone the underlying Vec in the Rc.
                        //
                        // Note that we do not need to include any of the information about `task`
                        // since the explanation for the removal is valid irregardless of the bounds
                        // of task.
                        (*explanation).clone().into()
                    },
                )?;
            }
        }
    }
    Ok(())
}

/// Initialises the `profile_explanation` structure with the [`Predicate::lower_bound_predicate`]
/// and [`Predicate::upper_bound_predicate`] of all of the [`ResourceProfile::profile_tasks`] of the
/// provided `profile`.
///
/// Creates an explanation consisting of all bounds of the variables causing a propagation (See [Section 4.5 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)).
/// Note that this is not necessarily a minimal explanation, it could be the case that some of the
/// tasks can be removed from the explanation depending on which task is propagated.
///
/// Note that this method stores an [`Rc`] in `profile_explanations` and then clones it, this means
/// that the [`Rc`] is created only once for each profile.
///
/// # Bibliography
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
fn create_profile_explanation<Var: IntegerVariable + 'static>(
    context: &mut PropagationContextMut,
    profile: &ResourceProfile<Var>,
    profile_explanation: &mut OnceCell<Rc<Vec<Predicate>>>,
) -> Rc<Vec<Predicate>> {
    Rc::clone(profile_explanation.get_or_init(|| {
        Rc::new(
            profile
                .profile_tasks
                .iter()
                .flat_map(|profile_task| {
                    [
                        predicate!(
                            profile_task.start_variable
                                >= context.lower_bound(&profile_task.start_variable)
                        ),
                        predicate!(
                            profile_task.start_variable
                                <= context.upper_bound(&profile_task.start_variable)
                        ),
                    ]
                })
                .collect::<Vec<Predicate>>(),
        )
    }))
}
