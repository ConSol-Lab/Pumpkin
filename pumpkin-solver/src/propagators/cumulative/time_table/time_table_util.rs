//! Defines common methods for [`Propagator`]s which make use of time-table
//! reasoning (see [`crate::propagators::cumulative::time_table`] for more information) such as
//! [`should_enqueue`] or [`propagate_based_on_timetable`].

use std::rc::Rc;

#[cfg(doc)]
use crate::basic_types::Inconsistency;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::PropagationContext;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::variables::IntegerVariable;
use crate::propagators::CumulativeParameters;
use crate::propagators::ResourceProfileInterface;
use crate::propagators::Task;
use crate::propagators::UpdatableStructures;
use crate::propagators::UpdatedTaskInfo;
use crate::pumpkin_assert_extreme;

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
    updatable_structures: &UpdatableStructures<Var>,
    updated_task: &Rc<Task<Var>>,
    context: PropagationContext,
    empty_time_table: bool,
) -> ShouldEnqueueResult<Var> {
    pumpkin_assert_extreme!(
        context.lower_bound(&updated_task.start_variable) > updatable_structures.get_stored_lower_bound(updated_task)
            || updatable_structures.get_stored_upper_bound(updated_task)
                >= context.upper_bound(&updated_task.start_variable)
        , "Either the stored lower-bound was larger than or equal to the actual lower bound or the upper-bound was smaller than or equal to the actual upper-bound\nThis either indicates that the propagator subscribed to events other than lower-bound and upper-bound updates or the stored bounds were not managed properly"
    );

    let mut result = ShouldEnqueueResult {
        decision: EnqueueDecision::Skip,
        update: None,
    };

    let old_lower_bound = updatable_structures.get_stored_lower_bound(updated_task);
    let old_upper_bound = updatable_structures.get_stored_upper_bound(updated_task);

    if old_lower_bound == context.lower_bound(&updated_task.start_variable)
        && old_upper_bound == context.upper_bound(&updated_task.start_variable)
    {
        return result;
    }

    // We check whether a mandatory part was extended/introduced
    if has_mandatory_part(context, updated_task) {
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
        if updatable_structures.has_changed_mandatory_parts() || result.update.is_some() {
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
        if !empty_time_table
            || updatable_structures.has_changed_mandatory_parts()
            || result.update.is_some()
        {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    };
    result
}

pub(crate) fn has_mandatory_part<Var: IntegerVariable + 'static>(
    context: PropagationContext,
    task: &Rc<Task<Var>>,
) -> bool {
    context.upper_bound(&task.start_variable)
        < context.lower_bound(&task.start_variable) + task.processing_time
}

/// Checks whether a specific task (indicated by id) has a mandatory part which overlaps with the
/// interval [start, end]
pub(crate) fn has_mandatory_part_in_interval<Var: IntegerVariable + 'static>(
    context: PropagationContext,
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
    context: PropagationContext,
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
#[allow(dead_code)]
fn debug_check_whether_profiles_are_maximal_and_sorted<'a, Var: IntegerVariable + 'static>(
    time_table: impl Iterator<Item = &'a (impl ResourceProfileInterface<Var> + 'a)> + Clone,
) -> bool {
    let collected_time_table = time_table.clone().collect::<Vec<_>>();
    let sorted_profiles = collected_time_table.is_empty()
        || (0..collected_time_table.len() - 1).all(|profile_index| {
            collected_time_table[profile_index].get_end()
                < collected_time_table[profile_index + 1].get_start()
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
                        current_profile.get_start(),
                        current_profile.get_end() + 1,
                        other_profile.get_start(),
                        other_profile.get_end(),
                    )
            })
        });
    if !non_overlapping_profiles {
        eprintln!("There was overlap between profiles in the provided time-table");
    }
    sorted_profiles && non_overlapping_profiles
}

pub(crate) fn insert_update<Var: IntegerVariable + 'static>(
    updated_task: &Rc<Task<Var>>,
    updatable_structures: &mut UpdatableStructures<Var>,
    potential_update: Option<UpdatedTaskInfo<Var>>,
) {
    updatable_structures.task_has_been_updated(Rc::clone(updated_task));
    if let Some(update) = potential_update {
        updatable_structures.task_has_mandatory_part_changed(updated_task);
        updatable_structures.insert_changed_mandatory_part_for_task(updated_task, update);
    }
}

pub(crate) fn backtrack_update<Var: IntegerVariable + 'static>(
    context: PropagationContext,
    updatable_structures: &mut UpdatableStructures<Var>,
    updated_task: &Rc<Task<Var>>,
) {
    updatable_structures.task_has_been_updated(Rc::clone(updated_task));
    // Stores whether the stored lower-bound is equal to the current lower-bound
    let lower_bound_equal_to_stored = updatable_structures.get_stored_lower_bound(updated_task)
        == context.lower_bound(&updated_task.start_variable);

    // Stores whether the stored upper-bound is equal to the current upper-bound
    let upper_bound_equal_to_stored = updatable_structures.get_stored_upper_bound(updated_task)
        == context.upper_bound(&updated_task.start_variable);

    // Stores whether the stored bounds did not include a mandatory part
    let previously_did_not_have_mandatory_part = updatable_structures
        .get_stored_upper_bound(updated_task)
        >= updatable_structures.get_stored_lower_bound(updated_task) + updated_task.processing_time;

    // If the stored bounds are already the same or the previous stored bounds did not include a
    // mandatory part (which means that this task will also not have mandatory part after
    // backtracking)
    if (lower_bound_equal_to_stored && upper_bound_equal_to_stored)
        || previously_did_not_have_mandatory_part
    {
        return;
    }

    // We insert this task into the updated category
    updatable_structures.task_has_mandatory_part_changed(updated_task);
    // And we add the type of update
    updatable_structures.insert_changed_mandatory_part_for_task(
        updated_task,
        UpdatedTaskInfo {
            task: Rc::clone(updated_task),
            old_lower_bound: updatable_structures.get_stored_lower_bound(updated_task),
            old_upper_bound: updatable_structures.get_stored_upper_bound(updated_task),
            new_lower_bound: context.lower_bound(&updated_task.start_variable),
            new_upper_bound: context.upper_bound(&updated_task.start_variable),
        },
    );
}
