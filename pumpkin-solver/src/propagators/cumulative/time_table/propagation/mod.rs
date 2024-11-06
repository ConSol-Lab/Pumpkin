mod sequence;
mod single_profile;

use std::rc::Rc;

use sequence::propagate_sequence_of_profiles;
use single_profile::propagate_single_profiles;

use super::time_table_util::has_mandatory_part_in_interval;
use crate::basic_types::PropagationStatusCP;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::ReadDomains;
use crate::propagators::cumulative::time_table::time_table_util::task_has_overlap_with_interval;
use crate::propagators::CumulativeParameters;
use crate::propagators::ResourceProfileInterface;
use crate::propagators::Task;
use crate::propagators::UpdatableStructures;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

/// Checks whether propagations should occur based on the current state of the time-table.
///
/// It goes over all profiles and all tasks and determines which ones should be propagated;
/// Note that this method is not idempotent and that it assumes that the [`ResourceProfile`]s are
/// sorted in increasing order in terms of [`ResourceProfile::start`] and that the
/// [`ResourceProfile`] is maximal (i.e. the [`ResourceProfile::start`] and [`ResourceProfile::end`]
/// cannot be increased or decreased, respectively).
pub(crate) fn propagate_based_on_timetable<
    'a,
    Var: IntegerVariable + 'static,
    const SHOULD_RESET_UPDATED: bool,
>(
    context: &mut PropagationContextMut,
    time_table: impl Iterator<Item = &'a mut (impl ResourceProfileInterface<Var> + 'a)>,
    parameters: &CumulativeParameters<Var>,
    updatable_structures: &mut UpdatableStructures<Var>,
) -> PropagationStatusCP {
    pumpkin_assert_extreme!(
        updatable_structures
            .get_unfixed_tasks()
            .all(|unfixed_task| !context.is_fixed(&unfixed_task.start_variable)),
        "All of the unfixed tasks should not be fixed at this point"
    );
    pumpkin_assert_extreme!(
        updatable_structures
            .get_fixed_tasks()
            .all(|fixed_task| context.is_fixed(&fixed_task.start_variable)),
        "All of the fixed tasks should be fixed at this point"
    );

    if parameters.options.generate_sequence {
        propagate_sequence_of_profiles::<Var, SHOULD_RESET_UPDATED>(
            context,
            time_table,
            updatable_structures,
            parameters,
        )?;
    } else {
        propagate_single_profiles::<Var, SHOULD_RESET_UPDATED>(
            context,
            time_table,
            updatable_structures,
            parameters,
        )?;
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
fn lower_bound_can_be_propagated_by_profile<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    context: PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfileType,
    capacity: i32,
) -> bool {
    pumpkin_assert_moderate!(
        profile.get_height() + task.resource_usage > capacity
            && task_has_overlap_with_interval(context, task, profile.get_start(), profile.get_end())
    , "It is checked whether a task can be propagated while the invariants do not hold - The task should overflow the capacity with the profile");
    (context.lower_bound(&task.start_variable) + task.processing_time) > profile.get_start()
        && context.lower_bound(&task.start_variable) <= profile.get_end()
}

/// Determines whether the upper bound of a task can be propagated by a [`ResourceProfile`] with the
/// provided start time and end time This method checks the following conditions:
///     * ub(s) + p > start, i.e. the latest completion time is after the start of the
///       [`ResourceProfile`]
///     * ub(s) <= end, i.e. the latest start time is before the end of the [`ResourceProfile`]
/// Note: It is assumed that the task is known to overflow the [`ResourceProfile`]
fn upper_bound_can_be_propagated_by_profile<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    context: PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfileType,
    capacity: i32,
) -> bool {
    pumpkin_assert_moderate!(
        profile.get_height()+ task.resource_usage > capacity
    , "It is checked whether a task can be propagated while the invariants do not hold - The task should overflow the capacity with the profile");
    (context.upper_bound(&task.start_variable) + task.processing_time) > profile.get_start()
        && context.upper_bound(&task.start_variable) <= profile.get_end()
}

/// Returns whether the provided `task` can be updated by the profile by checking the following:
/// 1. Whether the task and the profile together would overflow the resource capacity
/// 2. Whether the task has a mandatory part in the profile
/// 3. Whether the bounds of the task overlap with the profile
///
/// If the first condition is true, the second false and the third true then this method returns
/// true (otherwise it returns false)
fn can_be_updated_by_profile<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    context: PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfileType,
    capacity: i32,
) -> bool {
    profile.get_height() + task.resource_usage > capacity
        && !has_mandatory_part_in_interval(context, task, profile.get_start(), profile.get_end())
        && task_has_overlap_with_interval(context, task, profile.get_start(), profile.get_end())
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
fn find_possible_updates<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    context: &mut PropagationContextMut,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfileType,
    parameters: &CumulativeParameters<Var>,
) -> Vec<CanUpdate> {
    if !can_be_updated_by_profile(context.as_readonly(), task, profile, parameters.capacity) {
        // If the task cannot be updated by the profile then we simply return the empty list
        vec![]
    } else {
        // The task could be updated by the profile!
        let mut result = vec![];

        if lower_bound_can_be_propagated_by_profile(
            context.as_readonly(),
            task,
            profile,
            parameters.capacity,
        ) {
            // The lower-bound of the task can be updated by the profile
            result.push(CanUpdate::LowerBound)
        }
        if upper_bound_can_be_propagated_by_profile(
            context.as_readonly(),
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
