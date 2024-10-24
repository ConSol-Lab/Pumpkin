//! Contains the functions necessary for removing the appropriate profiles into the time-table
//! based on the reduced mandatory part.
use std::cmp::max;
use std::cmp::min;
use std::ops::Range;
use std::rc::Rc;

use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// The reduced mandatory part of `updated_task` (spanning `update_range`) overlaps with the
/// profiles in `[start_index, end_index]`. This function calculates the added, and updated
/// profiles and adds them to the `time-table` at the correct position.
pub(crate) fn reduce_profiles_overlapping_with_added_mandatory_part<
    Var: IntegerVariable + 'static,
>(
    time_table: &mut OverIntervalTimeTableType<Var>,
    start_index: usize,
    end_index: usize,
    update_range: &Range<i32>,
    updated_task: &Rc<Task<Var>>,
) {
    let mut to_add = vec![];

    for (index, profile) in time_table
        .iter()
        .enumerate()
        .take(end_index + 1)
        .skip(start_index)
    {
        // We need to check whether the first overlapping profile was split
        if index == start_index {
            split_first_profile(&mut to_add, update_range, profile);
        }

        // Then we need to add the updated profile due to the overlap between `profile` and
        // `updated_task`
        overlap_updated_profile(update_range, profile, &mut to_add, updated_task);

        // We need to check whether the last overlapping profile was split
        if index == end_index {
            split_last_profile(&mut to_add, update_range, profile)
        }
    }

    // We now update the time-table to insert the newly created profiles at
    // the right place to ensure the ordering invariant
    let _ = time_table.splice(start_index..end_index + 1, to_add);
}

/// Returns the provided `profile` with the provided `updated_task` removed.
fn remove_task_from_profile<Var: IntegerVariable + 'static>(
    updated_task: &Rc<Task<Var>>,
    start: i32,
    end: i32,
    profile: &ResourceProfile<Var>,
) -> ResourceProfile<Var> {
    let mut updated_profile_tasks = profile.profile_tasks.clone();
    let _ = updated_profile_tasks.swap_remove(
        updated_profile_tasks
            .iter()
            .position(|task| task.id == updated_task.id)
            .expect("Task should be in the profile if it is being removed"),
    );

    ResourceProfile {
        start,
        end,
        profile_tasks: updated_profile_tasks,
        height: profile.height - updated_task.resource_usage,
    }
}

/// If there is a partial overlap, this method creates a profile consisting of the original
/// profile before the overlap.
pub(crate) fn split_first_profile<Var: IntegerVariable + 'static>(
    to_add: &mut Vec<ResourceProfile<Var>>,
    update_range: &Range<i32>,
    first_profile: &ResourceProfile<Var>,
) {
    if update_range.start > first_profile.start {
        to_add.push(ResourceProfile {
            start: first_profile.start,
            end: min(update_range.start - 1, first_profile.end),
            profile_tasks: first_profile.profile_tasks.clone(),
            height: first_profile.height,
        });
    }
}

pub(crate) fn split_last_profile<Var: IntegerVariable + 'static>(
    to_add: &mut Vec<ResourceProfile<Var>>,
    update_range: &Range<i32>,
    last_profile: &ResourceProfile<Var>,
) {
    if last_profile.end >= update_range.end {
        // We are splitting the current profile into one or more parts
        // The update range ends before the end of the profile;
        // This if-statement takes care of creating a new (smaller)
        // profile which represents the previous profile after it is
        // split by the update range
        to_add.push(ResourceProfile {
            start: max(update_range.end, last_profile.start),
            end: last_profile.end,
            profile_tasks: last_profile.profile_tasks.clone(),
            height: last_profile.height,
        })
    }
}

/// This method creates a new profile based on the overlap with the provided `profile`.
pub(crate) fn overlap_updated_profile<Var: IntegerVariable + 'static>(
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var>,
    to_add: &mut Vec<ResourceProfile<Var>>,
    updated_task: &Rc<Task<Var>>,
) {
    if profile.height - updated_task.resource_usage == 0 {
        // If the removal of this task results in an empty profile then we simply do not add it
        return;
    }
    // Now we create a new profile which consists of the part of the
    // profile covered by the update range
    //
    // This means that we are removing the contribution of the updated
    // task to the profile and adjusting the bounds appropriately

    // Either the new profile starts at the start of the profile (in
    // case the update range starts before the profile start)
    // or the new profile starts at the start of the update range (since
    // we are only looking at the part where there is overlap between
    // the current profile and the update range)
    let new_profile_lower_bound = max(profile.start, update_range.start);

    // Either the new profile ends at the end of the profile (in case
    // the update range ends after the profile end)
    // or the new profile ends at the end of the update range (since we
    // are only looking at the part where there is overlap between the
    // current profile and the update range)
    let new_profile_upper_bound = min(profile.end, update_range.end - 1); // Note that the end of the update_range is exclusive (hence the -1)
    if new_profile_upper_bound >= new_profile_lower_bound {
        // A sanity check, there is a new profile to create consisting
        // of a combination of the previous profile and the updated task

        // We thus create a new profile consisting of the combination of
        // the previous profile and the updated task under consideration
        to_add.push(remove_task_from_profile(
            updated_task,
            new_profile_lower_bound,
            new_profile_upper_bound,
            profile,
        ))
    }
}
