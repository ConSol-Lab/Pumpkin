//! Contains the functions necessary for inserting the appropriate profiles into the time-table
//! based on the added mandatory part.
use std::ops::Range;
use std::rc::Rc;

use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::checks;
use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

/// The new mandatory part added by `updated_task` (spanning `update_range`) overlaps with the
/// profiles in `[start_index, end_index]`. This function calculates the added, and updated
/// profiles and adds them to the `time-table` at the correct position.
pub(crate) fn insert_profiles_overlapping_with_added_mandatory_part<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    time_table: &mut OverIntervalTimeTableType<Var, PVar, RVar>,
    start_index: usize,
    end_index: usize,
    update_range: &Range<i32>,
    updated_task: &Rc<Task<Var, PVar, RVar>>,
    capacity: i32,
) -> Result<(), ResourceProfile<Var, PVar, RVar>> {
    let mut to_add = Vec::new();

    // We keep track of whether a conflict has been found
    let mut conflict = None;

    // Go over all indices of the profiles which overlap with the updated
    // one and determine which one need to be updated
    for current_index in start_index..=end_index {
        let profile = &time_table[current_index];

        pumpkin_assert_extreme!(!profile.profile_tasks.contains(updated_task));

        // Check whether there is a new profile before the first overlapping
        // profile
        checks::new_profile_before_first_profile(
            current_index,
            start_index,
            update_range,
            profile,
            &mut to_add,
            updated_task,
        );

        // Check whether there is a new profile between the current profile
        // and the previous profile (beginning of profile remains unchanged)
        checks::new_profile_between_profiles(
            time_table,
            current_index,
            start_index,
            update_range,
            profile,
            &mut to_add,
            updated_task,
        );

        // Check whether the current profile is split by the added mandatory
        // part
        checks::split_profile_added_part_starts_after_profile_start(
            update_range,
            profile,
            &mut to_add,
        );

        // Check whether there is an increased profile due to overlap
        // between the current profile and the added mandatory part
        //
        // The addition of the mandatory part can lead to an overflow
        let result = checks::overlap_updated_profile(
            update_range,
            profile,
            &mut to_add,
            updated_task,
            capacity,
        );
        if result.is_err() && conflict.is_none() {
            conflict = Some(result)
        }

        // Check whether the current profile is split by the added mandatory
        // part (end of profile remains unchanged)
        checks::split_profile_added_part_ends_before_profile_end(
            update_range,
            profile,
            &mut to_add,
        );

        // Check whether there is a new profile before the last overlapping
        // profile
        checks::new_part_after_last_profile(
            current_index,
            end_index,
            update_range,
            profile,
            &mut to_add,
            updated_task,
        );
    }
    // We now update the time-table to insert the newly created profiles at
    // the right place to ensure the ordering invariant
    let _ = time_table.splice(start_index..end_index + 1, to_add);

    if let Some(conflict) = conflict {
        conflict
    } else {
        Ok(())
    }
}

/// The new mandatory part added by `updated_task` (spanning `update_range`) does not overlap
/// with any existing profile. This method inserts it at the position of `index_to_insert`
/// in the `time-table`.
pub(crate) fn insert_profile_new_mandatory_part<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    time_table: &mut OverIntervalTimeTableType<Var, PVar, RVar>,
    index_to_insert: usize,
    update_range: &Range<i32>,
    updated_task: &Rc<Task<Var, PVar, RVar>>,
) {
    pumpkin_assert_moderate!(
        index_to_insert <= time_table.len()
            || index_to_insert >= time_table.len()
            || time_table[index_to_insert].start > update_range.end - 1,
        "The index to insert at is incorrect"
    );

    // Insert the new profile at its index
    time_table.insert(
        index_to_insert,
        ResourceProfile {
            start: update_range.start,
            end: update_range.end - 1,
            profile_tasks: vec![Rc::clone(updated_task)],
            height: updated_task.resource_usage,
        },
    );
}
