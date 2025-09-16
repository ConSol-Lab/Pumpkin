//! Contains the checks which are done when a new mandatory part is added in the propagate method to
//! determine which profiles should be added and how existing profiles should be adjusted.
use crate::engine::cp::propagation::contexts::propagation_context::ReadDomains;
use std::cmp::max;
use std::cmp::min;
use std::ops::Range;
use std::rc::Rc;

use crate::engine::propagation::PropagationContext;
use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Determines whether the added mandatory part causes a new profile before the first overapping
/// profile.
pub(crate) fn new_profile_before_first_profile<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    context: PropagationContext,
    current_index: usize,
    start_index: usize,
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var, PVar, RVar>,
    to_add: &mut Vec<ResourceProfile<Var, PVar, RVar>>,
    task: &Rc<Task<Var, PVar, RVar>>,
) {
    if current_index == start_index && update_range.start < profile.start {
        // We are considering the first overlapping profile and there is
        // a part before the start of this profile
        // This means we need to add a new mandatory part before the
        // first element
        to_add.push(ResourceProfile {
            start: update_range.start,
            end: profile.start - 1, /* Note that this profile needs to end before the start
                                     * of the current profile, hence the -1 */
            profile_tasks: vec![Rc::clone(task)],
            height: context.lower_bound(&task.resource_usage),
        })
    }
}

/// Determines whether a new profile should be inserted between the current profile (pointed to
/// by `current_index`) and the previous profile.
#[allow(clippy::too_many_arguments, reason = "Should be refactored")]
pub(crate) fn new_profile_between_profiles<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    context: PropagationContext,
    time_table: &OverIntervalTimeTableType<Var, PVar, RVar>,
    current_index: usize,
    start_index: usize,
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var, PVar, RVar>,
    to_add: &mut Vec<ResourceProfile<Var, PVar, RVar>>,
    task: &Rc<Task<Var, PVar, RVar>>,
) {
    if current_index != start_index && current_index != 0 {
        // We are not considering the first profile and there could be a
        // new profile between the current profile and the previous one
        // caused by the updated task
        let previous_profile = &time_table[current_index - 1];

        // The following three points are checked:
        // - There is empty space between the current profile and the previous profile
        // - The update range starts before the end of the previous profile
        // - The update range ends after the start of the current profile
        if previous_profile.end < profile.start - 1
            && update_range.start <= previous_profile.end + 1
            && update_range.end > profile.start - 1
        {
            // There is empty space between the current profile and the
            // previous one, we should insert a new profile
            to_add.push(ResourceProfile {
                start: previous_profile.end + 1,
                end: profile.start - 1,
                profile_tasks: vec![Rc::clone(task)],
                height: context.lower_bound(&task.resource_usage),
            })
        }
    }
}

/// Determines whether the current profile is split by the added mandatory part due to the start
/// of the added mandatory part being after the start of the current profile.
///
/// Note that this function adds the unchanged part only (i.e. the part of the profile with
/// which the added mandatory part does **not** overlap), the updated part of this profile
/// is added in [`overlap_updated_profile`].
pub(crate) fn split_profile_added_part_starts_after_profile_start<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var, PVar, RVar>,
    to_add: &mut Vec<ResourceProfile<Var, PVar, RVar>>,
) {
    if update_range.start > profile.start {
        // We are splitting the current profile into one or more parts
        // The update range starts after the profile starts;
        // This if-statement takes care of creating a new (smaller)
        // profile which represents the previous profile up and until it
        // is split by the update range
        to_add.push(ResourceProfile {
            start: profile.start,
            end: min(update_range.start - 1, profile.end), /* It could be that the update
                                                            * range extends past the profile
                                                            * in which case we should create
                                                            * a profile until the end of the
                                                            * profile */
            profile_tasks: profile.profile_tasks.clone(),
            height: profile.height,
        })
    }
}

/// Determines whether a new profile which contains the overlap between `profile` and the added
/// mandatory part should be added.
pub(crate) fn overlap_updated_profile<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    context: PropagationContext,
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var, PVar, RVar>,
    to_add: &mut Vec<ResourceProfile<Var, PVar, RVar>>,
    task: &Rc<Task<Var, PVar, RVar>>,
    capacity: CVar,
) -> Result<(), ResourceProfile<Var, PVar, RVar>> {
    // Now we create a new profile which consists of the part of the
    // profile covered by the update range
    // This means that we are adding the contribution of the updated
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
        let mut new_profile_tasks = profile.profile_tasks.clone();
        new_profile_tasks.push(Rc::clone(task));

        let new_profile = ResourceProfile {
            start: new_profile_lower_bound,
            end: new_profile_upper_bound,
            profile_tasks: new_profile_tasks.clone(),
            height: profile.height + context.lower_bound(&task.resource_usage),
        };

        // We thus create a new profile consisting of the combination of
        // the previous profile and the updated task under consideration
        to_add.push(new_profile);

        // A sanity check, there is a new profile to create consisting
        // of a combination of the previous profile and the updated task
        if profile.height + context.lower_bound(&task.resource_usage)
            > context.upper_bound(&capacity)
        {
            // The addition of the new mandatory part to the profile
            // caused an overflow of the resource
            return Err(ResourceProfile {
                start: new_profile_lower_bound,
                end: new_profile_upper_bound,
                profile_tasks: new_profile_tasks,
                height: profile.height + context.lower_bound(&task.resource_usage),
            });
        }
    }
    Ok(())
}

/// Determines whether the current profile is split by the added mandatory part due to the end
/// of the added mandatory part being before the end of the profile.
///
/// Note that this function adds the unchanged part only (i.e. the part of the profile with
/// which the added mandatory part does **not** overlap), the updated part of this profile
/// is added in [`overlap_updated_profile`].
pub(crate) fn split_profile_added_part_ends_before_profile_end<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var, PVar, RVar>,
    to_add: &mut Vec<ResourceProfile<Var, PVar, RVar>>,
) {
    if profile.end >= update_range.end {
        // We are splitting the current profile into one or more parts
        // The update range ends before the end of the profile;
        // This if-statement takes care of creating a new (smaller)
        // profile which represents the previous profile after it is
        // split by the update range
        to_add.push(ResourceProfile {
            start: max(update_range.end, profile.start),
            end: profile.end,
            profile_tasks: profile.profile_tasks.clone(),
            height: profile.height,
        })
    }
}

/// Determines whether the added mandatory part causes a new profile after the last overapping
/// profile.
pub(crate) fn new_part_after_last_profile<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    context: PropagationContext,
    current_index: usize,
    end_index: usize,
    update_range: &Range<i32>,
    profile: &ResourceProfile<Var, PVar, RVar>,
    to_add: &mut Vec<ResourceProfile<Var, PVar, RVar>>,
    task: &Rc<Task<Var, PVar, RVar>>,
) {
    if current_index == end_index && update_range.end > profile.end + 1 {
        // We are considering the last overlapping profile and there is
        // a part after the end of this profile
        // This means we need to add a new mandatory part after the last
        // element
        to_add.push(ResourceProfile {
            start: profile.end + 1,
            end: update_range.end - 1,
            profile_tasks: vec![Rc::clone(task)],
            height: context.lower_bound(&task.resource_usage),
        })
    }
}
