//! Contains the checks which are done when a new mandatory part is added in the propagate method to
//! determine which profiles should be added and how existing profiles should be adjusted.
use std::cmp::max;
use std::cmp::min;
use std::ops::Range;
use std::rc::Rc;

use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::ResourceProfileInterface;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Determines whether the added mandatory part causes a new profile before the first overapping
/// profile.
pub(crate) fn new_profile_before_first_profile<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    current_index: usize,
    start_index: usize,
    update_range: &Range<i32>,
    profile: &ResourceProfileType,
    to_add: &mut Vec<ResourceProfileType>,
    task: &Rc<Task<Var>>,
) {
    if current_index == start_index && update_range.start < profile.get_start() {
        // We are considering the first overlapping profile and there is
        // a part before the start of this profile
        // This means we need to add a new mandatory part before the
        // first element
        to_add.push(ResourceProfileType::create_profile(
            update_range.start,
            profile.get_start() - 1, /* Note that this profile needs to end before the start
                                      * of the current profile, hence the -1 */
            vec![Rc::clone(task)],
            task.resource_usage,
        ));
    }
}

/// Determines whether a new profile should be inserted between the current profile (pointed to
/// by `current_index`) and the previous profile.
pub(crate) fn new_profile_between_profiles<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    time_table: &OverIntervalTimeTableType<ResourceProfileType>,
    current_index: usize,
    start_index: usize,
    update_range: &Range<i32>,
    profile: &ResourceProfileType,
    to_add: &mut Vec<ResourceProfileType>,
    task: &Rc<Task<Var>>,
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
        if previous_profile.get_end() < profile.get_start() - 1
            && update_range.start <= previous_profile.get_end() + 1
            && update_range.end > profile.get_start() - 1
        {
            // There is empty space between the current profile and the
            // previous one, we should insert a new profile
            to_add.push(ResourceProfileType::create_profile(
                previous_profile.get_end() + 1,
                profile.get_start() - 1,
                vec![Rc::clone(task)],
                task.resource_usage,
            ))
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
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    update_range: &Range<i32>,
    profile: &ResourceProfileType,
    to_add: &mut Vec<ResourceProfileType>,
) {
    if update_range.start > profile.get_start() {
        // We are splitting the current profile into one or more parts
        // The update range starts after the profile starts;
        // This if-statement takes care of creating a new (smaller)
        // profile which represents the previous profile up and until it
        // is split by the update range
        to_add.push(ResourceProfileType::create_profile(
            profile.get_start(),
            min(update_range.start - 1, profile.get_end()), /* It could be that the update
                                                             * range extends past the profile
                                                             * in which case we should create
                                                             * a profile until the end of the
                                                             * profile */
            profile.get_profile_tasks().clone(),
            profile.get_height(),
        ))
    }
}

/// Determines whether a new profile which contains the overlap between `profile` and the added
/// mandatory part should be added.
pub(crate) fn overlap_updated_profile<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    update_range: &Range<i32>,
    profile: &ResourceProfileType,
    to_add: &mut Vec<ResourceProfileType>,
    task: &Rc<Task<Var>>,
    capacity: i32,
) -> Result<(), ResourceProfileType> {
    // Now we create a new profile which consists of the part of the
    // profile covered by the update range
    // This means that we are adding the contribution of the updated
    // task to the profile and adjusting the bounds appropriately

    // Either the new profile starts at the start of the profile (in
    // case the update range starts before the profile start)
    // or the new profile starts at the start of the update range (since
    // we are only looking at the part where there is overlap between
    // the current profile and the update range)
    let new_profile_lower_bound = max(profile.get_start(), update_range.start);

    // Either the new profile ends at the end of the profile (in case
    // the update range ends after the profile end)
    // or the new profile ends at the end of the update range (since we
    // are only looking at the part where there is overlap between the
    // current profile and the update range)
    let new_profile_upper_bound = min(profile.get_end(), update_range.end - 1); // Note that the end of the update_range is exclusive (hence the -1)
    if new_profile_upper_bound >= new_profile_lower_bound {
        let mut new_profile_tasks = profile.get_profile_tasks().clone();
        new_profile_tasks.push(Rc::clone(task));

        let new_profile = ResourceProfileType::create_profile(
            new_profile_lower_bound,
            new_profile_upper_bound,
            new_profile_tasks.clone(),
            profile.get_height() + task.resource_usage,
        );

        // We thus create a new profile consisting of the combination of
        // the previous profile and the updated task under consideration
        to_add.push(new_profile);

        // A sanity check, there is a new profile to create consisting
        // of a combination of the previous profile and the updated task
        if profile.get_height() + task.resource_usage + task.resource_usage > capacity {
            // The addition of the new mandatory part to the profile
            // caused an overflow of the resource
            return Err(ResourceProfileType::create_profile(
                new_profile_lower_bound,
                new_profile_upper_bound,
                new_profile_tasks,
                profile.get_height() + task.resource_usage,
            ));
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
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    update_range: &Range<i32>,
    profile: &ResourceProfileType,
    to_add: &mut Vec<ResourceProfileType>,
) {
    if profile.get_end() >= update_range.end {
        // We are splitting the current profile into one or more parts
        // The update range ends before the end of the profile;
        // This if-statement takes care of creating a new (smaller)
        // profile which represents the previous profile after it is
        // split by the update range
        to_add.push(ResourceProfileType::create_profile(
            max(update_range.end, profile.get_start()),
            profile.get_end(),
            profile.get_profile_tasks().clone(),
            profile.get_height(),
        ))
    }
}

/// Determines whether the added mandatory part causes a new profile after the last overapping
/// profile.
pub(crate) fn new_part_after_last_profile<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    current_index: usize,
    end_index: usize,
    update_range: &Range<i32>,
    profile: &ResourceProfileType,
    to_add: &mut Vec<ResourceProfileType>,
    task: &Rc<Task<Var>>,
) {
    if current_index == end_index && update_range.end > profile.get_end() + 1 {
        // We are considering the last overlapping profile and there is
        // a part after the end of this profile
        // This means we need to add a new mandatory part after the last
        // element
        to_add.push(ResourceProfileType::create_profile(
            profile.get_end() + 1,
            update_range.end - 1,
            vec![Rc::clone(task)],
            task.resource_usage,
        ))
    }
}
