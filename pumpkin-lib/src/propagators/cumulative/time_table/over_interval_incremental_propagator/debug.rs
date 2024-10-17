/// Contains functions related to debugging
use std::ops::Range;

use crate::basic_types::HashSet;
use crate::engine::propagation::PropagationContext;
use crate::propagators::create_time_table_over_interval_from_scratch;
use crate::propagators::CumulativeParameters;
use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::ResourceProfile;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Determines whether the provided `time_table` is the same as the one creatd from scratch
/// using the following checks:
/// - The time-tables should contain the same number of profiles
/// - For each profile it should hold that
///      - The start times are the same
///      - The end times are the same
///      - The heights are the same
///      - The profile tasks should be the same; note that we do not check whether the order is the
///        same!
pub(crate) fn time_tables_are_the_same_interval<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    time_table: &OverIntervalTimeTableType<Var>,
    parameters: &CumulativeParameters<Var>,
) -> bool {
    let time_table_scratch = create_time_table_over_interval_from_scratch(*context, parameters)
        .expect("Expected no error");

    if time_table.is_empty() {
        return time_table_scratch.is_empty();
    }

    // First we merge all of the split profiles to ensure that it is the same as the
    // non-incremental time-table
    let mut time_table = time_table.clone();
    let time_table_len = time_table.len();
    merge_profiles(&mut time_table, 0, time_table_len - 1);

    // Then we compare whether the time-tables are the same with the following checks:
    // - The time-tables should contain the same number of profiles
    // - For each profile it should hold that
    //      - The starts are the same
    //      - The ends are the same
    //      - The heights are the same
    //      - The profile tasks of the profiles should be the same; note that we do not check
    //        whether the order is the same!
    time_table.len() == time_table_scratch.len()
        && time_table
            .iter()
            .zip(time_table_scratch)
            .all(|(actual, expected)| {
                let result = actual.height == expected.height
                    && actual.start == expected.start
                    && actual.end == expected.end
                    && actual.profile_tasks.len() == expected.profile_tasks.len()
                    && actual
                        .profile_tasks
                        .iter()
                        .all(|task| expected.profile_tasks.contains(task));
                result
            })
}

/// Merge all mergeable profiles (see [`are_mergeable`]) going from `[start_index, end_index]`
/// in the provided `time_table`.
fn merge_profiles<Var: IntegerVariable + 'static>(
    time_table: &mut OverIntervalTimeTableType<Var>,
    start_index: usize,
    end_index: usize,
) {
    pumpkin_assert_simple!(start_index <= end_index);
    if time_table.is_empty() {
        // In this case, there is nothing to merge so we do not do anything
        return;
    }

    let mut current_index = start_index;
    let mut end = end_index;

    let mut insertion_range: Option<Range<usize>> = None;
    let mut to_add: Option<Vec<ResourceProfile<Var>>> = None;

    // We go over all pairs of profiles, starting from start index until end index
    while current_index < end {
        let first = current_index;
        while current_index < end
            && are_mergeable(&time_table[current_index], &time_table[current_index + 1])
        {
            // We go over all pairs of profiles until we find a profile which cannot be merged
            // with the current profile
            current_index += 1;
        }

        if current_index > first {
            // We have found at least 2 profiles to merge (but perhaps more)
            let start_profile = &time_table[first];
            let end_profile = &time_table[current_index];

            // We create a new profile with the bounds which we have found
            let new_profile = ResourceProfile {
                start: start_profile.start,
                end: end_profile.end,
                profile_tasks: start_profile.profile_tasks.to_owned(),
                height: start_profile.height,
            };

            if let Some(to_add) = to_add.as_mut() {
                to_add.push(new_profile);
            } else {
                to_add = Some(vec![new_profile]);
            }

            insertion_range = Some(
                insertion_range
                    .map(|previous_range| previous_range.start..(current_index + 1))
                    .unwrap_or_else(|| first..(current_index + 1)),
            );
        } else if let Some(replacing_profiles) = to_add.take() {
            if let Some(replacing_range) = insertion_range.take() {
                end -= replacing_range.end - replacing_profiles.len();
                current_index = replacing_range.end - 1;
                let _ = time_table.splice(replacing_range, replacing_profiles);
            }
        }

        current_index += 1;
    }
    if let Some(replacing_profiles) = to_add.take() {
        if let Some(replacing_range) = insertion_range.take() {
            let _ = time_table.splice(replacing_range, replacing_profiles);
        }
    }
}

/// Determines whether 2 profiles are mergeable (i.e. they are next to each other, consist of
/// the same tasks and have the same height); this method is used in debugging to compare to a
/// time-table created from scratch.
///
/// It is assumed that the profile tasks of both profiles do not contain duplicates
fn are_mergeable<Var: IntegerVariable + 'static>(
    first_profile: &ResourceProfile<Var>,
    second_profile: &ResourceProfile<Var>,
) -> bool {
    pumpkin_assert_extreme!(
        first_profile
            .profile_tasks
            .iter()
            .collect::<HashSet<_>>()
            .len()
            == first_profile.profile_tasks.len(),
        "The first provided profile had duplicate profile tasks"
    );
    pumpkin_assert_extreme!(
        second_profile
            .profile_tasks
            .iter()
            .collect::<HashSet<_>>()
            .len()
            == second_profile.profile_tasks.len(),
        "The second provided profile had duplicate profile tasks"
    );
    // First we perform the simple checks, determining whether the two profiles are the same
    // height, whether they are next to one another and whether they contain the same number of
    // tasks
    let mergeable = first_profile.height == second_profile.height
        && first_profile.end == second_profile.start - 1
        && first_profile.profile_tasks.len() == second_profile.profile_tasks.len();
    if !mergeable {
        // The tasks have already been found to be not mergeable so we can avoid checking
        // equality of the profile tasks
        mergeable
    } else {
        // We check whether the profile tasks of both profiles are the same
        mergeable
            && first_profile
                .profile_tasks
                .iter()
                .all(|profile| second_profile.profile_tasks.contains(profile))
    }
}
