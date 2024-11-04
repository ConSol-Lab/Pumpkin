/// Contains functions related to debugging
use std::ops::Range;

use crate::basic_types::HashSet;
use crate::engine::propagation::PropagationContext;
use crate::propagators::create_time_table_over_interval_from_scratch;
use crate::propagators::CumulativeParameters;
use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::ResourceProfileInterface;
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
pub(crate) fn time_tables_are_the_same_interval<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
    const SYNCHRONISE: bool,
>(
    context: PropagationContext,
    time_table: &OverIntervalTimeTableType<ResourceProfileType>,
    parameters: &CumulativeParameters<Var>,
) -> bool {
    let time_table_scratch: OverIntervalTimeTableType<ResourceProfileType> =
        create_time_table_over_interval_from_scratch(context, parameters)
            .expect("Expected no error");

    if time_table.is_empty() {
        return time_table_scratch.is_empty();
    }

    // First we merge all of the split profiles to ensure that it is the same as the
    // non-incremental time-table
    let mut time_table = time_table.clone();
    let time_table_len = time_table.len();

    if !SYNCHRONISE {
        merge_profiles(&mut time_table, 0, time_table_len - 1);
    }

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
                let result = actual.get_height() == expected.get_height()
                    && actual.get_start() == expected.get_start()
                    && actual.get_end() == expected.get_end()
                    && actual.get_profile_tasks().len() == expected.get_profile_tasks().len()
                    && {
                        if SYNCHRONISE {
                            actual.get_profile_tasks() == expected.get_profile_tasks()
                        } else {
                            actual
                                .get_profile_tasks()
                                .iter()
                                .all(|task| expected.get_profile_tasks().contains(task))
                        }
                    };
                result
            })
}

/// Merge all mergeable profiles (see [`are_mergeable`]) going from `[start_index, end_index]`
/// in the provided `time_table`.
pub(crate) fn merge_profiles<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    time_table: &mut OverIntervalTimeTableType<ResourceProfileType>,
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

    // To avoid needless splicing, we keep track of the range at which insertions will take place
    let mut insertion_range: Option<Range<usize>> = None;
    // And the profiles which need to be added
    let mut to_add: Option<Vec<ResourceProfileType>> = None;

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
            let new_profile = ResourceProfileType::create_profile(
                start_profile.get_start(),
                end_profile.get_end(),
                start_profile.get_profile_tasks().to_owned(),
                start_profile.get_height(),
            );

            // And we add the new profiles to the profiles to add
            if let Some(to_add) = to_add.as_mut() {
                to_add.push(new_profile);
            } else {
                to_add = Some(vec![new_profile]);
            }

            // If the insertion range already exists then we update it to cover the newly added
            // profiles as well
            // Otherwise we initialise it to cover the range of profiles which can be merged
            insertion_range = Some(
                insertion_range
                    .map(|previous_range| previous_range.start..(current_index + 1))
                    .unwrap_or_else(|| first..(current_index + 1)),
            );
        } else if let Some(replacing_profiles) = to_add.take() {
            // We need to insert the merged profiles and remove the old profiles from the
            // time-table
            if let Some(replacing_range) = insertion_range.take() {
                // The end is decreased by the number of removed profiles (taking into account that
                // `|replacing_profiles|` profiles were added)
                end -= (replacing_range.end - replacing_range.start) - replacing_profiles.len();
                // The current index should point to after the range of newly added profiles which
                // means that it is updated to be the start of `replacing_range` + the number of
                // newly added profiles (note that we know that the profile directly after the
                // spliced profiles can also be skipped now, hence we do not add a -1)
                current_index = replacing_range.start + replacing_profiles.len();
                // Then we replace the profile in the range with the merged profiles
                let _ = time_table.splice(replacing_range, replacing_profiles);
            }
        }

        current_index += 1;
    }
    // It could be that there were merged profiles at the end which we have not merged yet, we
    // add them now (note that we do not need to adjust any of the other elements now)
    if let Some(replacing_profiles) = to_add.take() {
        if let Some(replacing_range) = insertion_range.take() {
            let _ = time_table.splice(replacing_range, replacing_profiles);
        }
    }

    // A sanity check which checks whether all of the adjacent profiles are not mergeable (which
    // would indicate that we missed merging some profiles)
    pumpkin_assert_extreme!(
        (0..time_table.len() - 1)
            .all(|index| { !are_mergeable(&time_table[index], &time_table[index + 1]) }),
        "The profiles were incorrectly merged"
    )
}

/// Determines whether 2 profiles are mergeable (i.e. they are next to each other, consist of
/// the same tasks and have the same height); this method is used in debugging to compare to a
/// time-table created from scratch.
///
/// It is assumed that the profile tasks of both profiles do not contain duplicates
fn are_mergeable<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    first_profile: &ResourceProfileType,
    second_profile: &ResourceProfileType,
) -> bool {
    pumpkin_assert_extreme!(
        first_profile
            .get_profile_tasks()
            .iter()
            .collect::<HashSet<_>>()
            .len()
            == first_profile.get_profile_tasks().len(),
        "The first provided profile had duplicate profile tasks"
    );
    pumpkin_assert_extreme!(
        second_profile
            .get_profile_tasks()
            .iter()
            .collect::<HashSet<_>>()
            .len()
            == second_profile.get_profile_tasks().len(),
        "The second provided profile had duplicate profile tasks"
    );
    // First we perform the simple checks, determining whether the two profiles are the same
    // height, whether they are next to one another and whether they contain the same number of
    // tasks
    let mergeable = first_profile.get_height() == second_profile.get_height()
        && first_profile.get_end() == second_profile.get_start() - 1
        && first_profile.get_profile_tasks().len() == second_profile.get_profile_tasks().len();
    if !mergeable {
        // The tasks have already been found to be not mergeable so we can avoid checking
        // equality of the profile tasks
        mergeable
    } else {
        // We check whether the profile tasks of both profiles are the same
        mergeable
            && first_profile
                .get_profile_tasks()
                .iter()
                .all(|profile| second_profile.get_profile_tasks().contains(profile))
    }
}
