use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Domains;
use pumpkin_core::variables::IntegerVariable;

use crate::cumulative::CumulativeParameters;
use crate::cumulative::time_table::OverIntervalTimeTableType;
use crate::cumulative::time_table::TimeTableMerger;
use crate::cumulative::time_table::create_time_table_over_interval_from_scratch;

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
    const SYNCHRONISE: bool,
>(
    mut context: Domains,
    inference_code: &InferenceCode,
    time_table: &OverIntervalTimeTableType<Var>,
    parameters: &CumulativeParameters<Var>,
) -> bool {
    let time_table_scratch = create_time_table_over_interval_from_scratch(
        context.reborrow(),
        parameters,
        inference_code,
    )
    .expect("Expected no error");

    if time_table.is_empty() {
        return time_table_scratch.is_empty();
    }

    // First we merge all of the split profiles to ensure that it is the same as the
    // non-incremental time-table
    let mut time_table = time_table.clone();
    let time_table_len = time_table.len();

    if !SYNCHRONISE {
        TimeTableMerger::merge_range(&mut time_table, 0, time_table_len - 1);
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
                actual.height == expected.height
                    && actual.start == expected.start
                    && actual.end == expected.end
                    && actual.profile_tasks.len() == expected.profile_tasks.len()
                    && {
                        if SYNCHRONISE {
                            actual.profile_tasks == expected.profile_tasks
                        } else {
                            actual
                                .profile_tasks
                                .iter()
                                .all(|task| expected.profile_tasks.contains(task))
                        }
                    }
            })
}
