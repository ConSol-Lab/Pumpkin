use std::ops::Range;

use crate::propagators::Task;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

/// An enum which specifies whether a current mandatory part was extended or whether a fully new
/// mandatory part is introduced; see [`generate_update_range`] for more information.
pub(crate) enum AddedMandatoryConsumption {
    /// There was an existing mandatory part but it has been extended by an update; the first
    /// [`Range`] is the added mandatory part due to an update of the upper-bound of the start time
    /// and the second [`Range`] is the added mandatory part due to an update of the lower-bound of
    /// the start time.
    AdditionalMandatoryParts(Range<i32>, Range<i32>),
    /// There was no existing mandatory part before the update but there is one now.
    FullyNewMandatoryPart(Range<i32>),
}

impl AddedMandatoryConsumption {
    /// There are two cases:
    /// - In the case of [`AddedMandatoryConsumption::AdditionalMandatoryParts`] - This function
    ///   will return first the range which has been added due to an update of the lower-bound and
    ///   then the range which has been added due to an update of the uppper-bound.
    /// - In the case of [`AddedMandatoryConsumption::FullyNewMandatoryPart`] - This function will
    ///   return the range consisting of the added mandatory part.
    ///
    /// This function is used by [`TimeTableOverIntervalIncremental::propagate`] since it needs to
    /// traverse the second added mandatory part first due to the way it processes indices.
    pub(crate) fn get_reverse_update_ranges(&self) -> Vec<Range<i32>> {
        match self {
            AddedMandatoryConsumption::AdditionalMandatoryParts(
                first_added_part,
                second_added_part,
            ) => {
                // First return the second part and then return the first part, filtering out the
                // empty mandatory parts
                [second_added_part.clone(), first_added_part.clone()]
                    .into_iter()
                    .filter(|added_part| !added_part.is_empty())
                    .collect()
            }
            AddedMandatoryConsumption::FullyNewMandatoryPart(added_mandatory_part) => {
                vec![added_mandatory_part.clone()]
            }
        }
    }
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
