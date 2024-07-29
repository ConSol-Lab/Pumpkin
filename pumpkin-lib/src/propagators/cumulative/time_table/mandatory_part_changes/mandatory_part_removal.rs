use std::ops::Range;

use crate::propagators::Task;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

/// An enum which specifies whether a current mandatory part was reduced or whether a full
/// mandatory part is remove; see [`generate_removal_range`] for more information.
pub(crate) enum ReducedMandatoryConsumption {
    /// There was an existing mandatory part but it has been reduced by an update; the first
    /// [`Range`] is the removed mandatory part due to an update of the upper-bound of the start
    /// time and the second [`Range`] is the revmoed mandatory part due to an update of the
    /// lower-bound of the start time.
    ReducedMandatoryParts(Range<i32>, Range<i32>),
    /// There was an existing mandatory part before the update but there is not one now.
    FullyRemovedMandatoryPart(Range<i32>),
}

pub(crate) fn generate_removal_range<Var: IntegerVariable + 'static>(
    task: &Task<Var>,
    prev_lower_bound: i32,
    prev_upper_bound: i32,
    new_lower_bound: i32,
    new_upper_bound: i32,
) -> ReducedMandatoryConsumption {
    pumpkin_assert_moderate!(
        new_lower_bound <= prev_lower_bound && new_upper_bound >= prev_upper_bound
    );

    if new_upper_bound >= new_lower_bound + task.processing_time {
        // Entire mandatory part was removed
        ReducedMandatoryConsumption::FullyRemovedMandatoryPart(
            prev_upper_bound..prev_lower_bound + task.processing_time,
        )
    } else {
        ReducedMandatoryConsumption::ReducedMandatoryParts(
            prev_upper_bound..new_upper_bound,
            new_lower_bound + task.processing_time..prev_lower_bound + task.processing_time,
        )
    }
}

impl ReducedMandatoryConsumption {
    /// There are two cases:
    /// - In the case of [`ReducedMandatoryConsumption::ReducedMandatoryParts`] - This function will
    ///   return first the range which has been added due to an update of the lower-bound and then
    ///   the range which has been added due to an update of the uppper-bound.
    /// - In the case of [`ReducedMandatoryConsumption::FullyRemovedMandatoryPart`] - This function
    ///   will return the range consisting of the removed mandatory part.
    ///
    /// This function is used by [`TimeTableOverIntervalIncremental::propagate`] since it needs to
    /// traverse the second added mandatory part first due to the way it processes indices.
    pub(crate) fn get_reverse_update_ranges(&self) -> Vec<Range<i32>> {
        match self {
            ReducedMandatoryConsumption::ReducedMandatoryParts(
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
            ReducedMandatoryConsumption::FullyRemovedMandatoryPart(added_mandatory_part) => {
                vec![added_mandatory_part.clone()]
            }
        }
    }
}

impl Iterator for ReducedMandatoryConsumption {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ReducedMandatoryConsumption::ReducedMandatoryParts(
                first_added_part,
                second_added_part,
            ) => first_added_part.next().or_else(|| second_added_part.next()),
            ReducedMandatoryConsumption::FullyRemovedMandatoryPart(fully_new_added_part) => {
                fully_new_added_part.next()
            }
        }
    }
}
