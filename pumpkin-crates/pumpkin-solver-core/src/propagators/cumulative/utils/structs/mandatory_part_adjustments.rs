use std::cmp::Ordering;
use std::ops::Range;

use super::UpdatedTaskInfo;

/// Represents adjustments to a mandatory part due to bound changes.
///
/// It contains both the additions to the mandatory part (stored in
/// [`MandatoryPartAdjustments::added_parts`]) and the removals from the mandatory part
/// [`MandatoryPartAdjustments::removed_parts`].
pub(crate) struct MandatoryPartAdjustments {
    /// The additions to the mandatory part
    added_parts: Vec<Range<i32>>,
    /// The removals from the mandatory part
    removed_parts: Vec<Range<i32>>,
}

impl MandatoryPartAdjustments {
    fn new(added_parts: Vec<Range<i32>>, removed_parts: Vec<Range<i32>>) -> Self {
        Self {
            added_parts,
            removed_parts,
        }
    }

    /// Returns an iterator over the removed ranges of the mandatory part; only returns non-empty
    /// intervals.
    pub(crate) fn get_removed_parts(&self) -> impl Iterator<Item = Range<i32>> + '_ {
        self.removed_parts
            .iter()
            .filter(|range| !range.is_empty())
            .cloned()
    }

    /// Returns an iterator over the added ranges of the mandatory part; only returns non-mepty
    /// intervals.
    pub(crate) fn get_added_parts(&self) -> impl Iterator<Item = Range<i32>> + '_ {
        self.added_parts
            .iter()
            .filter(|range| !range.is_empty())
            .cloned()
    }

    /// Creates an empty [`MandatoryPartAdjustments`] (i.e. with no added parts and with no removed
    /// parts).
    fn empty() -> Self {
        Self {
            added_parts: vec![],
            removed_parts: vec![],
        }
    }

    /// Creates a [`MandatoryPartAdjustments`] containing a single added part.
    fn from_added_part(added_part: Range<i32>) -> Self {
        Self {
            added_parts: vec![added_part],
            removed_parts: vec![],
        }
    }

    /// Creates a [`MandatoryPartAdjustments`] containing a single removed part.
    fn from_removed_part(removed_part: Range<i32>) -> Self {
        Self {
            added_parts: vec![],
            removed_parts: vec![removed_part],
        }
    }

    /// Creates a [`MandatoryPartAdjustments`] containing a single added part and a single removed
    /// part.
    fn from_added_and_removed_part(added_part: Range<i32>, removed_part: Range<i32>) -> Self {
        Self {
            added_parts: vec![added_part],
            removed_parts: vec![removed_part],
        }
    }
}

impl<Var> UpdatedTaskInfo<Var> {
    /// Returns the adjustments which need to be made to the time-table in the form of a
    /// [`MandatoryPartAdjustments`].
    pub(crate) fn get_mandatory_part_adjustments(&self) -> MandatoryPartAdjustments {
        // We get the previous mandatory part
        let previous_mandatory_part =
            self.old_upper_bound..self.old_lower_bound + self.task.processing_time;
        // We also get the new mandatory part
        let new_mandatory_part =
            self.new_upper_bound..self.new_lower_bound + self.task.processing_time;

        if previous_mandatory_part.is_empty() && new_mandatory_part.is_empty() {
            // If both are empty then no adjustments should be made
            return MandatoryPartAdjustments::empty();
        }

        if previous_mandatory_part.is_empty() {
            // There is no previous mandatory part, simply add the new one
            return MandatoryPartAdjustments::from_added_part(new_mandatory_part);
        }
        if new_mandatory_part.is_empty() {
            // There is no new mandatory part, simply remove the old mandatory part
            return MandatoryPartAdjustments::from_removed_part(previous_mandatory_part);
        }

        if previous_mandatory_part.start >= new_mandatory_part.end
            || new_mandatory_part.start >= previous_mandatory_part.end
        {
            // There is no overlap between the parts, we remove the old mandatory part and add the
            // new
            return MandatoryPartAdjustments::from_added_and_removed_part(
                new_mandatory_part,
                previous_mandatory_part,
            );
        }

        let mut removed_parts = vec![];
        let mut added_parts = vec![];

        // We first check the adjustments which need to be made based on the new end time of the
        // mandatory part
        match new_mandatory_part.end.cmp(&previous_mandatory_part.end) {
            Ordering::Less => {
                // The new mandatory parts ends before the previous mandatory part
                // This means that we need to remove from the time-table
                removed_parts.push(new_mandatory_part.end..previous_mandatory_part.end)
            }
            Ordering::Equal => {
                // Do nothing in case they are equal
            }
            Ordering::Greater => {
                // The new mandatory parts ends after the previous mandatory part
                // This means that we need to add to the time-table
                added_parts.push(previous_mandatory_part.end..new_mandatory_part.end);
            }
        }

        // Then we check the adjustments which need to be made based on the new start time of the
        // mandatory part
        match new_mandatory_part.start.cmp(&previous_mandatory_part.start) {
            Ordering::Less => {
                // The new mandatory part starts before the previous mandatory part
                // This means that we need to add to the time-table
                added_parts.push(new_mandatory_part.start..previous_mandatory_part.start);
            }
            Ordering::Equal => {

                // Do nothing in case they are equal
            }
            Ordering::Greater => {
                // The new mandatory part starts later than the previous mandatory part
                // This means that we need to remove from the time-table
                removed_parts.push(previous_mandatory_part.start..new_mandatory_part.start);
            }
        }
        MandatoryPartAdjustments::new(added_parts, removed_parts)
    }
}
