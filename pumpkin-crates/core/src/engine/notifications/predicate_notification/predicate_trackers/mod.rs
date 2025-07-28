use crate::basic_types::PredicateId;
use crate::containers::StorageKey;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

mod disequality_tracker;
mod equality_tracker;
mod lower_bound_tracker;
mod upper_bound_tracker;
pub(crate) use disequality_tracker::DisequalityTracker;
pub(crate) use equality_tracker::EqualityTracker;
pub(crate) use lower_bound_tracker::LowerBoundTracker;
pub(crate) use upper_bound_tracker::UpperBoundTracker;

use super::PredicateIdAssignments;
use super::PredicateValue;

/// A generic structure for keeping track of the polarity of [`Predicate`]s.
///
/// This structure is useful since there is a lot of overlap in the methods for the different
/// trackers.
#[derive(Debug, Clone)]
pub(crate) struct PredicateTracker {
    /// The [`DomainId`] which the tracker is tracking the polarity for.
    domain_id: DomainId,
    /// `smaller[i]` is the index of the element with the largest value such that it is smaller
    /// than `values[i]`
    smaller: Vec<i64>,
    /// `greater[i]` is the index of the element with the smallest value such that it is larger
    /// than `values[i]`
    greater: Vec<i64>,
    /// A [`TrailedInteger`] which points to the largest lowest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x >= 6]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 1.
    min_assigned: TrailedInteger,
    /// A [`TrailedInteger`] which points to the smallest largest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x <= 8]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 3.
    max_assigned: TrailedInteger,
    /// The values which are currently being tracked by this [`PredicateTracker`].
    ///
    /// Note that there is no specific order in which these values are stored.
    values: Vec<i32>,
    /// The [`PredicateId`] corresponding to the predicate for each value in
    /// [`PredicateTracker::values`].
    ids: Vec<PredicateId>,
}

impl PredicateTracker {
    pub(super) fn new() -> Self {
        Self {
            domain_id: DomainId::new(0),
            // We do not want to create the trailed integers until necessary
            min_assigned: TrailedInteger::create_from_index(0),
            max_assigned: TrailedInteger::create_from_index(0),
            smaller: Vec::default(),
            greater: Vec::default(),
            values: Vec::default(),
            ids: Vec::default(),
        }
    }
}

/// A trait for any structure which has a [`PredicateTracker`].
///
/// For an example of such a structure, see [`LowerBoundTracker`].
pub(crate) trait HasTracker {
    /// Returns a reference to the [`PredicateTracker`].
    fn get_tracker(&self) -> &PredicateTracker;
    /// Returns a mutable reference to the [`PredicateTracker`].
    fn get_tracker_mut(&mut self) -> &mut PredicateTracker;
}

/// A trait which specifies how to provide and retrieve information from a structure containing a
/// [`PredicateTracker`].
pub(crate) trait DomainTrackerInformation {
    /// Initialise the tracker.
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
        trailed_values: &mut TrailedValues,
    );

    /// Returns a reference to the stored [`PredicateId`]s.
    fn get_ids(&self) -> &Vec<PredicateId>;
    /// Returns a mutable reference to the stored [`PredicateId`]s.
    fn get_ids_mut(&mut self) -> &mut Vec<PredicateId>;

    /// Returns a reference to the stored values.
    fn get_values(&self) -> &Vec<i32>;
    /// Returns a mutable reference to the stored values.
    fn get_values_mut(&mut self) -> &mut Vec<i32>;

    /// Returns a reference to `smaller` where `smaller[i]` is the index of the element with the
    /// largest value such that it is smaller than `values[i]`
    fn get_smaller(&self) -> &Vec<i64>;
    /// Returns a mutable reference to `smaller` where `smaller[i]` is the index of the element with
    /// the largest value such that it is smaller than `values[i]`
    fn get_smaller_mut(&mut self) -> &mut Vec<i64>;

    /// Returns a reference to `greater` where `greater[i]` is the index of the element with the
    /// smallest value such that it is larger than `values[i]`
    fn get_greater(&self) -> &Vec<i64>;
    /// Returns a mutable reference to `greater` where `greater[i]` is the index of the element with
    /// the smallest value such that it is larger than `values[i]`
    fn get_greater_mut(&mut self) -> &mut Vec<i64>;

    /// Returns true if no [`Predicate`]s are currently being tracked.
    fn is_empty(&self) -> bool;
}

impl<Watcher: HasTracker> DomainTrackerInformation for Watcher {
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
        trailed_values: &mut TrailedValues,
    ) {
        if !self.get_values().is_empty() {
            // The structures has been initialised previously
            return;
        }

        self.get_tracker_mut().min_assigned = trailed_values.grow(0);
        self.get_tracker_mut().max_assigned = trailed_values.grow(1);

        // We set the tracking domain id
        self.get_tracker_mut().domain_id = domain_id;

        // Then we place some sentinels for simplicity's sake which are always true
        //
        // It is _probably_ okay to note use the `-1` and `+1`
        self.get_values_mut().push(initial_lower_bound - 1);
        self.get_values_mut().push(initial_upper_bound + 1);

        // These should never be queried so we provide a placeholder
        self.get_ids_mut().push(PredicateId { id: u32::MAX });
        self.get_ids_mut().push(PredicateId { id: u32::MAX });

        // Then we place the sentinels into the `smaller` structure
        //
        // For the first element (containing the lower-bound), there is no smaller element
        self.get_smaller_mut().push(i64::MAX);
        // For the second element (containing the upper-bound), the smaller element will currently
        // point to the lower-bound element
        self.get_smaller_mut().push(0);

        // Then we place the sentinels into the `greater` structure
        //
        // For the first element (containing the lower-bound), the greater element will currently
        // point to the upper-bound element
        self.get_greater_mut().push(1);
        // For the second element (containing the upper-bound), there is no greater element
        self.get_greater_mut().push(i64::MAX);
    }

    fn get_ids(&self) -> &Vec<PredicateId> {
        &self.get_tracker().ids
    }

    fn get_ids_mut(&mut self) -> &mut Vec<PredicateId> {
        &mut self.get_tracker_mut().ids
    }

    fn get_values(&self) -> &Vec<i32> {
        &self.get_tracker().values
    }

    fn get_values_mut(&mut self) -> &mut Vec<i32> {
        &mut self.get_tracker_mut().values
    }

    fn get_smaller(&self) -> &Vec<i64> {
        &self.get_tracker().smaller
    }

    fn get_smaller_mut(&mut self) -> &mut Vec<i64> {
        &mut self.get_tracker_mut().smaller
    }

    fn get_greater(&self) -> &Vec<i64> {
        &self.get_tracker().greater
    }

    fn get_greater_mut(&mut self) -> &mut Vec<i64> {
        &mut self.get_tracker_mut().greater
    }

    fn is_empty(&self) -> bool {
        self.get_tracker().values.is_empty()
    }
}

/// A trait which defines the common behaviours for structures which track [`Predicate`]s for a
/// specific [`DomainId`].
pub(crate) trait DomainTracker: DomainTrackerInformation {
    #[allow(unused, reason = "Could be useful for debugging")]
    /// Returns a predicate corresponding to the provided value.
    ///
    /// For example, for a lower-bound [`DomainTracker`] which tracks a variable `x`, the call
    /// `LowerBoundTracker::get_predicate_for_value(5)` will return the [`Predicate`] `[x >= 5].`
    fn get_predicate_for_value(&self, value: i32) -> Predicate;

    /// Allows the [`DomainTracker`] to indicate that a tracked [`Predicate`] has been satisfied.
    fn predicate_has_been_satisfied(
        &self,
        index: usize,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let predicate_id = self.get_ids()[index];
        if predicate_id.id == u32::MAX {
            // If it is a placeholder then we ignore it
            return;
        }
        predicate_id_assignments.store_predicate(predicate_id, PredicateValue::AssignedTrue);
    }

    /// Allows the [`DomainTracker`] to indicate that a tracked [`Predicate`] has been falsified.
    fn predicate_has_been_falsified(
        &self,
        index: usize,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let predicate_id = self.get_ids()[index];
        if predicate_id.id == u32::MAX {
            return;
        }
        predicate_id_assignments.store_predicate(predicate_id, PredicateValue::AssignedFalse);
    }

    /// Tracks a [`Predicate`] with a provided `value` and [`PredicateId`].
    ///
    /// Returns true if it was not already tracked and false otherwise.
    fn track(&mut self, value: i32, predicate_id: PredicateId) -> bool {
        pumpkin_assert_simple!(
            self.get_values().len() >= 2,
            "Initialise should have been called previously"
        );

        // We get a new index for this value to update the linked list
        let new_index = self.get_values().len() as i64;

        // Then we track the information for updating `smaller`
        let mut index_largest_value_smaller_than = i64::MAX;
        let mut largest_value_smaller_than = i32::MIN;

        // And we track the information for updating `greater`
        let mut index_smallest_value_larger_than = i64::MAX;
        let mut smallest_value_larger_than = i32::MAX;

        // Then we go over each value to determine where to place the element in the linked list.
        for index in 0..self.get_values().len() {
            let index_value = self.get_values()[index];
            if index_value == value {
                // This value is already being tracked
                return false;
            }

            // We first check whether we have found a value which is smaller than the provided
            // `value` but larger than the one we already found
            if index_value < value && index_value > largest_value_smaller_than {
                largest_value_smaller_than = index_value;
                index_largest_value_smaller_than = index as i64;
            }

            // We then check whether we have found a value which is larger than the provided
            // `value` but smaller than the one we already found
            if index_value > value && index_value < smallest_value_larger_than {
                smallest_value_larger_than = index_value;
                index_smallest_value_larger_than = index as i64;
            }

            // Now we check whether any _existing_ elements require their `smaller` to be updated
            // due to the addition of this new element

            // This is the case if the provided `value` is smaller than the one stored by this
            // element And it either had no smaller element previously Or the current
            // `value` is larger than the stored value
            if value < self.get_values()[index]
                && (self.get_smaller()[index] == i64::MAX
                    || value > self.get_values()[self.get_smaller()[index] as usize])
            {
                self.get_smaller_mut()[index] = new_index;
            }

            // Now we check whether any _existing_ elements require their `smaller` to be updated
            // due to the addition of this new element

            // This is the case if the provided `value` is larger than the one stored by this
            // element And it either had no greater element previously Or the current
            // `value` is smaller than the stored value
            if value > self.get_values()[index]
                && (self.get_greater()[index] == i64::MAX
                    || value < self.get_values()[self.get_greater()[index] as usize])
            {
                self.get_greater_mut()[index] = new_index;
            }
        }

        // Then we update the other structures
        self.get_values_mut().push(value);
        self.get_ids_mut().push(predicate_id);
        self.get_smaller_mut()
            .push(index_largest_value_smaller_than);
        self.get_greater_mut()
            .push(index_smallest_value_larger_than);

        pumpkin_assert_simple!(
            self.get_smaller().len() == self.get_greater().len()
                && self.get_greater().len() == self.get_values().len()
                && self.get_values().len() == self.get_ids().len()
        );

        true
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [Predicate]).
    ///
    /// This should update the appropriate structures of the [`DomainTracker`] and add the
    /// satisfied (falsified) [`Predicate`]s to `satisfied_predicates` (`falsified_predicates`).
    fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    );
}
