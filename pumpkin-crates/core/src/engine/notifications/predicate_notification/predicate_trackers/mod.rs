use crate::basic_types::PredicateId;
use crate::containers::StorageKey;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

mod disequality_tracker;
mod equality_tracker;
mod lower_bound_tracker;
mod upper_bound_tracker;
pub(crate) use disequality_tracker::DisequalityTracker;
pub(crate) use equality_tracker::EqualityTracker;
use indexmap::IndexSet;
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
    values: IndexSet<i32>,
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
            values: Default::default(),
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

    /// Returns true if all of the tracked [`Predicate`]s are assigned.
    fn is_fixed(&self, trailed_values: &TrailedValues) -> bool;

    fn insert_value(&mut self, value: i32) -> usize;
    fn get_value_at_index(&self, index: usize) -> i32;
    fn get_all_values(&self) -> impl Iterator<Item = i32>;
    fn get_index_of_value(&self, value: i32) -> Option<usize>;
}

impl<Watcher: HasTracker> DomainTrackerInformation for Watcher {
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
        trailed_values: &mut TrailedValues,
    ) {
        if !self.is_empty() {
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
        let _ = self.insert_value(initial_lower_bound - 1);
        let _ = self.insert_value(initial_upper_bound + 1);

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

    fn is_fixed(&self, trailed_values: &TrailedValues) -> bool {
        if self.is_empty() {
            // If it is empty, then it is trivially fixed
            return true;
        }

        // The idea is to use the `min_assigned` and `max_assigned` fields to infer whether any
        // updates can take place.
        //
        // Let's first look at an example for a variable `x`, imagine we have the following values
        // [0, 10, 5, 2, 3, 1] where `x \in [0, 10]` (i.e. the first two values are fixed);
        // we know that `min_assigned = 0` and `max_assigned = 1`; now we update the domain
        // of `x` to be `[4, 4]`. We know that `min_assigned = 4` (pointing to value 3), and
        // `max_assigned = 2` (pointing to value 5).
        //
        // If we now look at the successor of `min_assigned` (with index 2 and value 5) and the
        // predecessor of `max_assigned` (with index 5 and value 3), then we can see that
        // these are already assigned (according to `min_assigned` and `max_assigned`
        // respectively).
        //
        // Thus, we simply need to check whether either:
        // - The successor of `min_assigned` is equal to `max_assigned`
        // - The predecessor of `max_assigned` is equal to `min_assigned`
        let min_assigned_index = trailed_values.read(self.get_tracker().min_assigned) as usize;
        let min_unassigned_index = self.get_tracker().greater[min_assigned_index] as usize;
        pumpkin_assert_simple!(
            self.get_tracker().values[min_assigned_index]
                < self.get_tracker().values[min_unassigned_index]
        );

        let max_assigned_index = trailed_values.read(self.get_tracker().max_assigned) as usize;
        let max_unassigned_index = self.get_tracker().smaller[max_assigned_index] as usize;
        pumpkin_assert_simple!(
            self.get_tracker().values[max_assigned_index]
                > self.get_tracker().values[max_unassigned_index]
        );

        self.get_tracker().values[min_unassigned_index]
            >= self.get_tracker().values[max_assigned_index]
            || self.get_tracker().values[max_unassigned_index]
                <= self.get_tracker().values[min_assigned_index]
    }

    fn insert_value(&mut self, value: i32) -> usize {
        let index = self.get_tracker().values.len();
        let result = self.get_tracker_mut().values.insert(value);
        assert!(result);

        index
    }

    fn get_value_at_index(&self, index: usize) -> i32 {
        *self
            .get_tracker()
            .values
            .get_index(index)
            .expect("Expected provided index to exist")
    }

    fn get_all_values(&self) -> impl Iterator<Item = i32> {
        self.get_tracker().values.iter().copied()
    }

    fn get_index_of_value(&self, value: i32) -> Option<usize> {
        self.get_tracker().values.get_index_of(&value)
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
            !self.is_empty(),
            "Initialise should have been called previously"
        );

        // Then we track the information for updating `smaller`; recall that we place a sentinel
        // node with the smallest possible value at index 0
        let index_largest_value_smaller_than;

        // And we track the information for updating `greater`; recall that we place a sentinel
        // node with the largest possible value at index 1
        let index_smallest_value_larger_than;

        // Then we go over each value to determine where to place the element in the linked list.
        //
        // Note that the element at the 1st index has the largest value
        let mut index = 1;
        loop {
            let index_value = self.get_value_at_index(index);
            if index_value == value {
                // This value is already being tracked
                return false;
            }

            // As soon as we have found a value smaller than the to track value, we can stop
            if index_value < value {
                index_largest_value_smaller_than = index as i64;

                index_smallest_value_larger_than = self.get_greater()[index];
                break;
            }

            index = self.get_smaller()[index] as usize;
        }

        pumpkin_assert_eq_simple!(
            self.get_value_at_index(index_largest_value_smaller_than as usize),
            self.get_all_values()
                .filter(|&stored_value| stored_value < value)
                .max()
                .unwrap(),
        );
        pumpkin_assert_eq_simple!(
            self.get_value_at_index(index_smallest_value_larger_than as usize),
            self.get_all_values()
                .filter(|&stored_value| stored_value > value)
                .min()
                .unwrap()
        );

        let new_index = self.insert_value(value);

        self.get_greater_mut()[index_largest_value_smaller_than as usize] = new_index as i64;
        self.get_smaller_mut()[index_smallest_value_larger_than as usize] = new_index as i64;

        // Then we update the other structures
        self.get_ids_mut().push(predicate_id);
        self.get_smaller_mut()
            .push(index_largest_value_smaller_than);
        self.get_greater_mut()
            .push(index_smallest_value_larger_than);

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

#[cfg(test)]
mod tests {
    use crate::basic_types::PredicateIdGenerator;
    use crate::engine::notifications::predicate_notification::predicate_trackers::DomainTracker;
    use crate::engine::notifications::predicate_notification::predicate_trackers::DomainTrackerInformation;
    use crate::engine::notifications::predicate_notification::predicate_trackers::HasTracker;
    use crate::engine::notifications::predicate_notification::predicate_trackers::LowerBoundTracker;
    use crate::engine::notifications::predicate_notification::predicate_trackers::PredicateTracker;
    use crate::engine::notifications::NotificationEngine;
    use crate::engine::notifications::PredicateIdAssignments;
    use crate::engine::Assignments;
    use crate::engine::TrailedValues;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::variables::DomainId;

    #[test]
    fn is_fixed() {
        let mut tracker = LowerBoundTracker::new();
        let mut trailed_values = TrailedValues::default();
        let mut assignments = Assignments::default();
        let mut predicate_id_generator = PredicateIdGenerator::default();
        let mut notification_engine = NotificationEngine::default();

        let initial_lower_bound = 0;
        let initial_upper_bound = 10;
        let domain = assignments.grow(initial_lower_bound, initial_upper_bound);
        notification_engine.grow();

        tracker.initialise(
            domain,
            initial_lower_bound,
            initial_upper_bound,
            &mut trailed_values,
        );
        assert!(tracker.is_fixed(&trailed_values));
        let _ = tracker.track(
            5,
            predicate_id_generator.get_id(tracker.get_predicate_for_value(5)),
        );
        assert!(!tracker.is_fixed(&trailed_values));

        let _ = tracker.track(
            2,
            predicate_id_generator.get_id(tracker.get_predicate_for_value(5)),
        );
        assert!(!tracker.is_fixed(&trailed_values));

        let _ = tracker.track(
            3,
            predicate_id_generator.get_id(tracker.get_predicate_for_value(5)),
        );
        assert!(!tracker.is_fixed(&trailed_values));

        let _ = tracker.track(
            1,
            predicate_id_generator.get_id(tracker.get_predicate_for_value(5)),
        );
        assert!(!tracker.is_fixed(&trailed_values));

        let _ = assignments.post_predicate(predicate!(domain >= 4), None, &mut notification_engine);
        let _ = assignments.post_predicate(predicate!(domain <= 6), None, &mut notification_engine);

        tracker.on_update(
            predicate!(domain >= 4),
            &mut trailed_values,
            &mut PredicateIdAssignments::default(),
            1,
        );
        tracker.on_update(
            predicate!(domain <= 6),
            &mut trailed_values,
            &mut PredicateIdAssignments::default(),
            2,
        );
        assert!(!tracker.is_fixed(&trailed_values));

        let _ = assignments.post_predicate(predicate!(domain <= 4), None, &mut notification_engine);
        tracker.on_update(
            predicate!(domain <= 4),
            &mut trailed_values,
            &mut PredicateIdAssignments::default(),
            3,
        );
        assert!(tracker.is_fixed(&trailed_values));
    }
}
