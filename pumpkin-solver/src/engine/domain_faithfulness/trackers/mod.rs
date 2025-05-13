use log::info;

use crate::basic_types::PredicateId;
use crate::engine::Assignments;
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

/// A generic structure for keeping track of the polarity of [`Predicate`]s.
///
/// This structure is useful since there is a lot of overlap in the methods for the different
/// trackers.
#[derive(Debug, Clone)]
pub(crate) struct FaithfulnessTracker {
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
    /// then [`FaithfulnessTracker::min_assigned`] will point to index 1.
    min_assigned: TrailedInteger,
    /// A [`TrailedInteger`] which points to the smallest largest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x <= 8]` holds,
    /// then [`FaithfulnessTracker::min_assigned`] will point to index 3.
    max_unassigned: TrailedInteger,
    /// The values which are currently being tracked by this [`FaithfulnessTracker`].
    ///
    /// Note that there is no specific order in which these values are stored.
    values: Vec<i32>,
    /// The [`PredicateId`] corresponding to the predicate for each value in
    /// [`FaithfulnessTracker::values`].
    ids: Vec<PredicateId>,
}

impl FaithfulnessTracker {
    pub(super) fn new(trailed_values: &mut TrailedValues) -> Self {
        let min_unassigned = trailed_values.grow(0);
        let max_unassigned = trailed_values.grow(1);
        Self {
            domain_id: DomainId { id: 0 },
            min_assigned: min_unassigned,
            max_unassigned,
            smaller: Vec::default(),
            greater: Vec::default(),
            values: Vec::default(),
            ids: Vec::default(),
        }
    }
}

/// A trait for any structure which has a [`FaithfulnessTracker`].
///
/// For an example of such a structure, see [`LowerBoundTracker`].
pub(crate) trait HasTracker {
    /// Returns a reference to the [`FaithfulnessTracker`].
    fn get_tracker(&self) -> &FaithfulnessTracker;
    /// Returns a mutable reference to the [`FaithfulnessTracker`].
    fn get_tracker_mut(&mut self) -> &mut FaithfulnessTracker;
}

/// A trait which specifies how to provide and retrieve information from a structure containing a
/// [`FaithfulnessTracker`].
pub(crate) trait DomainTrackerInformation {
    /// Initialise the tracker.
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
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

    /// A [`TrailedInteger`] which points to the largest lowest value which is assigned.
    fn get_min_unassigned(&self) -> TrailedInteger;

    /// A [`TrailedInteger`] which points to the smallest largest value which is assigned.
    fn get_max_unassigned(&self) -> TrailedInteger;

    /// Returns true if no [`Predicate`]s are currently being tracked.
    fn is_empty(&self) -> bool;

    /// Returns the [`DomainId`] which is currently being tracked.
    fn get_domain_id(&self) -> DomainId;
}

impl<Watcher: HasTracker> DomainTrackerInformation for Watcher {
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
    ) {
        if !self.get_values().is_empty() {
            // The structures has been initialised previously
            return;
        }

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

    fn get_min_unassigned(&self) -> TrailedInteger {
        self.get_tracker().min_assigned
    }

    fn get_max_unassigned(&self) -> TrailedInteger {
        self.get_tracker().max_unassigned
    }

    fn is_empty(&self) -> bool {
        self.get_tracker().values.is_empty()
    }

    fn get_domain_id(&self) -> DomainId {
        self.get_tracker().domain_id
    }
}

/// A trait which defines the common behaviours for structures which track [`Predicate`]s for a
/// specific [`DomainId`].
pub(crate) trait DomainTracker: DomainTrackerInformation {
    /// Returns a predicate corresponding to the provided value.
    ///
    /// For example, for a lower-bound [`DomainTracker`] which tracks a variable `x`, the call
    /// `LowerBoundTracker::get_predicate_for_value(5)` will return the [`Predicate`] `[x >= 5].`
    fn get_predicate_for_value(&self, value: i32) -> Predicate;

    /// Allows the [`DomainTracker`] to indicate that a tracked [`PredicateId`] has been satisfied.
    fn predicate_id_has_been_satisfied(
        &self,
        predicate_id: PredicateId,
        satisfied_predicates: &mut Vec<PredicateId>,
    ) {
        if predicate_id.id == u32::MAX {
            // If it is a placeholder then we ignore it
            return;
        }
        info!("Satisfied: {predicate_id:?}");
        satisfied_predicates.push(predicate_id)
    }

    /// Allows the [`DomainTracker`] to indicate that a tracked [`Predicate`] has been satisfied.
    fn predicate_has_been_satisfied(
        &self,
        index: usize,
        satisfied_predicates: &mut Vec<PredicateId>,
    ) {
        let predicate_id = self.get_ids()[index];
        if predicate_id.id == u32::MAX {
            // If it is a placeholder then we ignore it
            return;
        }
        info!(
            "Satisfied: {:?}",
            self.get_predicate_for_value(self.get_values()[index])
        );
        satisfied_predicates.push(predicate_id)
    }

    /// Allows the [`DomainTracker`] to indicate that a tracked [`Predicate`] has been falsified.
    ///
    /// NOTE: Currently, no propagator is interested whether a predicate has been falsified, this
    /// will be changed in the future.
    fn predicate_has_been_falsified(
        &self,
        _index: usize,
        _falsified_predicates: &mut Vec<PredicateId>,
    ) {
        // TODO: At the moment, no propagator is interested
        // let predicate_id = self.get_ids()[index];
        // if predicate_id.id == u32::MAX {
        //    return;
        //}
        // info!(
        //    "Falsified: {:?}",
        //    self.get_predicate_for_value(self.get_values()[index])
        //);
        // falsified_predicates.push(self.get_ids()[index])
    }

    /// Tracks a [`Predicate`] with a provided `value` and [`PredicateId`].
    fn track(
        &mut self,
        value: i32,
        predicate_id: PredicateId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
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
            pumpkin_assert_simple!(
                index_value != value,
                "Found {value} already exists for {index_value} with bounds {}, {}",
                assignments.get_initial_lower_bound(self.get_domain_id()),
                assignments.get_initial_upper_bound(self.get_domain_id())
            );

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

        // We might also need to update the pointers to the indices of the elements which are
        // assigned.
        //
        // Note that we only look at the bounds when updating these indices; this has implications
        // when considering holes in the domain.
        //
        // We first check whethher the current predicate is implied by the current bounds
        if assignments.is_implied_by_bounds(self.get_predicate_for_value(value)) {
            // We now know that the current predicate is assigned
            //
            // If `value` is larger than the value currently pointed to by `min_unassigned` then we
            // need to update it
            if value > self.get_values()[trailed_values.read(self.get_min_unassigned()) as usize] {
                trailed_values.assign(self.get_min_unassigned(), new_index);
            }

            // Similarly, if `value` is smaller than the value currently pointed to by
            // `max_unassigned` then we need to update it
            if value < self.get_values()[trailed_values.read(self.get_max_unassigned()) as usize] {
                trailed_values.assign(self.get_max_unassigned(), new_index);
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
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [Predicate]).
    ///
    /// This should update the appropriate structures of the [`DomainTracker`] and add the
    /// satisfied (falsified) [`Predicate`]s to `satisfied_predicates` (`falsified_predicates`).
    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        predicate_id: Option<PredicateId>,
    );
}
