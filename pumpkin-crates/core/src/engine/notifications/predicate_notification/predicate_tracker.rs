use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;

use enumset::EnumSet;
use fnv::FnvBuildHasher;
use indexmap::Equivalent;
use indexmap::IndexSet;
use indexmap::set::MutableValues;

use super::PredicateIdAssignments;
use super::PredicateValue;
use crate::basic_types::PredicateId;
use crate::containers::StorageKey;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

/// A generic structure for keeping track of the polarity of [`Predicate`]s.
///
/// This structure keeps track of all different [`PredicateType`]s.
#[derive(Debug, Clone)]
pub(crate) struct PredicateTracker {
    /// The [`DomainId`] which the tracker is tracking the polarity for.
    domain_id: DomainId,
    /// `smaller[i]` is the index of the element with the largest value such that it is smaller
    /// than `values[i]`
    smaller: Vec<u32>,
    /// `greater[i]` is the index of the element with the smallest value such that it is larger
    /// than `values[i]`
    greater: Vec<u32>,
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
    /// A [`TrailedInteger`] which points to the largest lowest value which is assigned but not
    /// equal to the value.
    ///
    /// For example, if we have the values `x in [1, 6, 7, 9]` and we know that `[x >= 6]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 1.
    min_assigned_strict: TrailedInteger,
    /// A [`TrailedInteger`] which points to the smallest largest value which is assigned but not
    /// equal to the value.
    ///
    /// For example, if we have the values `x in [1, 5, 8, 9]` and we know that `[x <= 8]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 3.
    max_assigned_strict: TrailedInteger,
    /// The values which are currently being tracked by this [`PredicateTracker`].
    ///
    /// We want quick membership queries but a hash-based set cannot be used since we require the
    /// indices to remain consistent (since they are, for example, stored in [`Self::smaller`] and
    /// [`Self::greater`]). Thus, we use an [`IndexSet`] which allows us to perform efficient
    /// membership queries while also allowing us to index into the set.
    ///
    /// Note that these values are not sorted in any way.
    values: IndexSet<TrackedValue, FnvBuildHasher>,
    /// The [`PredicateId`]s corresponding to the predicates for each value in
    /// [`PredicateTracker::values`].
    ids: Vec<Vec<PredicateId>>,
}

// A value tracked by the [`PredicateTracker`], keeps track of the values in the lowest 4 bits.
#[derive(Clone, Copy, Debug)]
struct TrackedValue {
    value: i32,
    flags: EnumSet<PredicateType>,
}

impl TrackedValue {
    /// Creates a new [`TrackedValue`].
    fn new(value: i32) -> Self {
        Self {
            value,
            flags: EnumSet::new(),
        }
    }

    /// Store the provided [`PredicateType`] in the [`TrackedValue`].
    fn track_predicate_type(&mut self, predicate_type: PredicateType) {
        self.flags |= predicate_type;
    }

    /// Returns whether the provided [`PredicateType`] is tracked by this [`TrackedValue`].
    fn does_track_predicate_type(&self, predicate_type: PredicateType) -> bool {
        self.flags.contains(predicate_type)
    }

    /// Return the [`PredicateType`]s which are stored in this [`TrackedValue`].
    ///
    /// These are always returned in a pre-defined order, not the order in which they were
    /// inserted.
    fn get_predicate_types(&self) -> impl Iterator<Item = PredicateType> {
        self.flags.iter()
    }

    /// Returns the value which is stored in this [`TrackedValue`].
    fn get_value(&self) -> i32 {
        self.value
    }
}

impl PartialEq for TrackedValue {
    fn eq(&self, other: &Self) -> bool {
        self.get_value().eq(&other.get_value())
    }
}

impl Eq for TrackedValue {}

impl PartialOrd for TrackedValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TrackedValue {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_value().cmp(&other.get_value())
    }
}

impl Hash for TrackedValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_value().hash(state)
    }
}

impl Equivalent<TrackedValue> for i32 {
    fn equivalent(&self, key: &TrackedValue) -> bool {
        *self == key.get_value()
    }
}

impl PredicateTracker {
    pub(super) fn new() -> Self {
        Self {
            domain_id: DomainId::new(0),
            // We do not want to create the trailed integers until necessary
            min_assigned: TrailedInteger::create_from_index(0),
            max_assigned: TrailedInteger::create_from_index(0),
            min_assigned_strict: TrailedInteger::create_from_index(0),
            max_assigned_strict: TrailedInteger::create_from_index(0),
            smaller: Vec::default(),
            greater: Vec::default(),
            values: Default::default(),
            ids: Vec::default(),
        }
    }

    pub(super) fn initialise(
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

        self.min_assigned = trailed_values.grow(0);
        self.max_assigned = trailed_values.grow(1);
        self.min_assigned_strict = trailed_values.grow(0);
        self.max_assigned_strict = trailed_values.grow(1);

        // We set the tracking domain id
        self.domain_id = domain_id;

        // Then we place some sentinels for simplicity's sake which are always true
        //
        // It is _probably_ okay to note use the `-1` and `+1`
        let _ = self.insert_value(initial_lower_bound - 1);
        let _ = self.insert_value(initial_upper_bound + 1);

        // These should never be queried so we provide a placeholder
        self.ids.push(vec![]);
        self.ids.push(vec![]);

        // Then we place the sentinels into the `smaller` structure
        //
        // For the first element (containing the lower-bound), there is no smaller element
        self.smaller.push(u32::MAX);
        // For the second element (containing the upper-bound), the smaller element will currently
        // point to the lower-bound element
        self.smaller.push(0);

        // Then we place the sentinels into the `greater` structure
        //
        // For the first element (containing the lower-bound), the greater element will currently
        // point to the upper-bound element
        self.greater.push(1);
        // For the second element (containing the upper-bound), there is no greater element
        self.greater.push(u32::MAX);
    }

    /// Whether there are no values being tracked.
    ///
    /// Note that if [`Self::initialise`] has been called but no predicate is tracked, then this
    /// method will return true.
    fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Inserts the value into the internal structures.
    fn insert_value(&mut self, value: i32) -> usize {
        let index = self.values.len();
        let result = self.values.insert(TrackedValue::new(value));
        assert!(result);

        index
    }

    /// Returns the value at the provided index.
    ///
    /// If the index is out of bounds, this method will panic.
    fn get_value_at_index(&self, index: usize) -> TrackedValue {
        *self
            .values
            .get_index(index)
            .expect("Expected provided index to exist")
    }

    /// Returns all of the values currently present.
    fn get_all_values(&self) -> impl Iterator<Item = TrackedValue> {
        self.values.iter().copied()
    }

    /// Returns the index of the provided value if it is present.
    fn get_index_of_value(&self, value: i32) -> Option<usize> {
        self.values.get_index_of(&value)
    }

    /// Allows the [`PredicateTracker`] to indicate that a tracked [`Predicate`] has been satisfied.
    fn predicate_has_been_satisfied(
        &self,
        index: usize,
        predicate_index: usize,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let predicate_id = self.ids[index][predicate_index];
        if predicate_id.id == u32::MAX {
            // If it is a placeholder then we ignore it
            return;
        }
        predicate_id_assignments.store_predicate(predicate_id, PredicateValue::AssignedTrue);
    }

    /// Allows the [`PredicateTracker`] to indicate that a tracked [`Predicate`] has been falsified.
    fn predicate_has_been_falsified(
        &self,
        index: usize,
        predicate_index: usize,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let predicate_id = self.ids[index][predicate_index];
        if predicate_id.id == u32::MAX {
            return;
        }
        predicate_id_assignments.store_predicate(predicate_id, PredicateValue::AssignedFalse);
    }

    /// Tracks a [`Predicate`] with a provided `value` and [`PredicateId`].
    ///
    /// Returns true if it was not already tracked and false otherwise.
    pub(super) fn track(&mut self, predicate: Predicate, predicate_id: PredicateId) -> bool {
        pumpkin_assert_simple!(
            !self.is_empty(),
            "Initialise should have been called previously"
        );

        let value = predicate.get_right_hand_side();

        // We check whether it is already tracked
        if let Some((index, tracked_value)) = self.values.get_full_mut2(&value) {
            // Then we check whether this particular predicate type has already been tracked
            if !tracked_value.does_track_predicate_type(predicate.get_predicate_type()) {
                let current_mask = predicate.get_predicate_type() as u8;

                // We keep the predicate ids in the same order as they are returned by the
                // TrackedValue
                if let Some(pos) = tracked_value
                    .get_predicate_types()
                    .position(|predicate_type| predicate_type as u8 > current_mask)
                {
                    self.ids[index].insert(pos, predicate_id);
                } else {
                    self.ids[index].push(predicate_id);
                }

                tracked_value.track_predicate_type(predicate.get_predicate_type());

                return true;
            }
            return false;
        }

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
            pumpkin_assert_simple!(index_value.get_value() != value,);

            // As soon as we have found a value smaller than the to track value, we can stop
            if index_value.get_value() < value {
                index_largest_value_smaller_than = index as u32;

                index_smallest_value_larger_than = self.greater[index];
                break;
            }

            index = self.smaller[index] as usize;
        }

        pumpkin_assert_eq_simple!(
            self.get_value_at_index(index_largest_value_smaller_than as usize),
            self.get_all_values()
                .filter(|&stored_value| stored_value.get_value() < value)
                .max()
                .unwrap(),
        );
        pumpkin_assert_eq_simple!(
            self.get_value_at_index(index_smallest_value_larger_than as usize),
            self.get_all_values()
                .filter(|&stored_value| stored_value.get_value() > value)
                .min()
                .unwrap()
        );

        let new_index = self.insert_value(value);
        self.values
            .get_index_mut2(new_index)
            .unwrap()
            .track_predicate_type(predicate.get_predicate_type());

        self.greater[index_largest_value_smaller_than as usize] = new_index as u32;
        self.smaller[index_smallest_value_larger_than as usize] = new_index as u32;

        // Then we update the other structures
        self.smaller.push(index_largest_value_smaller_than);
        self.greater.push(index_smallest_value_larger_than);
        self.ids.push(vec![predicate_id]);

        true
    }

    pub(super) fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        // If there are <= 2 values being tracked, then the structure has not been asked to track
        // any values.
        if self.values.len() <= 2 {
            return;
        }

        let value = predicate.get_right_hand_side();

        if predicate.is_lower_bound_predicate() {
            let mut greater_strict =
                self.greater[trailed_values.read(self.min_assigned_strict) as usize];
            while greater_strict != u32::MAX
                && value > self.values[greater_strict as usize].get_value()
            {
                for (predicate_index, predicate_type) in self.values[greater_strict as usize]
                    .get_predicate_types()
                    .enumerate()
                {
                    match predicate_type {
                        PredicateType::UpperBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                greater_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                greater_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::LowerBound => {
                            self.predicate_has_been_satisfied(
                                greater_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                    }
                }
                trailed_values.assign(self.min_assigned_strict, greater_strict as i64);
                trailed_values.assign(self.min_assigned, greater_strict as i64);

                greater_strict = self.greater[greater_strict as usize];
            }

            let mut greater = self.greater[trailed_values.read(self.min_assigned) as usize];
            while greater != u32::MAX && value >= self.values[greater as usize].get_value() {
                if let Some(predicate_index) = self.values[greater as usize]
                    .get_predicate_types()
                    .position(|predicate_type| predicate_type == PredicateType::LowerBound)
                {
                    self.predicate_has_been_satisfied(
                        greater_strict as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.min_assigned, greater as i64);
                greater = self.greater[greater as usize];
            }
        } else if predicate.is_upper_bound_predicate() {
            let mut smaller_strict =
                self.smaller[trailed_values.read(self.max_assigned_strict) as usize];
            while smaller_strict != u32::MAX
                && value < self.values[smaller_strict as usize].get_value()
            {
                for (predicate_index, predicate_type) in self.values[smaller_strict as usize]
                    .get_predicate_types()
                    .enumerate()
                {
                    match predicate_type {
                        PredicateType::LowerBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                smaller_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                smaller_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::UpperBound => {
                            self.predicate_has_been_satisfied(
                                smaller_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                    }
                }
                trailed_values.assign(self.max_assigned_strict, smaller_strict as i64);
                trailed_values.assign(self.max_assigned, smaller_strict as i64);

                smaller_strict = self.smaller[smaller_strict as usize];
            }

            let mut smaller = self.smaller[trailed_values.read(self.max_assigned) as usize];
            while smaller != u32::MAX && value <= self.values[smaller as usize].get_value() {
                if let Some(predicate_index) = self.values[smaller as usize]
                    .get_predicate_types()
                    .position(|predicate_type| predicate_type == PredicateType::UpperBound)
                {
                    self.predicate_has_been_satisfied(
                        smaller as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.max_assigned, smaller as i64);
                smaller = self.smaller[smaller as usize];
            }
        } else if predicate.is_not_equal_predicate() {
            // If the right-hand side of the disequality predicate is smaller than the value
            // pointed to by `min_assigned` then no updates can take place
            if value
                <= self.values[trailed_values.read(self.min_assigned_strict) as usize].get_value()
            {
                return;
            }

            // If the right-hand side of the disequality predicate is larger than the value
            // pointed to by `max_assigned` then no updates can take place
            if value
                >= self.values[trailed_values.read(self.max_assigned_strict) as usize].get_value()
            {
                return;
            }

            if let Some(index) = self.get_index_of_value(value) {
                for (predicate_index, predicate_type) in
                    self.values[index].get_predicate_types().enumerate()
                {
                    match predicate_type {
                        PredicateType::NotEqual => self.predicate_has_been_satisfied(
                            index,
                            predicate_index,
                            predicate_id_assignments,
                        ),
                        PredicateType::Equal => self.predicate_has_been_falsified(
                            index,
                            predicate_index,
                            predicate_id_assignments,
                        ),
                        _ => {}
                    }
                }
            }
        } else if predicate.is_equality_predicate() {
            // First update the lower-bound if necessary
            let mut greater_strict =
                self.greater[trailed_values.read(self.min_assigned_strict) as usize];
            while greater_strict != u32::MAX
                && value > self.values[greater_strict as usize].get_value()
            {
                for (predicate_index, predicate_type) in self.values[greater_strict as usize]
                    .get_predicate_types()
                    .enumerate()
                {
                    match predicate_type {
                        PredicateType::UpperBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                greater_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                greater_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::LowerBound => {
                            self.predicate_has_been_satisfied(
                                greater_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                    }
                }
                trailed_values.assign(self.min_assigned_strict, greater_strict as i64);
                trailed_values.assign(self.min_assigned, greater_strict as i64);

                greater_strict = self.greater[greater_strict as usize];
            }

            let mut greater = self.greater[trailed_values.read(self.min_assigned) as usize];
            while greater != u32::MAX && value >= self.values[greater as usize].get_value() {
                if let Some(predicate_index) = self.values[greater as usize]
                    .get_predicate_types()
                    .position(|predicate_type| predicate_type == PredicateType::LowerBound)
                {
                    self.predicate_has_been_satisfied(
                        greater_strict as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.min_assigned, greater as i64);
                greater = self.greater[greater as usize];
            }

            // Then the upper-bound if necessary
            let mut smaller_strict =
                self.smaller[trailed_values.read(self.max_assigned_strict) as usize];
            while smaller_strict != u32::MAX
                && value < self.values[smaller_strict as usize].get_value()
            {
                for (predicate_index, predicate_type) in self.values[smaller_strict as usize]
                    .get_predicate_types()
                    .enumerate()
                {
                    match predicate_type {
                        PredicateType::LowerBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                smaller_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                smaller_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::UpperBound => {
                            self.predicate_has_been_satisfied(
                                smaller_strict as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                    }
                }
                trailed_values.assign(self.max_assigned_strict, smaller_strict as i64);
                trailed_values.assign(self.max_assigned, smaller_strict as i64);

                smaller_strict = self.smaller[smaller_strict as usize];
            }

            let mut smaller = self.smaller[trailed_values.read(self.max_assigned) as usize];
            while smaller != u32::MAX && value <= self.values[smaller as usize].get_value() {
                if let Some(predicate_index) = self.values[smaller as usize]
                    .get_predicate_types()
                    .position(|predicate_type| predicate_type == PredicateType::UpperBound)
                {
                    self.predicate_has_been_satisfied(
                        smaller as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.max_assigned, smaller as i64);
                smaller = self.smaller[smaller as usize];
            }

            let greater = self.greater[trailed_values.read(self.min_assigned_strict) as usize];
            if greater == self.smaller[trailed_values.read(self.max_assigned_strict) as usize]
                && self.values[greater as usize].get_value() == value
            {
                for (predicate_index, predicate_type) in self.values[greater as usize]
                    .get_predicate_types()
                    .enumerate()
                {
                    match predicate_type {
                        PredicateType::NotEqual => {
                            self.predicate_has_been_falsified(
                                greater as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::Equal => {
                            self.predicate_has_been_satisfied(
                                greater as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}

#[cfg(test)]
#[allow(deprecated, reason = "Will be replaced by the state API")]
mod tests {
    use crate::engine::Assignments;
    use crate::engine::TrailedValues;
    use crate::engine::notifications::PredicateIdAssignments;
    use crate::engine::notifications::predicate_notification::predicate_tracker::PredicateTracker;
    use crate::engine::notifications::predicate_notification::predicate_tracker::TrackedValue;
    use crate::predicate;
    use crate::predicates::PredicateIdGenerator;
    use crate::predicates::PredicateType;

    #[test]
    fn test_update_lower_bound() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        tracker.on_update(
            predicate!(x >= 5),
            &mut trailed_values,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x <= 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x == 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x != 5))));

        tracker.on_update(
            predicate!(x >= 6),
            &mut trailed_values,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &mut id_generator
        ));
    }

    #[test]
    fn test_update_upper_bound() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        tracker.on_update(
            predicate!(x <= 5),
            &mut trailed_values,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x >= 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x == 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x != 5))));

        tracker.on_update(
            predicate!(x <= 4),
            &mut trailed_values,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &mut id_generator
        ));
    }

    #[test]
    fn test_update_not_equals() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        tracker.on_update(
            predicate!(x != 5),
            &mut trailed_values,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x >= 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x <= 5))));
    }

    #[test]
    fn test_update_equals() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 6);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 6);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        tracker.on_update(
            predicate!(x == 6),
            &mut trailed_values,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x == 6)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x != 6)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &mut id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &mut id_generator
        ));
    }

    #[test]
    fn pack_negative_value() {
        let x = -255;

        let mut value = TrackedValue::new(x);

        assert_eq!(value.get_value(), x);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            Vec::<PredicateType>::new()
        );

        value.track_predicate_type(PredicateType::Equal);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate_type(PredicateType::LowerBound);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::LowerBound, PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate_type(PredicateType::NotEqual);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::NotEqual,
                PredicateType::Equal
            ]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate_type(PredicateType::UpperBound);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::UpperBound,
                PredicateType::NotEqual,
                PredicateType::Equal
            ]
        );
        assert_eq!(value.get_value(), x);
    }

    #[test]
    fn pack_positive_value() {
        let x = 255;

        let mut value = TrackedValue::new(x);

        assert_eq!(value.get_value(), x);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            Vec::<PredicateType>::new()
        );

        value.track_predicate_type(PredicateType::Equal);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate_type(PredicateType::LowerBound);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::LowerBound, PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate_type(PredicateType::NotEqual);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::NotEqual,
                PredicateType::Equal
            ]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate_type(PredicateType::UpperBound);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::UpperBound,
                PredicateType::NotEqual,
                PredicateType::Equal
            ]
        );
        assert_eq!(value.get_value(), x);
    }
}
