use fnv::FnvBuildHasher;
use indexmap::IndexSet;

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
    values: IndexSet<i32, FnvBuildHasher>,
    /// The [`PredicateId`]s corresponding to the predicates for each value in
    /// [`PredicateTracker::values`].
    ids: Vec<Vec<PredicateId>>,
    /// The [`PredicateType`]s for each predicate attached to a value.
    predicate_types: Vec<Vec<PredicateType>>,
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
            predicate_types: Vec::default(),
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

        // These should never be queried so we provide a placeholder
        self.predicate_types.push(vec![]);
        self.predicate_types.push(vec![]);

        // Then we place the sentinels into the `smaller` structure
        //
        // For the first element (containing the lower-bound), there is no smaller element
        self.smaller.push(i64::MAX);
        // For the second element (containing the upper-bound), the smaller element will currently
        // point to the lower-bound element
        self.smaller.push(0);

        // Then we place the sentinels into the `greater` structure
        //
        // For the first element (containing the lower-bound), the greater element will currently
        // point to the upper-bound element
        self.greater.push(1);
        // For the second element (containing the upper-bound), there is no greater element
        self.greater.push(i64::MAX);
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
        let result = self.values.insert(value);
        assert!(result);

        index
    }

    /// Returns the value at the provided index.
    ///
    /// If the index is out of bounds, this method will panic.
    fn get_value_at_index(&self, index: usize) -> i32 {
        *self
            .values
            .get_index(index)
            .expect("Expected provided index to exist")
    }

    /// Returns all of the values currently present.
    fn get_all_values(&self) -> impl Iterator<Item = i32> {
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
        if let Some(index) = self.get_index_of_value(value) {
            // Then we check whether this particular predicate type has already been tracked
            if !self.predicate_types[index].contains(&predicate.get_predicate_type()) {
                self.predicate_types[index].push(predicate.get_predicate_type());
                self.ids[index].push(predicate_id);
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
            pumpkin_assert_simple!(index_value != value);

            // As soon as we have found a value smaller than the to track value, we can stop
            if index_value < value {
                index_largest_value_smaller_than = index as i64;

                index_smallest_value_larger_than = self.greater[index];
                break;
            }

            index = self.smaller[index] as usize;
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

        self.greater[index_largest_value_smaller_than as usize] = new_index as i64;
        self.smaller[index_smallest_value_larger_than as usize] = new_index as i64;

        // Then we update the other structures
        self.smaller.push(index_largest_value_smaller_than);
        self.greater.push(index_smallest_value_larger_than);
        self.ids.push(vec![predicate_id]);
        self.predicate_types
            .push(vec![predicate.get_predicate_type()]);

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
            let mut greater = self.greater[trailed_values.read(self.min_assigned) as usize];
            while greater != i64::MAX && value >= self.values[greater as usize] {
                if let Some(predicate_index) = self.predicate_types[greater as usize]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::LowerBound)
                {
                    self.predicate_has_been_satisfied(
                        greater as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.min_assigned, greater);
                greater = self.greater[greater as usize];
            }

            let mut larger = self.greater[trailed_values.read(self.min_assigned_strict) as usize];
            while larger != i64::MAX && value > self.values[larger as usize] {
                for (predicate_index, predicate_type) in
                    self.predicate_types[larger as usize].iter().enumerate()
                {
                    match predicate_type {
                        PredicateType::UpperBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                larger as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                larger as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        _ => {}
                    }
                }
                trailed_values.assign(self.min_assigned_strict, larger);
                larger = self.greater[larger as usize];
            }
        } else if predicate.is_upper_bound_predicate() {
            let mut smaller = self.smaller[trailed_values.read(self.max_assigned) as usize];
            while smaller != i64::MAX && value <= self.values[smaller as usize] {
                if let Some(predicate_index) = self.predicate_types[smaller as usize]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::UpperBound)
                {
                    self.predicate_has_been_satisfied(
                        smaller as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.max_assigned, smaller);
                smaller = self.smaller[smaller as usize];
            }

            let mut smaller = self.smaller[trailed_values.read(self.max_assigned_strict) as usize];
            while smaller != i64::MAX && value < self.values[smaller as usize] {
                for (predicate_index, predicate_type) in
                    self.predicate_types[smaller as usize].iter().enumerate()
                {
                    match predicate_type {
                        PredicateType::LowerBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                smaller as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                smaller as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        _ => {}
                    }
                }
                trailed_values.assign(self.max_assigned_strict, smaller);
                smaller = self.smaller[smaller as usize];
            }
        } else if predicate.is_not_equal_predicate() {
            // If the right-hand side of the disequality predicate is smaller than the value
            // pointed to by `min_assigned` then no updates can take place
            if value <= self.values[trailed_values.read(self.min_assigned_strict) as usize] {
                return;
            }

            // If the right-hand side of the disequality predicate is larger than the value
            // pointed to by `max_assigned` then no updates can take place
            if value >= self.values[trailed_values.read(self.max_assigned_strict) as usize] {
                return;
            }

            if let Some(index) = self.get_index_of_value(value) {
                if let Some(predicate_index) = self.predicate_types[index]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::Equal)
                {
                    self.predicate_has_been_falsified(
                        index,
                        predicate_index,
                        predicate_id_assignments,
                    )
                }

                if let Some(predicate_index) = self.predicate_types[index]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::NotEqual)
                {
                    self.predicate_has_been_satisfied(
                        index,
                        predicate_index,
                        predicate_id_assignments,
                    )
                }
            }
        } else if predicate.is_equality_predicate() {
            let mut greater = self.greater[trailed_values.read(self.min_assigned) as usize];
            while greater != i64::MAX && value >= self.values[greater as usize] {
                if let Some(predicate_index) = self.predicate_types[greater as usize]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::LowerBound)
                {
                    self.predicate_has_been_satisfied(
                        greater as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.min_assigned, greater);
                greater = self.greater[greater as usize];
            }

            let mut larger = self.greater[trailed_values.read(self.min_assigned_strict) as usize];
            while larger != i64::MAX && value > self.values[larger as usize] {
                for (predicate_index, predicate_type) in
                    self.predicate_types[larger as usize].iter().enumerate()
                {
                    match predicate_type {
                        PredicateType::UpperBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                larger as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                larger as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        _ => {}
                    }
                }
                trailed_values.assign(self.min_assigned_strict, larger);
                larger = self.greater[larger as usize];
            }

            let mut smaller = self.smaller[trailed_values.read(self.max_assigned) as usize];
            while smaller != i64::MAX && value <= self.values[smaller as usize] {
                if let Some(predicate_index) = self.predicate_types[smaller as usize]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::UpperBound)
                {
                    self.predicate_has_been_satisfied(
                        smaller as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
                trailed_values.assign(self.max_assigned, smaller);
                smaller = self.smaller[smaller as usize];
            }

            let mut smaller = self.smaller[trailed_values.read(self.max_assigned_strict) as usize];
            while smaller != i64::MAX && value < self.values[smaller as usize] {
                for (predicate_index, predicate_type) in
                    self.predicate_types[smaller as usize].iter().enumerate()
                {
                    match predicate_type {
                        PredicateType::LowerBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                smaller as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual => {
                            self.predicate_has_been_satisfied(
                                smaller as usize,
                                predicate_index,
                                predicate_id_assignments,
                            );
                        }
                        _ => {}
                    }
                }
                trailed_values.assign(self.max_assigned_strict, smaller);
                smaller = self.smaller[smaller as usize];
            }

            let greater = self.greater[trailed_values.read(self.min_assigned_strict) as usize];
            if greater == self.smaller[trailed_values.read(self.max_assigned_strict) as usize]
                && self.values[greater as usize] == value
            {
                if let Some(predicate_index) = self.predicate_types[greater as usize]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::NotEqual)
                {
                    self.predicate_has_been_falsified(
                        greater as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }

                if let Some(predicate_index) = self.predicate_types[greater as usize]
                    .iter()
                    .position(|predicate_type| *predicate_type == PredicateType::Equal)
                {
                    self.predicate_has_been_satisfied(
                        greater as usize,
                        predicate_index,
                        predicate_id_assignments,
                    );
                }
            }
        }
    }
}
