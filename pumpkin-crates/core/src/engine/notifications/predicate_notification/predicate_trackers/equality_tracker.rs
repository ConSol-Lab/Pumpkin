use super::DomainTracker;
use super::HasTracker;
use super::PredicateTracker;
use super::TrailedValues;
use crate::engine::notifications::predicate_notification::predicate_trackers::DomainTrackerInformation;
use crate::engine::notifications::predicate_notification::PredicateIdAssignments;
use crate::predicate;
use crate::predicates::Predicate;

/// A tracker for equality [`Predicate`]s
#[derive(Debug, Clone)]
pub(crate) struct EqualityTracker {
    watcher: PredicateTracker,
}

impl EqualityTracker {
    pub(crate) fn new() -> Self {
        Self {
            watcher: PredicateTracker::new(),
        }
    }
}

impl HasTracker for EqualityTracker {
    fn get_tracker(&self) -> &PredicateTracker {
        &self.watcher
    }

    fn get_tracker_mut(&mut self) -> &mut PredicateTracker {
        &mut self.watcher
    }
}

impl DomainTracker for EqualityTracker {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id == value)
    }

    fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let value = predicate.get_right_hand_side();
        if predicate.is_lower_bound_predicate() {
            // In the case of a lower-bound update, it can only be the case that predicates
            // become falsified; we need to find the predicates which are now falsified and
            // update `min_assigned`
            //
            // First we get the element which is greater than the one pointed to by
            // `min_assigned`. This can never be `i64::MAX` due to sentinels being placed.
            let mut greater =
                self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
            // Note that an equality predicate can only become falsified if the new lower-bound
            // is _strictly_ larger than the value
            while greater != i64::MAX && value > self.watcher.values[greater as usize] {
                // The update has caused the predicate to become falsified
                self.predicate_has_been_falsified(greater as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.min_assigned, greater);
                greater = self.watcher.greater[greater as usize];
            }
        } else if predicate.is_upper_bound_predicate() {
            // In the case of an upper-bound update, it can only be the case that predicates
            // become falsified; we need to find the predicates which are now falsified and
            // update `max_assigned`
            //
            // First we get the element which is greater than the one pointed to by
            // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
            let mut smaller =
                self.watcher.smaller[trailed_values.read(self.watcher.max_assigned) as usize];
            // Note that an equality predicate can only become falsified if the new upper-bound
            // is _strictly_ smaller than the value
            while smaller != i64::MAX && value < self.watcher.values[smaller as usize] {
                // The update has caused the predicate to become falsified
                self.predicate_has_been_falsified(smaller as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.max_assigned, smaller);
                smaller = self.watcher.smaller[smaller as usize];
            }
        } else if predicate.is_not_equal_predicate() {
            // If the right-hand side of the disequality predicate is smaller than the value
            // pointed to by `min_assigned` then no updates can take place
            if value <= self.watcher.values[trailed_values.read(self.watcher.min_assigned) as usize]
            {
                return;
            }

            // If the right-hand side of the disequality predicate is larger than the value
            // pointed to by `max_assigned` then no updates can take place
            if value >= self.watcher.values[trailed_values.read(self.watcher.max_assigned) as usize]
            {
                return;
            }

            if let Some(index) = self.get_index_of_value(value) {
                self.predicate_has_been_falsified(index, predicate_id_assignments)
            }
        } else if predicate.is_equality_predicate() {
            // Similar to the lower-bound case, we need to check which equality predicates have
            // become falsified due to the update; we need to find the predicates which are now
            // falsified and update `min_assigned`
            //
            // First we get the element which is greater than the one pointed to by
            // `min_assigned`. This can never be `i64::MAX` due to sentinels being placed.
            let mut greater =
                self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
            // Note that an equality predicate can only become falsified if the new lower-bound
            // is _strictly_ larger than the value
            while greater != i64::MAX && value > self.watcher.values[greater as usize] {
                // The update has caused the predicate to become falsified
                self.predicate_has_been_falsified(greater as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.min_assigned, greater);
                greater = self.watcher.greater[greater as usize];
            }

            // Similar to the upper-bound case, we need to check which equality predicates have
            // become falsified due to the update; we need to find the predicates which are now
            // falsified and update `max_assigned`
            //
            // First we get the element which is greater than the one pointed to by
            // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
            let mut smaller =
                self.watcher.smaller[trailed_values.read(self.watcher.max_assigned) as usize];
            // Note that an equality predicate can only become falsified if the new upper-bound
            // is _strictly_ smaller than the value
            while smaller != i64::MAX && value < self.watcher.values[smaller as usize] {
                // The update has caused the predicate to become falsified
                self.predicate_has_been_falsified(smaller as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.max_assigned, smaller);
                smaller = self.watcher.smaller[smaller as usize];
            }

            // We know that a predicate is only true if both `greater[min_assigned]` and
            // `smaller[max_assigned]` point to the same value
            // _and_
            // this value is equal to the right-hand side of the equality predicate
            let greater =
                self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
            if greater
                == self.watcher.smaller[trailed_values.read(self.watcher.max_assigned) as usize]
                && self.watcher.values[greater as usize] == value
            {
                // The update has caused the predicate to become satisfied
                self.predicate_has_been_satisfied(greater as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.min_assigned, greater);
                trailed_values.assign(self.watcher.max_assigned, greater);
            }
        } else {
            panic!()
        }
    }
}
