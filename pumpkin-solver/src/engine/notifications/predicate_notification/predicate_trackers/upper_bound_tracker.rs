use super::DomainTracker;
use super::HasTracker;
use super::PredicateId;
use super::PredicateTracker;
use super::TrailedValues;
use crate::engine::notifications::predicate_notification::PredicateIdAssignments;
use crate::predicate;
use crate::predicates::Predicate;

/// A tracker for upper-bound [`Predicate`]s
#[derive(Debug, Clone)]
pub(crate) struct UpperBoundTracker {
    watcher: PredicateTracker,
}

impl UpperBoundTracker {
    pub(crate) fn new() -> Self {
        Self {
            watcher: PredicateTracker::new(),
        }
    }
}

impl HasTracker for UpperBoundTracker {
    fn get_tracker(&self) -> &PredicateTracker {
        &self.watcher
    }

    fn get_tracker_mut(&mut self) -> &mut PredicateTracker {
        &mut self.watcher
    }
}

impl DomainTracker for UpperBoundTracker {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id <= value)
    }

    fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
        _predicate_id: Option<PredicateId>,
    ) {
        // We only consider lower-bound and upper-bound updates
        let value = predicate.get_right_hand_side();
        if predicate.is_lower_bound_predicate() {
            // In the case of a lower-bound update, it can only be the case that predicates
            // become falsified; we need to find the predicates which are now falsified and
            // update `min_assigned`
            //
            // First we get the element which is greater than the one pointed to by
            // `min_assigned`. This can never be `i64::MAX` due to sentinels being placed.
            let mut larger =
                self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
            while larger != i64::MAX && value > self.watcher.values[larger as usize] {
                // The update has caused the predicate to become falsified
                self.predicate_has_been_falsified(larger as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.min_assigned, larger);
                larger = self.watcher.greater[larger as usize];
            }
        } else if predicate.is_upper_bound_predicate() {
            // In the case of an upper-bound update, it can only be the case that predicates
            // become satisfied; we need to find the predicates which are now satisfied and
            // update `max_assigned`
            //
            // First we get the element which is greater than the one pointed to by
            // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
            let mut smaller =
                self.watcher.smaller[trailed_values.read(self.watcher.max_assigned) as usize];
            while smaller != i64::MAX && value <= self.watcher.values[smaller as usize] {
                // The update has caused the predicate to become falsified
                self.predicate_has_been_satisfied(smaller as usize, predicate_id_assignments);
                trailed_values.assign(self.watcher.max_assigned, smaller);
                smaller = self.watcher.smaller[smaller as usize];
            }
        }
    }
}
