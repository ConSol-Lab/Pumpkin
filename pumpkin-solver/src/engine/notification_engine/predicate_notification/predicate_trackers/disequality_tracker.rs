use super::DomainTracker;
use super::FaithfulnessTracker;
use super::HasTracker;
use super::PredicateId;
use super::TrailedValues;
use crate::predicate;
use crate::predicates::Predicate;

/// A tracker for disequality [`Predicate`]s
#[derive(Debug, Clone)]
pub(crate) struct DisequalityTracker {
    watcher: FaithfulnessTracker,
}

impl DisequalityTracker {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            watcher: FaithfulnessTracker::new(trailed_values),
        }
    }
}

impl HasTracker for DisequalityTracker {
    fn get_tracker(&self) -> &FaithfulnessTracker {
        &self.watcher
    }

    fn get_tracker_mut(&mut self) -> &mut FaithfulnessTracker {
        &mut self.watcher
    }
}

impl DomainTracker for DisequalityTracker {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id != value)
    }

    fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        predicate_id: Option<PredicateId>,
    ) {
        // We are interested in all types of predicates
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                // In the case of a lower-bound update, it can only be the case that predicates
                // become satisfied; we need to find the predicates which are now satisfied and
                // update `min_assigned`
                //
                // First we get the element which is greater than the one pointed to by
                // `min_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut greater =
                    self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
                // Note that a disequality predicate can only become satisfied if the new
                // lower-bound is _strictly_ larger than the value
                while greater != i64::MAX && lower_bound > self.watcher.values[greater as usize] {
                    // The update has caused the predicate to become satisfied
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.min_assigned, greater);
                    greater = self.watcher.greater[greater as usize];
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                // In the case of an upper-bound update, it can only be the case that predicates
                // become satisfied; we need to find the predicates which are now satisfied and
                // update `max_assigned`
                //
                // First we get the element which is greater than the one pointed to by
                // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut smaller =
                    self.watcher.smaller[trailed_values.read(self.watcher.max_unassigned) as usize];
                // Note that a disequality predicate can only become satisfied if the new
                // upper-bound is _strictly_ smaller than the value
                while smaller != i64::MAX && upper_bound < self.watcher.values[smaller as usize] {
                    // The update has caused the predicate to become satisfied
                    self.predicate_has_been_satisfied(smaller as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.smaller[smaller as usize];
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                // Similar to the lower-bound case, we need to check which disequality predicates
                // have become satisfied due to the update; we need to find the
                // predicates which are now satisfied and update `min_assigned`
                //
                // First we get the element which is greater than the one pointed to by
                // `min_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut greater =
                    self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
                // Note that a disequality predicate can only become satisfied if the new
                // lower-bound is _strictly_ larger than the value
                while greater != i64::MAX
                    && equality_constant > self.watcher.values[greater as usize]
                {
                    // The update has caused the predicate to become satisfied
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.min_assigned, greater);
                    greater = self.watcher.greater[greater as usize];
                }

                // Similar to the upper-bound case, we need to check which disequality predicates
                // have become satisfied due to the update; we need to find the
                // predicates which are now satisfied and update `max_assigned`
                //
                // First we get the element which is greater than the one pointed to by
                // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut smaller =
                    self.watcher.smaller[trailed_values.read(self.watcher.max_unassigned) as usize];
                // Note that a disequality predicate can only become satisfied if the new
                // upper-bound is _strictly_ smaller than the value
                while smaller != i64::MAX
                    && equality_constant < self.watcher.values[smaller as usize]
                {
                    // The update has caused the predicate to become satisfied
                    self.predicate_has_been_satisfied(smaller as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.smaller[smaller as usize];
                }

                // We know that a predicate is only falsified if both `greater[min_assigned]` and
                // `smaller[max_assigned]` point to the same value
                // _and_
                // this value is equal to the right-hand side of the equality predicate
                let greater =
                    self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
                if greater
                    == self.watcher.smaller
                        [trailed_values.read(self.watcher.max_unassigned) as usize]
                    && self.watcher.values[greater as usize] == equality_constant
                {
                    self.predicate_has_been_falsified(greater as usize, falsified_predicates);

                    trailed_values.assign(self.watcher.min_assigned, greater);
                    trailed_values.assign(self.watcher.max_unassigned, greater);
                }
            }
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant: _,
            } => {
                // A relatively simple case stating that a predicate has now become satisfied for
                // disequalities
                //
                // TODO: This could be optimised
                if let Some(predicate_id) = predicate_id {
                    self.predicate_id_has_been_satisfied(predicate_id, satisfied_predicates)
                }
            }
        }
    }
}
