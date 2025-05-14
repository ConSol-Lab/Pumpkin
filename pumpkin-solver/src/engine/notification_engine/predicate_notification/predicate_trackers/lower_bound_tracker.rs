use super::DomainTracker;
use super::HasTracker;
use super::PredicateId;
use super::PredicateTracker;
use super::TrailedValues;
use crate::predicate;
use crate::predicates::Predicate;

/// A tracker for lower-bound [`Predicate`]s
#[derive(Debug, Clone)]
pub(crate) struct LowerBoundTracker {
    watcher: PredicateTracker,
}

impl LowerBoundTracker {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            watcher: PredicateTracker::new(trailed_values),
        }
    }
}

impl HasTracker for LowerBoundTracker {
    fn get_tracker(&self) -> &PredicateTracker {
        &self.watcher
    }

    fn get_tracker_mut(&mut self) -> &mut PredicateTracker {
        &mut self.watcher
    }
}

impl DomainTracker for LowerBoundTracker {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id >= value)
    }

    fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        _predicate_id: Option<PredicateId>,
    ) {
        // We only consider lower-bound and upper-bound updates
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
                while greater != i64::MAX && lower_bound >= self.watcher.values[greater as usize] {
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
                // become falsified; we need to find the predicates which are now falsified and
                // update `min_assigned`
                //
                // First we get the element which is smaller than the one pointed to by
                // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut smaller =
                    self.watcher.smaller[trailed_values.read(self.watcher.max_unassigned) as usize];
                while smaller != i64::MAX && upper_bound < self.watcher.values[smaller as usize] {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_falsified(smaller as usize, falsified_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.smaller[smaller as usize];
                }
            }
            _ => {}
        }
    }
}
