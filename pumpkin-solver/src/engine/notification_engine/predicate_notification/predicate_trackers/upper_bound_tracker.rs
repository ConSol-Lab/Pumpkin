use super::DomainTracker;
use super::FaithfulnessTracker;
use super::HasTracker;
use super::PredicateId;
use super::TrailedValues;
use crate::predicate;
use crate::predicates::Predicate;

/// A tracker for upper-bound [`Predicate`]s
#[derive(Debug, Clone)]
pub(crate) struct UpperBoundTracker {
    watcher: FaithfulnessTracker,
}

impl UpperBoundTracker {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            watcher: FaithfulnessTracker::new(trailed_values),
        }
    }
}

impl HasTracker for UpperBoundTracker {
    fn get_tracker(&self) -> &FaithfulnessTracker {
        &self.watcher
    }

    fn get_tracker_mut(&mut self) -> &mut FaithfulnessTracker {
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
                // become falsified; we need to find the predicates which are now falsified and
                // update `min_assigned`
                //
                // First we get the element which is greater than the one pointed to by
                // `min_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut larger =
                    self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
                while larger != i64::MAX && lower_bound > self.watcher.values[larger as usize] {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_falsified(larger as usize, falsified_predicates);
                    trailed_values.assign(self.watcher.min_assigned, larger);
                    larger = self.watcher.greater[larger as usize];
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
                while smaller != i64::MAX && upper_bound <= self.watcher.values[smaller as usize] {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_satisfied(smaller as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.smaller[smaller as usize];
                }
            }
            _ => {}
        }
    }
}
