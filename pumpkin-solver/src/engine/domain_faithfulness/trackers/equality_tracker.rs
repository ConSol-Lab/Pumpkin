use super::DomainTracker;
use super::FaithfulnessTracker;
use super::HasTracker;
use super::PredicateId;
use super::TrailedValues;
use crate::predicate;
use crate::predicates::Predicate;

/// A tracker for equality [`Predicate`]s
#[derive(Debug, Clone)]
pub(crate) struct EqualityTracker {
    watcher: FaithfulnessTracker,
}

impl EqualityTracker {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            watcher: FaithfulnessTracker::new(trailed_values),
        }
    }
}

impl HasTracker for EqualityTracker {
    fn get_tracker(&self) -> &FaithfulnessTracker {
        &self.watcher
    }

    fn get_tracker_mut(&mut self) -> &mut FaithfulnessTracker {
        &mut self.watcher
    }
}

impl DomainTracker for EqualityTracker {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id == value)
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        _predicate_id: Option<PredicateId>,
    ) {
        // We are interested in all types of predicates
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
                let mut greater =
                    self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
                // Note that an equality predicate can only become falsified if the new lower-bound
                // is _strictly_ larger than the value
                while greater != i64::MAX && lower_bound > self.watcher.values[greater as usize] {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_falsified(greater as usize, falsified_predicates);
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
                // update `max_assigned`
                //
                // First we get the element which is greater than the one pointed to by
                // `max_assigned`. This can never be `i64::MAX` due to sentinels being placed.
                let mut smaller =
                    self.watcher.smaller[trailed_values.read(self.watcher.max_unassigned) as usize];
                // Note that an equality predicate can only become falsified if the new upper-bound
                // is _strictly_ smaller than the value
                while smaller != i64::MAX && upper_bound < self.watcher.values[smaller as usize] {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_falsified(smaller as usize, falsified_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.smaller[smaller as usize];
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
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
                while greater != i64::MAX
                    && equality_constant > self.watcher.values[greater as usize]
                {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_falsified(greater as usize, falsified_predicates);
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
                    self.watcher.smaller[trailed_values.read(self.watcher.max_unassigned) as usize];
                // Note that an equality predicate can only become falsified if the new upper-bound
                // is _strictly_ smaller than the value
                while smaller != i64::MAX
                    && equality_constant < self.watcher.values[smaller as usize]
                {
                    // The update has caused the predicate to become falsified
                    self.predicate_has_been_falsified(smaller as usize, falsified_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.smaller[smaller as usize];
                }

                // We know that a predicate is only true if both `greater[min_assigned]` and
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
                    // The update has caused the predicate to become satisfied
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.min_assigned, greater);
                    trailed_values.assign(self.watcher.max_unassigned, greater);
                }
            }
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => {
                // If the right-hand side of the disequality predicate is smaller than the value
                // pointed to by `min_assigned` then no updates can take place
                if not_equal_constant
                    <= self.watcher.values[trailed_values.read(self.watcher.min_assigned) as usize]
                {
                    return;
                }

                // If the right-hand side of the disequality predicate is larger than the value
                // pointed to by `max_assigned` then no updates can take place
                if not_equal_constant
                    >= self.watcher.values
                        [trailed_values.read(self.watcher.max_unassigned) as usize]
                {
                    return;
                }

                // We go through all of the values and simply check whether its value correspond
                // with the right-hand side of the disequality update.
                //
                // If this is the case then it is falsified.
                //
                // Note that this might lead to a predicate "becoming falsified" multiple times if
                // the bounds are afterwards updated.
                //
                // TODO: There should be a more efficient way to do this

                // We start at the lower end of the values
                let mut current_index =
                    self.watcher.greater[trailed_values.read(self.watcher.min_assigned) as usize];
                while current_index != i64::MAX {
                    // If the right-hand side is now lower than the current value that we are
                    // inspecting then we can break
                    if not_equal_constant < self.watcher.values[current_index as usize] {
                        break;
                    }
                    // Otherwise, we check whether the current value is equal to the right-hand
                    // side
                    if not_equal_constant == self.watcher.values[current_index as usize] {
                        // The update has caused the predicate to become falsified
                        self.predicate_has_been_falsified(
                            current_index as usize,
                            falsified_predicates,
                        )
                    }
                    current_index = self.watcher.greater[current_index as usize];
                }
            }
        }
    }
}
