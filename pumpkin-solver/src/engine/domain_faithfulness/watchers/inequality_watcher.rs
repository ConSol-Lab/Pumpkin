use super::DomainWatcher;
use super::FaithfullnessWatcher;
use super::HasWatcher;
use super::PredicateId;
use super::TrailedValues;
use crate::predicate;
use crate::predicates::Predicate;

#[derive(Debug, Clone)]
pub(crate) struct InequalityWatcher {
    watcher: FaithfullnessWatcher,
}

impl InequalityWatcher {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            watcher: FaithfullnessWatcher::new(trailed_values),
        }
    }
}

impl HasWatcher for InequalityWatcher {
    fn get_watcher(&self) -> &FaithfullnessWatcher {
        &self.watcher
    }

    fn get_watcher_mut(&mut self) -> &mut FaithfullnessWatcher {
        &mut self.watcher
    }
}

impl DomainWatcher for InequalityWatcher {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id != value)
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        predicate_id: Option<PredicateId>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                let mut greater =
                    self.watcher.g[trailed_values.read(self.watcher.min_unassigned) as usize];
                while greater != i64::MAX && lower_bound > self.watcher.values[greater as usize] {
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.min_unassigned, greater);
                    greater = self.watcher.g[greater as usize];
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                let mut smaller =
                    self.watcher.s[trailed_values.read(self.watcher.max_unassigned) as usize];
                while smaller != i64::MAX && upper_bound < self.watcher.values[smaller as usize] {
                    self.predicate_has_been_satisfied(smaller as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.s[smaller as usize];
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                let mut greater =
                    self.watcher.g[trailed_values.read(self.watcher.min_unassigned) as usize];
                while greater != i64::MAX
                    && equality_constant > self.watcher.values[greater as usize]
                {
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.min_unassigned, greater);
                    greater = self.watcher.g[greater as usize];
                }

                let mut smaller =
                    self.watcher.s[trailed_values.read(self.watcher.max_unassigned) as usize];
                while smaller != i64::MAX
                    && equality_constant < self.watcher.values[smaller as usize]
                {
                    self.predicate_has_been_satisfied(smaller as usize, satisfied_predicates);
                    trailed_values.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.s[smaller as usize];
                }

                // If the next watcher pointed to by the indices is the same and it has a value
                // equal to the equality constant
                let greater =
                    self.watcher.g[trailed_values.read(self.watcher.min_unassigned) as usize];
                if greater
                    == self.watcher.s[trailed_values.read(self.watcher.max_unassigned) as usize]
                    && self.watcher.values[greater as usize] == equality_constant
                {
                    self.predicate_has_been_falsified(greater as usize, falsified_predicates);

                    trailed_values.assign(self.watcher.min_unassigned, greater);
                    trailed_values.assign(self.watcher.max_unassigned, greater);
                }
            }
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant: _,
            } => {
                if let Some(predicate_id) = predicate_id {
                    self.predicate_id_has_been_satisfied(predicate_id, satisfied_predicates)
                }
            }
        }
    }
}
