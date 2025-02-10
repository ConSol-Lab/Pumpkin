use super::DomainWatcher;
use super::FaithfullnessWatcher;
use super::HasWatcher;
use super::PredicateId;
use super::TrailedAssignments;
use crate::predicate;
use crate::predicates::Predicate;

#[derive(Debug, Clone)]
pub(crate) struct EqualityWatcher {
    watcher: FaithfullnessWatcher,
}

impl EqualityWatcher {
    pub(crate) fn new(stateful_assignments: &mut TrailedAssignments) -> Self {
        Self {
            watcher: FaithfullnessWatcher::new(stateful_assignments),
        }
    }
}

impl HasWatcher for EqualityWatcher {
    fn get_watcher(&self) -> &FaithfullnessWatcher {
        &self.watcher
    }

    fn get_watcher_mut(&mut self) -> &mut FaithfullnessWatcher {
        &mut self.watcher
    }
}

impl DomainWatcher for EqualityWatcher {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id == value)
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_assignments: &mut TrailedAssignments,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        _predicate_id: Option<PredicateId>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                let mut greater =
                    self.watcher.g[stateful_assignments.read(self.watcher.min_unassigned) as usize];
                while greater != i64::MAX && lower_bound > self.watcher.values[greater as usize] {
                    self.predicate_has_been_falsified(greater as usize, falsified_predicates);
                    stateful_assignments.assign(self.watcher.min_unassigned, greater);
                    greater = self.watcher.g[greater as usize];
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                let mut smaller =
                    self.watcher.s[stateful_assignments.read(self.watcher.max_unassigned) as usize];
                while smaller != i64::MAX && upper_bound < self.watcher.values[smaller as usize] {
                    self.predicate_has_been_falsified(smaller as usize, falsified_predicates);
                    stateful_assignments.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.s[smaller as usize];
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                let mut greater =
                    self.watcher.g[stateful_assignments.read(self.watcher.min_unassigned) as usize];
                while greater != i64::MAX
                    && equality_constant > self.watcher.values[greater as usize]
                {
                    self.predicate_has_been_falsified(greater as usize, falsified_predicates);
                    stateful_assignments.assign(self.watcher.min_unassigned, greater);
                    greater = self.watcher.g[greater as usize];
                }

                let mut smaller =
                    self.watcher.s[stateful_assignments.read(self.watcher.max_unassigned) as usize];
                while smaller != i64::MAX
                    && equality_constant < self.watcher.values[smaller as usize]
                {
                    self.predicate_has_been_falsified(smaller as usize, falsified_predicates);
                    stateful_assignments.assign(self.watcher.max_unassigned, smaller);
                    smaller = self.watcher.s[smaller as usize];
                }

                let greater =
                    self.watcher.g[stateful_assignments.read(self.watcher.min_unassigned) as usize];
                if greater
                    == self.watcher.s
                        [stateful_assignments.read(self.watcher.max_unassigned) as usize]
                    && self.watcher.values[greater as usize] == equality_constant
                {
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
                    stateful_assignments.assign(self.watcher.min_unassigned, greater);
                    stateful_assignments.assign(self.watcher.max_unassigned, greater);
                }
            }
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => {
                if not_equal_constant
                    <= self.watcher.values
                        [stateful_assignments.read(self.watcher.min_unassigned) as usize]
                {
                    return;
                }

                if not_equal_constant
                    >= self.watcher.values
                        [stateful_assignments.read(self.watcher.max_unassigned) as usize]
                {
                    return;
                }

                // We go through all of the values and simply check whether it is being watched
                // TODO: There should be a more efficient way to do this
                let mut current_index =
                    self.watcher.g[stateful_assignments.read(self.watcher.min_unassigned) as usize];
                while current_index != i64::MAX {
                    if not_equal_constant < self.watcher.values[current_index as usize] {
                        break;
                    }
                    if not_equal_constant == self.watcher.values[current_index as usize] {
                        self.predicate_has_been_falsified(
                            current_index as usize,
                            falsified_predicates,
                        )
                    }
                    current_index = self.watcher.g[current_index as usize];
                }
            }
        }
    }
}
