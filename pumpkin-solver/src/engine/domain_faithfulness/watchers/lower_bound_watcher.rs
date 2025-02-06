use super::DomainWatcher;
use super::FaithfullnessWatcher;
use super::HasWatcher;
use super::PredicateId;
use super::TrailedAssignments;
use crate::predicate;
use crate::predicates::Predicate;

#[derive(Debug, Clone)]
pub(crate) struct LowerBoundWatcher {
    watcher: FaithfullnessWatcher,
}

impl LowerBoundWatcher {
    pub(crate) fn new(stateful_assignments: &mut TrailedAssignments) -> Self {
        Self {
            watcher: FaithfullnessWatcher::new(stateful_assignments),
        }
    }
}

impl HasWatcher for LowerBoundWatcher {
    fn get_watcher(&self) -> &FaithfullnessWatcher {
        &self.watcher
    }

    fn get_watcher_mut(&mut self) -> &mut FaithfullnessWatcher {
        &mut self.watcher
    }
}

impl DomainWatcher for LowerBoundWatcher {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id >= value)
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
                while greater != i64::MAX && lower_bound >= self.watcher.values[greater as usize] {
                    self.predicate_has_been_satisfied(greater as usize, satisfied_predicates);
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
            _ => {}
        }
    }
}
