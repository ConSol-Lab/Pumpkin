use super::DomainWatcher;
use super::FaithfullnessWatcher;
use super::HasWatcher;
use super::PredicateId;
use crate::basic_types::Trail;
use crate::engine::Assignments;
use crate::engine::StateChange;
use crate::predicate;
use crate::predicates::Predicate;

#[derive(Default, Debug, Clone)]
pub(crate) struct UpperBoundWatcher {
    watcher: FaithfullnessWatcher,
}

impl HasWatcher for UpperBoundWatcher {
    fn get_watcher(&self) -> &FaithfullnessWatcher {
        &self.watcher
    }

    fn get_watcher_mut(&mut self) -> &mut FaithfullnessWatcher {
        &mut self.watcher
    }
}

impl DomainWatcher for UpperBoundWatcher {
    fn get_predicate_for_value(&self, value: i32) -> Predicate {
        predicate!(self.watcher.domain_id <= value)
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
        _predicate_id: Option<PredicateId>,
        last_updated: usize,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                let mut larger = self.watcher.g[self.watcher.min_unassigned.read() as usize];
                while larger != i64::MAX && lower_bound >= self.watcher.values[larger as usize] {
                    self.predicate_has_been_falsified(larger as usize, falsified_predicates);
                    self.watcher.min_unassigned.assign(larger, stateful_trail);
                    larger = self.watcher.g[larger as usize];
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                let mut smaller = self.watcher.s[self.watcher.max_unassigned.read() as usize];
                while smaller != i64::MAX && upper_bound <= self.watcher.values[smaller as usize] {
                    self.predicate_has_been_satisfied(smaller as usize, satisfied_predicates);
                    self.watcher.max_unassigned.assign(smaller, stateful_trail);
                    smaller = self.watcher.s[smaller as usize];
                }
            }
            _ => {}
        }
    }
}
