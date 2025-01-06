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
pub(crate) struct InequalityWatcher {
    watcher: FaithfullnessWatcher,
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

    fn find_sentinels(
        &mut self,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        if self.watcher.values.is_empty() {
            return;
        }
        let mut min_value = i32::MAX;
        let mut min_index = i64::MAX;

        let mut max_value = i32::MIN;
        let mut max_index = i64::MAX;

        for index in 0..self.watcher.values.len() {
            let predicate = self.get_predicate_for_value(self.watcher.values[index]);
            if assignments.is_predicate_satisfied(predicate)
                || assignments.is_predicate_falsified(predicate)
            {
                continue;
            }
            let index_value = self.watcher.values[index];
            if index_value < min_value {
                min_value = index_value;
                min_index = index as i64;
            }

            if index_value > max_value {
                max_value = index_value;
                max_index = index as i64;
            }
        }

        self.watcher
            .min_unassigned
            .assign(min_index, stateful_trail);
        self.watcher
            .max_unassigned
            .assign(max_index, stateful_trail);
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
        predicate_id: Option<PredicateId>,
        last_updated: usize,
    ) {
        self.check_for_updated_sentinel(assignments, stateful_trail, last_updated);

        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.watcher.min_unassigned.read() != i64::MAX
                    && lower_bound
                        > self.watcher.values[self.watcher.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.watcher.min_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.watcher.min_unassigned.assign(
                        self.watcher.g[self.watcher.min_unassigned.read() as usize],
                        stateful_trail,
                    );
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.watcher.max_unassigned.read() != i64::MAX
                    && upper_bound
                        < self.watcher.values[self.watcher.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.watcher.max_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.watcher.max_unassigned.assign(
                        self.watcher.s[self.watcher.max_unassigned.read() as usize],
                        stateful_trail,
                    );
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                while self.watcher.min_unassigned.read() != i64::MAX
                    && equality_constant
                        > self.watcher.values[self.watcher.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.watcher.min_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.watcher.min_unassigned.assign(
                        self.watcher.g[self.watcher.min_unassigned.read() as usize],
                        stateful_trail,
                    );
                }
                while self.watcher.max_unassigned.read() != i64::MAX
                    && equality_constant
                        < self.watcher.values[self.watcher.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.watcher.max_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.watcher.max_unassigned.assign(
                        self.watcher.s[self.watcher.max_unassigned.read() as usize],
                        stateful_trail,
                    );
                }

                // TODO: perhaps the second check is not necessary?
                if self.watcher.min_unassigned.read() != i64::MAX
                    && self.watcher.min_unassigned.read() == self.watcher.max_unassigned.read()
                    && self.watcher.values[self.watcher.min_unassigned.read() as usize]
                        == equality_constant
                {
                    self.predicate_has_been_falsified(
                        self.watcher.min_unassigned.read() as usize,
                        falsified_predicates,
                    );
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
