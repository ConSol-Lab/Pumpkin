use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::StateChange;
use crate::engine::StatefulInt;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

#[derive(Default, Debug)]
pub(crate) struct DomainFaithfulness {
    predicate_to_id: PredicateIdGenerator,

    domain_id_to_faithfullness: KeyedVec<DomainId, Faithfullness>,
}

impl DomainFaithfulness {
    pub(crate) fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        if self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        // Otherwise we update the structures
        self.domain_id_to_faithfullness[predicate.get_domain()]
            .has_been_updated(predicate, stateful_trail);
    }

    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        // If it is already watched then at the moment we do nothing
        if self.predicate_to_id.has_id_for_predicate(predicate) {
            return;
        }

        // We create a new predicate ID for the predicate
        let id = self.predicate_to_id.get_id(predicate);

        if self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
            self.domain_id_to_faithfullness
                .resize(predicate.get_domain().index() + 1, Faithfullness::default());
        }

        // Then we update the structures
        self.domain_id_to_faithfullness[predicate.get_domain()].watch_predicate(
            predicate,
            id,
            stateful_trail,
        );
    }
}

#[derive(Debug, Clone, Default)]
struct Faithfullness {
    lower_bound: FaithfullnessWatcher,
    upper_bound: FaithfullnessWatcher,
    inequality: FaithfullnessWatcher,
    equality: FaithfullnessWatcher,
}

impl Faithfullness {
    fn watch_predicate(
        &mut self,
        predicate: Predicate,
        id: PredicateId,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => self.lower_bound.add(lower_bound, id, stateful_trail),
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => self.upper_bound.add(upper_bound, id, stateful_trail),
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => self.inequality.add(not_equal_constant, id, stateful_trail),
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => self.equality.add(equality_constant, id, stateful_trail),
        }
    }

    fn has_been_updated(&mut self, predicate: Predicate, stateful_trail: &mut Trail<StateChange>) {
        if !self.lower_bound.is_empty() {
            self.lower_bound
                .has_been_updated_lower_bound(predicate, stateful_trail);
        }

        if !self.upper_bound.is_empty() {
            self.upper_bound
                .has_been_updated_upper_bound(predicate, stateful_trail);
        }

        if !self.inequality.is_empty() {
            self.inequality
                .has_been_updated_inequality(predicate, stateful_trail);
        }

        if !self.equality.is_empty() {
            self.equality
                .has_been_updated_equality(predicate, stateful_trail);
        }
    }
}

#[derive(Debug, Clone)]
struct FaithfullnessWatcher {
    s: Vec<usize>,
    g: Vec<usize>,
    min_unassigned: StatefulInt,
    max_unassigned: StatefulInt,

    values: Vec<i32>,
    ids: Vec<PredicateId>,
}
impl Default for FaithfullnessWatcher {
    fn default() -> Self {
        Self {
            min_unassigned: StatefulInt::new(i64::MAX),
            max_unassigned: StatefulInt::new(i64::MAX),
            s: Vec::default(),
            g: Vec::default(),
            values: Vec::default(),
            ids: Vec::default(),
        }
    }
}

impl FaithfullnessWatcher {
    fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    fn predicate_has_been_satisfied(&self, index: usize) {
        todo!();
    }

    fn predicate_has_been_falsified(&self, index: usize) {
        todo!()
    }

    fn add(
        &mut self,
        value: i32,
        predicate_id: PredicateId,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        // TODO: check whether it is unassigned
        let new_index = self.values.len();
        let mut has_updated_sentinels = false;

        self.values.push(value);
        self.ids.push(predicate_id);

        if self.values.len() == 1 {
            self.s.push(usize::MAX);
            self.g.push(usize::MAX);
            self.min_unassigned.assign(0, stateful_trail);
            self.max_unassigned.assign(0, stateful_trail);

            return;
        }

        if value < self.values[self.min_unassigned.read() as usize] {
            has_updated_sentinels = true;

            self.s[self.min_unassigned.read() as usize] = new_index;
            self.s.push(usize::MAX);
            self.g.push(self.min_unassigned.read() as usize);
            self.min_unassigned.assign(new_index as i64, stateful_trail);
        }

        if value > self.values[self.max_unassigned.read() as usize] {
            self.g[self.max_unassigned.read() as usize] = new_index;

            if has_updated_sentinels {
                self.s[new_index] = self.max_unassigned.read() as usize;
            } else {
                has_updated_sentinels = true;
                self.s.push(self.max_unassigned.read() as usize);
                self.g.push(usize::MAX);
            }
            self.max_unassigned.assign(new_index as i64, stateful_trail);
        }

        if !has_updated_sentinels {
            let mut index_largest_value_smaller_than = usize::MAX;
            let mut largest_value_smaller_than = i32::MIN;

            let mut index_smallest_value_larger_than = usize::MAX;
            let mut smallest_value_larger_than = i32::MAX;

            for index in 0..self.values.len() - 1 {
                let index_value = self.values[index];
                pumpkin_assert_simple!(index_value != value);
                if index_value < value && index_value > largest_value_smaller_than {
                    largest_value_smaller_than = index_value;
                    index_largest_value_smaller_than = index;
                }

                if index_value > value && index_value < smallest_value_larger_than {
                    smallest_value_larger_than = index_value;
                    index_smallest_value_larger_than = index;
                }
            }

            self.s[new_index] = index_largest_value_smaller_than;
            self.g[new_index] = index_smallest_value_larger_than;
        }
    }

    fn has_been_updated_inequality(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(self.min_unassigned.read() as usize);
                    self.min_unassigned.assign(
                        self.g[self.min_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(self.max_unassigned.read() as usize);
                    self.max_unassigned.assign(
                        self.s[self.max_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && equality_constant > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(self.min_unassigned.read() as usize);
                    self.min_unassigned.assign(
                        self.g[self.min_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
                while self.max_unassigned.read() != i64::MAX
                    && equality_constant < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(self.max_unassigned.read() as usize);
                    self.max_unassigned.assign(
                        self.s[self.max_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }

                // TODO: perhaps the second check is not necessary?
                if self.min_unassigned.read() == self.max_unassigned.read()
                    && self.values[self.min_unassigned.read() as usize] == equality_constant
                {
                    self.predicate_has_been_falsified(self.min_unassigned.read() as usize);
                }
            }
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant: _,
            } => todo!(),
        }
    }

    fn has_been_updated_equality(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(self.min_unassigned.read() as usize);
                    self.min_unassigned.assign(
                        self.g[self.min_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(self.max_unassigned.read() as usize);
                    self.max_unassigned.assign(
                        self.s[self.max_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && equality_constant > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(self.min_unassigned.read() as usize);
                    self.min_unassigned.assign(
                        self.g[self.min_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
                while self.max_unassigned.read() != i64::MAX
                    && equality_constant < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(self.max_unassigned.read() as usize);
                    self.max_unassigned.assign(
                        self.s[self.max_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }

                // TODO: perhaps the second check is not necessary?
                if self.min_unassigned.read() == self.max_unassigned.read()
                    && self.values[self.min_unassigned.read() as usize] == equality_constant
                {
                    self.predicate_has_been_satisfied(self.min_unassigned.read() as usize);
                }
            }
            _ => {}
        }
    }

    fn has_been_updated_lower_bound(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound >= self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(self.min_unassigned.read() as usize);
                    self.min_unassigned.assign(
                        self.g[self.min_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound <= self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(self.max_unassigned.read() as usize);
                    self.max_unassigned.assign(
                        self.s[self.max_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            _ => {}
        }
    }

    fn has_been_updated_upper_bound(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound >= self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(self.min_unassigned.read() as usize);
                    self.min_unassigned.assign(
                        self.g[self.min_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound <= self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(self.max_unassigned.read() as usize);
                    self.max_unassigned.assign(
                        self.s[self.max_unassigned.read() as usize] as i64,
                        stateful_trail,
                    );
                }
            }
            _ => {}
        }
    }
}
