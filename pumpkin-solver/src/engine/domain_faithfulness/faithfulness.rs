use log::info;

use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
use crate::engine::StateChange;
use crate::engine::StatefulInt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

#[derive(Default, Debug)]
pub(crate) struct DomainFaithfulness {
    pub(crate) predicate_to_id: PredicateIdGenerator,

    domain_id_to_faithfullness: KeyedVec<DomainId, Faithfullness>,

    falsified_predicates: Vec<PredicateId>,
    satisfied_predicates: Vec<PredicateId>,
}

impl DomainFaithfulness {
    pub(crate) fn drain_falsified_predicates(
        &mut self,
    ) -> impl Iterator<Item = PredicateId> + use<'_> {
        self.falsified_predicates.drain(..)
    }

    pub(crate) fn drain_satisfied_predicates(
        &mut self,
    ) -> impl Iterator<Item = PredicateId> + use<'_> {
        self.satisfied_predicates.drain(..)
    }

    pub(crate) fn get_predicate_for_id(&self, predicate_id: PredicateId) -> Predicate {
        self.predicate_to_id.get_predicate(predicate_id).unwrap()
    }

    pub(crate) fn get_id_for_predicate(&mut self, predicate: Predicate) -> Option<PredicateId> {
        if !self.predicate_to_id.has_id_for_predicate(predicate) {
            return None;
        }

        Some(self.predicate_to_id.get_id(predicate))
    }

    pub(crate) fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        if self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        // Otherwise we update the structures
        self.domain_id_to_faithfullness[predicate.get_domain()].has_been_updated(
            predicate,
            stateful_trail,
            &mut self.falsified_predicates,
            &mut self.satisfied_predicates,
            assignments,
            self.predicate_to_id
                .has_id_for_predicate(predicate)
                .then(|| self.predicate_to_id.get_id(predicate)),
        );
    }

    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) -> PredicateId {
        // If it is already watched then at the moment we do nothing
        let has_id_for_predicate = self.predicate_to_id.has_id_for_predicate(predicate);

        // We create a new predicate ID for the predicate
        let id = self.predicate_to_id.get_id(predicate);

        if !has_id_for_predicate {
            while self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
                let _ = self
                    .domain_id_to_faithfullness
                    .push(Faithfullness::default());
            }

            self.domain_id_to_faithfullness[predicate.get_domain()]
                .set_domain_id(predicate.get_domain());

            // Then we update the structures
            self.domain_id_to_faithfullness[predicate.get_domain()].watch_predicate(
                predicate,
                id,
                stateful_trail,
                assignments,
            );
        }

        id
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
        assignments: &Assignments,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => self
                .lower_bound
                .add(lower_bound, id, stateful_trail, assignments, Mode::LB),
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => self
                .upper_bound
                .add(upper_bound, id, stateful_trail, assignments, Mode::UB),
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => self.inequality.add(
                not_equal_constant,
                id,
                stateful_trail,
                assignments,
                Mode::NE,
            ),
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => self
                .equality
                .add(equality_constant, id, stateful_trail, assignments, Mode::E),
        }
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
        predicate_id: Option<PredicateId>,
    ) {
        if !self.lower_bound.is_empty() {
            self.lower_bound.has_been_updated_lower_bound(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
            );
        }

        if !self.upper_bound.is_empty() {
            self.upper_bound.has_been_updated_upper_bound(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
            );
        }

        if !self.inequality.is_empty() {
            self.inequality.has_been_updated_inequality(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
                predicate_id,
            );
        }

        if !self.equality.is_empty() {
            self.equality.has_been_updated_equality(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
            );
        }
    }

    fn set_domain_id(&mut self, domain_id: DomainId) {
        self.lower_bound.set_domain_id(domain_id);
        self.upper_bound.set_domain_id(domain_id);
        self.inequality.set_domain_id(domain_id);
        self.equality.set_domain_id(domain_id);
    }
}

#[derive(Debug, Clone)]
struct FaithfullnessWatcher {
    domain_id: DomainId,
    s: Vec<i64>,
    g: Vec<i64>,
    min_unassigned: StatefulInt,
    max_unassigned: StatefulInt,

    values: Vec<i32>,
    ids: Vec<PredicateId>,
}
impl Default for FaithfullnessWatcher {
    fn default() -> Self {
        Self {
            domain_id: DomainId { id: 0 },
            min_unassigned: StatefulInt::new(i64::MAX),
            max_unassigned: StatefulInt::new(i64::MAX),
            s: Vec::default(),
            g: Vec::default(),
            values: Vec::default(),
            ids: Vec::default(),
        }
    }
}

#[derive(Debug)]
enum Mode {
    LB,
    UB,
    NE,
    E,
}

impl FaithfullnessWatcher {
    fn set_domain_id(&mut self, domain_id: DomainId) {
        self.domain_id = domain_id;
    }

    fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    fn predicate_id_has_been_satisfied(
        &self,
        predicate_id: PredicateId,
        satisfied_predicates: &mut Vec<PredicateId>,
    ) {
        satisfied_predicates.push(predicate_id)
    }

    fn predicate_has_been_satisfied(
        &self,
        index: usize,
        satisfied_predicates: &mut Vec<PredicateId>,
    ) {
        satisfied_predicates.push(self.ids[index])
    }

    fn predicate_has_been_falsified(
        &self,
        index: usize,
        falsified_predicates: &mut Vec<PredicateId>,
    ) {
        falsified_predicates.push(self.ids[index])
    }

    fn add(
        &mut self,
        value: i32,
        predicate_id: PredicateId,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
        mode: Mode,
    ) {
        // TODO: check whether it is unassigned
        let new_index = self.values.len() as i64;
        let mut has_updated_sentinels = false;

        self.values.push(value);
        self.ids.push(predicate_id);

        if self.values.len() == 1 {
            self.s.push(i64::MAX);
            self.g.push(i64::MAX);
            self.min_unassigned.assign(0, stateful_trail);
            self.max_unassigned.assign(0, stateful_trail);
            pumpkin_assert_simple!(
                self.s.len() == self.g.len()
                    && self.g.len() == self.values.len()
                    && self.values.len() == self.ids.len()
            );

            return;
        } else if self.min_unassigned.read() == i64::MAX || self.max_unassigned.read() == i64::MAX {
            match mode {
                Mode::LB => {
                    self.find_sentinels_lower_bound(stateful_trail, assignments);
                }
                Mode::UB => self.find_sentinels_upper_bound(stateful_trail, assignments),
                Mode::NE => self.find_sentinels_inequality(stateful_trail, assignments),
                Mode::E => self.find_sentinels_equality(stateful_trail, assignments),
            }
        }
        if self.min_unassigned.read() != i64::MAX {
            if value < self.values[self.min_unassigned.read() as usize] {
                has_updated_sentinels = true;

                self.s[self.min_unassigned.read() as usize] = new_index;
                self.s.push(i64::MAX);
                self.g.push(self.min_unassigned.read());
                self.min_unassigned.assign(new_index, stateful_trail);
            }
        }
        if self.max_unassigned.read() != i64::MAX {
            if value > self.values[self.max_unassigned.read() as usize] {
                self.g[self.max_unassigned.read() as usize] = new_index;

                if has_updated_sentinels {
                    self.s[new_index as usize] = self.max_unassigned.read();
                } else {
                    has_updated_sentinels = true;
                    self.s.push(self.max_unassigned.read());
                    self.g.push(i64::MAX);
                }
                self.max_unassigned.assign(new_index, stateful_trail);
            }
        }

        if !has_updated_sentinels {
            let mut index_largest_value_smaller_than = i64::MAX;
            let mut largest_value_smaller_than = i32::MIN;

            let mut index_smallest_value_larger_than = i64::MAX;
            let mut smallest_value_larger_than = i32::MAX;

            for index in 0..self.values.len() - 1 {
                let index_value = self.values[index];
                pumpkin_assert_simple!(index_value != value);
                if index_value < value && index_value > largest_value_smaller_than {
                    largest_value_smaller_than = index_value;
                    index_largest_value_smaller_than = index as i64;
                }

                if index_value > value && index_value < smallest_value_larger_than {
                    smallest_value_larger_than = index_value;
                    index_smallest_value_larger_than = index as i64;
                }
            }

            self.s.push(index_largest_value_smaller_than);
            self.g.push(index_smallest_value_larger_than);
        }
        pumpkin_assert_simple!(
            self.s.len() == self.g.len()
                && self.g.len() == self.values.len()
                && self.values.len() == self.ids.len()
        );
    }

    fn has_been_updated_equality(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
    ) {
        // if !self.values.is_empty() {
        //    self.find_sentinels_equality(stateful_trail, assignments);
        //}

        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(
                        self.min_unassigned.read() as usize,
                        falsified_predicates,
                    );
                    self.min_unassigned
                        .assign(self.g[self.min_unassigned.read() as usize], stateful_trail);
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(
                        self.max_unassigned.read() as usize,
                        falsified_predicates,
                    );
                    self.max_unassigned
                        .assign(self.s[self.max_unassigned.read() as usize], stateful_trail);
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && equality_constant > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(
                        self.min_unassigned.read() as usize,
                        falsified_predicates,
                    );
                    self.min_unassigned
                        .assign(self.g[self.min_unassigned.read() as usize], stateful_trail);
                }
                while self.max_unassigned.read() != i64::MAX
                    && equality_constant < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(
                        self.max_unassigned.read() as usize,
                        falsified_predicates,
                    );
                    self.max_unassigned
                        .assign(self.s[self.max_unassigned.read() as usize], stateful_trail);
                }

                // TODO: perhaps the second check is not necessary?
                if self.min_unassigned.read() != i64::MAX
                    && self.min_unassigned.read() == self.max_unassigned.read()
                    && self.values[self.min_unassigned.read() as usize] == equality_constant
                {
                    self.predicate_has_been_satisfied(
                        self.min_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                }
            }
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => {
                if self.min_unassigned.read() == i64::MAX || self.max_unassigned.read() == i64::MAX
                {
                    return;
                }

                if not_equal_constant < self.values[self.min_unassigned.read() as usize] {
                    return;
                }

                if not_equal_constant > self.values[self.max_unassigned.read() as usize] {
                    return;
                }

                let mut current_index = self.g[self.min_unassigned.read() as usize];
                while current_index != i64::MAX {
                    if not_equal_constant > self.values[current_index as usize] {
                        break;
                    }
                    if not_equal_constant == self.values[current_index as usize] {
                        self.predicate_has_been_falsified(
                            current_index as usize,
                            falsified_predicates,
                        )
                    }
                    current_index = self.g[current_index as usize];
                }
            }
        }
    }

    fn has_been_updated_inequality(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
        predicate_id: Option<PredicateId>,
    ) {
        // if !self.values.is_empty() {
        //    self.find_sentinels_inequality(stateful_trail, assignments);
        //}

        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.min_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.min_unassigned
                        .assign(self.g[self.min_unassigned.read() as usize], stateful_trail);
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.max_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.max_unassigned
                        .assign(self.s[self.max_unassigned.read() as usize], stateful_trail);
                }
            }
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && equality_constant > self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.min_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.min_unassigned
                        .assign(self.g[self.min_unassigned.read() as usize], stateful_trail);
                }
                while self.max_unassigned.read() != i64::MAX
                    && equality_constant < self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.max_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.max_unassigned
                        .assign(self.s[self.max_unassigned.read() as usize], stateful_trail);
                }

                // TODO: perhaps the second check is not necessary?
                if self.min_unassigned.read() != i64::MAX
                    && self.min_unassigned.read() == self.max_unassigned.read()
                    && self.values[self.min_unassigned.read() as usize] == equality_constant
                {
                    self.predicate_has_been_falsified(
                        self.min_unassigned.read() as usize,
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

    fn has_been_updated_lower_bound(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
    ) {
        // if !self.values.is_empty() {
        //    self.find_sentinels_lower_bound(stateful_trail, assignments);
        //}

        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound >= self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.min_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.min_unassigned
                        .assign(self.g[self.min_unassigned.read() as usize], stateful_trail);
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound <= self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(
                        self.max_unassigned.read() as usize,
                        falsified_predicates,
                    );
                    self.max_unassigned
                        .assign(self.s[self.max_unassigned.read() as usize], stateful_trail);
                }
            }
            _ => {}
        }
    }

    fn has_been_updated_upper_bound(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        assignments: &Assignments,
    ) {
        // if !self.values.is_empty() {
        //    self.find_sentinels_upper_bound(stateful_trail, assignments);
        //}
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                while self.min_unassigned.read() != i64::MAX
                    && lower_bound >= self.values[self.min_unassigned.read() as usize]
                {
                    self.predicate_has_been_falsified(
                        self.min_unassigned.read() as usize,
                        falsified_predicates,
                    );
                    self.min_unassigned
                        .assign(self.g[self.min_unassigned.read() as usize], stateful_trail);
                }
            }
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                while self.max_unassigned.read() != i64::MAX
                    && upper_bound <= self.values[self.max_unassigned.read() as usize]
                {
                    self.predicate_has_been_satisfied(
                        self.max_unassigned.read() as usize,
                        satisfied_predicates,
                    );
                    self.max_unassigned
                        .assign(self.s[self.max_unassigned.read() as usize], stateful_trail);
                }
            }
            _ => {}
        }
    }

    fn find_sentinels_lower_bound(
        &mut self,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        if self.values.is_empty() {
            return;
        }
        let mut min_value = i32::MAX;
        let mut min_index = i64::MAX;

        let mut max_value = i32::MIN;
        let mut max_index = i64::MAX;

        for index in 0..self.values.len() {
            if assignments.is_predicate_satisfied(predicate!(self.domain_id >= self.values[index]))
                || assignments
                    .is_predicate_falsified(predicate!(self.domain_id >= self.values[index]))
            {
                continue;
            }
            let index_value = self.values[index];
            if index_value < min_value {
                min_value = index_value;
                min_index = index as i64;
            }

            if index_value > max_value {
                max_value = index_value;
                max_index = index as i64;
            }
        }

        self.min_unassigned.assign(min_index, stateful_trail);
        self.max_unassigned.assign(max_index, stateful_trail);
    }

    fn find_sentinels_upper_bound(
        &mut self,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        if self.values.is_empty() {
            return;
        }
        let mut min_value = i32::MAX;
        let mut min_index = i64::MAX;

        let mut max_value = i32::MIN;
        let mut max_index = i64::MAX;

        for index in 0..self.values.len() {
            if assignments.is_predicate_satisfied(predicate!(self.domain_id <= self.values[index]))
                || assignments
                    .is_predicate_falsified(predicate!(self.domain_id <= self.values[index]))
            {
                continue;
            }
            let index_value = self.values[index];
            if index_value < min_value {
                min_value = index_value;
                min_index = index as i64;
            }

            if index_value > max_value {
                max_value = index_value;
                max_index = index as i64;
            }
        }

        self.min_unassigned.assign(min_index, stateful_trail);
        self.max_unassigned.assign(max_index, stateful_trail);
    }

    fn find_sentinels_inequality(
        &mut self,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        if self.values.is_empty() {
            return;
        }
        let mut min_value = i32::MAX;
        let mut min_index = i64::MAX;

        let mut max_value = i32::MIN;
        let mut max_index = i64::MAX;

        for index in 0..self.values.len() {
            if assignments.is_predicate_satisfied(predicate!(self.domain_id != self.values[index]))
                || assignments
                    .is_predicate_falsified(predicate!(self.domain_id != self.values[index]))
            {
                continue;
            }
            let index_value = self.values[index];
            if index_value < min_value {
                min_value = index_value;
                min_index = index as i64;
            }

            if index_value > max_value {
                max_value = index_value;
                max_index = index as i64;
            }
        }

        self.min_unassigned.assign(min_index, stateful_trail);
        self.max_unassigned.assign(max_index, stateful_trail);
    }

    fn find_sentinels_equality(
        &mut self,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        if self.values.is_empty() {
            return;
        }
        let mut min_value = i32::MAX;
        let mut min_index = i64::MAX;

        let mut max_value = i32::MIN;
        let mut max_index = i64::MAX;

        for index in 0..self.values.len() {
            if assignments.is_predicate_satisfied(predicate!(self.domain_id == self.values[index]))
                || assignments
                    .is_predicate_falsified(predicate!(self.domain_id == self.values[index]))
            {
                continue;
            }
            let index_value = self.values[index];
            if index_value < min_value {
                min_value = index_value;
                min_index = index as i64;
            }

            if index_value > max_value {
                max_value = index_value;
                max_index = index as i64;
            }
        }

        self.min_unassigned.assign(min_index, stateful_trail);
        self.max_unassigned.assign(max_index, stateful_trail);
    }
}
