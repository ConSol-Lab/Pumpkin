use log::info;

use crate::basic_types::PredicateId;
use crate::basic_types::Trail;
use crate::engine::Assignments;
use crate::engine::StateChange;
use crate::engine::StatefulInt;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

mod equality_watcher;
mod inequality_watcher;
mod lower_bound_watcher;
mod upper_bound_watcher;
pub(crate) use equality_watcher::EqualityWatcher;
pub(crate) use inequality_watcher::InequalityWatcher;
pub(crate) use lower_bound_watcher::LowerBoundWatcher;
pub(crate) use upper_bound_watcher::UpperBoundWatcher;

#[derive(Debug, Clone)]
pub(crate) struct FaithfullnessWatcher {
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
            min_unassigned: StatefulInt::new(0),
            max_unassigned: StatefulInt::new(1),
            s: Vec::default(),
            g: Vec::default(),
            values: Vec::default(),
            ids: Vec::default(),
        }
    }
}

pub(crate) trait HasWatcher {
    fn get_watcher(&self) -> &FaithfullnessWatcher;
    fn get_watcher_mut(&mut self) -> &mut FaithfullnessWatcher;
}

pub(crate) trait DomainWatcherInformation {
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
    );

    fn get_ids(&self) -> &Vec<PredicateId>;
    fn get_ids_mut(&mut self) -> &mut Vec<PredicateId>;

    fn get_values(&self) -> &Vec<i32>;
    fn get_values_mut(&mut self) -> &mut Vec<i32>;

    fn get_smaller(&self) -> &Vec<i64>;
    fn get_smaller_mut(&mut self) -> &mut Vec<i64>;

    fn get_greater(&self) -> &Vec<i64>;
    fn get_greater_mut(&mut self) -> &mut Vec<i64>;

    fn get_min_unassigned(&self) -> &StatefulInt;
    fn get_min_unassigned_mut(&mut self) -> &mut StatefulInt;

    fn get_max_unassigned(&self) -> &StatefulInt;
    fn get_max_unassigned_mut(&mut self) -> &mut StatefulInt;

    fn is_empty(&self) -> bool;
}

impl<Watcher: HasWatcher> DomainWatcherInformation for Watcher {
    fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
    ) {
        if !self.get_values().is_empty() {
            return;
        }
        self.get_watcher_mut().domain_id = domain_id;

        self.get_values_mut().push(initial_lower_bound);
        self.get_values_mut().push(initial_upper_bound);

        // These should never be queried
        self.get_ids_mut().push(PredicateId { id: u32::MAX });
        self.get_ids_mut().push(PredicateId { id: u32::MAX });

        self.get_smaller_mut().push(i64::MAX);
        self.get_smaller_mut().push(0);

        self.get_greater_mut().push(1);
        self.get_greater_mut().push(i64::MAX);
    }

    fn get_ids(&self) -> &Vec<PredicateId> {
        &self.get_watcher().ids
    }

    fn get_ids_mut(&mut self) -> &mut Vec<PredicateId> {
        &mut self.get_watcher_mut().ids
    }

    fn get_values(&self) -> &Vec<i32> {
        &self.get_watcher().values
    }

    fn get_values_mut(&mut self) -> &mut Vec<i32> {
        &mut self.get_watcher_mut().values
    }

    fn get_smaller(&self) -> &Vec<i64> {
        &self.get_watcher().s
    }

    fn get_smaller_mut(&mut self) -> &mut Vec<i64> {
        &mut self.get_watcher_mut().s
    }

    fn get_greater(&self) -> &Vec<i64> {
        &self.get_watcher().g
    }

    fn get_greater_mut(&mut self) -> &mut Vec<i64> {
        &mut self.get_watcher_mut().g
    }

    fn get_min_unassigned(&self) -> &StatefulInt {
        &self.get_watcher().min_unassigned
    }

    fn get_min_unassigned_mut(&mut self) -> &mut StatefulInt {
        &mut self.get_watcher_mut().min_unassigned
    }

    fn get_max_unassigned(&self) -> &StatefulInt {
        &self.get_watcher().max_unassigned
    }

    fn get_max_unassigned_mut(&mut self) -> &mut StatefulInt {
        &mut self.get_watcher_mut().max_unassigned
    }

    fn is_empty(&self) -> bool {
        self.get_watcher().values.is_empty()
    }
}

pub(crate) trait DomainWatcher: DomainWatcherInformation {
    fn get_predicate_for_value(&self, value: i32) -> Predicate;

    fn predicate_id_has_been_satisfied(
        &self,
        predicate_id: PredicateId,
        satisfied_predicates: &mut Vec<PredicateId>,
    ) {
        if predicate_id.id == u32::MAX {
            return;
        }
        info!("Satisfied: {predicate_id:?}");
        satisfied_predicates.push(predicate_id)
    }

    fn predicate_has_been_satisfied(
        &self,
        index: usize,
        satisfied_predicates: &mut Vec<PredicateId>,
    ) {
        let predicate_id = self.get_ids()[index];
        if predicate_id.id == u32::MAX {
            return;
        }
        info!(
            "Satisfied: {:?}",
            self.get_predicate_for_value(self.get_values()[index])
        );
        satisfied_predicates.push(predicate_id)
    }

    fn predicate_has_been_falsified(
        &self,
        index: usize,
        falsified_predicates: &mut Vec<PredicateId>,
    ) {
        falsified_predicates.push(self.get_ids()[index])
    }

    fn add(
        &mut self,
        value: i32,
        predicate_id: PredicateId,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        pumpkin_assert_simple!(self.get_values().len() >= 2);

        let new_index = self.get_values().len() as i64;

        let mut index_largest_value_smaller_than = i64::MAX;
        let mut largest_value_smaller_than = i32::MIN;

        let mut index_smallest_value_larger_than = i64::MAX;
        let mut smallest_value_larger_than = i32::MAX;

        let mut same_value = None;

        for index in 0..self.get_values().len() {
            let index_value = self.get_values()[index];

            if index_value == value {
                same_value = Some(index);
            }

            // First we check whether we can update the indices for the newly added id
            if index_value < value && index_value > largest_value_smaller_than {
                largest_value_smaller_than = index_value;
                index_largest_value_smaller_than = index as i64;
            }

            if index_value > value && index_value < smallest_value_larger_than {
                smallest_value_larger_than = index_value;
                index_smallest_value_larger_than = index as i64;
            }

            // Then we check whether we need to update any of the other values in the list
            if value < self.get_values()[index]
                && (self.get_smaller()[index] == i64::MAX
                    || value > self.get_values()[self.get_smaller()[index] as usize])
            {
                self.get_smaller_mut()[index] = new_index;
            }

            if value > self.get_values()[index]
                && (self.get_greater()[index] == i64::MAX
                    || value < self.get_values()[self.get_greater()[index] as usize])
            {
                self.get_greater_mut()[index] = new_index;
            }
        }

        if let Some(same_index) = same_value {
            self.get_ids_mut()[same_index] = predicate_id;
        } else {
            // We might need to update the sentinels
            if assignments.is_predicate_assigned(self.get_predicate_for_value(value)) {
                if value > self.get_values()[self.get_min_unassigned().read() as usize] {
                    self.get_min_unassigned_mut()
                        .assign(new_index, stateful_trail);
                }

                if value < self.get_values()[self.get_max_unassigned().read() as usize] {
                    self.get_max_unassigned_mut()
                        .assign(new_index, stateful_trail);
                }
            }

            self.get_values_mut().push(value);
            self.get_ids_mut().push(predicate_id);
            self.get_smaller_mut()
                .push(index_largest_value_smaller_than);
            self.get_greater_mut()
                .push(index_smallest_value_larger_than);
        }

        pumpkin_assert_simple!(
            self.get_smaller().len() == self.get_greater().len()
                && self.get_greater().len() == self.get_values().len()
                && self.get_values().len() == self.get_ids().len()
        );
    }

    fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        predicate_id: Option<PredicateId>,
    );
}
