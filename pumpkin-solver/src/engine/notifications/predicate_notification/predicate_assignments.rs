use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
use crate::pumpkin_assert_extreme;

#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateIdAssignments {
    trail: Trail<PredicateId>,
    predicate_values: KeyedVec<PredicateId, PredicateValue>,
    falsified_predicates: Vec<PredicateId>,
    satisfied_predicates: Vec<PredicateId>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum PredicateValue {
    Unassigned,
    AssignedTrue,
    AssignedFalse,
    Unknown,
}

impl PredicateValue {
    fn is_satisified(&self) -> bool {
        matches!(self, PredicateValue::AssignedTrue)
    }

    fn is_falsified(&self) -> bool {
        matches!(self, PredicateValue::AssignedFalse)
    }

    fn is_unknown(&self) -> bool {
        matches!(self, PredicateValue::Unknown)
    }

    fn is_unassigned(&self) -> bool {
        matches!(self, PredicateValue::Unassigned)
    }
}

impl PredicateIdAssignments {
    fn predicate_ids(&self) -> impl Iterator<Item = PredicateId> + '_ {
        self.predicate_values.keys()
    }

    /// Returns the falsified predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_falsified_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.falsified_predicates.drain(..)
    }

    /// Returns the satisfied predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_satisfied_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.satisfied_predicates.drain(..)
    }

    pub(crate) fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    /// Stores a predicate in the [`PredicateIdAssignments`] with its corresponding
    /// [`PredicateIdInfo`].
    pub(crate) fn store_predicate(&mut self, predicate_id: PredicateId, value: PredicateValue) {
        while predicate_id.index() >= self.predicate_values.len() {
            let _ = self.predicate_values.push(PredicateValue::Unknown);
        }
        pumpkin_assert_extreme!(
            self.predicate_values[predicate_id] == PredicateValue::Unknown
                || self.predicate_values[predicate_id] == PredicateValue::Unassigned
                || self.predicate_values[predicate_id] == value,
            "Expected {:?} to be either unknown/untracked or for it to equal {value:?} for {predicate_id:?}",
            self.predicate_values[predicate_id]
        );
        if self.predicate_values[predicate_id] != value {
            match value {
                PredicateValue::AssignedTrue => self.satisfied_predicates.push(predicate_id),
                PredicateValue::AssignedFalse => self.falsified_predicates.push(predicate_id),
                _ => {}
            }
            self.predicate_values[predicate_id] = value;
            self.trail.push(predicate_id)
        }
    }

    fn update_unknown(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) {
        let predicate = predicate_id_generator.get_predicate(predicate_id);
        let value = match assignments.evaluate_predicate(predicate) {
            Some(satisfied) => {
                if satisfied {
                    PredicateValue::AssignedTrue
                } else {
                    PredicateValue::AssignedFalse
                }
            }
            None => PredicateValue::Unassigned,
        };
        self.store_predicate(predicate_id, value);
    }

    pub(crate) fn is_satisfied(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> bool {
        if self.predicate_values[predicate_id].is_unknown() {
            self.update_unknown(predicate_id, assignments, predicate_id_generator);
        }
        self.predicate_values[predicate_id].is_satisified()
    }

    pub(crate) fn is_falsified(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> bool {
        if self.predicate_values[predicate_id].is_unknown() {
            self.update_unknown(predicate_id, assignments, predicate_id_generator);
        }
        self.predicate_values[predicate_id].is_falsified()
    }

    pub(crate) fn synchronise(&mut self, new_decision_level: usize) {
        self.satisfied_predicates.clear();
        self.falsified_predicates.clear();
        self.trail
            .synchronise(new_decision_level)
            .for_each(|predicate_id| {
                // If the predicate id is unassigned then backtracking will not change anything;
                // this is more of a sanity check since it should not be on the trail if it is
                // unassigned
                if !self.predicate_values[predicate_id].is_unassigned() {
                    self.predicate_values[predicate_id] = PredicateValue::Unknown
                }
            })
    }

    pub(crate) fn is_unknown(&self, predicate_id: PredicateId) -> bool {
        self.predicate_values[predicate_id].is_unknown()
    }

    pub(crate) fn debug_empty_clone(&self) -> Self {
        let mut predicate_id_assignments = PredicateIdAssignments::default();
        for predicate_id in self.predicate_ids() {
            predicate_id_assignments.store_predicate(predicate_id, PredicateValue::Unknown);
        }
        predicate_id_assignments
    }

    pub(crate) fn debug_create_from_assignments(
        &mut self,
        assignments: &Assignments,
        predicate_to_id: &mut PredicateIdGenerator,
    ) {
        self.predicate_ids()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|predicate_id| {
                let predicate = predicate_to_id.get_predicate(predicate_id);

                let value = if self.is_unknown(predicate_id) {
                    PredicateValue::Unknown
                } else {
                    match assignments.evaluate_predicate(predicate) {
                        Some(assigned) => {
                            if assigned {
                                PredicateValue::AssignedTrue
                            } else {
                                PredicateValue::AssignedFalse
                            }
                        }
                        None => PredicateValue::Unassigned,
                    }
                };
                self.store_predicate(predicate_id, value);
            });
    }
}
