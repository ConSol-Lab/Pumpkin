use log::info;

use crate::{
    basic_types::{PredicateId, Trail},
    containers::KeyedVec,
    engine::predicates::predicate,
    pumpkin_assert_moderate, pumpkin_assert_simple,
};

#[derive(Default, Debug, Clone)]
pub(crate) struct FaithfulnessAssignments {
    pub(crate) assignments: KeyedVec<PredicateId, Option<bool>>,
    pub(crate) trail: Trail<TrailEntry>,
    retained: Vec<TrailEntry>,
}

#[derive(Debug, Clone, Copy)]
struct TrailEntry {
    id: u32,
    level: usize,
}

impl FaithfulnessAssignments {
    pub(crate) fn grow(&mut self, value: Option<bool>, decision_level: Option<usize>) {
        let id = self.assignments.push(value);
        if value.is_some() {
            self.trail.push(TrailEntry {
                id: id.id,
                level: decision_level.unwrap(),
            })
        }
    }

    pub(crate) fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub(crate) fn synchronise(&mut self, new_decision_level: usize) {
        info!("Synchronising to {new_decision_level}");

        self.trail
            .synchronise(new_decision_level)
            .for_each(|trail_entry| {
                if trail_entry.level >= new_decision_level {
                    info!("Undo {:?}", trail_entry.id);
                    self.assignments[trail_entry.id.into()] = None
                } else {
                    self.retained.push(trail_entry);
                    info!("Not undoing {:?}", trail_entry.id)
                }
            });
        self.retained
            .drain(..)
            .for_each(|trail_entry| self.trail.push(trail_entry))
    }

    pub(crate) fn set_predicate_id(&mut self, predicate_id: PredicateId, value: bool) {
        info!(
            "Setting predicate {predicate_id:?} to {value} at decision level {}",
            self.trail.get_decision_level()
        );
        pumpkin_assert_simple!(
            self.assignments[predicate_id].is_none()
                || self.assignments[predicate_id] == Some(value),
            "{predicate_id:?} had conflicting values at decision level {}",
            self.trail.get_decision_level()
        );
        if self.assignments[predicate_id] != Some(value) {
            self.trail.push(TrailEntry {
                id: predicate_id.into(),
                level: self.trail.get_decision_level(),
            });
            self.assignments[predicate_id] = Some(value);
        }
    }

    pub(crate) fn get_entry(&self, index: usize) -> PredicateId {
        self.trail.get_trail_entry(index).id.into()
    }

    pub(crate) fn get_state(&self, predicate_id: PredicateId) -> Option<bool> {
        self.assignments[predicate_id]
    }

    #[allow(dead_code, reason = "Will be used in the future")]
    pub(crate) fn is_predicate_id_true(&self, predicate_id: PredicateId) -> bool {
        self.assignments[predicate_id].is_some_and(|value| value)
    }

    pub(crate) fn is_predicate_id_false(&self, predicate_id: PredicateId) -> bool {
        self.assignments[predicate_id].is_some_and(|value| !value)
    }

    #[allow(dead_code, reason = "Will be used in the future")]
    pub(crate) fn is_predicate_id_assigned(&self, predicate_id: PredicateId) -> bool {
        self.assignments[predicate_id].is_some()
    }

    pub(crate) fn num_entries(&self) -> usize {
        self.trail.len()
    }
}
