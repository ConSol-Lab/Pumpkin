use crate::basic_types::PredicateId;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;

#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateIdAssignments {
    trail: Trail<PredicateIdTrailEntry>,
    domains: KeyedVec<PredicateId, PredicateIdInfo>,
    falsified_predicates: Vec<PredicateId>,
    satisfied_predicates: Vec<PredicateId>,
}

#[derive(Clone, Debug)]
struct PredicateIdTrailEntry {
    id: PredicateId,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum PredicateIdInfo {
    Unassigned,
    AssignedTrue,
    AssignedFalse,
    Untracked,
}

impl PredicateIdInfo {
    fn is_satisified(&self) -> bool {
        matches!(self, PredicateIdInfo::AssignedTrue)
    }

    fn is_falsified(&self) -> bool {
        matches!(self, PredicateIdInfo::AssignedFalse)
    }

    fn is_unassigned(&self) -> bool {
        matches!(self, PredicateIdInfo::Unassigned)
    }

    fn is_untracked(&self) -> bool {
        matches!(self, PredicateIdInfo::Untracked)
    }
}

impl PredicateIdAssignments {
    pub(crate) fn predicate_ids(&self) -> impl Iterator<Item = PredicateId> + '_ {
        self.domains.keys()
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

    pub(crate) fn store_predicate(&mut self, predicate_id: PredicateId, value: PredicateIdInfo) {
        while predicate_id.index() >= self.domains.len() {
            let _ = self.domains.push(PredicateIdInfo::Untracked);
        }
        pumpkin_assert_extreme!(
            self.domains[predicate_id] == PredicateIdInfo::Untracked
                || self.domains[predicate_id] == PredicateIdInfo::Unassigned
                || self.domains[predicate_id] == value,
            "Expected {:?} to be either untracked or for it to equal {value:?}",
            self.domains[predicate_id]
        );
        if self.domains[predicate_id] != value {
            match value {
                PredicateIdInfo::AssignedTrue => self.satisfied_predicates.push(predicate_id),
                PredicateIdInfo::AssignedFalse => self.falsified_predicates.push(predicate_id),
                _ => {}
            }
            self.domains[predicate_id] = value;
            self.trail.push(PredicateIdTrailEntry { id: predicate_id })
        }
    }

    pub(crate) fn is_satisfied(&self, predicate_id: PredicateId) -> bool {
        pumpkin_assert_simple!(!self.domains[predicate_id].is_untracked());
        self.domains[predicate_id].is_satisified()
    }

    pub(crate) fn is_falsified(&self, predicate_id: PredicateId) -> bool {
        pumpkin_assert_simple!(!self.domains[predicate_id].is_untracked());
        self.domains[predicate_id].is_falsified()
    }

    pub(crate) fn is_unassigned(&self, predicate_id: PredicateId) -> bool {
        pumpkin_assert_simple!(!self.domains[predicate_id].is_untracked());
        self.domains[predicate_id].is_unassigned()
    }

    pub(crate) fn is_untracked(&self, predicate_id: PredicateId) -> bool {
        self.domains[predicate_id].is_untracked()
    }

    pub(crate) fn synchronise(&mut self, new_decision_level: usize) {
        self.trail
            .synchronise(new_decision_level)
            .for_each(|entry| self.domains[entry.id] = PredicateIdInfo::Unassigned);
    }
}
