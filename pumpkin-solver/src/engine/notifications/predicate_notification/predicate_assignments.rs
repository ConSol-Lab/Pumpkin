use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
#[cfg(doc)]
use crate::predicates::Predicate;
use crate::pumpkin_assert_extreme;

/// A structure for (lazily) storing information concerning the value of [`Predicate`]s.
///
/// When the status of a [`Predicate`] is unknown, this structure recalculates it using the
/// [`Assignments`].
#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateIdAssignments {
    /// A trail of the [`PredicateId`]s which have been assigned.
    ///
    /// Note that this structure will not be fully accurate as it is dependent on when a
    /// [`Predicate`] is stored.
    trail: Trail<PredicateId>,
    /// The known value for each [`Predicate`] (represented by a [`PredicateId`]).
    predicate_values: KeyedVec<PredicateId, PredicateValue>,
    /// The [`Predicate`]s which are currently known to be falsified used for notification.
    ///
    /// Note that this structure does not contain _all_ of the falsified [`Predicate`]s but rather
    /// the one which have been falsified since the last round of notifications.
    falsified_predicates: Vec<PredicateId>,
    /// The [`Predicate`]s which are currently known to be satisfied used for notification.
    ///
    /// Note that this structure does not contain _all_ of the satisfied [`Predicate`]s but rather
    /// the one which have been satisfied since the last round of notifications.
    satisfied_predicates: Vec<PredicateId>,
}

/// The current value of a [`Predicate`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum PredicateValue {
    /// The [`Predicate`] is currently unassigned.
    Unassigned,
    /// The [`Predicate`] is currently true.
    AssignedTrue,
    /// The [`Predicate`] is currently false.
    AssignedFalse,
    /// The [`Predicate`] is currently unknown.
    ///
    /// If the value of a [`Predicate`] with this value is retrieved then it will be recalculated.
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
    /// [`PredicateValue`].
    ///
    /// If the [`Predicate`] is true/false then it will be checked whether this information is
    /// already stored; if it is not, then it will also store the [`Predicate`] for notification.
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

    /// Recalculates the value of a [`Predicate`] *if* it is unknown.
    fn update_if_unknown(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) {
        if self.predicate_values[predicate_id].is_unknown() {
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
    }

    /// Returns whether the [`Predicate`] is currently satsified.
    ///
    /// If the value of the [`Predicate`] is unknown then it will be recalculated using the
    /// provided [`Assignments`].
    pub(crate) fn is_satisfied(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> bool {
        self.update_if_unknown(predicate_id, assignments, predicate_id_generator);

        self.predicate_values[predicate_id].is_satisified()
    }

    /// Returns whether the [`Predicate`] is currently falsified.
    ///
    /// If the value of the [`Predicate`] is unknown then it will be recalculated using the
    /// provided [`Assignments`].
    pub(crate) fn is_falsified(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> bool {
        self.update_if_unknown(predicate_id, assignments, predicate_id_generator);

        self.predicate_values[predicate_id].is_falsified()
    }

    pub(crate) fn synchronise(&mut self, new_decision_level: usize) {
        // We also need to clear the stored updated predicates; if this is not done, then it can be
        // the case that a predicate is erroneously said to be satisfied/falsified while it is not
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

    /// Returns whether the status of the [`Predicate`] is unknown.
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
