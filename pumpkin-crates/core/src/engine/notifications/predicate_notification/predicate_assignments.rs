#[cfg(doc)]
use super::predicate_tracker::PredicateTracker;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
#[cfg(doc)]
use crate::predicates::Predicate;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;

/// A structure for (lazily) storing information concerning the value of [`Predicate`]s.
///
/// When the status of a [`Predicate`] is unknown, this structure recalculates it using the
/// [`Assignments`].
///
/// It is also responsible for storing the updates which have occurred since the last round of
/// notifications.
#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateIdAssignments {
    /// A trail of the [`PredicateId`]s which have been assigned.
    ///
    /// Note that this structure will not be fully accurate as it is dependent on when a
    /// [`Predicate`] is stored.
    trail: Trail<PredicateId>,
    /// A trail of the [`PredicateId`]s which were stored after they had been assigned, see
    /// [`PredicateIdAssignments::store_assigned_predicate`].
    ///
    /// Such a predicate can be assigned at an earlier checkpoint than the one at which it is on
    /// this trail, so it is only reset when backtracking undoes its assignment.
    assigned_before_stored_trail: Trail<PredicateId>,
    /// The known value for each [`Predicate`] (represented by a [`PredicateId`]).
    predicate_values: KeyedVec<PredicateId, PredicateValue>,
    /// The [`Predicate`]s which are currently known to be satisfied used for notification.
    ///
    /// Note that this structure does not contain _all_ of the satisfied [`Predicate`]s but rather
    /// the one which have been satisfied since the last round of notifications.
    satisfied_predicates: Vec<PredicateId>,
}

/// The current value of a [`Predicate`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum PredicateValue {
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
}

impl PredicateIdAssignments {
    fn predicate_ids(&self) -> impl Iterator<Item = PredicateId> + '_ {
        self.predicate_values.keys()
    }

    /// Returns the satisfied predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_satisfied_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.satisfied_predicates.drain(..)
    }

    pub(crate) fn new_checkpoint(&mut self) {
        self.trail.new_checkpoint();
        self.assigned_before_stored_trail.new_checkpoint();
    }

    /// Stores the value of a predicate which was already assigned when it started to be tracked,
    /// without storing it for notification.
    ///
    /// Otherwise, the [`PredicateTracker`] would report the predicate as satisfied at a later
    /// update, and again after each backtrack which does not undo its assignment.
    /// If its value was already stored when it was queried, then the notification stored by that
    /// query is removed as well.
    ///
    /// For example, let `[x >= 6]` be posted at checkpoint 1, and let `[x >= 5]` start to be
    /// tracked at checkpoint 3.
    /// Then `[x >= 5]` is stored as true, and it stays true until backtracking to the root undoes
    /// `[x >= 6]`.
    pub(crate) fn store_assigned_predicate(
        &mut self,
        predicate_id: PredicateId,
        value: PredicateValue,
    ) {
        self.predicate_values
            .accomodate(predicate_id, PredicateValue::Unknown);

        if self.predicate_values[predicate_id].is_unknown() {
            self.predicate_values[predicate_id] = value;
        } else {
            // The value was stored when the predicate was queried, which can be later than when
            // it was assigned; that store is on `trail` and stored the predicate for notification.
            pumpkin_assert_simple!(self.predicate_values[predicate_id] == value);
            self.satisfied_predicates
                .retain(|&satisfied_predicate| satisfied_predicate != predicate_id);
        }

        self.assigned_before_stored_trail.push(predicate_id);
    }

    /// Stores a predicate in the [`PredicateIdAssignments`] with its corresponding
    /// [`PredicateValue`].
    ///
    /// If the [`Predicate`] is true/false then it will be checked whether this information is
    /// already stored; if it is not, then it will also store the [`Predicate`] for notification.
    pub(crate) fn store_predicate(&mut self, predicate_id: PredicateId, value: PredicateValue) {
        // First we make space for it if we have not seen the predicate yet
        self.predicate_values
            .accomodate(predicate_id, PredicateValue::Unknown);

        pumpkin_assert_extreme!(
            self.predicate_values[predicate_id] == PredicateValue::Unknown
                || self.predicate_values[predicate_id] == value,
            "Expected {:?} to be either unknown/untracked or for it to equal {value:?} for {predicate_id:?}",
            self.predicate_values[predicate_id]
        );
        // If we already have the value in the cache then we do not do anything
        if self.predicate_values[predicate_id] != value {
            // If it is not the same as what is already in the cache then we need to store it and
            // (potentially) update the predicates which should be notified
            if value == PredicateValue::AssignedTrue {
                self.satisfied_predicates.push(predicate_id)
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
        predicate_id_generator: &PredicateIdGenerator,
    ) {
        // First we make space for it if we have not seen the predicate yet
        if predicate_id.index() >= self.predicate_values.len() {
            self.predicate_values
                .resize(predicate_id.index() + 1, PredicateValue::Unknown);
        }

        if self.predicate_values[predicate_id].is_unknown() {
            // First we calculate the current value of the predicate in the assignments structure
            let predicate = predicate_id_generator.get_predicate(predicate_id);
            let value = match assignments.evaluate_predicate(predicate) {
                Some(satisfied) => {
                    if satisfied {
                        PredicateValue::AssignedTrue
                    } else {
                        PredicateValue::AssignedFalse
                    }
                }
                None => PredicateValue::Unknown,
            };
            // Then we store it in the cache
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
        predicate_id_generator: &PredicateIdGenerator,
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
        predicate_id_generator: &PredicateIdGenerator,
    ) -> bool {
        self.update_if_unknown(predicate_id, assignments, predicate_id_generator);

        self.predicate_values[predicate_id].is_falsified()
    }

    pub(crate) fn evaluate(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
        predicate_id_generator: &PredicateIdGenerator,
    ) -> Option<bool> {
        self.update_if_unknown(predicate_id, assignments, predicate_id_generator);

        match self.predicate_values[predicate_id] {
            PredicateValue::AssignedTrue => Some(true),
            PredicateValue::AssignedFalse => Some(false),
            PredicateValue::Unknown => None,
        }
    }

    /// Backtracks to `new_checkpoint`; `assignments` should already have been backtracked.
    pub(crate) fn synchronise(
        &mut self,
        new_checkpoint: usize,
        assignments: &Assignments,
        predicate_id_generator: &PredicateIdGenerator,
    ) {
        // We also need to clear the stored updated predicates; if this is not done, then it can be
        // the case that a predicate is erroneously said to be satisfied/falsified while it is not
        self.satisfied_predicates.clear();

        self.trail
            .synchronise(new_checkpoint)
            .for_each(|predicate_id| {
                // If the predicate id is unassigned then backtracking will not change anything;
                // this is more of a sanity check since it should not be on the trail if it is
                // unassigned
                self.predicate_values[predicate_id] = PredicateValue::Unknown
            });

        // The predicates are collected, since predicates which are still assigned are added to
        // the trail again.
        let backtracked_predicates = self
            .assigned_before_stored_trail
            .synchronise(new_checkpoint)
            .collect::<Vec<_>>();
        for predicate_id in backtracked_predicates {
            let predicate = predicate_id_generator.get_predicate(predicate_id);
            if let Some(is_true) = assignments.evaluate_predicate(predicate) {
                // The predicate was assigned before the checkpoint at which it was stored.
                //
                // Its value is stored again, since backtracking `trail` can have reset it.
                self.predicate_values[predicate_id] = if is_true {
                    PredicateValue::AssignedTrue
                } else {
                    PredicateValue::AssignedFalse
                };
                self.assigned_before_stored_trail.push(predicate_id);
            } else {
                self.predicate_values[predicate_id] = PredicateValue::Unknown;
            }
        }
    }

    /// Returns whether the status of the [`Predicate`] is unknown.
    pub(crate) fn is_unknown(&self, predicate_id: PredicateId) -> bool {
        predicate_id.index() >= self.predicate_values.len()
            || self.predicate_values[predicate_id].is_unknown()
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
        predicate_to_id: &PredicateIdGenerator,
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
                        None => PredicateValue::Unknown,
                    }
                };
                self.store_predicate(predicate_id, value);
            });
    }
}
