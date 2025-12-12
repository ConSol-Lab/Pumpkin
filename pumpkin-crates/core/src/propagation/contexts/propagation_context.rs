use crate::basic_types::PredicateId;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::EmptyDomainConflict;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::engine::notifications::NotificationEngine;
use crate::engine::notifications::PredicateIdAssignments;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::reason::StoredReason;
use crate::engine::variables::Literal;
use crate::proof::InferenceCode;
use crate::propagation::Domains;
#[cfg(doc)]
use crate::propagation::Propagator;
use crate::propagation::PropagatorId;
#[cfg(doc)]
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_simple;

#[derive(Debug)]
pub struct PropagationContextWithTrailedValues<'a> {
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) assignments: &'a Assignments,
    pub(crate) predicate_id_assignments: &'a PredicateIdAssignments,
}

impl<'a> PropagationContextWithTrailedValues<'a> {
    pub(crate) fn new(
        trailed_values: &'a mut TrailedValues,
        assignments: &'a Assignments,
        predicate_id_assignments: &'a PredicateIdAssignments,
    ) -> Self {
        Self {
            trailed_values,
            assignments,
            predicate_id_assignments,
        }
    }

    pub(crate) fn domains(&self) -> Domains<'_> {
        Domains {
            assignments: self.assignments,
        }
    }
}

/// Provides information about the state of the solver to a propagator.
///
/// Domains can be read through the implementation of [`ReadDomains`], and changes to the state can
/// be made via [`PropagationContextMut::post`].
#[derive(Debug)]
pub struct PropagationContextMut<'a> {
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) assignments: &'a mut Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) propagator_id: PropagatorId,
    pub(crate) notification_engine: &'a mut NotificationEngine,
    reification_literal: Option<Literal>,
}

impl<'a> PropagationContextMut<'a> {
    pub(crate) fn new(
        trailed_values: &'a mut TrailedValues,
        assignments: &'a mut Assignments,
        reason_store: &'a mut ReasonStore,
        notification_engine: &'a mut NotificationEngine,
        propagator_id: PropagatorId,
    ) -> Self {
        PropagationContextMut {
            trailed_values,
            assignments,
            reason_store,
            propagator_id,
            notification_engine,
            reification_literal: None,
        }
    }

    /// Register the propagator to be enqueued when the provided [`Predicate`] becomes true.
    ///
    /// Returns the [`PredicateId`] assigned to the provided predicate, which will be provided
    /// to [`Propagator::notify_predicate_id_satisfied`].
    pub fn register_predicate(&mut self, predicate: Predicate) -> PredicateId {
        self.notification_engine.watch_predicate(
            predicate,
            self.propagator_id,
            self.trailed_values,
            self.assignments,
        )
    }

    /// Get the [`Predicate`] for a given [`PredicateId`].
    pub fn get_predicate(&mut self, predicate_id: PredicateId) -> Predicate {
        self.notification_engine.get_predicate(predicate_id)
    }

    /// Get a [`PredicateId`] for the given [`Predicate`].
    ///
    /// If no ID exists, one will be created.
    pub fn get_id(&mut self, predicate: Predicate) -> PredicateId {
        self.notification_engine.get_id(predicate)
    }

    /// Apply a reification literal to all the explanations that are passed to the context.
    pub(crate) fn with_reification(&mut self, reification_literal: Literal) {
        pumpkin_assert_simple!(
            self.reification_literal.is_none(),
            "cannot reify an already reified propagation context"
        );

        self.reification_literal = Some(reification_literal);
    }

    pub(crate) fn as_trailed_readonly(&mut self) -> PropagationContextWithTrailedValues<'_> {
        PropagationContextWithTrailedValues {
            trailed_values: self.trailed_values,
            assignments: self.assignments,
            predicate_id_assignments: self.notification_engine.predicate_id_assignments(),
        }
    }

    /// Get the current domain information.
    pub fn domains(&self) -> Domains<'_> {
        Domains {
            assignments: self.assignments,
        }
    }

    pub(crate) fn get_checkpoint(&self) -> usize {
        self.assignments.get_checkpoint()
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// satisfied.
    pub(crate) fn is_predicate_id_falsified(&mut self, predicate_id: PredicateId) -> bool {
        self.notification_engine
            .is_predicate_id_falsified(predicate_id, self.assignments)
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// satisfied.
    pub(crate) fn is_predicate_id_satisfied(&mut self, predicate_id: PredicateId) -> bool {
        self.notification_engine
            .is_predicate_id_satisfied(predicate_id, self.assignments)
    }

    /// Returns the number of [`PredicateId`]s.
    pub(crate) fn num_predicate_ids(&self) -> usize {
        self.notification_engine.num_predicate_ids()
    }
}

pub(crate) trait HasTrailedValues {
    fn trailed_values(&self) -> &TrailedValues;
    fn trailed_values_mut(&mut self) -> &mut TrailedValues;
}

mod private {
    use super::*;
    use crate::propagation::HasAssignments;

    impl HasTrailedValues for PropagationContextWithTrailedValues<'_> {
        fn trailed_values(&self) -> &TrailedValues {
            self.trailed_values
        }

        fn trailed_values_mut(&mut self) -> &mut TrailedValues {
            self.trailed_values
        }
    }

    impl HasTrailedValues for PropagationContextMut<'_> {
        fn trailed_values(&self) -> &TrailedValues {
            self.trailed_values
        }

        fn trailed_values_mut(&mut self) -> &mut TrailedValues {
            self.trailed_values
        }
    }

    impl HasAssignments for PropagationContextMut<'_> {
        fn assignments(&self) -> &Assignments {
            self.assignments
        }
    }

    impl HasAssignments for PropagationContextWithTrailedValues<'_> {
        fn assignments(&self) -> &Assignments {
            self.assignments
        }
    }
}

pub(crate) trait ManipulateTrailedValues: HasTrailedValues {
    fn new_trailed_integer(&mut self, initial_value: i64) -> TrailedInteger {
        self.trailed_values_mut().grow(initial_value)
    }

    fn value(&self, trailed_integer: TrailedInteger) -> i64 {
        self.trailed_values().read(trailed_integer)
    }

    fn add_assign(&mut self, trailed_integer: TrailedInteger, addition: i64) {
        self.trailed_values_mut()
            .add_assign(trailed_integer, addition);
    }

    fn assign(&mut self, trailed_integer: TrailedInteger, value: i64) {
        self.trailed_values_mut().assign(trailed_integer, value);
    }
}

impl<T: HasTrailedValues> ManipulateTrailedValues for T {}

impl PropagationContextMut<'_> {
    /// Assign the truth-value of the given [`Predicate`] to `true` in the current partial
    /// assignment.
    ///
    /// If the truth-value is already `true`, then this is a no-op. Alternatively, if the
    /// truth-value is `false`, then a conflict is triggered and the [`EmptyDomain`] error is
    /// returned. At that point, no-more propagation should happen.
    pub fn post(
        &mut self,
        predicate: Predicate,
        reason: impl Into<Reason>,
        inference_code: InferenceCode,
    ) -> Result<(), EmptyDomainConflict> {
        let slot = self.reason_store.new_slot();

        let modification_result = self.assignments.post_predicate(
            predicate,
            Some((slot.reason_ref(), inference_code)),
            self.notification_engine,
        );

        match modification_result {
            Ok(false) => Ok(()),
            Ok(true) => {
                let _ = slot.populate(
                    self.propagator_id,
                    build_reason(reason, self.reification_literal),
                );
                Ok(())
            }
            Err(EmptyDomain) => {
                let _ = slot.populate(
                    self.propagator_id,
                    build_reason(reason, self.reification_literal),
                );
                let (trigger_predicate, trigger_reason, trigger_inference_code) =
                    self.assignments.remove_last_trail_element();

                Err(EmptyDomainConflict {
                    trigger_predicate,
                    trigger_reason,
                    trigger_inference_code,
                })
            }
        }
    }
}

fn build_reason(reason: impl Into<Reason>, reification_literal: Option<Literal>) -> StoredReason {
    match reason.into() {
        Reason::Eager(mut conjunction) => {
            conjunction.extend(
                reification_literal
                    .iter()
                    .map(|lit| lit.get_true_predicate()),
            );
            StoredReason::Eager(conjunction)
        }
        Reason::DynamicLazy(code) => StoredReason::DynamicLazy(code),
    }
}
