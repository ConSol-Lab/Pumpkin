use crate::basic_types::PredicateId;
use crate::engine::conflict_analysis::SemanticMinimiser;
use crate::engine::notifications::NotificationEngine;
use crate::engine::notifications::PredicateIdAssignments;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagatorId;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::reason::StoredReason;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::proof::InferenceCode;
use crate::pumpkin_assert_simple;

pub(crate) struct PropagationContextWithTrailedValues<'a> {
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

    pub(crate) fn as_readonly(&self) -> PropagationContext<'_> {
        PropagationContext {
            assignments: self.assignments,
        }
    }
}

/// [`PropagationContext`] is passed to propagators during propagation.
/// It may be queried to retrieve information about the current variable domains such as the
/// lower-bound of a particular variable, or used to apply changes to the domain of a variable
/// e.g. set `[x >= 5]`.
///
///
/// Note that the [`PropagationContext`] is the only point of communication beween
/// the propagations and the solver during propagation.
#[derive(Clone, Copy, Debug)]
pub(crate) struct PropagationContext<'a> {
    pub assignments: &'a Assignments,
}

impl<'a> PropagationContext<'a> {
    pub(crate) fn new(assignments: &'a Assignments) -> Self {
        PropagationContext { assignments }
    }
}

#[derive(Debug)]
pub(crate) struct PropagationContextMut<'a> {
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) assignments: &'a mut Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) propagator_id: PropagatorId,
    pub(crate) semantic_minimiser: &'a mut SemanticMinimiser,
    pub(crate) notification_engine: &'a mut NotificationEngine,
    reification_literal: Option<Literal>,
}

impl<'a> PropagationContextMut<'a> {
    pub(crate) fn new(
        trailed_values: &'a mut TrailedValues,
        assignments: &'a mut Assignments,
        reason_store: &'a mut ReasonStore,
        semantic_minimiser: &'a mut SemanticMinimiser,
        notification_engine: &'a mut NotificationEngine,
        propagator_id: PropagatorId,
    ) -> Self {
        PropagationContextMut {
            trailed_values,
            assignments,
            reason_store,
            propagator_id,
            notification_engine,
            semantic_minimiser,
            reification_literal: None,
        }
    }

    pub(crate) fn get_predicate(&mut self, predicate_id: PredicateId) -> Predicate {
        self.notification_engine.get_predicate(predicate_id)
    }

    pub(crate) fn get_id(&mut self, predicate: Predicate) -> PredicateId {
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

    pub(crate) fn as_readonly(&self) -> PropagationContext<'_> {
        PropagationContext {
            assignments: self.assignments,
        }
    }

    pub(crate) fn get_decision_level(&self) -> usize {
        self.assignments.get_decision_level()
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

/// A trait which defines common methods for retrieving the [`Assignments`] and
/// [`AssignmentsPropositional`] from the structure which implements this trait.
pub trait HasAssignments {
    /// Returns the stored [`Assignments`].
    fn assignments(&self) -> &Assignments;
}

pub(crate) trait HasTrailedValues {
    fn trailed_values(&self) -> &TrailedValues;
    fn trailed_values_mut(&mut self) -> &mut TrailedValues;
}

mod private {
    use super::*;

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

    impl HasAssignments for PropagationContext<'_> {
        fn assignments(&self) -> &Assignments {
            self.assignments
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

pub(crate) trait ReadDomains: HasAssignments {
    fn is_predicate_satisfied(&self, predicate: Predicate) -> bool {
        self.assignments()
            .evaluate_predicate(predicate)
            .is_some_and(|truth_value| truth_value)
    }

    fn is_predicate_falsified(&self, predicate: Predicate) -> bool {
        self.assignments()
            .evaluate_predicate(predicate)
            .is_some_and(|truth_value| !truth_value)
    }

    fn is_literal_true(&self, literal: &Literal) -> bool {
        literal
            .get_integer_variable()
            .lower_bound(self.assignments())
            == 1
    }

    fn is_literal_false(&self, literal: &Literal) -> bool {
        literal
            .get_integer_variable()
            .upper_bound(self.assignments())
            == 0
    }

    fn is_literal_fixed(&self, literal: &Literal) -> bool {
        self.is_fixed(literal)
    }

    /// Returns the holes which were created on the current decision level.
    fn get_holes_on_current_decision_level<Var: IntegerVariable>(
        &self,
        var: &Var,
    ) -> impl Iterator<Item = i32> {
        var.get_holes_on_current_decision_level(self.assignments())
    }

    /// Returns all of the holes (currently) in the domain of `var` (including ones which were
    /// created at previous decision levels).
    fn get_holes<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32> {
        var.get_holes(self.assignments())
    }

    /// Returns `true` if the domain of the given variable is singleton.
    fn is_fixed<Var: IntegerVariable>(&self, var: &Var) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    fn lower_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.lower_bound(self.assignments())
    }

    #[allow(unused, reason = "Will be part of the API")]
    fn lower_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32 {
        var.lower_bound_at_trail_position(self.assignments(), trail_position)
    }

    fn upper_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.upper_bound(self.assignments())
    }

    #[allow(unused, reason = "Will be part of the API")]
    fn upper_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32 {
        var.upper_bound_at_trail_position(self.assignments(), trail_position)
    }

    fn contains<Var: IntegerVariable>(&self, var: &Var, value: i32) -> bool {
        var.contains(self.assignments(), value)
    }

    fn contains_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        value: i32,
        trail_position: usize,
    ) -> bool {
        var.contains_at_trail_position(self.assignments(), value, trail_position)
    }

    fn iterate_domain<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32> {
        var.iterate_domain(self.assignments())
    }
}

impl<T: HasAssignments> ReadDomains for T {}

impl PropagationContextMut<'_> {
    pub(crate) fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool> {
        self.assignments.evaluate_predicate(predicate)
    }

    /// Assign the truth-value of the given [`Predicate`] to `true` in the current partial
    /// assignment.
    ///
    /// If the truth-value is already `true`, then this is a no-op. Alternatively, if the
    /// truth-value is `false`, then a conflict is triggered and the [`EmptyDomain`] error is
    /// returned. At that point, no-more propagation should happen.
    pub(crate) fn post(
        &mut self,
        predicate: Predicate,
        reason: impl Into<Reason>,
        inference_code: InferenceCode,
    ) -> Result<(), EmptyDomain> {
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
            Err(e) => {
                let _ = slot.populate(
                    self.propagator_id,
                    build_reason(reason, self.reification_literal),
                );
                Err(e)
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
