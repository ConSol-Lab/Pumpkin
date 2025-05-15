use crate::engine::conflict_analysis::SemanticMinimiser;
use crate::engine::notifications::PredicateNotifier;
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
use crate::pumpkin_assert_simple;

pub(crate) struct PropagationContextWithTrailedValues<'a> {
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) assignments: &'a Assignments,
}

impl<'a> PropagationContextWithTrailedValues<'a> {
    pub(crate) fn new(trailed_values: &'a mut TrailedValues, assignments: &'a Assignments) -> Self {
        Self {
            trailed_values,
            assignments,
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
    pub(crate) predicate_notifier: &'a mut PredicateNotifier,
    reification_literal: Option<Literal>,
}

impl<'a> PropagationContextMut<'a> {
    pub(crate) fn new(
        trailed_values: &'a mut TrailedValues,
        assignments: &'a mut Assignments,
        reason_store: &'a mut ReasonStore,
        semantic_minimiser: &'a mut SemanticMinimiser,
        predicate_notifier: &'a mut PredicateNotifier,
        propagator_id: PropagatorId,
    ) -> Self {
        PropagationContextMut {
            trailed_values,
            assignments,
            reason_store,
            propagator_id,
            predicate_notifier,
            semantic_minimiser,
            reification_literal: None,
        }
    }

    /// Apply a reification literal to all the explanations that are passed to the context.
    pub(crate) fn with_reification(&mut self, reification_literal: Literal) {
        pumpkin_assert_simple!(
            self.reification_literal.is_none(),
            "cannot reify an already reified propagation context"
        );

        self.reification_literal = Some(reification_literal);
    }

    fn build_reason(&self, reason: Reason) -> StoredReason {
        match reason {
            Reason::Eager(mut conjunction) => {
                conjunction.extend(
                    self.reification_literal
                        .iter()
                        .map(|lit| lit.get_true_predicate()),
                );
                StoredReason::Eager(conjunction)
            }
            Reason::DynamicLazy(code) => {
                if let Some(reification_literal) = self.reification_literal {
                    StoredReason::ReifiedLazy(reification_literal, code)
                } else {
                    StoredReason::DynamicLazy(code)
                }
            }
        }
    }

    pub(crate) fn as_trailed_readonly(&mut self) -> PropagationContextWithTrailedValues {
        PropagationContextWithTrailedValues {
            trailed_values: self.trailed_values,
            assignments: self.assignments,
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
        self.is_predicate_satisfied(literal.get_true_predicate())
    }

    fn is_literal_false(&self, literal: &Literal) -> bool {
        self.is_predicate_satisfied(literal.get_false_predicate())
    }

    fn is_literal_fixed(&self, literal: &Literal) -> bool {
        self.is_fixed(literal)
    }

    /// Returns `true` if the domain of the given variable is singleton.
    fn is_fixed<Var: IntegerVariable>(&self, var: &Var) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    fn lower_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.lower_bound(self.assignments())
    }

    fn upper_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.upper_bound(self.assignments())
    }

    fn contains<Var: IntegerVariable>(&self, var: &Var, value: i32) -> bool {
        var.contains(self.assignments(), value)
    }

    fn iterate_domain<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32> {
        var.iterate_domain(self.assignments())
    }
}

impl<T: HasAssignments> ReadDomains for T {}

impl PropagationContextMut<'_> {
    pub(crate) fn remove<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        value: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if var.contains(self.assignments, value) {
            let reason = self.build_reason(reason.into());
            let reason_ref = self.reason_store.push(self.propagator_id, reason);
            return var.remove(self.assignments, value, Some(reason_ref));
        }
        Ok(())
    }

    pub(crate) fn set_upper_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound < var.upper_bound(self.assignments) {
            let reason = self.build_reason(reason.into());
            let reason_ref = self.reason_store.push(self.propagator_id, reason);
            return var.set_upper_bound(self.assignments, bound, Some(reason_ref));
        }
        Ok(())
    }

    pub(crate) fn set_lower_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound > var.lower_bound(self.assignments) {
            let reason = self.build_reason(reason.into());
            let reason_ref = self.reason_store.push(self.propagator_id, reason);
            return var.set_lower_bound(self.assignments, bound, Some(reason_ref));
        }

        Ok(())
    }

    pub(crate) fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool> {
        self.assignments.evaluate_predicate(predicate)
    }

    pub(crate) fn post_predicate<R: Into<Reason>>(
        &mut self,
        predicate: Predicate,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        match predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.set_lower_bound(&domain_id, lower_bound, reason),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.set_upper_bound(&domain_id, upper_bound, reason),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.remove(&domain_id, not_equal_constant, reason),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => {
                if self
                    .assignments
                    .is_value_in_domain(domain_id, equality_constant)
                    && !self.assignments.is_domain_assigned(&domain_id)
                {
                    let reason = self.build_reason(reason.into());
                    let reason = self.reason_store.push(self.propagator_id, reason);
                    self.assignments
                        .make_assignment(domain_id, equality_constant, Some(reason))?;
                }

                Ok(())
            }
        }
    }

    pub(crate) fn assign_literal<R: Into<Reason> + Clone>(
        &mut self,
        boolean: &Literal,
        truth_value: bool,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        match truth_value {
            true => self.set_lower_bound(boolean, 1, reason),
            false => self.set_upper_bound(boolean, 0, reason),
        }
    }
}
