use super::PropagatorId;
use crate::basic_types::ConstraintReference;
use crate::basic_types::Inconsistency;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagatorVariable;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::EmptyDomain;

/// [`PropagationContext`] is passed to propagators during propagation.
/// It may be queried to retrieve information about the current variable domains such as the
/// lower-bound of a particular variable, or used to apply changes to the domain of a variable
/// e.g. set `[x >= 5]`.
///
///
/// Note that the [`PropagationContext`] is the only point of communication beween
/// the propagations and the solver during propagation.
#[derive(Debug)]
pub struct PropagationContext<'a> {
    assignments_integer: &'a AssignmentsInteger,
    assignments_propositional: &'a AssignmentsPropositional,
}

impl<'a> PropagationContext<'a> {
    pub fn new(
        assignments_integer: &'a AssignmentsInteger,
        assignments_propositional: &'a AssignmentsPropositional,
    ) -> Self {
        PropagationContext {
            assignments_integer,
            assignments_propositional,
        }
    }
}

#[derive(Debug)]
pub struct PropagationContextMut<'a> {
    assignments_integer: &'a mut AssignmentsInteger,
    reason_store: &'a mut ReasonStore,
    assignments_propositional: &'a mut AssignmentsPropositional,
    propagator: PropagatorId,
}

impl<'a> PropagationContextMut<'a> {
    pub fn new(
        assignments_integer: &'a mut AssignmentsInteger,
        reason_store: &'a mut ReasonStore,
        assignments_propositional: &'a mut AssignmentsPropositional,
        propagator: PropagatorId,
    ) -> Self {
        PropagationContextMut {
            assignments_integer,
            reason_store,
            assignments_propositional,
            propagator,
        }
    }
}

/// A trait which defines common methods for retrieving the [`AssignmentsInteger`] and
/// [`AssignmentsPropositional`] from the structure which implements this trait.
pub trait HasAssignments {
    /// Returns the stored [`AssignmentsInteger`].
    fn assignments_integer(&self) -> &AssignmentsInteger;

    /// Returns the stored [`AssignmentsPropositional`].
    fn assignments_propositional(&self) -> &AssignmentsPropositional;
}

mod private {
    use super::*;

    impl HasAssignments for PropagationContext<'_> {
        fn assignments_integer(&self) -> &AssignmentsInteger {
            self.assignments_integer
        }

        fn assignments_propositional(&self) -> &AssignmentsPropositional {
            self.assignments_propositional
        }
    }

    impl HasAssignments for PropagationContextMut<'_> {
        fn assignments_integer(&self) -> &AssignmentsInteger {
            self.assignments_integer
        }

        fn assignments_propositional(&self) -> &AssignmentsPropositional {
            self.assignments_propositional
        }
    }
}

pub(crate) trait ReadDomains: HasAssignments {
    fn is_literal_fixed(&self, var: &PropagatorVariable<Literal>) -> bool {
        self.assignments_propositional()
            .is_literal_assigned(var.inner)
    }

    fn is_literal_true(&self, var: &PropagatorVariable<Literal>) -> bool {
        self.assignments_propositional()
            .is_literal_assigned_true(var.inner)
    }

    fn is_literal_false(&self, var: &PropagatorVariable<Literal>) -> bool {
        self.assignments_propositional()
            .is_literal_assigned_false(var.inner)
    }

    /// Returns `true` if the domain of the given variable is singleton.
    fn is_fixed<Var: IntegerVariable>(&self, var: &PropagatorVariable<Var>) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    fn lower_bound<Var: IntegerVariable>(&self, var: &PropagatorVariable<Var>) -> i32 {
        var.inner.lower_bound(self.assignments_integer())
    }

    fn upper_bound<Var: IntegerVariable>(&self, var: &PropagatorVariable<Var>) -> i32 {
        var.inner.upper_bound(self.assignments_integer())
    }

    fn contains<Var: IntegerVariable>(&self, var: &PropagatorVariable<Var>, value: i32) -> bool {
        var.inner.contains(self.assignments_integer(), value)
    }

    fn describe_domain<Var: IntegerVariable>(
        &self,
        var: &PropagatorVariable<Var>,
    ) -> Vec<Predicate> {
        var.inner.describe_domain(self.assignments_integer())
    }
}

impl<T: HasAssignments> ReadDomains for T {}

impl PropagationContextMut<'_> {
    pub fn remove<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if var.inner.contains(self.assignments_integer, value) {
            let reason = self.reason_store.push(self.propagator, reason.into());
            return var
                .inner
                .remove(self.assignments_integer, value, Some(reason));
        }
        Ok(())
    }

    pub fn set_upper_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound < var.inner.upper_bound(self.assignments_integer) {
            let reason = self.reason_store.push(self.propagator, reason.into());
            return var
                .inner
                .set_upper_bound(self.assignments_integer, bound, Some(reason));
        }
        Ok(())
    }

    pub fn set_lower_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound > var.inner.lower_bound(self.assignments_integer) {
            let reason = self.reason_store.push(self.propagator, reason.into());
            return var
                .inner
                .set_lower_bound(self.assignments_integer, bound, Some(reason));
        }
        Ok(())
    }

    pub fn apply_integer_predicate<R: Into<Reason>>(
        &mut self,
        integer_predicate: IntegerPredicate,
        _reason: R,
    ) -> Result<(), EmptyDomain> {
        if self
            .assignments_integer
            .does_integer_predicate_hold(integer_predicate)
        {
            return Ok(());
        }
        todo!();
        // match integer_predicate {
        // IntegerPredicate::LowerBound {
        // domain_id,
        // lower_bound,
        // } => self.set_lower_bound(domain_id, lower_bound, reason),
        // IntegerPredicate::UpperBound {
        // domain_id,
        // upper_bound,
        // } => self.tighten_upper_bound(domain_id, upper_bound, reason),
        // IntegerPredicate::NotEqual {
        // domain_id,
        // not_equal_constant,
        // } => self.remove_value_from_domain(domain_id, not_equal_constant, reason),
        // IntegerPredicate::Equal {
        // domain_id,
        // equality_constant,
        // } => self.make_assignment(domain_id, equality_constant, reason),
        // }
    }

    pub fn assign_literal<R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Literal>,
        bound: bool,
        reason: R,
    ) -> Result<(), Inconsistency> {
        if !self
            .assignments_propositional
            .is_literal_assigned(var.inner)
        {
            let reason = self.reason_store.push(self.propagator, reason.into());
            let enqueue_result = self.assignments_propositional.enqueue_propagated_literal(
                if bound { var.inner } else { !var.inner },
                ConstraintReference::create_reason_reference(reason),
            );
            if let Some(conflict_info) = enqueue_result {
                return Err(Inconsistency::Other(conflict_info));
            }
        }

        Ok(())
    }
}
