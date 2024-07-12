use super::PropagatorId;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::Literal;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use AssignmentsInteger;
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
}

impl<'a> PropagationContext<'a> {
    pub fn new(assignments_integer: &'a AssignmentsInteger) -> Self {
        PropagationContext {
            assignments_integer,
        }
    }
}

#[derive(Debug)]
pub struct PropagationContextMut<'a> {
    assignments_integer: &'a mut AssignmentsInteger,
    reason_store: &'a mut ReasonStore,

    propagator_id: PropagatorId,
}

impl<'a> PropagationContextMut<'a> {
    pub fn new(
        assignments_integer: &'a mut AssignmentsInteger,
        reason_store: &'a mut ReasonStore,
        propagator: PropagatorId,
    ) -> Self {
        PropagationContextMut {
            assignments_integer,
            reason_store,

            propagator_id: propagator,
        }
    }
}

/// A trait which defines common methods for retrieving the [`AssignmentsInteger`] and
/// [`AssignmentsPropositional`] from the structure which implements this trait.
pub trait HasAssignments {
    /// Returns the stored [`AssignmentsInteger`].
    fn assignments_integer(&self) -> &AssignmentsInteger;
}

mod private {
    use super::*;

    impl HasAssignments for PropagationContext<'_> {
        fn assignments_integer(&self) -> &AssignmentsInteger {
            self.assignments_integer
        }
    }

    impl HasAssignments for PropagationContextMut<'_> {
        fn assignments_integer(&self) -> &AssignmentsInteger {
            self.assignments_integer
        }
    }
}

pub(crate) trait ReadDomains: HasAssignments {
    fn is_boolean_true(&self, boolean: Literal) -> bool {
        self.lower_bound(&DomainId::from(boolean)) == 1
    }

    fn is_boolean_false(&self, boolean: Literal) -> bool {
        self.upper_bound(&DomainId::from(boolean)) == 0
    }

    fn is_boolean_fixed(&self, boolean: Literal) -> bool {
        self.is_fixed(&DomainId::from(boolean))
    }

    /// Returns `true` if the domain of the given variable is singleton.
    fn is_fixed<Var: IntegerVariable>(&self, var: &Var) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    fn lower_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.lower_bound(self.assignments_integer())
    }

    fn upper_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.upper_bound(self.assignments_integer())
    }

    fn contains<Var: IntegerVariable>(&self, var: &Var, value: i32) -> bool {
        var.contains(self.assignments_integer(), value)
    }

    fn describe_domain<Var: IntegerVariable>(&self, var: &Var) -> Vec<IntegerPredicate> {
        var.describe_domain(self.assignments_integer())
    }
}

impl<T: HasAssignments> ReadDomains for T {}

impl PropagationContextMut<'_> {
    pub fn remove<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        value: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if var.contains(self.assignments_integer, value) {
            let reason = self.reason_store.push(self.propagator_id, reason.into());
            return var.remove(self.assignments_integer, value, Some(reason));
        }
        Ok(())
    }

    pub fn set_upper_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound < var.upper_bound(self.assignments_integer) {
            let reason = self.reason_store.push(self.propagator_id, reason.into());
            return var.set_upper_bound(self.assignments_integer, bound, Some(reason));
        }
        Ok(())
    }

    pub fn set_lower_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound > var.lower_bound(self.assignments_integer) {
            let reason = self.reason_store.push(self.propagator_id, reason.into());
            return var.set_lower_bound(self.assignments_integer, bound, Some(reason));
        }
        Ok(())
    }

    pub fn evaluate_predicate(&self, integer_predicate: IntegerPredicate) -> Option<bool> {
        self.assignments_integer
            .evaluate_predicate(integer_predicate)
    }

    pub fn post_predicate<R: Into<Reason> + Clone>(
        &mut self,
        integer_predicate: IntegerPredicate,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        match integer_predicate {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.set_lower_bound(&domain_id, lower_bound, reason),
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.set_upper_bound(&domain_id, upper_bound, reason),
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.remove(&domain_id, not_equal_constant, reason),
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => {
                self.set_lower_bound(&domain_id, equality_constant, reason.clone())?;
                self.set_upper_bound(&domain_id, equality_constant, reason)
            }
        }
    }

    pub fn assign_boolean<R: Into<Reason> + Clone>(
        &mut self,
        boolean: Literal,
        truth_value: bool,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        let domain_id = DomainId::from(boolean);
        match truth_value {
            true => self.set_lower_bound(&domain_id, 1, reason),
            false => self.set_upper_bound(&domain_id, 0, reason),
        }
    }
}
