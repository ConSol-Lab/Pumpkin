use super::PropagatorId;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsInteger;
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
    assignments: &'a AssignmentsInteger,
}

impl<'a> PropagationContext<'a> {
    pub fn new(assignments_integer: &'a AssignmentsInteger) -> Self {
        PropagationContext {
            assignments: assignments_integer,
        }
    }

    pub fn get_decision_level(&self) -> usize {
        self.assignments.get_decision_level()
    }
}

#[derive(Debug)]
pub struct PropagationContextMut<'a> {
    assignments: &'a mut AssignmentsInteger,
    reason_store: &'a mut ReasonStore,

    propagator_id: PropagatorId,
}

impl<'a> PropagationContextMut<'a> {
    pub fn new(
        assignments: &'a mut AssignmentsInteger,
        reason_store: &'a mut ReasonStore,
        propagator: PropagatorId,
    ) -> Self {
        PropagationContextMut {
            assignments,
            reason_store,

            propagator_id: propagator,
        }
    }

    pub fn get_decision_level(&self) -> usize {
        self.assignments.get_decision_level()
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
            self.assignments
        }
    }

    impl HasAssignments for PropagationContextMut<'_> {
        fn assignments_integer(&self) -> &AssignmentsInteger {
            self.assignments
        }
    }
}

pub(crate) trait ReadDomains: HasAssignments {
    fn is_predicate_satisfied(&self, predicate: IntegerPredicate) -> bool {
        self.assignments_integer()
            .evaluate_predicate(predicate)
            .is_some_and(|truth_value| truth_value)
    }

    fn is_predicate_falsified(&self, predicate: IntegerPredicate) -> bool {
        self.assignments_integer()
            .evaluate_predicate(predicate)
            .is_some_and(|truth_value| !truth_value)
    }

    fn is_literal_true(&self, literal: Literal) -> bool {
        self.lower_bound(&literal) == 1
    }

    fn is_literal_false(&self, literal: Literal) -> bool {
        self.upper_bound(&literal) == 0
    }

    fn is_literal_fixed(&self, literal: Literal) -> bool {
        self.is_fixed(&literal)
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
        if var.contains(self.assignments, value) {
            let reason = self.reason_store.push(self.propagator_id, reason.into());
            return var.remove(self.assignments, value, Some(reason));
        }
        Ok(())
    }

    pub fn set_upper_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound < var.upper_bound(self.assignments) {
            let reason = self.reason_store.push(self.propagator_id, reason.into());
            return var.set_upper_bound(self.assignments, bound, Some(reason));
        }
        Ok(())
    }

    pub fn set_lower_bound<Var: IntegerVariable, R: Into<Reason>>(
        &mut self,
        var: &Var,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound > var.lower_bound(self.assignments) {
            let reason = self.reason_store.push(self.propagator_id, reason.into());
            return var.set_lower_bound(self.assignments, bound, Some(reason));
        }
        Ok(())
    }

    pub fn evaluate_predicate(&self, integer_predicate: IntegerPredicate) -> Option<bool> {
        self.assignments.evaluate_predicate(integer_predicate)
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

    pub fn assign_literal<R: Into<Reason> + Clone>(
        &mut self,
        boolean: Literal,
        truth_value: bool,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        match truth_value {
            true => self.set_lower_bound(&boolean, 1, reason),
            false => self.set_upper_bound(&boolean, 0, reason),
        }
    }
}
