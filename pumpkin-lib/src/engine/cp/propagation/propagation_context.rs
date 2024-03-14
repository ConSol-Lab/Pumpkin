use crate::basic_types::variables::IntVar;
use crate::basic_types::ConstraintReference;
use crate::basic_types::Inconsistency;
use crate::basic_types::Literal;
use crate::basic_types::Predicate;
use crate::engine::propagation::PropagatorVariable;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::EmptyDomain;

/// ['PropagationContext'] is passed to propagators during propagation.
/// It may be queried to retrieve information about the current variable domains,
/// e.g., the lower bound of a particular variable),
/// or apply changes to the domain,
/// e.g., set [x >= 5].
///
///
/// Note that the ['PropagationContext'] is the only point of communication beween
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
}

impl<'a> PropagationContextMut<'a> {
    pub fn new(
        assignments_integer: &'a mut AssignmentsInteger,
        reason_store: &'a mut ReasonStore,
        assignments_propositional: &'a mut AssignmentsPropositional,
    ) -> Self {
        PropagationContextMut {
            assignments_integer,
            reason_store,
            assignments_propositional,
        }
    }
}

mod private {
    use super::*;

    pub(crate) trait HasAssignments {
        fn assignments_integer(&self) -> &AssignmentsInteger;
        fn assignments_propositional(&self) -> &AssignmentsPropositional;
    }

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

pub(crate) trait ReadDomains: private::HasAssignments {
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
    fn is_fixed<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    fn lower_bound<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> i32 {
        var.inner.lower_bound(self.assignments_integer())
    }

    fn upper_bound<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> i32 {
        var.inner.upper_bound(self.assignments_integer())
    }

    fn contains<Var: IntVar>(&self, var: &PropagatorVariable<Var>, value: i32) -> bool {
        var.inner.contains(self.assignments_integer(), value)
    }

    fn describe_domain<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> Vec<Predicate> {
        var.inner.describe_domain(self.assignments_integer())
    }
}

impl<T: private::HasAssignments> ReadDomains for T {}

impl PropagationContextMut<'_> {
    pub fn remove<Var: IntVar, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if var.inner.contains(self.assignments_integer, value) {
            let reason = self.reason_store.push(reason.into());
            return var
                .inner
                .remove(self.assignments_integer, value, Some(reason));
        }
        Ok(())
    }

    pub fn set_upper_bound<Var: IntVar, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound < var.inner.upper_bound(self.assignments_integer) {
            let reason = self.reason_store.push(reason.into());
            return var
                .inner
                .set_upper_bound(self.assignments_integer, bound, Some(reason));
        }
        Ok(())
    }

    pub fn set_lower_bound<Var: IntVar, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        if bound > var.inner.lower_bound(self.assignments_integer) {
            let reason = self.reason_store.push(reason.into());
            return var
                .inner
                .set_lower_bound(self.assignments_integer, bound, Some(reason));
        }
        Ok(())
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
            let reason = self.reason_store.push(reason.into());
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
