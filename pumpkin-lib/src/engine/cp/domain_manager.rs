use crate::basic_types::{DomainId, Predicate};

use super::{
    assignments_integer::DomainOperationOutcome, AssignmentsInteger, LocalId, PropagatorId,
    PropagatorVarId,
};

/// The domain manager is passed to the variables to allow them to access their domains inside
/// propagators.
pub struct DomainManager<'a> {
    propagator: PropagatorId,
    local_id: LocalId,
    assignments_integer: &'a mut AssignmentsInteger,
}

impl DomainManager<'_> {
    pub fn new(
        propagator: PropagatorId,
        assignments_integer: &mut AssignmentsInteger,
    ) -> DomainManager<'_> {
        DomainManager {
            propagator,
            local_id: LocalId::from(0),
            assignments_integer,
        }
    }

    pub(crate) fn set_local_id(&mut self, local_id: LocalId) {
        self.local_id = local_id;
    }

    fn propagator_var_id(&mut self) -> PropagatorVarId {
        PropagatorVarId {
            propagator: self.propagator,
            variable: self.local_id,
        }
    }
}

//methods for getting info about the domains
impl DomainManager<'_> {
    pub fn get_lower_bound(&self, domain: DomainId) -> i32 {
        self.assignments_integer.get_lower_bound(domain)
    }

    pub fn get_upper_bound(&self, domain: DomainId) -> i32 {
        self.assignments_integer.get_upper_bound(domain)
    }

    pub fn get_lower_bound_predicate(&self, domain: DomainId) -> Predicate {
        self.assignments_integer.get_lower_bound_predicate(domain)
    }

    pub fn get_upper_bound_predicate(&self, domain: DomainId) -> Predicate {
        self.assignments_integer.get_upper_bound_predicate(domain)
    }

    pub fn get_lower_bound_predicates<'a, I: Iterator<Item = &'a DomainId>>(
        &self,
        domains: I,
    ) -> Vec<Predicate> {
        self.assignments_integer.get_lower_bound_predicates(domains)
    }

    pub fn get_upper_bound_predicates<'a, I: Iterator<Item = &'a DomainId>>(
        &self,
        domains: I,
    ) -> Vec<Predicate> {
        self.assignments_integer.get_upper_bound_predicates(domains)
    }

    pub fn get_bound_predicates<'a, I: Iterator<Item = &'a DomainId>>(
        &self,
        domains: I,
    ) -> Vec<Predicate> {
        self.assignments_integer.get_bound_predicates(domains)
    }

    pub fn is_value_in_domain(&self, domain: DomainId, value: i32) -> bool {
        self.assignments_integer.is_value_in_domain(domain, value)
    }

    pub fn is_domain_assigned(&self, domain: DomainId) -> bool {
        self.assignments_integer
            .is_integer_variable_assigned(domain)
    }
}

//methods to change the domains
impl DomainManager<'_> {
    pub fn tighten_lower_bound(
        &mut self,
        domain: DomainId,
        new_lower_bound: i32,
    ) -> DomainOperationOutcome {
        let reason = Some(self.propagator_var_id());

        self.assignments_integer
            .tighten_lower_bound(domain, new_lower_bound, reason)
    }

    pub fn tighten_upper_bound(
        &mut self,
        domain: DomainId,
        new_upper_bound: i32,
    ) -> DomainOperationOutcome {
        let reason = Some(self.propagator_var_id());

        self.assignments_integer
            .tighten_upper_bound(domain, new_upper_bound, reason)
    }

    pub fn make_assignment(
        &mut self,
        domain: DomainId,
        assigned_value: i32,
    ) -> DomainOperationOutcome {
        let reason = Some(self.propagator_var_id());

        self.assignments_integer
            .make_assignment(domain, assigned_value, reason)
    }

    pub fn remove_value_from_domain(
        &mut self,
        domain: DomainId,
        removed_value_from_domain: i32,
    ) -> DomainOperationOutcome {
        let reason = Some(self.propagator_var_id());

        self.assignments_integer
            .remove_value_from_domain(domain, removed_value_from_domain, reason)
    }
}
