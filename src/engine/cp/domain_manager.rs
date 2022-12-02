use crate::basic_types::{IntegerVariable, Predicate, PropagatorIdentifier};

use super::{assignments_integer::DomainOperationOutcome, AssignmentsInteger};

//todo explain, this is a wrapper struct
pub struct DomainManager<'a> {
    propagator_identifier: Option<PropagatorIdentifier>,
    assignments_integer: &'a mut AssignmentsInteger,
}

impl DomainManager<'_> {
    pub fn new(
        propagator_index: usize,
        assignments_integer: &'_ mut AssignmentsInteger,
    ) -> DomainManager {
        DomainManager {
            propagator_identifier: Some(PropagatorIdentifier {
                id: propagator_index as u32,
            }),
            assignments_integer,
        }
    }

    pub fn num_trail_entries(&self) -> usize {
        self.assignments_integer.num_trail_entries()
    }
}

//methods for getting info about the domains
impl DomainManager<'_> {
    pub fn get_lower_bound(&self, integer_variable: IntegerVariable) -> i32 {
        self.assignments_integer.get_lower_bound(integer_variable)
    }

    pub fn get_upper_bound(&self, integer_variable: IntegerVariable) -> i32 {
        self.assignments_integer.get_upper_bound(integer_variable)
    }

    pub fn get_lower_bound_predicate(&self, integer_variable: IntegerVariable) -> Predicate {
        self.assignments_integer
            .get_lower_bound_predicate(integer_variable)
    }

    pub fn get_upper_bound_predicate(&self, integer_variable: IntegerVariable) -> Predicate {
        self.assignments_integer
            .get_upper_bound_predicate(integer_variable)
    }

    pub fn get_lower_bound_predicates(
        &self,
        integer_variables: &[IntegerVariable],
    ) -> Vec<Predicate> {
        self.assignments_integer
            .get_lower_bound_predicates(integer_variables)
    }

    pub fn get_upper_bound_predicates(
        &self,
        integer_variables: &[IntegerVariable],
    ) -> Vec<Predicate> {
        self.assignments_integer
            .get_upper_bound_predicates(integer_variables)
    }

    pub fn get_bound_predicates(&self, integer_variables: &[IntegerVariable]) -> Vec<Predicate> {
        self.assignments_integer
            .get_bound_predicates(integer_variables)
    }

    pub fn is_value_in_domain(&self, integer_variable: IntegerVariable, value: i32) -> bool {
        self.assignments_integer
            .is_value_in_domain(integer_variable, value)
    }

    pub fn is_integer_variable_assigned(&self, integer_variable: IntegerVariable) -> bool {
        self.assignments_integer
            .is_integer_variable_assigned(integer_variable)
    }
}

//methods to change the domains
impl DomainManager<'_> {
    pub fn tighten_lower_bound(
        &mut self,
        integer_variable: IntegerVariable,
        new_lower_bound: i32,
    ) -> DomainOperationOutcome {
        self.assignments_integer.tighten_lower_bound_no_notify(
            integer_variable,
            new_lower_bound,
            self.propagator_identifier,
        )
    }

    pub fn tighten_upper_bound(
        &mut self,
        integer_variable: IntegerVariable,
        new_upper_bound: i32,
    ) -> DomainOperationOutcome {
        self.assignments_integer.tighten_upper_bound_no_notify(
            integer_variable,
            new_upper_bound,
            self.propagator_identifier,
        )
    }

    pub fn make_assignment(
        &mut self,
        integer_variable: IntegerVariable,
        assigned_value: i32,
    ) -> DomainOperationOutcome {
        self.assignments_integer.make_assignment_no_notify(
            integer_variable,
            assigned_value,
            self.propagator_identifier,
        )
    }

    pub fn remove_value_from_domain(
        &mut self,
        integer_variable: IntegerVariable,
        removed_value_from_domain: i32,
    ) -> DomainOperationOutcome {
        self.assignments_integer.remove_value_from_domain_no_notify(
            integer_variable,
            removed_value_from_domain,
            self.propagator_identifier,
        )
    }
}
