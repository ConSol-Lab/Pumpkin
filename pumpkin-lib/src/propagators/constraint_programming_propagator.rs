use crate::{
    basic_types::{
        EnqueueStatus, IntegerVariable, Predicate, PropagationStatusCP, PropositionalConjunction,
    },
    engine::DomainManager,
};

pub trait ConstraintProgrammingPropagator {
    //Propagate method that will be called during search
    //	extends the current partial assignments with inferred domain changes
    //  in case no conflict has been detected, returns PropagationStatusCP::NoConflictDetected
    //      otherwise returns the reason for failure in PropagationStatusCP::ConflictDetected { failure_reason }
    //      note that the failure (explanation) is given as a conjunction of predicates that lead to the failure
    fn propagate(&mut self, domains: &mut DomainManager) -> PropagationStatusCP;

    //Called each time the solver backtracks
    //  the propagator can then update its internal data structures given the new variable domains
    fn synchronise(&mut self, domains: &DomainManager);

    //Notifies the propagator that a domain change occured with respect to the variable
    //  a domain change is always more constraining, e.g., the new lower bound will be greater than the old lower bound
    //	usually the propagator will update internal data structures to prepare for propagation
    //The return value indicates if the propagator should be enqueued for propagation
    //	note: the propagator registers which variables are relevant for it using 'get_integer_variables_to_watch_for_...' (see below)
    fn notify_lower_bound_integer_variable_change(
        &mut self,
        integer_variable: IntegerVariable,
        old_lower_bound: i32,
        new_lower_bound: i32,
        domains: &DomainManager,
    ) -> EnqueueStatus;

    fn notify_upper_bound_integer_variable_change(
        &mut self,
        integer_variable: IntegerVariable,
        old_upper_bound: i32,
        new_upper_bound: i32,
        domains: &DomainManager,
    ) -> EnqueueStatus;

    fn notify_domain_hole_integer_variable_change(
        &mut self,
        integer_variable: IntegerVariable,
        removed_value_from_domain: i32,
        domains: &DomainManager,
    ) -> EnqueueStatus;

    //Returns the reason for propagation as a conjunction of predicates that imply the propagation
    //  reason -> predicate
    //  note that the input predicate is not expected to be part of the reason
    fn get_reason_for_propagation(&mut self, predicate: Predicate) -> PropositionalConjunction;

    //Returns the priority of the propagator represented as a integer
    //	lower values mean higher priority
    //	the priority determines the order in which propagators will be asked to propagate
    //		i.e., after the clausal propagator, propagators with lower priority values are called before those with higher priority
    //  it is custom for simpler propagators to have lower priority values
    fn priority(&self) -> u32;

    //Return the name of the propagator
    //  this is a convenience method that is used for printing
    fn name(&self) -> &str;

    //These methods indicates for which variables and which events should the propagator be notified
    fn get_integer_variables_to_watch_for_lower_bound_changes(&self) -> Vec<IntegerVariable>;
    fn get_integer_variables_to_watch_for_upper_bound_changes(&self) -> Vec<IntegerVariable>;
    fn get_integer_variables_to_watch_for_domain_hole_changes(&self) -> Vec<IntegerVariable>;

    //Initialises the propagator and does root propagation
    //	called only once by the solver when the propagator is added
    //The return value is the same as for the 'propagate' method
    fn initialise_at_root(&mut self, domains: &mut DomainManager) -> PropagationStatusCP;

    //Another propagation method that is used to help debugging
    //	this method propagates without relying on internal data structures, hence immutable &self
    //	it is usually best to implement this propagation method in the simplest but correct way
    //  when the assert level is set to advanced or extreme (see pumpkin_asserts.rs)
    //      this method will be called to double check the reasons for failures and propagations that have been reported by this propagator
    //  note that the propagator will not be asked to provide reasons for propagations done by this method
    fn debug_propagate_from_scratch(&self, domains: &mut DomainManager) -> PropagationStatusCP;
}
