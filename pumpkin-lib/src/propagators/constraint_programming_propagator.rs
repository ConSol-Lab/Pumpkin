use crate::{
    basic_types::{Predicate, PropagationStatusCP, PropositionalConjunction},
    engine::{DomainManager, Watchers},
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

    /// Indicate what variables to watch for this propagator.
    fn register_watches(&self, watchers: &mut Watchers<'_>);

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
