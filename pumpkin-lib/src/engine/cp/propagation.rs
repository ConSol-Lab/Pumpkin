use std::ops::{Index, IndexMut};

use crate::basic_types::{
    variables::IntVar, Predicate, PredicateConstructor, PropagationStatusCP,
    PropositionalConjunction,
};

use super::{AssignmentsInteger, DomainEvent, DomainManager, EmptyDomain, WatchListCP, Watchers};

/// A local id uniquely identifies a variable within a specific propagator. A local id can be
/// thought of as the index of the variable in the propagator.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct LocalId(u32);

impl LocalId {
    pub const fn from(value: u32) -> Self {
        LocalId(value)
    }

    pub fn unpack(self) -> u32 {
        self.0
    }
}

impl std::fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// An identifier to a propagator instance within the solver.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct PropagatorId(pub(crate) u32);

impl std::fmt::Display for PropagatorId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PropagatorId({})", self.0)
    }
}

impl<T> Index<PropagatorId> for Vec<T> {
    type Output = T;

    fn index(&self, index: PropagatorId) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T> IndexMut<PropagatorId> for Vec<T> {
    fn index_mut(&mut self, index: PropagatorId) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

/// A propagator variable is a handle to a variable for a propagator. It keeps track of the
/// [`LocalId`] when modifying the domain. To obtain a propagator variable, the
/// [`PropagatorConstructorContext::register()`] method should be used.
pub struct PropagatorVariable<Var> {
    inner: Var,
    local_id: LocalId,
}

impl<Var: std::fmt::Debug> std::fmt::Debug for PropagatorVariable<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var({}, {:?})", self.local_id, self.inner)
    }
}

impl<Var: IntVar> PropagatorVariable<Var> {
    pub fn unpack(&self, delta: Delta) -> DomainChange {
        self.inner.unpack_delta(delta)
    }
}

impl<Var: PredicateConstructor> PredicateConstructor for PropagatorVariable<Var> {
    type Value = Var::Value;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.lower_bound_predicate(bound)
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.upper_bound_predicate(bound)
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.equality_predicate(bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.disequality_predicate(bound)
    }
}

/// A handle to a variable registered to a propagator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PropagatorVarId {
    pub propagator: PropagatorId,
    pub variable: LocalId,
}

/// A delta is a propagated change to a variable. A propagator can be asked to explain why it
/// applied this particular change to the variable. It is a predicate that is projected onto the
/// local id of a propagator.
pub struct Delta(LocalId, DomainChange);

impl Delta {
    pub(crate) fn from_predicate(local_id: LocalId, predicate: Predicate) -> Delta {
        let change = match predicate {
            Predicate::LowerBound { lower_bound, .. } => DomainChange::LowerBound(lower_bound),
            Predicate::UpperBound { upper_bound, .. } => DomainChange::UpperBound(upper_bound),
            Predicate::NotEqual {
                not_equal_constant, ..
            } => DomainChange::Removal(not_equal_constant),
            Predicate::Equal { .. } => todo!(),
        };

        Delta(local_id, change)
    }

    pub(crate) fn unwrap_change(self) -> DomainChange {
        self.1
    }

    /// Get the [`LocalId`] of the variable this delta is for.
    pub fn affected_local_id(&self) -> LocalId {
        self.0
    }
}

/// A change is a modification of a particular variable, independant of any variable. In effect, a
/// predicate is a DomainId + Change.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum DomainChange {
    Removal(i32),
    LowerBound(i32),
    UpperBound(i32),
}

/// A wrapper for a domain event, which forces the propagator implementation to map the event
/// through the variable view.
pub struct OpaqueDomainEvent(DomainEvent);

impl From<DomainEvent> for OpaqueDomainEvent {
    fn from(event: DomainEvent) -> Self {
        OpaqueDomainEvent(event)
    }
}

impl OpaqueDomainEvent {
    pub(crate) fn unwrap(self) -> DomainEvent {
        self.0
    }
}

pub struct PropagationContext<'a> {
    domain_manager: DomainManager<'a>,
}

impl PropagationContext<'_> {
    pub(crate) fn new(
        assignment: &mut AssignmentsInteger,
        propagator_id: PropagatorId,
    ) -> PropagationContext<'_> {
        let domain_manager = DomainManager::new(propagator_id, assignment);
        PropagationContext { domain_manager }
    }

    /// Returns `true` if the domain of the given variable is singleton.
    pub fn is_fixed<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    pub fn lower_bound<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> i32 {
        var.inner.lower_bound(&self.domain_manager)
    }

    pub fn upper_bound<Var: IntVar>(&self, var: &PropagatorVariable<Var>) -> i32 {
        var.inner.upper_bound(&self.domain_manager)
    }

    pub fn contains<Var: IntVar>(&self, var: &PropagatorVariable<Var>, value: i32) -> bool {
        var.inner.contains(&self.domain_manager, value)
    }

    pub fn remove<Var: IntVar>(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
    ) -> Result<(), EmptyDomain> {
        self.domain_manager.set_local_id(var.local_id);
        var.inner.remove(&mut self.domain_manager, value)
    }

    pub fn set_upper_bound<Var: IntVar>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
    ) -> Result<(), EmptyDomain> {
        self.domain_manager.set_local_id(var.local_id);
        var.inner.set_upper_bound(&mut self.domain_manager, bound)
    }

    pub fn set_lower_bound<Var: IntVar>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
    ) -> Result<(), EmptyDomain> {
        self.domain_manager.set_local_id(var.local_id);
        var.inner.set_lower_bound(&mut self.domain_manager, bound)
    }
}

/// A CP propagator constructor takes an argument struct and constructs an implementation of
/// [`ConstraintProgrammingPropagator`].
pub trait CPPropagatorConstructor {
    /// The arguments to the constructor.
    type Args;

    /// The constructor function.
    fn create(
        args: Self::Args,
        context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator>;
}

pub struct PropagatorConstructorContext<'a> {
    watch_list: &'a mut WatchListCP,
    propagator_id: PropagatorId,
}

impl PropagatorConstructorContext<'_> {
    pub(crate) fn new(
        watch_list: &'_ mut WatchListCP,
        propagator_id: PropagatorId,
    ) -> PropagatorConstructorContext<'_> {
        PropagatorConstructorContext {
            watch_list,
            propagator_id,
        }
    }

    pub fn register<Var: IntVar>(
        &mut self,
        var: Var,
        event: DomainEvent,
        local_id: LocalId,
    ) -> PropagatorVariable<Var> {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch(&mut watchers, event);

        PropagatorVariable {
            inner: var,
            local_id,
        }
    }
}

/// Indicator of what to do when a propagator is notified.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnqueueDecision {
    /// The propagator should be enqueued.
    Enqueue,
    /// THe propagator should not be enqueued.
    Skip,
}

pub trait ConstraintProgrammingPropagator {
    //Propagate method that will be called during search
    //	extends the current partial assignments with inferred domain changes
    //  in case no conflict has been detected, returns PropagationStatusCP::NoConflictDetected
    //      otherwise returns the reason for failure in PropagationStatusCP::ConflictDetected { failure_reason }
    //      note that the failure (explanation) is given as a conjunction of predicates that lead to the failure
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP;

    /// Called when an event happens to one of the variables the propagator is subscribed to. It
    /// indicates whether the provided event should cause the propagator to be enqueued.
    ///
    /// This can be used to incrementally maintain datastructures or perform propagations, and
    /// should only be used for computationally cheap logic. Expensive computation should be
    /// performed in the [`ConstraintProgrammingPropagator::propagate()`] method.
    ///
    /// By default the propagator is always enqueued for every event. Not all propagators will
    /// benefit from implementing this, so it is not required to do so.
    fn notify(
        &mut self,
        _context: &mut PropagationContext,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        EnqueueDecision::Enqueue
    }

    //Called each time the solver backtracks
    //  the propagator can then update its internal data structures given the new variable domains
    fn synchronise(&mut self, context: &PropagationContext);

    //Returns the reason for propagation as a conjunction of predicates that imply the propagation
    //  reason -> predicate
    //  note that the input predicate is not expected to be part of the reason
    fn get_reason_for_propagation(
        &mut self,
        context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction;

    //Returns the priority of the propagator represented as a integer
    //	lower values mean higher priority
    //	the priority determines the order in which propagators will be asked to propagate
    //		i.e., after the clausal propagator, propagators with lower priority values are called before those with higher priority
    //  it is custom for simpler propagators to have lower priority values
    fn priority(&self) -> u32;

    //Return the name of the propagator
    //  this is a convenience method that is used for printing
    fn name(&self) -> &str;

    //Initialises the propagator and does root propagation
    //	called only once by the solver when the propagator is added
    //The return value is the same as for the 'propagate' method
    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP;

    //Another propagation method that is used to help debugging
    //	this method propagates without relying on internal data structures, hence immutable &self
    //	it is usually best to implement this propagation method in the simplest but correct way
    //  when the assert level is set to advanced or extreme (see pumpkin_asserts.rs)
    //      this method will be called to double check the reasons for failures and propagations that have been reported by this propagator
    //  note that the propagator will not be asked to provide reasons for propagations done by this method
    fn debug_propagate_from_scratch(&self, context: &mut PropagationContext)
        -> PropagationStatusCP;
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Delta {
        pub fn new(local_id: LocalId, change: DomainChange) -> Delta {
            Delta(local_id, change)
        }
    }
}
