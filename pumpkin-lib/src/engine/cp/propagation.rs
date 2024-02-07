use std::ops::Index;
use std::ops::IndexMut;

use enumset::enum_set;
use enumset::EnumSet;

use crate::basic_types::variables::IntVar;
use crate::basic_types::ConstraintReference;
use crate::basic_types::Inconsistency;
use crate::basic_types::Literal;
use crate::basic_types::Predicate;
use crate::basic_types::PredicateConstructor;
use crate::basic_types::PropagationStatusCP;
use crate::engine::cp::AssignmentsInteger;
use crate::engine::cp::BooleanDomainEvent;
use crate::engine::cp::EmptyDomain;
use crate::engine::cp::IntDomainEvent;
use crate::engine::cp::WatchListCP;
use crate::engine::cp::WatchListPropositional;
use crate::engine::cp::Watchers;
use crate::engine::cp::WatchersPropositional;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::AssignmentsPropositional;

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
#[derive(Hash, Eq, PartialEq, Clone)]
pub struct PropagatorVariable<Var> {
    inner: Var,
}

impl PropagatorVariable<Literal> {
    pub fn get_literal(&self) -> Literal {
        self.inner
    }
}

impl<Var: std::fmt::Debug> std::fmt::Debug for PropagatorVariable<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var({:?})", self.inner)
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
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct PropagatorVarId {
    pub propagator: PropagatorId,
    pub variable: LocalId,
}

/// A wrapper for a domain event, which forces the propagator implementation to map the event
/// through the variable view.
#[derive(Clone, Debug, Copy)]
pub struct OpaqueDomainEvent(IntDomainEvent);

impl From<IntDomainEvent> for OpaqueDomainEvent {
    fn from(event: IntDomainEvent) -> Self {
        OpaqueDomainEvent(event)
    }
}

impl OpaqueDomainEvent {
    pub(crate) fn unwrap(self) -> IntDomainEvent {
        self.0
    }
}

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

    pub trait HasAssignments {
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

pub trait ReadDomains: private::HasAssignments {
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
        let reason = self.reason_store.push(reason.into());
        var.inner
            .remove(self.assignments_integer, value, Some(reason))
    }

    pub fn set_upper_bound<Var: IntVar, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        let reason = self.reason_store.push(reason.into());
        var.inner
            .set_upper_bound(self.assignments_integer, bound, Some(reason))
    }

    pub fn set_lower_bound<Var: IntVar, R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Var>,
        bound: i32,
        reason: R,
    ) -> Result<(), EmptyDomain> {
        let reason = self.reason_store.push(reason.into());
        var.inner
            .set_lower_bound(self.assignments_integer, bound, Some(reason))
    }

    pub fn assign_literal<R: Into<Reason>>(
        &mut self,
        var: &PropagatorVariable<Literal>,
        bound: bool,
        reason: R,
    ) -> Result<(), Inconsistency> {
        let reason = self.reason_store.push(reason.into());
        let enqueue_result = self.assignments_propositional.enqueue_propagated_literal(
            if bound { var.inner } else { !var.inner },
            ConstraintReference::create_reason_reference(reason),
        );
        if let Some(conflict_info) = enqueue_result {
            return Err(Inconsistency::Other(conflict_info));
        }
        Ok(())
    }
}

/// A CP propagator constructor turns an argument struct into an implementation of
/// [`ConstraintProgrammingPropagator`].
pub trait CPPropagatorConstructor {
    /// The propagator to construct.
    type Propagator: ConstraintProgrammingPropagator;

    /// The constructor function.
    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator;

    /// A handy boxed constructor function.
    fn create_boxed(
        self,
        context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator>
    where
        Self: Sized,
        Self::Propagator: 'static,
    {
        Box::new(self.create(context))
    }
}

#[derive(Debug)]
pub struct PropagatorConstructorContext<'a> {
    watch_list: &'a mut WatchListCP,
    watch_list_propositional: &'a mut WatchListPropositional,
    propagator_id: PropagatorId,
}

impl PropagatorConstructorContext<'_> {
    pub(crate) fn new<'a>(
        watch_list: &'a mut WatchListCP,
        watch_list_propositional: &'a mut WatchListPropositional,
        propagator_id: PropagatorId,
    ) -> PropagatorConstructorContext<'a> {
        PropagatorConstructorContext {
            watch_list,
            watch_list_propositional,
            propagator_id,
        }
    }

    pub fn register<Var: IntVar>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> PropagatorVariable<Var> {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all(&mut watchers, domain_events.get_int_events());

        PropagatorVariable { inner: var }
    }

    pub fn register_literal(
        &mut self,
        var: Literal,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> PropagatorVariable<Literal> {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        let mut watchers =
            WatchersPropositional::new(propagator_var, self.watch_list_propositional);
        watchers.watch_all(var, domain_events.get_bool_events());

        PropagatorVariable { inner: var }
    }
}

impl DomainEvents {
    ///DomainEvents for assigning true to literal
    pub const ASSIGNED_TRUE: DomainEvents =
        DomainEvents::create_with_bool_events(enum_set!(BooleanDomainEvent::AssignedTrue));
    ///DomainEvents for assigning false to literal
    pub const ASSIGNED_FALSE: DomainEvents =
        DomainEvents::create_with_bool_events(enum_set!(BooleanDomainEvent::AssignedFalse));
    ///DomainEvents for assigning true and false to literal
    pub const ANY_BOOL: DomainEvents = DomainEvents::create_with_bool_events(enum_set!(
        BooleanDomainEvent::AssignedTrue | BooleanDomainEvent::AssignedFalse
    ));
    /// DomainEvents with both lower and upper bound tightening (but not other value removal).
    pub const BOUNDS: DomainEvents = DomainEvents::create_with_int_events(enum_set!(
        IntDomainEvent::LowerBound | IntDomainEvent::UpperBound
    ));
    // this is all options right now, but won't be once we add variables of other types
    /// DomainEvents with lower and upper bound tightening, assigning to a single value, and
    ///  single value removal.
    pub const ANY_INT: DomainEvents = DomainEvents::create_with_int_events(enum_set!(
        IntDomainEvent::Assign
            | IntDomainEvent::LowerBound
            | IntDomainEvent::UpperBound
            | IntDomainEvent::Removal
    ));
    /// DomainEvents with only lower bound tightening.
    pub const LOWER_BOUND: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(IntDomainEvent::LowerBound));
    /// DomainEvents with only upper bound tightening.
    pub const UPPER_BOUND: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(IntDomainEvent::UpperBound));
    /// DomainEvents with only assigning to a single value.
    pub const ASSIGN: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(IntDomainEvent::Assign));
}

#[derive(Debug, Copy, Clone)]
pub struct DomainEvents {
    int_events: Option<EnumSet<IntDomainEvent>>,
    boolean_events: Option<EnumSet<BooleanDomainEvent>>,
}

impl DomainEvents {
    pub const fn create_with_int_events(int_events: EnumSet<IntDomainEvent>) -> DomainEvents {
        DomainEvents {
            int_events: Some(int_events),
            boolean_events: None,
        }
    }

    pub const fn create_with_bool_events(
        boolean_events: EnumSet<BooleanDomainEvent>,
    ) -> DomainEvents {
        DomainEvents {
            int_events: None,
            boolean_events: Some(boolean_events),
        }
    }

    pub fn get_int_events(&self) -> EnumSet<IntDomainEvent> {
        self.int_events
            .expect("Tried to retrieve int_events when it was not initialized")
    }

    pub fn get_bool_events(&self) -> EnumSet<BooleanDomainEvent> {
        self.boolean_events
            .expect("Tried to retrieve boolean_events when it was not initialized")
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
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP;

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
        _context: &mut PropagationContextMut,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        EnqueueDecision::Enqueue
    }

    ///Notifies the propagator when the domain of a literal has changed (i.e. it is assigned)
    fn notify_literal(
        &mut self,
        _context: &mut PropagationContextMut,
        _local_id: LocalId,
        _event: BooleanDomainEvent,
    ) -> EnqueueDecision {
        EnqueueDecision::Enqueue
    }

    //Called each time the solver backtracks
    //  the propagator can then update its internal data structures given the new variable domains
    fn synchronise(&mut self, context: &PropagationContext);

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
    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP;

    //Another propagation method that is used to help debugging
    //	this method propagates without relying on internal data structures, hence immutable &self
    //	it is usually best to implement this propagation method in the simplest but correct way
    //  when the assert level is set to advanced or extreme (see pumpkin_asserts.rs)
    //      this method will be called to double check the reasons for failures and propagations that have been reported by this propagator
    //  note that the propagator will not be asked to provide reasons for propagations done by this method
    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP;
}
