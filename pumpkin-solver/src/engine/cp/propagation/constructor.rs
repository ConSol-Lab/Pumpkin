use super::LocalId;
use super::PropagationContext;
use super::Propagator;
use super::PropagatorId;
use super::PropagatorVarId;
use crate::engine::Assignments;
use crate::engine::DomainEvents;
use crate::engine::TrailedAssignments;
use crate::engine::WatchListCP;
use crate::engine::Watchers;
use crate::variables::IntegerVariable;

/// A propagator constructor creates a fully initialized instance of a [`Propagator`].
///
/// The constructor is responsible for indicating on which events the propagator should be
/// enqueued. Additionally, the propagator can be initialized with values that come from the state
/// of the solver.
pub(crate) trait PropagatorConstructor {
    /// The propagator that is produced by this constructor.
    type PropagatorImpl: Propagator;

    /// Create the propagator instance from `Self`.
    fn create(self, context: &mut PropagatorConstructorContext) -> Self::PropagatorImpl;
}

/// [`PropagatorConstructorContext`] is used when [`Propagator`]s are initialised after creation.
///
/// It represents a communication point between the [`Solver`] and the [`Propagator`].
/// Propagators use the [`PropagatorConstructorContext`] to register to domain changes
/// of variables and to retrieve the current bounds of variables.
#[derive(Debug)]
pub(crate) struct PropagatorConstructorContext<'a> {
    watch_list: &'a mut WatchListCP,
    trailed_values: &'a mut TrailedAssignments,
    propagator_id: PropagatorId,
    next_local_id: LocalId,

    pub assignments: &'a mut Assignments,
}

impl PropagatorConstructorContext<'_> {
    pub(crate) fn new<'a>(
        watch_list: &'a mut WatchListCP,
        trailed_values: &'a mut TrailedAssignments,
        propagator_id: PropagatorId,
        assignments: &'a mut Assignments,
    ) -> PropagatorConstructorContext<'a> {
        PropagatorConstructorContext {
            watch_list,
            trailed_values,
            propagator_id,
            next_local_id: LocalId::from(0),

            assignments,
        }
    }

    pub(crate) fn as_readonly(&self) -> PropagationContext {
        PropagationContext::new(self.assignments)
    }

    /// Subscribes the propagator to the given [`DomainEvents`].
    ///
    /// The domain events determine when [`Propagator::notify()`] will be called on the propagator.
    /// The [`LocalId`] is internal information related to the propagator,
    /// which is used when calling [`Propagator::notify()`] to identify the variable.
    ///
    /// Each variable *must* have a unique [`LocalId`]. Most often this would be its index of the
    /// variable in the internal array of variables.
    ///
    /// Note that the [`LocalId`] is used to differentiate between [`DomainId`]s and
    /// [`AffineView`]s.
    pub(crate) fn register(
        &mut self,
        var: impl IntegerVariable,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.next_local_id = self.next_local_id.max(LocalId::from(local_id.unpack() + 1));

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all(&mut watchers, domain_events.get_int_events());
    }

    /// Subscribes the propagator to the given [`DomainEvents`] when they are undone during
    /// backtracking. This method is complementary to [`PropagatorConstructorContext::register`],
    /// the [`LocalId`]s provided to both of these method should be the same for the same variable.
    ///
    /// The domain events determine when [`Propagator::notify_backtrack()`] will be called on the
    /// propagator. The [`LocalId`] is internal information related to the propagator,
    /// which is used when calling [`Propagator::notify_backtrack()`] to identify the variable.
    ///
    /// Each variable *must* have a unique [`LocalId`]. Most often this would be its index of the
    /// variable in the internal array of variables.
    ///
    /// Note that the [`LocalId`] is used to differentiate between [`DomainId`]s and
    /// [`AffineView`]s.
    pub(crate) fn register_for_backtrack_events<Var: IntegerVariable>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.next_local_id = self.next_local_id.max(LocalId::from(local_id.unpack() + 1));

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all_backtrack(&mut watchers, domain_events.get_int_events());
    }

    pub(crate) fn get_next_local_id(&self) -> LocalId {
        self.next_local_id
    }
}

mod private {
    use super::*;
    use crate::engine::propagation::contexts::HasAssignments;
    use crate::engine::propagation::contexts::HasStatefulAssignments;

    impl HasAssignments for PropagatorConstructorContext<'_> {
        fn assignments(&self) -> &Assignments {
            self.assignments
        }
    }

    impl HasStatefulAssignments for PropagatorConstructorContext<'_> {
        fn stateful_assignments(&self) -> &TrailedAssignments {
            self.trailed_values
        }

        fn stateful_assignments_mut(&mut self) -> &mut TrailedAssignments {
            self.trailed_values
        }
    }
}
