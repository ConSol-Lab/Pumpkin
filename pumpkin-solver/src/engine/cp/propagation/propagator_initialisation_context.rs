use super::PropagationContext;
use super::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::engine::WatchListCP;
use crate::engine::Watchers;

/// [`PropagatorInitialisationContext`] is used when [`Propagator`]s are initialised after creation.
///
/// It represents a communication point between the [`Solver`] and the [`Propagator`].
/// Propagators use the [`PropagatorInitialisationContext`] to register to domain changes
/// of variables and to retrieve the current bounds of variables.
#[derive(Debug)]
pub struct PropagatorInitialisationContext<'a> {
    watch_list: &'a mut WatchListCP,
    propagator_id: PropagatorId,
    next_local_id: LocalId,

    context: PropagationContext<'a>,
}

impl PropagatorInitialisationContext<'_> {
    pub(crate) fn new<'a>(
        watch_list: &'a mut WatchListCP,
        propagator_id: PropagatorId,
        assignments: &'a Assignments,
    ) -> PropagatorInitialisationContext<'a> {
        PropagatorInitialisationContext {
            watch_list,
            propagator_id,
            next_local_id: LocalId::from(0),

            context: PropagationContext::new(assignments),
        }
    }

    pub(crate) fn as_readonly(&self) -> PropagationContext {
        self.context
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
    pub fn register<Var: IntegerVariable>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> Var {
        if self.context.is_fixed(&var) {
            return var;
        }
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.next_local_id = self.next_local_id.max(LocalId::from(local_id.unpack() + 1));

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all(&mut watchers, domain_events.get_int_events());

        var
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
    pub fn register_for_backtrack_events<Var: IntegerVariable>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> Var {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.next_local_id = self.next_local_id.max(LocalId::from(local_id.unpack() + 1));

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all_backtrack(&mut watchers, domain_events.get_int_events());

        var
    }

    pub fn get_next_local_id(&self) -> LocalId {
        self.next_local_id
    }
}

mod private {
    use super::*;
    use crate::engine::propagation::propagation_context::HasAssignments;

    impl HasAssignments for PropagatorInitialisationContext<'_> {
        fn assignments(&self) -> &Assignments {
            self.context.assignments
        }
    }
}
