use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::WatchListCP;
use crate::engine::WatchListPropositional;
use crate::engine::Watchers;
use crate::engine::WatchersPropositional;

/// [`PropagatorConstructorContext`] is used by when adding propagators to the solver.
/// It represents a communication point between the solver and the propagator.
/// Propagators use the [`PropagatorConstructorContext`] to register to domain changes
/// of variables.
#[derive(Debug)]
pub struct PropagatorConstructorContext<'a> {
    watch_list: &'a mut WatchListCP,
    watch_list_propositional: &'a mut WatchListPropositional,
    propagator_id: PropagatorId,
    next_local_id: LocalId,
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
            next_local_id: LocalId::from(0),
        }
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
    /// Note that the [`LocalId`] is used since internally the propagator variable is a wrapper
    /// around a variable 'view'.
    ///
    /// If `register_for_backtrack_events` is set to false then the propagator will not be
    /// registered for backtrack events. See [`Propagator::notify_backtrack`] for more information.
    pub fn register<Var: IntegerVariable>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
        register_for_backtrack_events: bool,
    ) -> Var {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.next_local_id = self.next_local_id.max(LocalId::from(local_id.unpack() + 1));

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all(
            &mut watchers,
            domain_events.get_int_events(),
            register_for_backtrack_events,
        );

        var
    }

    pub fn register_literal(
        &mut self,
        var: Literal,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> Literal {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.next_local_id = self.next_local_id.max(LocalId::from(local_id.unpack() + 1));

        let mut watchers =
            WatchersPropositional::new(propagator_var, self.watch_list_propositional);
        watchers.watch_all(var, domain_events.get_bool_events());

        var
    }

    pub fn get_next_local_id(&self) -> LocalId {
        self.next_local_id
    }
}
