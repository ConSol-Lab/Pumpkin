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

    /// Subscribes the propagator to the given [`DomainEvents`].
    ///
    /// The domain events determine when [`Propagator::notify()`] will be called on the propagator.
    /// The [`LocalId`] is internal information related to the propagator,
    /// which is used when calling [`Propagator::notify()`] to identify the variable.
    /// Each variable *must* have a unique [`LocalId`]. Most often this would be its index of the
    /// variable in the internal array of variables.
    ///
    /// Note that the [`LocalId`] is used since internally the propagator variable is a wrapper
    /// around a variable 'view'.
    pub fn register<Var: IntegerVariable>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> Var {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        let mut watchers = Watchers::new(propagator_var, self.watch_list);
        var.watch_all(&mut watchers, domain_events.get_int_events());

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

        let mut watchers =
            WatchersPropositional::new(propagator_var, self.watch_list_propositional);
        watchers.watch_all(var, domain_events.get_bool_events());

        var
    }
}
