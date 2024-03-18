use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagatorId;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::propagation::PropagatorVariable;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::WatchListCP;
use crate::engine::WatchListPropositional;
use crate::engine::Watchers;
use crate::engine::WatchersPropositional;

/// ['PropagatorConstructorContext'] is used by when adding propagators to the solver.
/// It represents a communcation point between the solver and the propagator.
/// Propagators use the ['PropagatorConstructorContext'] to register to domain changes
/// of variables and obtain ['PropagatorVariables'].
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

    pub fn register<Var: IntegerVariable>(
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
