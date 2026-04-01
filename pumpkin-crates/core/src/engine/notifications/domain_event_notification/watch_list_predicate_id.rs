use enumset::EnumSet;

use crate::basic_types::PredicateId;
use crate::containers::HashMap;
use crate::containers::KeyedVec;
use crate::propagation::DomainEvent;
use crate::propagation::LocalId;
use crate::propagation::PropagatorVarId;
use crate::state::PropagatorId;
use crate::variables::Literal;

#[derive(Debug, Default, Clone)]
pub(crate) struct PredicateWatchList {
    /// The watch list from predicates to propagators.
    pub(crate) watch_list_predicate_id: KeyedVec<PredicateId, Vec<PropagatorId>>,
    // TODO: Should use direct hashing
    pub(crate) literal_watch_list:
        HashMap<Literal, HashMap<PropagatorId, (LocalId, EnumSet<DomainEvent>)>>,
    pub(crate) literal_watch_list_backtrack:
        HashMap<Literal, HashMap<PropagatorId, (LocalId, EnumSet<DomainEvent>)>>,
}

impl PredicateWatchList {
    pub(crate) fn watch_predicate_id(
        &mut self,
        predicate_id: PredicateId,
        propagator_id: PropagatorId,
    ) {
        self.watch_list_predicate_id
            .accomodate(predicate_id, vec![]);
        self.watch_list_predicate_id[predicate_id].push(propagator_id);
    }

    pub(crate) fn watchers_predicate_id_mut(
        &mut self,
        predicate_id: PredicateId,
    ) -> Option<&mut Vec<PropagatorId>> {
        self.watch_list_predicate_id.get_mut(predicate_id)
    }

    pub(crate) fn watchers_predicate_id(
        &self,
        predicate_id: PredicateId,
    ) -> Option<&Vec<PropagatorId>> {
        self.watch_list_predicate_id.get(predicate_id)
    }

    pub(crate) fn insert_literal_watcher(
        &mut self,
        literal: Literal,
        propagator_var: PropagatorVarId,
        events: EnumSet<DomainEvent>,
    ) {
        let entry = self
            .literal_watch_list
            .entry(literal)
            .or_default()
            .entry(propagator_var.propagator)
            .or_insert((propagator_var.variable, events));
        entry.1 |= events;
    }

    pub(crate) fn insert_literal_watcher_backtrack(
        &mut self,
        literal: Literal,
        propagator_var: PropagatorVarId,
        events: EnumSet<DomainEvent>,
    ) {
        let entry = self
            .literal_watch_list_backtrack
            .entry(literal)
            .or_default()
            .entry(propagator_var.propagator)
            .or_insert((propagator_var.variable, events));
        entry.1 |= events;
    }

    pub(crate) fn watchers_literal_propagator(
        &self,
        literal: Literal,
        propagator_id: PropagatorId,
    ) -> Option<&(LocalId, EnumSet<DomainEvent>)> {
        self.literal_watch_list
            .get(&literal)
            .and_then(|inner| inner.get(&propagator_id))
            .or_else(|| {
                self.literal_watch_list
                    .get(&(!literal))
                    .and_then(|inner| inner.get(&propagator_id))
            })
    }
}
