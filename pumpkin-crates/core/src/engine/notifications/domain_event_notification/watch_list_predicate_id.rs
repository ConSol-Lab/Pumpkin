use enumset::EnumSet;

use crate::basic_types::PredicateId;
use crate::containers::HashMap;
use crate::containers::KeyedVec;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::engine::notifications::PredicateNotifier;
use crate::predicates::Predicate;
use crate::propagation::DomainEvent;
use crate::propagation::LocalId;
use crate::propagation::PropagatorVarId;
use crate::state::PropagatorId;
use crate::variables::Literal;

#[derive(Debug, Default, Clone)]
pub(crate) struct PredicateWatchList {
    /// The watch list from predicates to propagators.
    pub(crate) watch_list_predicate_id: KeyedVec<PredicateId, Vec<PredicateWatcher>>,
    pub(crate) watch_list_predicate_id_backtrack: HashMap<Literal, Vec<PredicateWatcher>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PredicateWatcher {
    pub(crate) propagator_id: PropagatorId,
    pub(crate) local_id: Option<LocalId>,
    pub(crate) events: Option<EnumSet<DomainEvent>>,
}

impl PredicateWatcher {
    pub(crate) fn new_predicate_watcher(propagator_id: PropagatorId) -> Self {
        PredicateWatcher {
            propagator_id,
            local_id: None,
            events: None,
        }
    }

    pub(crate) fn new_literal_watcher(
        propagator_id: PropagatorId,
        local_id: LocalId,
        events: EnumSet<DomainEvent>,
    ) -> Self {
        Self {
            propagator_id,
            local_id: Some(local_id),
            events: Some(events),
        }
    }
}

impl PredicateWatchList {
    pub(crate) fn watch_predicate_id(
        &mut self,
        predicate_id: PredicateId,
        propagator_id: PropagatorId,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
        predicate_notifier: &mut PredicateNotifier,
    ) {
        self.watch_list_predicate_id
            .accomodate(predicate_id, vec![]);
        self.watch_list_predicate_id[predicate_id]
            .push(PredicateWatcher::new_predicate_watcher(propagator_id));

        predicate_notifier.track_predicate(predicate_id, trailed_values, assignments);
    }

    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        propagator_id: PropagatorId,
        predicate_notifier: &mut PredicateNotifier,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
    ) -> PredicateId {
        let predicate_id = predicate_notifier.predicate_to_id.get_id(predicate);
        self.watch_predicate_id(
            predicate_id,
            propagator_id,
            assignments,
            trailed_values,
            predicate_notifier,
        );

        predicate_id
    }

    pub(crate) fn unwatch_predicate(
        &mut self,
        predicate_id: PredicateId,
        propagator_to_unwatch: PropagatorId,
    ) {
        if let Some(watch_list) = self.watchers_predicate_id_mut(predicate_id) {
            let index = watch_list
                .iter()
                .position(|&watched_propagator| {
                    watched_propagator.propagator_id == propagator_to_unwatch
                })
                .expect("cannot unwatch a (predicate, propagator) pair if it was not watched");

            let _ = watch_list.swap_remove(index);
        }

        // TODO: Can we remove the predicate from being tracked if it does not have watchers?
    }

    fn add_literal_watcher(
        &mut self,
        predicate: Predicate,
        propagator_var: PropagatorVarId,
        predicate_notifier: &mut PredicateNotifier,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
        events: EnumSet<DomainEvent>,
    ) {
        let predicate_id = predicate_notifier.predicate_to_id.get_id(predicate);

        self.watch_list_predicate_id
            .accomodate(predicate_id, vec![]);
        self.watch_list_predicate_id[predicate_id].push(PredicateWatcher::new_literal_watcher(
            propagator_var.propagator,
            propagator_var.variable,
            events,
        ));
        predicate_notifier.track_predicate(predicate_id, trailed_values, assignments);
    }

    pub(crate) fn watch_literal(
        &mut self,
        literal: Literal,
        propagator_var: PropagatorVarId,
        events: EnumSet<DomainEvent>,
        predicate_notifier: &mut PredicateNotifier,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
    ) {
        for event in events {
            match event {
                DomainEvent::Assign => {
                    self.add_literal_watcher(
                        literal.inner,
                        propagator_var,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                        events,
                    );
                    self.add_literal_watcher(
                        !literal.inner,
                        propagator_var,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                        events,
                    );
                }
                DomainEvent::LowerBound => {
                    self.add_literal_watcher(
                        literal.inner,
                        propagator_var,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                        events,
                    );
                }
                DomainEvent::UpperBound => {
                    self.add_literal_watcher(
                        !literal.inner,
                        propagator_var,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                        events,
                    );
                }
                DomainEvent::Removal => {
                    self.add_literal_watcher(
                        literal.inner,
                        propagator_var,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                        events,
                    );
                    self.add_literal_watcher(
                        !literal.inner,
                        propagator_var,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                        events,
                    );
                }
            };
        }
    }

    pub(crate) fn watchers_predicate_id_mut(
        &mut self,
        predicate_id: PredicateId,
    ) -> Option<&mut Vec<PredicateWatcher>> {
        self.watch_list_predicate_id.get_mut(predicate_id)
    }

    pub(crate) fn watchers_predicate_id(
        &self,
        predicate_id: PredicateId,
    ) -> Option<&Vec<PredicateWatcher>> {
        self.watch_list_predicate_id.get(predicate_id)
    }

    pub(crate) fn watch_literal_backtrack(
        &mut self,
        literal: Literal,
        propagator_var: PropagatorVarId,
        events: EnumSet<DomainEvent>,
        predicate_notifier: &mut PredicateNotifier,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
    ) {
        self.watch_list_predicate_id_backtrack
            .entry(literal)
            .or_default()
            .push(PredicateWatcher::new_literal_watcher(
                propagator_var.propagator,
                propagator_var.variable,
                events,
            ));
        for event in events {
            match event {
                DomainEvent::Assign => {
                    self.add_literal_watcher_backtrack(
                        literal.inner,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                    );
                    self.add_literal_watcher_backtrack(
                        !literal.inner,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                    );
                }
                DomainEvent::LowerBound => {
                    self.add_literal_watcher_backtrack(
                        literal.inner,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                    );
                }
                DomainEvent::UpperBound => {
                    self.add_literal_watcher_backtrack(
                        !literal.inner,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                    );
                }
                DomainEvent::Removal => {
                    self.add_literal_watcher_backtrack(
                        literal.inner,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                    );
                    self.add_literal_watcher_backtrack(
                        !literal.inner,
                        predicate_notifier,
                        trailed_values,
                        assignments,
                    );
                }
            };
        }
    }

    fn add_literal_watcher_backtrack(
        &mut self,
        predicate: Predicate,
        predicate_notifier: &mut PredicateNotifier,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        let predicate_id = predicate_notifier.predicate_to_id.get_id(predicate);

        predicate_notifier.track_predicate(predicate_id, trailed_values, assignments);
    }

    pub(crate) fn backtrack_watcher_for_literal_and_propagator(
        &self,
        literal: Literal,
        propagator_id: PropagatorId,
    ) -> Option<PredicateWatcher> {
        self.watch_list_predicate_id_backtrack
            .get(&literal)
            .and_then(|watch_list| {
                watch_list
                    .iter()
                    .find(|watcher| watcher.propagator_id == propagator_id)
            })
            .copied()
    }
}
