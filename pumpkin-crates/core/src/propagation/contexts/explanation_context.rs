use std::sync::LazyLock;

use super::HasAssignments;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyValueHeap;
use crate::engine::Assignments;
use crate::engine::notifications::NotificationEngine;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::Propagator;

/// The context that is available when lazily explaining propagations.
///
/// See [`Propagator`] for more information.
#[derive(Debug)]
pub struct ExplanationContext<'a> {
    assignments: &'a Assignments,
    pub(crate) notification_engine: &'a mut NotificationEngine,
    current_nogood: CurrentNogood<'a>,
    trail_position: usize,
}

impl<'a> ExplanationContext<'a> {
    #[cfg(test)]
    pub(crate) fn test_new(
        value: &'a Assignments,
        notification_engine: &'a mut NotificationEngine,
    ) -> Self {
        ExplanationContext {
            assignments: value,
            notification_engine,
            current_nogood: CurrentNogood::empty(),
            trail_position: 0,
        }
    }

    pub(crate) fn new(
        assignments: &'a Assignments,
        current_nogood: CurrentNogood<'a>,
        trail_position: usize,
        notification_engine: &'a mut NotificationEngine,
    ) -> Self {
        ExplanationContext {
            assignments,
            current_nogood,
            trail_position,
            notification_engine,
        }
    }

    pub(crate) fn without_working_nogood(
        assignments: &'a Assignments,
        trail_position: usize,
        notification_engine: &'a mut NotificationEngine,
    ) -> Self {
        ExplanationContext {
            assignments,
            current_nogood: CurrentNogood::empty(),
            trail_position,
            notification_engine,
        }
    }

    pub fn get_predicate(&mut self, predicate_id: PredicateId) -> Predicate {
        self.notification_engine.get_predicate(predicate_id)
    }

    /// Get the current working nogood.
    ///
    /// The working nogood does not necessarily contain the predicate that is being explained.
    /// However, the explanation will be used to either resolve with the working nogood or minimize
    /// it some other way.
    pub fn working_nogood(&self) -> impl Iterator<Item = Predicate> + '_ {
        self.current_nogood.iter()
    }

    /// Returns the trail position before which the propagation took place.
    ///
    /// For example, if the context is created for explaining a predicate `[x >= v]` at trail
    /// position i, then this method will return `i - 1`
    pub fn get_trail_position(&self) -> usize {
        self.trail_position - 1
    }
}

impl HasAssignments for ExplanationContext<'_> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }
}

static EMPTY_HEAP: KeyValueHeap<PredicateId, u32> = KeyValueHeap::new();

static EMPTY_PREDICATE_IDS: LazyLock<PredicateIdGenerator> =
    LazyLock::new(PredicateIdGenerator::default);

static EMPTY_PREDICATES: [Predicate; 0] = [];

#[derive(Debug)]
pub struct CurrentNogood<'a> {
    heap: &'a KeyValueHeap<PredicateId, u32>,
    visited: &'a [Predicate],
    ids: &'a PredicateIdGenerator,
}

impl<'a> CurrentNogood<'a> {
    pub(crate) fn new(
        heap: &'a KeyValueHeap<PredicateId, u32>,
        visited: &'a [Predicate],
        ids: &'a PredicateIdGenerator,
    ) -> Self {
        Self { heap, visited, ids }
    }

    pub fn empty() -> CurrentNogood<'a> {
        // The variable here is necessary for lifetime coersion.
        let reference: &[Predicate] = &EMPTY_PREDICATES;
        Self::from(reference)
    }

    fn iter<'this, 'ids>(&'this self) -> impl Iterator<Item = Predicate> + 'this
    where
        'ids: 'this,
    {
        self.heap
            .keys()
            .map(|id| self.ids.get_predicate(id))
            .chain(self.visited.iter().copied())
    }
}

impl<'a> From<&'a [Predicate]> for CurrentNogood<'a> {
    fn from(value: &'a [Predicate]) -> Self {
        CurrentNogood {
            heap: &EMPTY_HEAP,
            visited: value,
            ids: &EMPTY_PREDICATE_IDS,
        }
    }
}
