use std::cmp::Ordering;
use std::collections::BinaryHeap;

use pumpkin_core::predicates::Predicate;
use pumpkin_core::state::State;

/// A heap of predicates. The keys are the trail positions of the predicates in the state.
#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateHeap {
    heap: BinaryHeap<PredicateToExplain>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PredicateToExplain {
    predicate: Predicate,
    priority: usize,
}

impl PartialOrd for PredicateToExplain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PredicateToExplain {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PredicateHeap {
    /// See [`BinaryHeap::is_empty`].
    pub(crate) fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    /// See [`BinaryHeap::pop`].
    pub(crate) fn pop(&mut self) -> Option<Predicate> {
        self.heap.pop().map(|to_explain| to_explain.predicate)
    }

    /// Push a new predicate onto the heap.
    ///
    /// Its priority will be based on its trail position in the given `state`. This heap will
    /// return elements through [`Self::pop`] by reverse-trail order.
    pub(crate) fn push(&mut self, predicate: Predicate, state: &State) {
        let trail_position = state
            .trail_position(predicate)
            .expect("predicate must be true in given state");

        let priority = if state.is_on_trail(predicate) {
            trail_position * 2
        } else {
            trail_position * 2 + 1
        };

        self.heap.push(PredicateToExplain {
            predicate,
            priority,
        });
    }
}
