use std::cmp::Ordering;
use std::collections::BinaryHeap;

use crate::predicates::Predicate;
use crate::state::State;

/// A max-heap of predicates. The keys are based on the trail positions of the predicates in the
/// state, meaning predicates are popped in reverse trail order. Implied predicates are popped
/// before the predicate on the trail that implies the predicate.
#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateHeap {
    heap: BinaryHeap<PredicateToExplain>,
}

impl PredicateHeap {
    /// See [`BinaryHeap::len`].
    pub(crate) fn len(&self) -> usize {
        self.heap.len()
    }

    /// See [`BinaryHeap::pop`].
    pub(crate) fn pop(&mut self) -> Option<Predicate> {
        self.heap.pop().map(|to_explain| to_explain.predicate)
    }

    /// See [`BinaryHeap::drain`].
    pub(crate) fn drain(&mut self) -> impl ExactSizeIterator<Item = Predicate> + '_ {
        self.heap.drain().map(|to_explain| to_explain.predicate)
    }

    /// See [`BinaryHeap::iter`].
    pub(crate) fn iter(&self) -> impl ExactSizeIterator<Item = Predicate> + '_ {
        self.heap.iter().map(|to_explain| to_explain.predicate)
    }

    /// Push a new predicate onto the heap.
    ///
    /// Its priority will be based on its trail position in the given `state`. This heap will
    /// return elements through [`Self::pop`] by reverse-trail order.
    ///
    /// If the predicate is not true in the given state, this method panics.
    pub(crate) fn push(&mut self, predicate: Predicate, state: &State) {
        // TODO: This can probably be optimized. But only do so once profiling shows this
        // as a problem.
        if self.heap.iter().any(|pte| pte.predicate == predicate) {
            return;
        }

        let trail_position = state
            .trail_position(predicate)
            .expect("predicate must be true in given state");

        self.heap.push(PredicateToExplain {
            predicate,
            trail_position,
        });

        assert_eq!(
            self.heap
                .iter()
                .filter(|pte| pte.predicate == predicate)
                .count(),
            1
        );
    }
}

/// Used to order the predicates in the [`PredicateHeap`].
///
/// The priority is calculated based on the trail position of the predicate and whether the
/// predicate is on the trail or implied.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PredicateToExplain {
    predicate: Predicate,
    trail_position: usize,
}

impl PartialOrd for PredicateToExplain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PredicateToExplain {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.trail_position.cmp(&other.trail_position) {
            ord @ (Ordering::Less | Ordering::Greater) => ord,

            Ordering::Equal => {
                assert_eq!(self.predicate.get_domain(), other.predicate.get_domain());

                let self_implies_other = self.predicate.implies(other.predicate);
                let other_implies_self = other.predicate.implies(self.predicate);

                if (self_implies_other && other_implies_self) {
                    Ordering::Equal
                } else if self_implies_other {
                    Ordering::Less
                } else if other_implies_self {
                    Ordering::Greater
                } else {
                    // The predicates are incomparable, so their order is irrelevant.
                    Ordering::Equal
                }
            }
        }
    }
}
