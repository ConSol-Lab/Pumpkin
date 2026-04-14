use std::cmp::Ordering;
use std::collections::BinaryHeap;

use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::state::State;

/// A max-heap of predicates. The keys are based on the trail positions of the predicates in the
/// state, meaning predicates are popped in reverse trail order. Implied predicates are popped
/// before the predicate on the trail that implies the predicate.
#[derive(Clone, Debug, Default)]
pub(crate) struct PredicateHeap {
    heap: BinaryHeap<PredicateToExplain>,
}

impl PredicateHeap {
    /// See [`BinaryHeap::is_empty`].
    pub(crate) fn is_empty(&mut self) -> bool {
        self.heap.is_empty()
    }

    /// See [`BinaryHeap::pop`].
    ///
    /// Predicates are popped in reverse-trail order, and implied predicates are ordered such
    /// that stronger predicates go before weaker predicates.
    pub(crate) fn pop(&mut self) -> Option<Predicate> {
        self.heap.pop().map(|to_explain| to_explain.predicate)
    }

    /// See [`BinaryHeap::drain`].
    pub(crate) fn drain(&mut self) -> impl ExactSizeIterator<Item = Predicate> + '_ {
        self.heap.drain().map(|to_explain| to_explain.predicate)
    }

    /// See [`BinaryHeap::retain`].
    pub(crate) fn retain(&mut self, mut f: impl FnMut(Predicate) -> bool) {
        self.heap.retain(move |pte| f(pte.predicate));
    }

    /// See [`BinaryHeap::iter`].
    pub(crate) fn iter(&self) -> impl ExactSizeIterator<Item = Predicate> + '_ {
        self.heap.iter().map(|to_explain| to_explain.predicate)
    }

    /// Push a new predicate onto the heap.
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

        let is_implied = state.trail_entry(trail_position).predicate != predicate;

        self.heap.push(PredicateToExplain {
            predicate,
            trail_position,
            is_implied,
        });

        if cfg!(feature = "hl-checks") {
            assert_eq!(
                self.heap
                    .iter()
                    .filter(|pte| pte.predicate == predicate)
                    .count(),
                1
            );
        }
    }

    /// Returns true if the given [`Predicate`] is part of the heap.
    pub(crate) fn contains(&self, predicate: Predicate) -> bool {
        self.heap.iter().any(|pte| pte.predicate == predicate)
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
    is_implied: bool,
}

impl PartialOrd for PredicateToExplain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PredicateToExplain {
    /// Returns a "virtual rank" that approximates where this predicate would appear in an
    /// imagined SAT encoding of the CP trail. Higher rank = pops first from the max-heap.
    ///
    /// When two predicates share the same CP trail position, we order them as if the CP solver
    /// were a SAT solver that lazily propagates implied predicates after the direct trail entry:
    ///
    /// - **Rank 4** – `NotEqual` implied by the trail entry (a propagated consequence of an
    ///   equality or a bound that excluded the value): these are the "furthest derived" facts.
    /// - **Rank 3** – `Equal` implied by a combination of bounds: the equality was established only
    ///   once both bounds met, making it more derived than either individual bound.
    /// - **Rank 2** – The direct trail entry itself (`Equal`, `LowerBound`, or `UpperBound`): the
    ///   actual CP fact that was recorded.
    /// - **Rank 1** – Implied `LowerBound`/`UpperBound`: weaker bounds that became true as a
    ///   side-effect of the direct entry but carry strictly less information.
    /// - **Rank 0** – Direct `NotEqual` trail entry: these are typically narrow, single-value
    ///   removals whose implied equalities and bounds are of higher semantic content.
    fn virtual_rank(&self) -> u8 {
        match (self.predicate.get_predicate_type(), self.is_implied) {
            (PredicateType::NotEqual, true) => 4,
            (PredicateType::Equal, true) => 3,
            (
                PredicateType::Equal | PredicateType::LowerBound | PredicateType::UpperBound,
                false,
            ) => 2,
            (PredicateType::LowerBound | PredicateType::UpperBound, true) => 1,
            (PredicateType::NotEqual, false) => 0,
        }
    }
}

impl Ord for PredicateToExplain {
    fn cmp(&self, other: &Self) -> Ordering {
        let trail_order = self.trail_position.cmp(&other.trail_position);

        if matches!(trail_order, Ordering::Less | Ordering::Greater) {
            // If the predicates are from different trail positions, then the order of
            // the trail position is the order of the predicates in the heap.
            return trail_order;
        }

        assert_eq!(trail_order, Ordering::Equal);
        assert_eq!(self.predicate.get_domain(), other.predicate.get_domain());

        match self.virtual_rank().cmp(&other.virtual_rank()) {
            ord @ (Ordering::Less | Ordering::Greater) => ord,

            // Same virtual rank: break ties by predicate strength / value.
            Ordering::Equal => {
                let self_rhs = self.predicate.get_right_hand_side();
                let other_rhs = other.predicate.get_right_hand_side();

                match (
                    self.predicate.get_predicate_type(),
                    other.predicate.get_predicate_type(),
                ) {
                    // Stronger lower bound (higher value) gets higher priority.
                    (PredicateType::LowerBound, PredicateType::LowerBound) => {
                        self_rhs.cmp(&other_rhs)
                    }
                    // Stronger upper bound (lower value) gets higher priority.
                    (PredicateType::UpperBound, PredicateType::UpperBound) => {
                        other_rhs.cmp(&self_rhs)
                    }
                    // Among implied not-equals the relative order is unspecified;
                    // break ties by ascending RHS value for a deterministic result.
                    (PredicateType::NotEqual, PredicateType::NotEqual) => other_rhs.cmp(&self_rhs),
                    // Mixed types at the same rank are not expected but handled
                    // consistently via the natural predicate ordering.
                    _ => self.predicate.cmp(&other.predicate),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate;
    use crate::state::State;

    fn post(state: &mut State, predicate: Predicate) {
        let _ = state.post(predicate).unwrap();
    }

    #[test]
    fn new_heap_is_empty() {
        let mut heap = PredicateHeap::default();
        assert!(heap.is_empty());
    }

    #[test]
    fn pop_from_empty_heap_returns_none() {
        let mut heap = PredicateHeap::default();
        assert_eq!(heap.pop(), None);
    }

    #[test]
    fn is_not_empty_after_push() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 3]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);

        assert!(!heap.is_empty());
    }

    #[test]
    fn push_and_pop_single_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 3]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);

        assert_eq!(heap.pop(), Some(predicate![x >= 3]));
        assert!(heap.is_empty());
    }

    #[test]
    fn predicates_are_popped_in_reverse_trail_order() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));

        // Post x >= 3 first, y >= 5 second — y has the higher trail position.
        post(&mut state, predicate![x >= 3]);
        post(&mut state, predicate![y >= 5]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);
        heap.push(predicate![y >= 5], &state);

        // y >= 5 was posted later (higher trail position) and is popped first.
        assert_eq!(heap.pop(), Some(predicate![y >= 5]));
        assert_eq!(heap.pop(), Some(predicate![x >= 3]));
        assert!(heap.is_empty());
    }

    #[test]
    fn duplicate_push_is_ignored() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 3]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);
        heap.push(predicate![x >= 3], &state);

        assert_eq!(heap.pop(), Some(predicate![x >= 3]));
        assert_eq!(heap.pop(), None);
    }

    #[test]
    fn contains_returns_true_for_pushed_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 3]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);

        assert!(heap.contains(predicate![x >= 3]));
    }

    #[test]
    fn contains_returns_false_for_absent_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 5]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 5], &state);

        // x >= 4 is true in the state but was never pushed.
        assert!(!heap.contains(predicate![x >= 4]));
    }

    #[test]
    fn retain_keeps_matching_predicates() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        post(&mut state, predicate![x >= 3]);
        post(&mut state, predicate![y >= 5]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);
        heap.push(predicate![y >= 5], &state);

        let y_domain = predicate![y >= 5].get_domain();
        heap.retain(|p| p.get_domain() == y_domain);

        assert!(!heap.contains(predicate![x >= 3]));
        assert!(heap.contains(predicate![y >= 5]));
        assert_eq!(heap.pop(), Some(predicate![y >= 5]));
        assert!(heap.is_empty());
    }

    #[test]
    fn drain_empties_the_heap_and_returns_all_elements() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        post(&mut state, predicate![x >= 3]);
        post(&mut state, predicate![y >= 5]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);
        heap.push(predicate![y >= 5], &state);

        let drained: Vec<_> = heap.drain().collect();

        assert_eq!(drained.len(), 2);
        assert!(drained.contains(&predicate![x >= 3]));
        assert!(drained.contains(&predicate![y >= 5]));
        assert!(heap.is_empty());
    }

    #[test]
    fn iter_yields_all_pushed_predicates_without_removing_them() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        post(&mut state, predicate![x >= 3]);
        post(&mut state, predicate![y >= 5]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 3], &state);
        heap.push(predicate![y >= 5], &state);

        let iterated: Vec<_> = heap.iter().collect();

        assert_eq!(iterated.len(), 2);
        assert!(iterated.contains(&predicate![x >= 3]));
        assert!(iterated.contains(&predicate![y >= 5]));
        // Predicates are still in the heap after iterating.
        assert!(!heap.is_empty());
    }

    #[test]
    fn implied_predicates_ordered_as_if_they_are_set_by_a_propagator() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 5]);

        // Confirm both land at the same trail position.
        assert_eq!(
            state.trail_position(predicate![x >= 5]),
            state.trail_position(predicate![x >= 4]),
        );

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 4], &state); // implied
        heap.push(predicate![x >= 5], &state); // direct trail predicate

        assert_eq!(heap.pop(), Some(predicate![x >= 5]));
        assert_eq!(heap.pop(), Some(predicate![x >= 4]));
        assert_eq!(heap.pop(), None);
    }

    #[test]
    fn equality_is_implicitly_propagated_by_two_bounds() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x <= 5]);
        post(&mut state, predicate![x >= 5]);

        assert_eq!(state.truth_value(predicate![x == 5]), Some(true));

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x == 5], &state);
        heap.push(predicate![x >= 5], &state);

        assert_eq!(heap.pop(), Some(predicate![x == 5]));
        assert_eq!(heap.pop(), Some(predicate![x >= 5]));
        assert_eq!(heap.pop(), None);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x >= 5], &state);
        heap.push(predicate![x == 5], &state);

        assert_eq!(heap.pop(), Some(predicate![x == 5]));
        assert_eq!(heap.pop(), Some(predicate![x >= 5]));
        assert_eq!(heap.pop(), None);
    }

    #[test]
    fn not_equals_are_implied_by_equality() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x == 5]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x != 4], &state);
        heap.push(predicate![x == 5], &state);
        heap.push(predicate![x != 6], &state);

        assert_eq!(heap.pop(), Some(predicate![x != 4]));
        assert_eq!(heap.pop(), Some(predicate![x != 6]));
        assert_eq!(heap.pop(), Some(predicate![x == 5]));
        assert_eq!(heap.pop(), None);
    }

    #[test]
    fn not_equals_may_imply_equality_if_bounds_are_present() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        post(&mut state, predicate![x >= 4]);
        post(&mut state, predicate![x <= 5]);
        post(&mut state, predicate![x != 4]);

        let mut heap = PredicateHeap::default();
        heap.push(predicate![x == 5], &state);
        heap.push(predicate![x >= 5], &state);
        heap.push(predicate![x != 4], &state);

        assert_eq!(heap.pop(), Some(predicate![x == 5]));
        assert_eq!(heap.pop(), Some(predicate![x >= 5]));
        assert_eq!(heap.pop(), Some(predicate![x != 4]));
        assert_eq!(heap.pop(), None);
    }
}
