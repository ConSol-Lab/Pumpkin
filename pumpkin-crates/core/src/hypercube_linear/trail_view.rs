use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::predicates::Predicate;
use crate::variables::AffineView;
use crate::variables::DomainId;

/// Abstracts all trail queries the hypercube linear resolver needs.
///
/// This trait is implemented by [`crate::state::State`] for production use, and by
/// `FakeTrail` in tests.
pub(crate) trait TrailView {
    fn trail_position(&self, predicate: Predicate) -> Option<usize>;
    fn checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize>;
    fn current_checkpoint(&self) -> usize;
    fn trail_position_at_checkpoint(&self, checkpoint: usize) -> usize;
    /// Returns the predicate that is directly recorded at `trail_position` on the trail.
    ///
    /// Used by [`crate::hypercube_linear::predicate_heap::PredicateHeap`] to determine whether a
    /// predicate is a direct trail entry or an implied predicate.
    fn predicate_at_trail_position(&self, trail_position: usize) -> Predicate;
    fn truth_value_at(&self, predicate: Predicate, trail_position: usize) -> Option<bool>;
    fn lower_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32;
    fn upper_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32;
    /// Returns the explanation for why `predicate` was propagated.
    ///
    /// Panics if the predicate is not propagated (i.e. is a decision or not on the trail).
    fn reason_for(&mut self, predicate: Predicate) -> HypercubeLinearExplanation;
    /// Returns the trail position of the last entry on the trail.
    ///
    /// Only valid during conflict analysis, where the trail is guaranteed to be non-empty.
    fn current_trail_position(&self) -> usize;
}

/// Computes the lower bound of an [`AffineView`] at the given trail position.
///
/// Mirrors [`crate::variables::AffineView::lower_bound_at_trail_position`].
pub(super) fn affine_lower_bound_at(
    trail: &impl TrailView,
    term: AffineView<DomainId>,
    trail_position: usize,
) -> i32 {
    let domain_bound = if term.scale < 0 {
        trail.upper_bound_at_trail_position(term.inner, trail_position)
    } else {
        trail.lower_bound_at_trail_position(term.inner, trail_position)
    };
    term.scale * domain_bound + term.offset
}

/// Computes the upper bound of an [`AffineView`] at the given trail position.
///
/// Mirrors [`crate::variables::AffineView::upper_bound_at_trail_position`].
pub(super) fn affine_upper_bound_at(
    trail: &impl TrailView,
    term: AffineView<DomainId>,
    trail_position: usize,
) -> i32 {
    let domain_bound = if term.scale < 0 {
        trail.lower_bound_at_trail_position(term.inner, trail_position)
    } else {
        trail.upper_bound_at_trail_position(term.inner, trail_position)
    };
    term.scale * domain_bound + term.offset
}

// ======== impl TrailView for State ========

use crate::hypercube_linear::explanation::HypercubeLinear;
use crate::propagation::ExplanationContext;
use crate::state::CurrentNogood;
use crate::state::State;

impl TrailView for State {
    fn trail_position(&self, predicate: Predicate) -> Option<usize> {
        self.assignments.get_trail_position(&predicate)
    }

    fn checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize> {
        self.assignments.get_checkpoint_for_predicate(&predicate)
    }

    fn current_checkpoint(&self) -> usize {
        self.assignments.get_checkpoint()
    }

    fn trail_position_at_checkpoint(&self, checkpoint: usize) -> usize {
        self.assignments
            .get_trail_position_at_checkpoint(checkpoint)
    }

    fn predicate_at_trail_position(&self, trail_position: usize) -> Predicate {
        self.assignments.get_trail_entry(trail_position).predicate
    }

    fn truth_value_at(&self, predicate: Predicate, trail_position: usize) -> Option<bool> {
        self.assignments
            .evaluate_predicate_at_trail_position(predicate, trail_position)
    }

    fn lower_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32 {
        self.assignments
            .get_lower_bound_at_trail_position(domain, trail_position)
    }

    fn upper_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32 {
        self.assignments
            .get_upper_bound_at_trail_position(domain, trail_position)
    }

    fn current_trail_position(&self) -> usize {
        self.trail_len() - 1
    }

    fn reason_for(&mut self, pivot: Predicate) -> HypercubeLinearExplanation {
        let trail_position = self
            .assignments
            .get_trail_position(&pivot)
            .expect("pivot must be on trail");

        let (reason_ref, _) = self
            .assignments
            .get_trail_entry(trail_position)
            .reason
            .expect("pivot is propagated");

        if let Some(code) = self.reason_store.get_lazy_code(reason_ref) {
            let propagator_id = self.reason_store.get_propagator(reason_ref);

            if let Some((hypercube, linear)) = self.propagators[propagator_id]
                .explain_as_hypercube_linear(
                    code,
                    ExplanationContext::without_working_nogood(
                        &self.assignments,
                        trail_position,
                        &mut self.notification_engine,
                    ),
                )
            {
                return HypercubeLinearExplanation::Proper(HypercubeLinear { hypercube, linear });
            }
        }

        let mut clause = vec![];
        let _ = self.get_propagation_reason(pivot, &mut clause, CurrentNogood::empty());
        clause.push(!pivot);
        HypercubeLinearExplanation::Conjunction(clause)
    }
}
