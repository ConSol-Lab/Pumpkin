use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::AffineView;
use crate::variables::DomainId;

/// Abstracts all trail queries the hypercube linear resolver needs.
///
/// This trait is implemented by [`crate::state::State`] for production use, and by
/// `FakeTrail` in tests.
pub(crate) trait TrailView {
    /// Get the trail position at which the given predicate became assigned.
    ///
    /// Panics if the predicate is unassigned.
    fn trail_position_of_predicate(&self, predicate: Predicate) -> Option<usize>;

    /// Get the checkpoint at which the given predicate became assigned.
    ///
    /// Panics if the predicate is unassigned.
    fn checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize>;

    /// Get the current checkpoint.
    fn current_checkpoint(&self) -> usize;

    /// Get the last trail position for the given checkpoint.
    fn trail_position_at_checkpoint(&self, checkpoint: usize) -> usize;

    /// Returns the predicate that is directly recorded at `trail_position` on the trail.
    ///
    /// Used by [`crate::hypercube_linear::predicate_heap::PredicateHeap`] to determine whether a
    /// predicate is a direct trail entry or an implied predicate.
    fn predicate_at_trail_position(&self, trail_position: usize) -> Predicate;

    /// Returns true if the entry at `trail_position` is a decision, i.e. it has no reason.
    fn is_decision(&self, trail_position: usize) -> bool;

    /// Evaluate the predicate at the given trail position.
    fn truth_value_at(&self, predicate: Predicate, trail_position: usize) -> Option<bool>;

    /// Get the lower bound of the domain at the trail position.
    fn lower_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32;

    /// Get the upper bound of the domain at the trail position.
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
/// The bound is computed in i64, since the scaled bound of a domain need not fit in an i32.
pub(super) fn affine_lower_bound_at<T: TrailView + ?Sized>(
    trail: &T,
    term: AffineView<DomainId>,
    trail_position: usize,
) -> i64 {
    let domain_bound = if term.scale < 0 {
        trail.upper_bound_at_trail_position(term.inner, trail_position)
    } else {
        trail.lower_bound_at_trail_position(term.inner, trail_position)
    };
    i64::from(term.scale) * i64::from(domain_bound) + i64::from(term.offset)
}

/// Computes the upper bound of an [`AffineView`] at the given trail position.
///
/// The bound is computed in i64, since the scaled bound of a domain need not fit in an i32.
pub(super) fn affine_upper_bound_at<T: TrailView + ?Sized>(
    trail: &T,
    term: AffineView<DomainId>,
    trail_position: usize,
) -> i64 {
    let domain_bound = if term.scale < 0 {
        trail.lower_bound_at_trail_position(term.inner, trail_position)
    } else {
        trail.upper_bound_at_trail_position(term.inner, trail_position)
    };
    i64::from(term.scale) * i64::from(domain_bound) + i64::from(term.offset)
}

/// The predicate `[term >= lb(term)]` at the given trail position, expressed over the domain of
/// the term, so that it can be represented even if the scaled bound does not fit in an i32.
pub(super) fn affine_lower_bound_predicate_at<T: TrailView + ?Sized>(
    trail: &T,
    term: AffineView<DomainId>,
    trail_position: usize,
) -> Predicate {
    let domain = term.inner;
    if term.scale < 0 {
        let bound = trail.upper_bound_at_trail_position(domain, trail_position);
        predicate![domain <= bound]
    } else {
        let bound = trail.lower_bound_at_trail_position(domain, trail_position);
        predicate![domain >= bound]
    }
}

// ======== impl TrailView for State ========

use crate::hypercube_linear::explanation::HypercubeLinear;
use crate::propagation::ExplanationContext;
use crate::state::CurrentNogood;
use crate::state::State;

impl TrailView for State {
    fn trail_position_of_predicate(&self, predicate: Predicate) -> Option<usize> {
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

    fn is_decision(&self, trail_position: usize) -> bool {
        self.assignments
            .get_trail_entry(trail_position)
            .reason
            .is_none()
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

        let trail_entry = self.assignments.get_trail_entry(trail_position);

        // The hypercube linear of the propagator explains the trail entry, so it only explains the
        // pivot if the entry implies the pivot. When holes make the pivot stronger than the entry,
        // e.g. [x <= -1] from the entry [x <= 0] and the hole [x != 0], the propositional reason
        // below decomposes the pivot into the entry and the holes.
        if trail_entry.predicate.implies(pivot) {
            let reason_ref = trail_entry.reason.expect("pivot is propagated");

            if let Some(code) = self.reason_store.get_lazy_code(reason_ref) {
                let propagator_id = self.reason_store.get_propagator(reason_ref);

                if let Some((hypercube, linear, _)) = self.propagators[propagator_id]
                    .explain_as_hypercube_linear(
                        code,
                        trail_entry.predicate,
                        ExplanationContext::without_working_nogood(
                            &self.assignments,
                            trail_position,
                            &mut self.notification_engine,
                        ),
                    )
                {
                    return HypercubeLinearExplanation::Proper(HypercubeLinear {
                        hypercube,
                        linear,
                    });
                }
            }
        }

        let mut nogood = vec![];
        let _ = self.get_propagation_reason(pivot, &mut nogood, CurrentNogood::empty());
        nogood.push(!pivot);
        HypercubeLinearExplanation::Conjunction(nogood)
    }
}
