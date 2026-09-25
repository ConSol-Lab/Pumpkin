use std::cell::RefCell;
use std::rc::Rc;

use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::Trace;
use crate::hypercube_linear::predicate_heap::PredicateHeap;
use crate::hypercube_linear::trail_view::TrailView;
use crate::hypercube_linear::trail_view::affine_lower_bound_at;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::AffineView;
use crate::variables::DomainId;

#[derive(Clone, Debug)]
pub(crate) struct ConflictState {
    pub(crate) working_hypercube: Hypercube,
    pub(crate) hypercube_predicates_on_conflict_dl: PredicateHeap,
    pub(crate) predicates_to_explain: PredicateHeap,
    pub(crate) conflicting_linear: LinearInequality,
    pub(crate) proof_file: Rc<RefCell<Trace>>,
}

impl ConflictState {
    pub(crate) fn new(proof_file: Rc<RefCell<Trace>>) -> Self {
        Self {
            working_hypercube: Default::default(),
            hypercube_predicates_on_conflict_dl: Default::default(),
            predicates_to_explain: Default::default(),
            conflicting_linear: Default::default(),
            proof_file,
        }
    }

    /// Adds a predicate to the conflicting hypercube.
    ///
    /// Depending on the checkpoint that the predicate is assigned, the predicate is either
    /// explained further or stored as part of the final learned constraint.
    pub(crate) fn add_hypercube_predicate(&mut self, trail: &dyn TrailView, predicate: Predicate) {
        let checkpoint = trail
            .checkpoint_for_predicate(predicate)
            .unwrap_or_else(|| panic!("adding unassigned predicate {predicate} to hypercube"));

        #[cfg(feature = "hl-checks")]
        assert!(
            trail.checkpoint_for_predicate(predicate).is_some(),
            "adding untrue predicate {predicate} to hypercube"
        );

        if checkpoint == 0 {
            self.proof_file.borrow_mut().axiom([!predicate], [], -1);
        } else if checkpoint == trail.current_checkpoint() {
            self.predicates_to_explain.push(predicate, trail);
            self.hypercube_predicates_on_conflict_dl
                .push(predicate, trail);
        } else {
            self.working_hypercube = std::mem::take(&mut self.working_hypercube)
                .with_predicate(predicate)
                .expect("cannot create trivially false hypercube");
        }
    }

    /// Enqueue the contributions of the linear terms to be explained.
    pub(crate) fn explain_linear(
        &mut self,
        trail: &dyn TrailView,
        linear: &LinearInequality,
        trail_position: usize,
    ) {
        for term in linear.terms() {
            let term_bound = affine_lower_bound_at(trail, term, trail_position);
            let predicate = predicate![term >= term_bound];

            let checkpoint = trail
                .checkpoint_for_predicate(predicate)
                .expect("the predicate is true");

            if checkpoint == trail.current_checkpoint() {
                self.predicates_to_explain.push(predicate, trail);
            }
        }
    }

    pub(crate) fn contributes_to_conflict(&self, pivot: Predicate) -> bool {
        let is_in_hypercube = self.hypercube_predicates_on_conflict_dl.contains(pivot);
        let contributes_to_linear_conflict = self
            .conflicting_linear
            .term_for_domain(pivot.get_domain())
            .is_some_and(|term| predicate_applies_to_term(pivot, term));

        is_in_hypercube || contributes_to_linear_conflict
    }
}

pub(super) fn predicate_applies_to_term(pivot: Predicate, term: AffineView<DomainId>) -> bool {
    if pivot.get_domain() != term.inner {
        return false;
    }

    if term.scale.is_positive() {
        pivot.is_lower_bound_predicate()
    } else {
        pivot.is_upper_bound_predicate()
    }
}
