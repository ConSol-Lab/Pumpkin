use std::borrow::Cow;
use std::cell::RefCell;
use std::num::NonZero;
use std::rc::Rc;

use itertools::Itertools;
use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
#[cfg(feature = "hl-checks")]
use crate::containers::HashMap;
use crate::create_statistics_struct;
use crate::hypercube_linear::BoundComparator;
use crate::hypercube_linear::BoundPredicate;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearConstructor;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::Trace;
use crate::hypercube_linear::explanation::HypercubeLinear;
use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::hypercube_linear::predicate_heap::PredicateHeap;
use crate::hypercube_linear::trail_view::TrailView;
use crate::hypercube_linear::trail_view::affine_lower_bound_at;
use crate::hypercube_linear::trail_view::affine_upper_bound_at;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
#[cfg(feature = "hl-checks")]
use crate::proof::ConstraintTag;
use crate::propagation::ExplanationContext;
use crate::state::Conflict;
use crate::state::EmptyDomainConflict;
use crate::state::State;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::TransformableVariable;

create_statistics_struct!(ResolverStatistics {
    num_propositional_resolutions: usize,
    num_skipped_propositional_resolutions: usize,
    num_propositional_resolutions_use_explanation_linear: usize,
    num_successful_fourier_resolutions: usize,
    num_integer_overflow_errors: usize,
    num_conflicts: usize,
    num_learned_clauses: usize,
    num_learned_hls: usize,
});

#[derive(Clone, Debug)]
pub struct HypercubeLinearResolver {
    /// The hypercube that is being discovered during conflict resolution.
    working_hypercube: Hypercube,
    /// Predicates in the conflicting hypercube that are true on the current decision level.
    hypercube_predicates_on_conflict_dl: PredicateHeap,
    /// Predicates in the conflict hypercube that are set at the decision level of the conflict.
    predicates_to_explain: PredicateHeap,
    /// The linear inequality of the conflict constraint.
    conflicting_linear: LinearInequality,

    /// The statistics gathered by the resolver.
    statistics: ResolverStatistics,

    /// The proof file of the analysis.
    proof_file: Rc<RefCell<Trace>>,
    /// True if the names are written to the trace, false if not.
    logged_variable_names: bool,

    /// All learned constraints with their constraint tag.
    ///
    /// Used to detect when re-learning the same constraint again.
    #[cfg(feature = "hl-checks")]
    learned_constraints: HashMap<(Hypercube, LinearInequality), ConstraintTag>,
}

impl HypercubeLinearResolver {
    pub fn new(trace: Trace) -> Self {
        Self {
            working_hypercube: Default::default(),
            hypercube_predicates_on_conflict_dl: Default::default(),
            predicates_to_explain: Default::default(),
            conflicting_linear: Default::default(),
            statistics: Default::default(),
            proof_file: Rc::new(RefCell::new(trace)),
            logged_variable_names: false,
            #[cfg(feature = "hl-checks")]
            learned_constraints: Default::default(),
        }
    }
}

impl ConflictResolver for HypercubeLinearResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        if !self.logged_variable_names {
            self.proof_file.borrow_mut().write_variables(context.state);
            self.logged_variable_names = true;
        }

        debug!("Resolving conflict with hypercube linear resolution");

        self.statistics.num_conflicts += 1;

        let conflict = match context.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator(conflict) => conflict.into(),
            StoredConflictInfo::EmptyDomain(conflict) => conflict.into(),
            _ => unreachable!("can only resolve empty domain or propagator conflicts"),
        };

        self.resolve_conflict_impl(context, conflict);
    }

    fn log_statistics(&self, logger: StatisticLogger) {
        self.statistics.log(logger);
    }
}

#[derive(Clone)]
pub(crate) struct LearnedHypercubeLinear {
    pub(crate) hypercube: Hypercube,
    pub(crate) linear: LinearInequality,
    propagates_at: usize,
}

impl HypercubeLinearResolver {
    fn resolve_conflict_impl(&mut self, context: &mut ConflictAnalysisContext, conflict: Conflict) {
        let learned_constraint = self.learn_hypercube_linear(context.state, conflict);
        let constraint_tag = context.state.new_constraint_tag();

        #[cfg(feature = "hl-checks")]
        self.assert_new_constraint(learned_constraint.clone(), constraint_tag);

        debug!(
            "Learned {} -> {}",
            learned_constraint.hypercube, learned_constraint.linear,
        );

        self.proof_file.borrow_mut().deduction(
            constraint_tag,
            learned_constraint.hypercube.iter_predicates(),
            learned_constraint.linear.terms(),
            learned_constraint.linear.bound(),
            context.state.get_checkpoint(),
            learned_constraint.propagates_at,
        );

        context.restore_to(learned_constraint.propagates_at);

        let handle = context.state.add_propagator(HypercubeLinearConstructor {
            hypercube: learned_constraint.hypercube,
            linear: learned_constraint.linear,
            constraint_tag,
        });

        debug!(
            "  with ID = {:?} and tag = {constraint_tag:?}",
            handle.propagator_id(),
        );
    }

    /// Learns a conflicting hypercube linear that would have propagated at an earlier checkpoint.
    fn learn_hypercube_linear(
        &mut self,
        state: &mut State,
        conflict: Conflict,
    ) -> LearnedHypercubeLinear {
        let (initial_predicates, conflicting_linear) =
            self.collect_initial_conflict(state, conflict);
        self.run_resolution(state, initial_predicates, conflicting_linear)
    }

    /// Set up the initial conflict from `initial_predicates` and `conflicting_linear`, then run
    /// the resolution loop.
    ///
    /// For empty-domain conflicts triggered by a hypercube linear, `conflicting_linear` contains
    /// the linear part; [`Self::explain_linear`] seeds `predicates_to_explain` with the linear
    /// term predicates at the conflict DL before the main loop starts.
    ///
    /// The returned [`LearnedHypercubeLinear`] contains the learned `(hypercube, linear)` pair.
    pub(crate) fn run_resolution(
        &mut self,
        trail: &mut impl TrailView,
        initial_predicates: impl IntoIterator<Item = Predicate>,
        conflicting_linear: LinearInequality,
    ) -> LearnedHypercubeLinear {
        assert!(self.hypercube_predicates_on_conflict_dl.is_empty());
        assert!(self.predicates_to_explain.is_empty());

        self.conflicting_linear = conflicting_linear;

        // Seed predicates_to_explain with linear term predicates at the conflict DL.
        // For hypercube-linear empty-domain conflicts this replaces the manual loop that
        // was previously in collect_initial_conflict_from_empty_domain. For trivially-false
        // linears (propagator / clausal conflicts) this is a no-op.
        let conflict_tp = trail.current_trail_position();
        let linear_for_explain = self.conflicting_linear.clone();
        self.explain_linear(trail, &linear_for_explain, conflict_tp);

        for predicate in initial_predicates {
            self.add_hypercube_predicate(trail, predicate);
        }

        self.run_resolution_loop(trail)
    }

    fn run_resolution_loop(&mut self, trail: &mut impl TrailView) -> LearnedHypercubeLinear {
        let mut trail_position = usize::MAX;

        loop {
            trace!("--------- new iteration in conflict analysis",);

            trace!(
                "conflict constraint: {} -> {}",
                self.working_hypercube
                    .iter_predicates()
                    .chain(self.hypercube_predicates_on_conflict_dl.iter())
                    .format(" & "),
                self.conflicting_linear,
            );

            trace!(
                "to explain: {}",
                self.predicates_to_explain
                    .iter()
                    .map(|p| format!("{p} @ {}", trail.trail_position(p).unwrap()))
                    .format(", "),
            );

            #[cfg(feature = "hl-checks")]
            self.assert_loop_invariants(trail, trail_position);

            if let Some(dl) = self.will_propagate_on_previous_dl(trail) {
                return self.extract_learned_hypercube_linear(trail, dl);
            }

            let pivot = self
                .predicates_to_explain
                .pop()
                .expect("there are at least two predicates to explain");

            trail_position = {
                let tp = trail
                    .trail_position(pivot)
                    .expect("all predicates are true");

                assert!(
                    trail_position >= tp,
                    "last_tp = {trail_position}, tp = {tp}"
                );

                tp
            };

            trace!("applying HL resolution on {pivot} @ {trail_position}");
            trace!(
                "  trail predicate @ {trail_position} = {}",
                trail.predicate_at_trail_position(trail_position)
            );

            if !self.contributes_to_conflict(pivot) {
                trace!("  => no longer contributes, skipping");
                continue;
            }

            self.proof_file.borrow_mut().intermediate_deduction(
                self.working_hypercube
                    .iter_predicates()
                    .chain(self.hypercube_predicates_on_conflict_dl.iter()),
                self.conflicting_linear.terms(),
                self.conflicting_linear.bound(),
            );

            let explanation = self.explain(trail, pivot);
            trace!("explanation = {explanation}");

            self.resolve(trail, trail_position, pivot, explanation);
            self.simplify_conflict();
        }
    }

    /// Simplify the conflicting hypercube linear given any equalities in the conflicting hypercube.
    fn simplify_conflict(&mut self) {
        let equality_predicates = self
            .working_hypercube
            .iter_predicates()
            .chain(self.hypercube_predicates_on_conflict_dl.iter())
            .filter_map(|predicate| {
                if predicate.is_equality_predicate() {
                    Some((predicate.get_domain(), predicate.get_right_hand_side()))
                } else {
                    None
                }
            });

        for (domain, value) in equality_predicates {
            let Some(term) = self.conflicting_linear.term_for_domain(domain) else {
                continue;
            };

            let comparator = match term.scale.is_positive() {
                true => BoundComparator::LowerBound,
                false => BoundComparator::UpperBound,
            };

            self.conflicting_linear = std::mem::take(&mut self.conflicting_linear)
                .weaken_to_zero(BoundPredicate {
                    domain,
                    comparator,
                    value,
                })
                .expect("does not weaken to trivially satisfiable");
        }
    }

    fn resolve(
        &mut self,
        trail: &mut impl TrailView,
        trail_position: usize,
        pivot: Predicate,
        explanation: HypercubeLinearExplanation,
    ) {
        if let HypercubeLinearExplanation::Proper(hl) = &explanation {
            match self.fourier_resolve(trail, trail_position, pivot, hl) {
                Ok(()) => {
                    self.statistics.num_successful_fourier_resolutions += 1;

                    #[cfg(feature = "hl-checks")]
                    assert!(
                        self.conflicting_linear
                            .term_for_domain(pivot.get_domain())
                            .is_none(),
                        "fourier resolve succeeded but pivot domain still in conflicting linear"
                    );
                }
                Err(FourierError::NoVariableElimination) => {}
                Err(FourierError::ResultOfEliminationTriviallySatisfiable) => {
                    panic!("should this happen?")
                }
                Err(FourierError::IntegerOverflow) => {
                    self.statistics.num_integer_overflow_errors += 1;
                }
            }
        }

        self.propositional_resolve(trail, trail_position, pivot, explanation);
    }

    fn explain(
        &mut self,
        trail: &mut impl TrailView,
        pivot: Predicate,
    ) -> HypercubeLinearExplanation {
        let explanation = trail.reason_for(pivot);

        match &explanation {
            HypercubeLinearExplanation::Proper(hl) => {
                trace!("explaining with HL");
                self.proof_file.borrow_mut().axiom(
                    hl.hypercube.iter_predicates(),
                    hl.linear.terms(),
                    hl.linear.bound(),
                );
            }
            HypercubeLinearExplanation::Conjunction(predicates) => {
                trace!("explaining with clause");

                // Add reason predicates (all except !pivot) to the hypercube.
                for &predicate in predicates.iter().filter(|&&p| p != !pivot) {
                    self.add_hypercube_predicate(trail, predicate);

                    #[cfg(feature = "hl-checks")]
                    {
                        let pivot_tp = trail.trail_position(pivot).expect("pivot is on trail");
                        let tp = trail
                            .trail_position(predicate)
                            .expect("all predicates are true");
                        assert!(pivot_tp >= tp, "pivot_tp = {pivot_tp}, tp = {tp}");
                    }
                }

                self.proof_file
                    .borrow_mut()
                    .axiom(predicates.iter().copied(), [], -1);
            }
        }

        explanation
    }

    /// Collects the initial conflict predicates and linear from the given [`Conflict`].
    ///
    /// Returns the hypercube predicates and the conflicting linear. For empty-domain conflicts
    /// triggered by a hypercube linear, only the hypercube predicates are returned here; the
    /// linear term predicates are seeded into [`Self::predicates_to_explain`] by
    /// [`Self::run_resolution`] via [`Self::explain_linear`].
    fn collect_initial_conflict(
        &mut self,
        state: &mut State,
        conflict: Conflict,
    ) -> (Vec<Predicate>, LinearInequality) {
        match conflict {
            Conflict::Propagator(propagator_conflict) => {
                trace!("Converting propagator conflict to hypercube");

                self.proof_file.borrow_mut().axiom(
                    propagator_conflict.conjunction.iter().copied(),
                    [],
                    -1,
                );

                (
                    propagator_conflict.conjunction.into_iter().collect(),
                    LinearInequality::trivially_false(),
                )
            }
            Conflict::EmptyDomain(empty_domain_conflict) => {
                self.collect_initial_conflict_from_empty_domain(state, empty_domain_conflict)
            }
        }
    }

    /// See [`Self::collect_initial_conflict`].
    fn collect_initial_conflict_from_empty_domain(
        &mut self,
        state: &mut State,
        empty_domain_conflict: EmptyDomainConflict,
    ) -> (Vec<Predicate>, LinearInequality) {
        let EmptyDomainConflict {
            trigger_reason,
            trigger_predicate,
            ..
        } = empty_domain_conflict;

        assert_eq!(state.truth_value(trigger_predicate), Some(false));

        let trigger_reason =
            trigger_reason.expect("cannot resolve conflict that was triggered by an assumption");

        trace!("{trigger_predicate:?} caused an empty domain, computing conflict constraint");

        // Check whether we can explain using a hypercube linear. If that is possible, use
        // it. Otherwise, fall back to the clausal explanation.
        if let Some(code) = state.reason_store.get_lazy_code(trigger_reason) {
            let propagator_id = state.reason_store.get_propagator(trigger_reason);
            let trail_position = state.trail_len() - 1;

            if let Some((hypercube, linear)) = state.propagators[propagator_id]
                .explain_as_hypercube_linear(
                    code,
                    ExplanationContext::without_working_nogood(
                        &state.assignments,
                        trail_position,
                        &mut state.notification_engine,
                    ),
                )
            {
                trace!("constructing conflict from HL");
                self.proof_file.borrow_mut().axiom(
                    hypercube.iter_predicates(),
                    linear.terms(),
                    linear.bound(),
                );

                // Only the hypercube predicates are returned; the linear term predicates at
                // the conflict DL are seeded by run_resolution via explain_linear.
                let predicates = hypercube.iter_predicates().collect();
                return (predicates, linear);
            }
        }

        trace!("constructing conflict from clause");
        let mut clausal_conflict = vec![];
        let _ = state.reason_store.get_or_compute(
            trigger_reason,
            ExplanationContext::without_working_nogood(
                &state.assignments,
                // -1 is not necessary, conflicting trail entry is undone
                state.trail_len(),
                &mut state.notification_engine,
            ),
            &mut state.propagators,
            &mut clausal_conflict,
            trigger_predicate,
        );
        clausal_conflict.push(!trigger_predicate);

        self.proof_file
            .borrow_mut()
            .axiom(clausal_conflict.iter().copied(), [], -1);

        trace!("conflicting predicate = {trigger_predicate:?}");

        if cfg!(feature = "hl-checks") {
            let unsatisfied_predicates = clausal_conflict
                .iter()
                .copied()
                .filter(|&predicate| state.truth_value(predicate) != Some(true))
                .collect::<Vec<_>>();

            if !unsatisfied_predicates.is_empty() {
                for p in unsatisfied_predicates {
                    eprintln!("  - {p} = {:?}", state.truth_value(p));
                }
                panic!("not all predicates in the conflict are satisfied");
            }
        }

        (clausal_conflict, LinearInequality::trivially_false())
    }

    /// Adds a predicate to the conflicting hypercube.
    ///
    /// Depending on the checkpoint that the predicate is assigned, the predicate is either
    /// explained further or stored as part of the final learned constraint.
    fn add_hypercube_predicate(&mut self, trail: &impl TrailView, predicate: Predicate) {
        let checkpoint = trail
            .checkpoint_for_predicate(predicate)
            .unwrap_or_else(|| panic!("adding unassigned predicate {predicate} to hypercube"));

        #[cfg(feature = "hl-checks")]
        assert!(
            trail.checkpoint_for_predicate(predicate).is_some(),
            "adding untrue predicate {predicate} to hypercube"
        );

        if checkpoint == 0 {
            // Ignore it. We don't need to process it further.
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

    /// Build the learned hypercube linear from the current state of the resolver.
    fn extract_learned_hypercube_linear(
        &mut self,
        trail: &impl TrailView,
        propagates_at: usize,
    ) -> LearnedHypercubeLinear {
        let hypercube = std::mem::take(&mut self.working_hypercube)
            .with_predicates(self.hypercube_predicates_on_conflict_dl.drain())
            .expect("can never encounter inconsistent hypercube");

        let _ = self.predicates_to_explain.drain();

        // Determine whether the linear can propagate something at some point.
        // If not, replace it with a trivially false linear to save memory and
        // registrations for bound events.
        let root_trail_position = trail.trail_position_at_checkpoint(0);
        let hl_slack_at_root = compute_hl_slack_at_trail_position(
            trail,
            &hypercube,
            &self.conflicting_linear,
            root_trail_position,
        );

        let linear = if hl_slack_at_root < 0 {
            self.statistics.num_learned_clauses += 1;

            // Make sure to add in the inferences to the proof.
            for term in self.conflicting_linear.terms() {
                let term_lb = affine_lower_bound_at(trail, term, root_trail_position);

                self.proof_file
                    .borrow_mut()
                    .axiom([predicate![term <= term_lb - 1]], [], -1);
            }

            LinearInequality::trivially_false()
        } else {
            self.statistics.num_learned_hls += 1;
            std::mem::take(&mut self.conflicting_linear)
        };

        LearnedHypercubeLinear {
            hypercube,
            linear,
            propagates_at,
        }
    }

    /// Ensures the learned constraint is a new constraint, rather than a previously learned
    /// one.
    #[cfg(feature = "hl-checks")]
    fn assert_new_constraint(
        &mut self,
        constraint: LearnedHypercubeLinear,
        constraint_tag: ConstraintTag,
    ) {
        assert_eq!(
            self.learned_constraints
                .insert((constraint.hypercube, constraint.linear), constraint_tag),
            None,
            "relearned the same constraint"
        );
    }

    fn fourier_resolve(
        &mut self,
        trail: &mut impl TrailView,
        trail_position: usize,
        pivot: Predicate,
        explanation: &HypercubeLinear,
    ) -> Result<(), FourierError> {
        let maybe_term_in_conflicting = self.conflicting_linear.term_for_domain(pivot.get_domain());
        let maybe_term_in_reason = explanation.linear.term_for_domain(pivot.get_domain());

        let (weight_in_conflicting, weight_in_reason) =
            match (maybe_term_in_conflicting, maybe_term_in_reason) {
                (Some(term_in_conflicting), Some(term_in_reason))
                    if term_in_conflicting.scale.is_positive()
                        != term_in_reason.scale.is_positive() =>
                {
                    (term_in_conflicting.scale, term_in_reason.scale)
                }

                // Either the domain is not in one of the two constraints, or they don't have
                // opposing signs. In both cases, we cannot perform fourier
                // elimination on the specified domain.
                _ => return Err(FourierError::NoVariableElimination),
            };

        let reason_hypercube_satisfied = explanation
            .hypercube
            .iter_predicates()
            .all(|p| trail.truth_value_at(p, trail_position) == Some(true));

        // This is important if the hypercube linear propagated when all but one hypercube bound
        // was satisfied.
        if !reason_hypercube_satisfied {
            return Err(FourierError::NoVariableElimination);
        }

        let contributes_to_conflict_in_linear = (weight_in_conflicting.is_positive()
            && pivot.is_lower_bound_predicate())
            || (weight_in_conflicting.is_negative() && pivot.is_upper_bound_predicate());

        // We should only do fourier elimination if the pivot predicate actually contributes to the
        // conflict. Otherwise performing the combination will not remove any contribution to the
        // conflict.
        if !contributes_to_conflict_in_linear {
            return Err(FourierError::NoVariableElimination);
        }

        let reason_slack =
            compute_linear_slack_at_trail_position(trail, &explanation.linear, trail_position);

        trace!("applying fourier elimination on {pivot}");
        trace!(
            "  - slack conflict: {}",
            compute_linear_slack_at_trail_position(trail, &self.conflicting_linear, trail_position),
        );
        trace!("  - slack b: {reason_slack}");

        let tightly_propagating_reason = compute_tightly_propagating_reason(
            trail,
            trail_position,
            explanation,
            reason_slack,
            pivot.get_domain().scaled(weight_in_reason),
        );

        trace!("  - tightly propagating: {tightly_propagating_reason}");

        let tp_slack = compute_linear_slack_at_trail_position(
            trail,
            &tightly_propagating_reason.linear,
            trail_position,
        );
        trace!("     - slack: {tp_slack}");

        // Extend the hypercube of the conflict with the predicates from the hypercube of
        // the explanation.
        for predicate in tightly_propagating_reason.hypercube.iter_predicates() {
            self.add_hypercube_predicate(trail, predicate);
        }

        self.explain_linear(trail, &tightly_propagating_reason.linear, trail_position);

        let scale_reason = weight_in_conflicting.abs();
        let mut linear_terms = self
            .conflicting_linear
            .terms()
            .map(|term| Ok((term.scale, term.inner)))
            .chain(
                tightly_propagating_reason
                    .linear
                    .terms()
                    .map(|reason_term| {
                        reason_term
                            .scale
                            .checked_mul(scale_reason)
                            .ok_or(FourierError::IntegerOverflow)
                            .map(|scaled_weight| (scaled_weight, reason_term.inner))
                    }),
            )
            .collect::<Result<Vec<(_, _)>, _>>()?;

        let mut linear_rhs = self
            .conflicting_linear
            .bound()
            .checked_add(
                tightly_propagating_reason
                    .linear
                    .bound()
                    .checked_mul(scale_reason)
                    .ok_or(FourierError::IntegerOverflow)?,
            )
            .ok_or(FourierError::IntegerOverflow)?;

        // Normalize the linear component of the hypercube linear to hopefully avoid overflows in
        // the future.
        let normalize_by = linear_terms
            .iter()
            .map(|(weight, _)| *weight)
            .chain(std::iter::once(linear_rhs))
            .reduce(gcd)
            .unwrap_or(linear_rhs);

        linear_terms.iter_mut().for_each(|(weight, _)| {
            *weight = <i32 as NumExt>::div_ceil(*weight, normalize_by);
        });
        linear_rhs = <i32 as NumExt>::div_ceil(linear_rhs, normalize_by);

        self.conflicting_linear = LinearInequality::new(
            linear_terms
                .into_iter()
                .map(|(weight, domain)| (NonZero::new(weight).unwrap(), domain)),
            linear_rhs,
        )
        .ok_or(FourierError::ResultOfEliminationTriviallySatisfiable)?;

        Ok(())
    }

    fn propositional_resolve(
        &mut self,
        trail: &mut impl TrailView,
        trail_position: usize,
        pivot: Predicate,
        mut explanation: HypercubeLinearExplanation,
    ) {
        trace!("applying propositional resolution on {pivot}");

        // No propositional resolution happens when the following are true:
        // - the pivot does not imply a predicate in the current conflict hypercube,
        // - the pivot does not contribute to the negative slack of the linear.

        let pivot_implies_predicate_in_conflict = self
            .hypercube_predicates_on_conflict_dl
            .iter()
            .any(|p| pivot.implies(p));

        let pivot_relevant_for_linear_slack = self
            .conflicting_linear
            .terms()
            .any(|t| predicate_applies_to_term(pivot, t));

        if !pivot_implies_predicate_in_conflict && !pivot_relevant_for_linear_slack {
            trace!("  => skipping propositional resolution");
            self.statistics.num_skipped_propositional_resolutions += 1;

            #[cfg(feature = "hl-checks")]
            assert!(
                !self.hypercube_predicates_on_conflict_dl.contains(pivot),
                "skipped propositional resolution but pivot still in conflict DL heap"
            );

            return;
        }

        let pivot_as_bound = BoundPredicate::new(pivot);

        // Both the conflict and the explanation are weakened on the pivot. This ensures
        // the propositional resolution removes all contribution of the pivot to the
        // conflict in the linear inequalities.
        if let Some(bound_predicate) = pivot_as_bound {
            self.hypercube_predicates_on_conflict_dl
                .push(bound_predicate.into(), trail);
            self.conflicting_linear = std::mem::take(&mut self.conflicting_linear)
                .weaken_to_zero(bound_predicate)
                .expect(
                    "weakening the conflict will never result in a trivially satisfiable linear",
                );

            trace!(
                "weakened conflict constraint: {} -> {}",
                self.working_hypercube
                    .iter_predicates()
                    .chain(self.hypercube_predicates_on_conflict_dl.iter())
                    .format(" & "),
                self.conflicting_linear,
            );

            // Then, we have to do the same on the explanation.
            explanation = std::mem::take(&mut explanation)
                .weaken_to_zero(!bound_predicate)
                .expect("cannot weaken to trivially satisfiable");

            trace!("weakened explanation: {explanation}");
        }

        self.statistics.num_propositional_resolutions += 1;

        // Remove the predicates from the hypercube that are resolved on.
        self.hypercube_predicates_on_conflict_dl
            .retain(|p| !pivot.implies(p));

        #[cfg(feature = "hl-checks")]
        assert!(
            !self.hypercube_predicates_on_conflict_dl.contains(pivot),
            "pivot still in conflict DL heap after propositional resolution"
        );

        let linear_propagated_pivot_to_false = explanation.iter_predicates().any(|p| p == !pivot);

        let linear_slack_is_negative = if let Some(linear) = explanation.linear() {
            compute_linear_slack_at_trail_position(trail, linear, trail_position - 1) < 0
        } else {
            true
        };

        let can_substitute_with_explanation_linear =
            linear_propagated_pivot_to_false && linear_slack_is_negative;

        if self.conflicting_linear.is_trivially_false() && can_substitute_with_explanation_linear {
            // If the conflicting linear is a clause, then we do not need to clausify
            // the explanation. Instead, the linear of the conflicting constraint
            // becomes the linear of the explanation and the hypercube of the conflict
            // is extended with the hypercube of the conflict.

            trace!(
                "since the linear in the conflict is trivially false, use linear from explanation"
            );

            for predicate in explanation.iter_predicates() {
                let truth_value = trail
                    .truth_value_at(predicate, trail_position)
                    .expect("all predicates in explanation hypercube are assigned");

                // If the predicate is false, then it is propagated. In that case, it does
                // not feature in the resolvent so we can continue to the next predicate.
                if !truth_value {
                    continue;
                }

                self.add_hypercube_predicate(trail, predicate);
            }

            // Set the linear from the explanation as the conflict linear.
            //
            // This requires explaining why the linear is conflicting.
            let linear = explanation.take_linear();
            self.explain_linear(trail, &linear, trail_position - 1);
            self.conflicting_linear = linear;

            self.statistics
                .num_propositional_resolutions_use_explanation_linear += 1;
        } else {
            let clausal_explanation = explanation.into_clause(trail, pivot, trail_position);

            trace!(
                "clausal explanation: {}",
                clausal_explanation.iter().format(" & ")
            );
            for predicate in clausal_explanation {
                let truth_value = trail
                    .truth_value_at(predicate, trail_position)
                    .expect("all predicates in explanation hypercube are assigned");

                // If the predicate is false, then it is propagated. In that case, it does
                // not feature in the resolvent so we can continue to the next predicate.
                if !truth_value {
                    continue;
                }

                self.add_hypercube_predicate(trail, predicate);
            }
        }
    }

    fn will_propagate_on_previous_dl(&self, trail: &impl TrailView) -> Option<usize> {
        trace!("Testing propagation at previous dl");
        let current_dl = trail.current_checkpoint();
        let decision_levels = (0..current_dl).rev();

        // Before we test whether we can backtrack, we test whether the predicates that are
        // true at the current decision level cover at most one domain. If not, then we for
        // sure cannot backtrack.
        let mut d1 = None;
        for p in self.hypercube_predicates_on_conflict_dl.iter() {
            if d1 == None {
                d1 = Some(p.get_domain());
            } else if d1 != Some(p.get_domain()) {
                // If there are two different domains in the predicates that the
                // current decision level, then we know for sure we cannot backjump.
                return None;
            }
        }

        for decision_level in decision_levels {
            trace!("  => testing dl = {decision_level}");
            let trail_position = trail.trail_position_at_checkpoint(decision_level);

            // TODO: Optimize this
            let final_hypercube = self
                .working_hypercube
                .clone()
                .with_predicates(self.hypercube_predicates_on_conflict_dl.iter())
                .expect("no inconsistent hypercube");
            if !propagates_at(
                trail,
                trail_position,
                &final_hypercube,
                &self.conflicting_linear,
            ) {
                trace!("    => does not propagate");
                // The initial assumption is that the hypercube linear is conflicting, so it will be
                // propagating at the current decision level. That means the first time it does not
                // propagate is one decision level too far back.

                let backjump_dl = decision_level + 1;

                if backjump_dl < current_dl {
                    return Some(decision_level + 1);
                } else {
                    return None;
                }
            }
        }

        // At this point the constraint is propagating at decision level 0, since that is the last
        // decision level tested in the loop.
        Some(0)
    }

    /// Assert loop invariants at the top of each resolution iteration.
    #[cfg(feature = "hl-checks")]
    fn assert_loop_invariants(&self, trail: &impl TrailView, trail_position: usize) {
        // All predicates at the conflict DL must have the current checkpoint.
        let current_cp = trail.current_checkpoint();
        for p in self.hypercube_predicates_on_conflict_dl.iter() {
            assert_eq!(
                trail.checkpoint_for_predicate(p),
                Some(current_cp),
                "predicate {p} in conflict DL heap has wrong checkpoint"
            );
        }
        for p in self.predicates_to_explain.iter() {
            assert_eq!(
                trail.checkpoint_for_predicate(p),
                Some(current_cp),
                "predicate {p} in predicates_to_explain has wrong checkpoint"
            );
        }
        // All predicates in the working hypercube must be at a strictly earlier checkpoint.
        for p in self.working_hypercube.iter_predicates() {
            let cp = trail
                .checkpoint_for_predicate(p)
                .expect("working hypercube predicate must be assigned");
            assert!(
                cp > 0 && cp < current_cp,
                "predicate {p} in working_hypercube has checkpoint {cp}, expected 0 < cp < {current_cp}"
            );
        }

        // The working conflict must be genuinely violated: all hypercube predicates satisfied and
        // linear has negative slack.
        if trail_position == usize::MAX {
            return;
        }

        let last_tp_prev_cp = trail.trail_position_at_checkpoint(current_cp - 1);
        assert!(
            trail_position > last_tp_prev_cp,
            "trail_position {trail_position} should be after previous checkpoint end {last_tp_prev_cp}"
        );

        let linear_slack =
            compute_linear_slack_at_trail_position(trail, &self.conflicting_linear, trail_position);

        if !linear_slack.is_negative() {
            eprintln!("Bounds in conflicting linear:");
            for term in self.conflicting_linear.terms() {
                let lb = affine_lower_bound_at(trail, term, trail_position);
                eprintln!(
                    "  - {} {} >= {} @ {:?}",
                    term.scale,
                    term.inner,
                    lb,
                    trail.trail_position(predicate![term >= lb])
                );
            }
            panic!(
                "conflicting constraint linear is not conflicting at trail position {trail_position}"
            );
        }

        let unsatisfied = self
            .hypercube_predicates_on_conflict_dl
            .iter()
            .chain(self.working_hypercube.iter_predicates())
            .filter(|&p| trail.truth_value_at(p, trail_position) != Some(true))
            .collect::<Vec<_>>();
        assert_eq!(
            unsatisfied,
            vec![],
            "hypercube contains unsatisfied predicates at trail position {trail_position}"
        );
    }

    fn contributes_to_conflict(&self, pivot: Predicate) -> bool {
        let is_in_hypercube = self.hypercube_predicates_on_conflict_dl.contains(pivot);
        let contributes_to_linear_conflict = self
            .conflicting_linear
            .term_for_domain(pivot.get_domain())
            .is_some_and(|term| predicate_applies_to_term(pivot, term));

        is_in_hypercube || contributes_to_linear_conflict
    }

    /// Enqueue the contributions of the linear terms to be explained.
    fn explain_linear(
        &mut self,
        trail: &impl TrailView,
        linear: &LinearInequality,
        trail_position: usize,
    ) {
        for term in linear.terms() {
            let term_bound = affine_lower_bound_at(trail, term, trail_position);
            let predicate = predicate![term >= term_bound];

            let checkpoint = trail
                .checkpoint_for_predicate(predicate)
                .expect("the predicate is true");

            // Only explain the predicate if it is at the conflict checkpoint.
            if checkpoint == trail.current_checkpoint() {
                self.predicates_to_explain.push(predicate, trail);
            }
        }
    }
}

fn predicate_applies_to_term(pivot: Predicate, term: AffineView<DomainId>) -> bool {
    if pivot.get_domain() != term.inner {
        return false;
    }

    if term.scale.is_positive() {
        pivot.is_lower_bound_predicate()
    } else {
        pivot.is_upper_bound_predicate()
    }
}

/// Returns true if the given hypercube linear propagates at the given trail position.
fn propagates_at(
    trail: &impl TrailView,
    trail_position: usize,
    hypercube: &Hypercube,
    linear: &LinearInequality,
) -> bool {
    // Get the predicates that are not assigned to true.
    let unsatisfied_predicates_in_hypercube = hypercube
        .iter_predicates()
        .filter(|&predicate| trail.truth_value_at(predicate, trail_position) != Some(true))
        .collect::<Vec<_>>();

    if unsatisfied_predicates_in_hypercube.len() > 1 {
        // If more than one predicate remains unassigned, we cannot do anything.
        return false;
    }

    let slack = compute_hl_slack_at_trail_position(trail, hypercube, linear, trail_position);

    if unsatisfied_predicates_in_hypercube.len() == 1 {
        let unassigned_predicate = unsatisfied_predicates_in_hypercube[0];
        let domain_of_predicate = unassigned_predicate.get_domain();

        if slack < 0 {
            return true;
        } else if let Some(term) = linear.term_for_domain(domain_of_predicate) {
            let bound_in_state = affine_lower_bound_at(trail, term, trail_position);
            let bound_in_hypercube = hypercube.lower_bound(&term);
            let bound_i64 = i64::from(i32::max(bound_in_state, bound_in_hypercube));

            let new_upper_bound = match i32::try_from(slack + bound_i64) {
                Ok(bound) => bound,
                Err(_) => return false,
            };

            let predicate_to_propagate = predicate![term <= new_upper_bound];
            let predicate_truth_value =
                trail.truth_value_at(predicate_to_propagate, trail_position);

            return (!unassigned_predicate).implies(predicate_to_propagate)
                && predicate_truth_value != Some(true);
        }
    } else {
        assert!(unsatisfied_predicates_in_hypercube.is_empty());
        return linear_propagates_at(trail, trail_position, linear);
    }

    false
}

/// Returns true if the given linear propagates at the given trail position.
fn linear_propagates_at(
    trail: &impl TrailView,
    trail_position: usize,
    conflicting_linear: &LinearInequality,
) -> bool {
    if conflicting_linear.is_trivially_false() {
        return true;
    }

    let slack = compute_linear_slack_at_trail_position(trail, conflicting_linear, trail_position);

    for term in conflicting_linear.terms() {
        let term_lower_bound = i64::from(affine_lower_bound_at(trail, term, trail_position));
        let new_term_upper_bound_i64 = slack + term_lower_bound;
        let new_term_upper_bound = match i32::try_from(new_term_upper_bound_i64) {
            Ok(bound) => bound,
            // The upper bound is smaller than i32::MIN, which means this would propagate (even
            // if we cannot perform the propagation due to our domains being 32-bit).
            Err(_) if new_term_upper_bound_i64.is_negative() => return true,
            // If we want to set the upper bound to a value larger than i32::MAX,
            // it can never tighten the existing bound of `term_to_propagate`.
            Err(_) => continue,
        };

        if new_term_upper_bound < affine_upper_bound_at(trail, term, trail_position) {
            return true;
        }
    }

    false
}

/// Use weakening to obtain a hypercube linear that propagates the given term without any rounding.
fn compute_tightly_propagating_reason<'expl>(
    trail: &impl TrailView,
    trail_position: usize,
    original_reason: &'expl HypercubeLinear,
    reason_slack: i64,
    pivot_term: AffineView<DomainId>,
) -> Cow<'expl, HypercubeLinear> {
    let pivot_term_upper_bound =
        reason_slack + i64::from(affine_lower_bound_at(trail, pivot_term, trail_position));

    if pivot_term_upper_bound % i64::from(pivot_term.scale) == 0 {
        return Cow::Borrowed(original_reason);
    }

    let divisor = pivot_term.scale.abs();

    let bounds_to_weaken: Vec<_> = original_reason
        .linear
        .terms()
        .filter_map(|term| {
            // If the weight of the term is divisible by the weight of the pivot term, then we
            // keep it in the linear part. Otherwise, it is weakened on.
            if term.scale % divisor == 0 {
                return None;
            }

            let bound = affine_lower_bound_at(trail, term, trail_position);
            Some(BoundPredicate::new(predicate![term >= bound]).expect("only doing bounds"))
        })
        .collect();

    let mut tightly_propagating_reason = original_reason.clone();

    for bound in bounds_to_weaken {
        tightly_propagating_reason.hypercube =
            std::mem::take(&mut tightly_propagating_reason.hypercube)
                .with_predicate(bound.into())
                .expect("bound is true and original is true so hypercube is not inconsistent");

        let term = tightly_propagating_reason
            .linear
            .term_for_domain(bound.domain)
            .expect("the bound is computed based on terms");

        let num_weakenings = (term.scale % divisor).abs();

        tightly_propagating_reason.linear = tightly_propagating_reason
            .linear
            .weaken(bound, num_weakenings)
            .expect("never becomes trivially satisfiable");
    }

    tightly_propagating_reason.linear.divide(divisor);

    Cow::Owned(tightly_propagating_reason)
}

fn compute_hl_slack_at_trail_position(
    trail: &impl TrailView,
    hypercube: &Hypercube,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| {
            let state_lb = affine_lower_bound_at(trail, term, trail_position);
            let hypercube_lb = hypercube.lower_bound(&term);
            i64::from(i32::max(state_lb, hypercube_lb))
        })
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}

fn compute_linear_slack_at_trail_position(
    trail: &impl TrailView,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| i64::from(affine_lower_bound_at(trail, term, trail_position)))
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}

enum FourierError {
    NoVariableElimination,
    ResultOfEliminationTriviallySatisfiable,
    IntegerOverflow,
}

// Taken from https://docs.rs/num-integer/latest/src/num_integer/lib.rs.html#420-422
#[allow(unused, reason = "experimentation")]
fn gcd(a: i32, b: i32) -> i32 {
    let mut m = a;
    let mut n = b;
    if m == 0 || n == 0 {
        return (m | n).abs();
    }

    // find common factors of 2
    let shift = (m | n).trailing_zeros();

    // The algorithm needs positive numbers, but the minimum value
    // can't be represented as a positive one.
    // It's also a power of two, so the gcd can be
    // calculated by bitshifting in that case

    // Assuming two's complement, the number created by the shift
    // is positive for all numbers except gcd = abs(min value)
    // The call to .abs() causes a panic in debug mode
    if m == i32::MIN || n == i32::MIN {
        let i: i32 = 1 << shift;
        return i.abs();
    }

    // guaranteed to be positive now, rest like unsigned algorithm
    m = m.abs();
    n = n.abs();

    // divide n and m by 2 until odd
    m >>= m.trailing_zeros();
    n >>= n.trailing_zeros();

    while m != n {
        if m > n {
            m -= n;
            m >>= m.trailing_zeros();
        } else {
            n -= m;
            n >>= n.trailing_zeros();
        }
    }
    m << shift
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;
    use crate::hypercube_linear::Trace;
    use crate::hypercube_linear::explanation::HypercubeLinear;
    use crate::hypercube_linear::fake_trail::FakeTrail;
    use crate::{linear_inequality, predicate};

    /// One Fourier elimination step (on x) produces y + z ≤ 7.
    ///
    /// Conflict: x + y ≤ 9,  with x ≥ 5 (at DL 2) and y ≥ 1 (at DL 2) both violated.
    /// Reason for x ≥ 5: {} → −x + z ≤ −2  (z ≥ 3 at DL 1 implies z − x ≤ −2, i.e. x ≥ z+2 ≥ 5).
    ///
    /// Fourier combination: (x + y ≤ 9) + (−x + z ≤ −2) = y + z ≤ 7.
    ///
    /// After Fourier: only y ≥ 1 remains on the conflict DL; z ≥ 3 is at an earlier DL.
    /// The constraint {z ≥ 3, y ≥ 1} → y + z ≤ 7 is unit-propagating at DL 1 (slack = −1),
    /// so we backjump there and the learned constraint is ({z ≥ 3, y ≥ 1}, y + z ≤ 7).
    #[test]
    fn one_fourier_step_yields_y_plus_z_le_7() {
        let mut trail_builder = FakeTrail::builder();

        let x = trail_builder.domain(0, 10);
        let y = trail_builder.domain(0, 10);
        let z = trail_builder.domain(0, 10);

        // Reason for x ≥ 5: {} → −x + z ≤ −2
        let reason_for_x = HypercubeLinear {
            hypercube: Hypercube::default(),
            linear: linear_inequality!(-1 x + 1 z <= -2),
        };

        let mut trail = trail_builder
            .decide(predicate![z >= 3])
            .decide(predicate![y >= 1])
            .propagate(predicate![x >= 5], reason_for_x)
            .build();

        let mut resolver = HypercubeLinearResolver::new(Trace::discard());
        let result = resolver.run_resolution(
            &mut trail,
            [predicate![x >= 5], predicate![y >= 1]],
            linear_inequality!(1 x + 1 y <= 5),
        );

        // The Fourier step eliminates x: conflict becomes y + z ≤ 3.
        // HL-slack at DL0 (y≥1, z≥3 in hypercube, bound=3): 3−1−3 = −1 < 0 → trivially_false.
        // Hypercube should contain z ≥ 3 (from DL 1) and y ≥ 1 (from DL 2).
        let expected_hypercube =
            Hypercube::new([predicate![y >= 1], predicate![z >= 3]]).expect("not inconsistent");

        assert_eq!(result.hypercube, expected_hypercube,);
        // Linear is trivially false because the HL is already a clause at DL 0.
        assert!(result.linear.is_trivially_false());
    }

    /// Propositional resolution on x ≥ 5 adds its reason predicate y ≥ 3 (DL 1) to the
    /// working hypercube. A second conflict-DL predicate w ≥ 2 keeps two domains at the conflict
    /// DL so that `will_propagate_on_previous_dl` cannot terminate early.
    ///
    /// After resolving x ≥ 5 (adding y ≥ 3 to the working hypercube), only w ≥ 2 remains on
    /// the conflict DL. The constraint {y ≥ 3, w ≥ 2} → trivially_false propagates at DL 1
    /// (HL-slack = −1 with one unsatisfied predicate), so we backjump there.
    #[test]
    fn propositional_resolution_adds_conjunction_reason_to_working_hypercube() {
        let mut trail_builder = FakeTrail::builder();

        let x = trail_builder.domain(0, 10);
        let y = trail_builder.domain(0, 10);
        let w = trail_builder.domain(0, 10);

        let mut trail = trail_builder
            .decide(predicate![y >= 3])
            .decide(predicate![w >= 2])
            .propagate(
                predicate![x >= 5],
                vec![
                    predicate![y >= 3],
                    predicate![x <= 4], // !pivot
                ],
            )
            .build();

        // Conflict: trivially_false linear with two conflict-DL predicates.
        // Two different domains → multi-domain check returns None → resolution proceeds.
        let mut resolver = HypercubeLinearResolver::new(Trace::discard());
        let result = resolver.run_resolution(
            &mut trail,
            [predicate![x >= 5], predicate![w >= 2]],
            LinearInequality::trivially_false(),
        );

        let expected_hypercube =
            Hypercube::new([predicate![y >= 3], predicate![w >= 2]]).expect("not inconsistent");

        assert_eq!(result.hypercube, expected_hypercube,);
        assert!(result.linear.is_trivially_false());
    }
}
