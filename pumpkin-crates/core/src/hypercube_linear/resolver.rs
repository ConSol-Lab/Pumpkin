use std::cell::RefCell;
use std::fmt::Display;
use std::num::NonZero;
use std::rc::Rc;

use itertools::Itertools;
use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
#[cfg(feature = "hl-checks")]
use crate::containers::HashSet;
use crate::create_statistics_struct;
use crate::hypercube_linear::BoundComparator;
use crate::hypercube_linear::BoundPredicate;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearConstructor;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::Trace;
use crate::hypercube_linear::predicate_heap::PredicateHeap;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::ExplanationContext;
use crate::state::Conflict;
use crate::state::EmptyDomainConflict;
use crate::state::State;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

create_statistics_struct!(ResolverStatistics {
    num_propositional_resolutions: usize,
    num_successful_fourier_resolutions: usize,
    num_integer_overflow_errors: usize,
    num_conflicts: usize,
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

    /// A temporary buffer for storing reasons.
    reason_buffer: Vec<Predicate>,

    /// The proof file of the analysis.
    proof_file: Rc<RefCell<Trace>>,

    /// Set of all learned constraints.
    ///
    /// Used to detect when re-learning the same constraint again.
    #[cfg(feature = "hl-checks")]
    learned_constraints: HashSet<(Hypercube, LinearInequality)>,
}

impl HypercubeLinearResolver {
    pub fn new(trace: Trace) -> Self {
        Self {
            working_hypercube: Default::default(),
            hypercube_predicates_on_conflict_dl: Default::default(),
            predicates_to_explain: Default::default(),
            conflicting_linear: Default::default(),
            statistics: Default::default(),
            reason_buffer: Default::default(),
            proof_file: Rc::new(RefCell::new(trace)),
            #[cfg(feature = "hl-checks")]
            learned_constraints: Default::default(),
        }
    }
}

impl ConflictResolver for HypercubeLinearResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
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
struct LearnedHypercubeLinear {
    hypercube: Hypercube,
    linear: LinearInequality,
    propagates_at: usize,
}

impl HypercubeLinearResolver {
    fn resolve_conflict_impl(&mut self, context: &mut ConflictAnalysisContext, conflict: Conflict) {
        let learned_constraint = self.learn_hypercube_linear(context.state, conflict);

        #[cfg(feature = "hl-checks")]
        self.assert_new_constraint(learned_constraint.clone());

        debug!(
            "Learned {} -> {}",
            learned_constraint.hypercube, learned_constraint.linear,
        );

        context.restore_to(learned_constraint.propagates_at);

        let constraint_tag = context.state.new_constraint_tag();

        self.proof_file.borrow_mut().deduction(
            constraint_tag,
            learned_constraint.hypercube.iter_predicates(),
            learned_constraint.linear.terms(),
            learned_constraint.linear.bound(),
        );

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

    /// Learns a conflicting hypercube linear that would have propagated at an earlier
    /// checkpoint.
    fn learn_hypercube_linear(
        &mut self,
        state: &mut State,
        conflict: Conflict,
    ) -> LearnedHypercubeLinear {
        assert!(self.hypercube_predicates_on_conflict_dl.is_empty());
        assert!(self.predicates_to_explain.is_empty());

        self.compute_conflicting_hypercube_linear(state, conflict);

        let mut trail_position = usize::MAX;

        loop {
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
                    .map(|p| format!("{p} @ {}", state.trail_position(p).unwrap()))
                    .format(", "),
            );

            if cfg!(feature = "hl-checks") {
                self.assert_invariants(state, trail_position);
            }

            if let Some(dl) = self.will_propagate_on_previous_dl(state) {
                return self.extract_learned_hypercube_linear(dl);
            }

            let pivot = self
                .predicates_to_explain
                .pop()
                .expect("there are at least two predicates to explain");

            trail_position = {
                let tp = state
                    .trail_position(pivot)
                    .expect("all predicates are true");

                assert!(
                    trail_position >= tp,
                    "last_tp = {trail_position}, tp = {tp}"
                );

                tp
            };

            trace!("applying HL resolution on {pivot} @ {trail_position}");

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

            let explanation = self.explain(state, pivot, trail_position);
            trace!("explanation = {explanation}");

            self.resolve(state, trail_position, pivot, explanation);
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
        state: &mut State,
        trail_position: usize,
        pivot: Predicate,
        explanation: HypercubeLinearExplanation,
    ) {
        if let HypercubeLinearExplanation::Proper(hl) = &explanation {
            match self.fourier_resolve(state, pivot, hl) {
                Ok(()) => {
                    self.statistics.num_successful_fourier_resolutions += 1;
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

        self.propositional_resolve(state, trail_position, pivot, explanation);
    }

    fn explain(
        &mut self,
        state: &mut State,
        pivot: Predicate,
        trail_position: usize,
    ) -> HypercubeLinearExplanation {
        assert!(self.reason_buffer.is_empty());

        // If possible, explain using hypercube linear.
        if state.is_on_trail(pivot) {
            let (reason_ref, _) = state
                .trail_entry(trail_position)
                .reason
                .expect("pivot is propagated");

            if let Some(code) = state.reason_store.get_lazy_code(reason_ref) {
                let propagator_id = state.reason_store.get_propagator(reason_ref);

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
                    trace!("explaining with HL");

                    self.proof_file.borrow_mut().axiom(
                        hypercube.iter_predicates(),
                        linear.terms(),
                        linear.bound(),
                    );

                    for predicate in hypercube.iter_predicates() {
                        // The predicate may be false if it was propagated by the
                        // linear being a conflict. In that case, we do not need to
                        // explain the predicate.
                        if !predicate.implies(!pivot) {
                            self.add_hypercube_predicate(state, predicate);
                        }
                    }

                    for term in linear.terms() {
                        let term_bound =
                            term.lower_bound_at_trail_position(&state.assignments, trail_position);
                        let predicate = predicate![term >= term_bound];

                        let checkpoint = state
                            .get_checkpoint_for_predicate(predicate)
                            .expect("the predicate is true");

                        // Only explain the predicate if it is at the conflict
                        // checkpoint.
                        if checkpoint == state.get_checkpoint() {
                            self.predicates_to_explain.push(predicate, state);
                        }
                    }

                    return HypercubeLinearExplanation::Proper(HypercubeLinear {
                        hypercube,
                        linear,
                    });
                }
            }
        }

        trace!("explaining with clause");

        // Enqueue predicates to be explained.
        //
        // For this we do some borrow-checker evasion.
        let reason_buffer = std::mem::take(&mut self.reason_buffer);
        for predicate in reason_buffer.iter().copied() {
            self.add_hypercube_predicate(state, predicate);

            if cfg!(feature = "hl-checks") {
                let tp = state
                    .trail_position(predicate)
                    .expect("all predicates are true");

                let pivot_tp = state
                    .trail_position(pivot)
                    .expect("all predicates are true");

                assert!(pivot_tp >= tp, "pivot_tp = {pivot_tp}, tp = {tp}");
            }
        }
        self.reason_buffer = reason_buffer;
        self.reason_buffer.push(!pivot);

        self.proof_file
            .borrow_mut()
            .axiom(self.reason_buffer.iter().copied(), [], -1);

        // In this case the explanation explains the semantics of the atomic
        // constraint. That results in an inconsistent hypercube, but we need to
        // perform the resolution anyways.
        HypercubeLinearExplanation::Conjunction(self.reason_buffer.drain(..).collect())
    }

    /// Computes the conflicting hypercube linear constraint.
    ///
    /// The hypercube is added to the hypercube on `self`, and the linear inequality is
    /// returned here.
    fn compute_conflicting_hypercube_linear(&mut self, state: &mut State, conflict: Conflict) {
        match conflict {
            Conflict::Propagator(propagator_conflict) => {
                trace!("Converting propagator conflict to hypercube");

                self.proof_file.borrow_mut().axiom(
                    propagator_conflict.conjunction.iter().copied(),
                    [],
                    -1,
                );

                for predicate in propagator_conflict.conjunction {
                    self.add_hypercube_predicate(state, predicate);
                }

                self.conflicting_linear = LinearInequality::trivially_false();
            }
            Conflict::EmptyDomain(empty_domain_conflict) => self
                .compute_conflicting_hypercube_linear_from_empty_domain(
                    state,
                    empty_domain_conflict,
                ),
        }
    }

    /// See [`Self::compute_conflicting_hypercube_linear`].
    fn compute_conflicting_hypercube_linear_from_empty_domain(
        &mut self,
        state: &mut State,
        empty_domain_conflict: EmptyDomainConflict,
    ) {
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
                self.proof_file.borrow_mut().axiom(
                    hypercube.iter_predicates(),
                    linear.terms(),
                    linear.bound(),
                );

                for predicate in hypercube.iter_predicates() {
                    self.add_hypercube_predicate(state, predicate);
                }

                for term in linear.terms() {
                    let term_bound =
                        term.lower_bound_at_trail_position(&state.assignments, trail_position);
                    let predicate = predicate![term >= term_bound];

                    let checkpoint = state
                        .get_checkpoint_for_predicate(predicate)
                        .expect("the predicate is true");

                    // Only explain the predicate if it is at the conflict
                    // checkpoint.
                    if checkpoint == state.get_checkpoint() {
                        self.predicates_to_explain.push(predicate, state);
                    }
                }

                self.conflicting_linear = linear;
                return;
            }
        }

        let mut clausal_conflict = vec![];
        let _ = state.reason_store.get_or_compute(
            trigger_reason,
            ExplanationContext::without_working_nogood(
                &state.assignments,
                state.trail_len() - 1,
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
            assert!(
                clausal_conflict
                    .iter()
                    .all(|&predicate| state.truth_value(predicate) == Some(true))
            );
        }

        for predicate in clausal_conflict {
            self.add_hypercube_predicate(state, predicate);
        }

        self.conflicting_linear = LinearInequality::trivially_false();
    }

    /// Adds a predicate to the conflicting hypercube.
    ///
    /// Depending on the checkpoint that the predicate is assigned, the predicate is either
    /// explained further or stored as part of the final learned constraint.
    fn add_hypercube_predicate(&mut self, state: &State, predicate: Predicate) {
        let checkpoint = state
            .get_checkpoint_for_predicate(predicate)
            .unwrap_or_else(|| panic!("adding unassigned predicate {predicate} to hypercube"));

        if cfg!(feature = "hl-checks") {
            assert_eq!(
                state.truth_value(predicate),
                Some(true),
                "adding untrue predicate {predicate} to hypercube"
            );
        }

        if checkpoint == 0 {
            // Ignore it. We don't need to process it further.
            self.proof_file.borrow_mut().axiom([!predicate], [], -1);
        } else if checkpoint == state.get_checkpoint() {
            self.predicates_to_explain.push(predicate, state);
            self.hypercube_predicates_on_conflict_dl
                .push(predicate, state);
        } else {
            self.working_hypercube = std::mem::take(&mut self.working_hypercube)
                .with_predicate(predicate)
                .expect("cannot create trivially false hypercube");
        }
    }

    /// Build the learned hypercube linear from the current state of the resolver.
    fn extract_learned_hypercube_linear(&mut self, propagates_at: usize) -> LearnedHypercubeLinear {
        let hypercube = std::mem::take(&mut self.working_hypercube)
            .with_predicates(self.hypercube_predicates_on_conflict_dl.drain())
            .expect("can never encounter inconsistent hypercube");

        let _ = self.predicates_to_explain.drain();

        LearnedHypercubeLinear {
            hypercube,
            linear: std::mem::take(&mut self.conflicting_linear),
            propagates_at,
        }
    }

    /// Ensures the learned constraint is a new constraint, rather than a previously learned
    /// one.
    #[cfg(feature = "hl-checks")]
    fn assert_new_constraint(&mut self, constraint: LearnedHypercubeLinear) {
        assert!(
            self.learned_constraints
                .insert((constraint.hypercube, constraint.linear)),
            "relearned the same constraint"
        );
    }

    fn fourier_resolve(
        &mut self,
        state: &State,
        pivot: Predicate,
        explanation: &HypercubeLinear,
    ) -> Result<(), FourierError> {
        let trail_position = state
            .assignments
            .get_trail_position(&pivot)
            .expect("the pivot is true");

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

        let reason_hypercube_satisfied = explanation.hypercube.iter_predicates().all(|p| {
            state
                .assignments
                .evaluate_predicate_at_trail_position(p, trail_position)
                == Some(true)
        });

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
            compute_linear_slack_at_trail_position(state, &explanation.linear, trail_position);

        trace!("applying fourier elimination on {pivot}",);
        trace!(
            "  - slack conflict: {}",
            compute_linear_slack_at_trail_position(state, &self.conflicting_linear, trail_position),
        );
        trace!("  - slack b: {reason_slack}");

        let tightly_propagating_reason = compute_tightly_propagating_reason(
            state,
            trail_position,
            explanation,
            pivot.get_domain().scaled(weight_in_reason),
        );

        trace!("  - tightly propagating: {tightly_propagating_reason}");

        let tp_slack = compute_linear_slack_at_trail_position(
            state,
            &tightly_propagating_reason.linear,
            trail_position,
        );
        trace!("     - slack: {tp_slack}");

        let term_in_reformulated_reason = tightly_propagating_reason
            .linear
            .term_for_domain(pivot.get_domain())
            .unwrap();
        assert_eq!(term_in_reformulated_reason.scale.abs(), 1);

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

        for predicate in tightly_propagating_reason.hypercube.iter_predicates() {
            self.add_hypercube_predicate(state, predicate);
        }

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
        state: &State,
        trail_position: usize,
        pivot: Predicate,
        mut explanation: HypercubeLinearExplanation,
    ) {
        trace!("applying propositional resolution on {pivot}");

        let pivot_as_bound = BoundPredicate::new(pivot);

        // Both the conflict and the explanation are weakened on the pivot. This ensures
        // the propositional resolution removes all contribution of the pivot to the
        // conflict in the linear inequalities.
        if let Some(bound_predicate) = pivot_as_bound {
            self.conflicting_linear = std::mem::take(&mut self.conflicting_linear)
                .weaken_to_zero(bound_predicate)
                .expect(
                    "weakening the conflict will never result in a trivially satisfiable linear",
                );

            trace!(
                "weakened conflict constraint: {pivot} & {} -> {}",
                self.working_hypercube
                    .iter_predicates()
                    .chain(self.hypercube_predicates_on_conflict_dl.iter())
                    .format(" & "),
                self.conflicting_linear,
            );

            // Then, we have to do the same on the explanation.
            //
            // This is unnecessary if we end up converting the explanation to a clause.
            // However, for alternative resolution implementations it may be useful to
            // have the explanation without any of the integer contribution in the
            // linear.
            explanation = std::mem::take(&mut explanation)
                .weaken_to_zero(!bound_predicate)
                .expect("cannot weaken to trivially satisfiable");

            trace!("weakened explanation: {explanation}");
        }

        // Remove the predicates from the hypercube that are resolved on.
        self.hypercube_predicates_on_conflict_dl
            .retain(|p| !pivot.implies(p));

        let clausal_explanation = explanation.into_clause(state, trail_position);
        trace!(
            "clausal explanation: {}",
            clausal_explanation.iter().format(" & ")
        );
        for predicate in clausal_explanation {
            let truth_value = state
                .truth_value(predicate)
                .expect("all predicates in explanation hypercube are assigned");

            // If the predicate is false, then it is propagated. In that case, it does
            // not feature in the resolvent so we can continue to the next predicate.
            if !truth_value {
                continue;
            }

            self.add_hypercube_predicate(state, predicate);
        }
    }

    fn will_propagate_on_previous_dl(&self, state: &State) -> Option<usize> {
        trace!("Testing propagation at previous dl");
        let current_dl = state.get_checkpoint();
        let decision_levels = (0..current_dl).rev();

        for decision_level in decision_levels {
            trace!("  => testing dl = {decision_level}");
            let trail_position = state
                .assignments
                .get_trail_position_at_checkpoint(decision_level);

            if !self.propagates_at(state, trail_position) {
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

    /// Returns true if the given hypercube linear propagates at the given trail position.
    fn propagates_at(&self, state: &State, trail_position: usize) -> bool {
        // TODO: Optimize this
        let final_hypercube = self
            .working_hypercube
            .clone()
            .with_predicates(self.hypercube_predicates_on_conflict_dl.iter())
            .expect("no inconsistent hypercube");

        // println!(
        //     "testing propagation of {final_hypercube} -> {}",
        //     self.conflicting_linear
        // );

        // Get the predicates that are not assigned to true.
        let unsatisfied_predicates_in_hypercube = final_hypercube
            .iter_predicates()
            .filter(|&predicate| {
                state
                    .assignments
                    .evaluate_predicate_at_trail_position(predicate, trail_position)
                    != Some(true)
            })
            .collect::<Vec<_>>();

        if unsatisfied_predicates_in_hypercube.len() > 1 {
            // dbg!(unsatisfied_predicates_in_hypercube);
            // If more than one predicate remains unassigned, we cannot do anything.
            return false;
        }

        let slack =
            compute_linear_slack_at_trail_position(state, &self.conflicting_linear, trail_position);

        if unsatisfied_predicates_in_hypercube.len() == 1 {
            let unassigned_predicate = unsatisfied_predicates_in_hypercube[0];

            if slack < 0 {
                return true;
            } else if let Some(term) = self
                .conflicting_linear
                .term_for_domain(unassigned_predicate.get_domain())
            {
                let term_lower_bound =
                    term.lower_bound_at_trail_position(&state.assignments, trail_position);
                let new_upper_bound = match i32::try_from(slack + i64::from(term_lower_bound)) {
                    Ok(bound) => bound,
                    Err(_) => return false,
                };

                return new_upper_bound
                    < term.upper_bound_at_trail_position(&state.assignments, trail_position);
            }
        } else {
            assert!(unsatisfied_predicates_in_hypercube.is_empty());
            return self.linear_propagates_at(state, trail_position);
        }

        false
    }

    /// Returns true if the given linear propagates at the given trail position.
    fn linear_propagates_at(&self, state: &State, trail_position: usize) -> bool {
        if self.conflicting_linear.is_trivially_false() {
            return true;
        }

        let slack =
            compute_linear_slack_at_trail_position(state, &self.conflicting_linear, trail_position);

        for term in self.conflicting_linear.terms() {
            let term_lower_bound =
                i64::from(term.lower_bound_at_trail_position(&state.assignments, trail_position));
            let new_term_upper_bound_i64 = slack + term_lower_bound;
            let new_term_upper_bound = match i32::try_from(new_term_upper_bound_i64) {
                Ok(bound) => bound,
                // The upper bound is smaller than i32::MIN, which means this would propagate (even
                // if wee cannot perform the propagation due to our domains being
                // 32-bit).
                Err(_) if new_term_upper_bound_i64.is_negative() => return true,
                // If we want to set the upper bound to a value larger than i32::MAX,
                // it can never tighten the existing bound of `term_to_propagate`.
                Err(_) => continue,
            };

            if new_term_upper_bound
                < term.upper_bound_at_trail_position(&state.assignments, trail_position)
            {
                return true;
            }
        }

        false
    }

    /// Assert the invariants of the resolver are kept.
    fn assert_invariants(&self, state: &State, trail_position: usize) {
        let last_trail_position_at_previous_checkpoint = state
            .assignments
            .get_trail_position_at_checkpoint(state.get_checkpoint() - 1);

        assert!(trail_position > last_trail_position_at_previous_checkpoint);

        let linear_slack =
            compute_linear_slack_at_trail_position(state, &self.conflicting_linear, trail_position);

        if !linear_slack.is_negative() {
            eprintln!("Bounds:");

            for term in self.conflicting_linear.terms() {
                let lb = term.lower_bound_at_trail_position(&state.assignments, trail_position);
                eprintln!("  - {} {} >= {}", term.scale, term.inner, lb);
            }

            panic!(
                "conflicting constraint linear is not conflicting at trail position {trail_position}"
            );
        }

        let unsatisfied_hypercube_predicates = self
            .hypercube_predicates_on_conflict_dl
            .iter()
            .chain(self.working_hypercube.iter_predicates())
            .filter(|&p| {
                state
                    .assignments
                    .evaluate_predicate_at_trail_position(p, trail_position)
                    != Some(true)
            })
            .collect::<Vec<_>>();
        assert_eq!(
            unsatisfied_hypercube_predicates,
            vec![],
            "hypercube of conflicting constraint contains unsatisfied predicates at trail position {trail_position}"
        );
    }

    fn contributes_to_conflict(&self, pivot: Predicate) -> bool {
        let is_in_hypercube = self.hypercube_predicates_on_conflict_dl.contains(pivot);
        let contributes_to_linear_conflict = self
            .conflicting_linear
            .term_for_domain(pivot.get_domain())
            .is_some_and(|term| {
                if term.scale.is_positive() {
                    pivot.is_lower_bound_predicate()
                } else {
                    pivot.is_upper_bound_predicate()
                }
            });

        is_in_hypercube || contributes_to_linear_conflict
    }
}

/// Use weakening to obtain a hypercube linear that has the term for `pivot_domain` be unit in the
/// linear part.
fn compute_tightly_propagating_reason(
    state: &State,
    trail_position: usize,
    original_reason: &HypercubeLinear,
    pivot_term: AffineView<DomainId>,
) -> HypercubeLinear {
    let divisor = pivot_term.scale.abs();

    let bounds_to_weaken: Vec<_> = original_reason
        .linear
        .terms()
        .filter_map(|term| {
            // If the weight of the term is divisible by the weight of the pivot term, then we
            // keep it in the linear part. Otherwise, it is weakened on.

            if term.scale % divisor == 0 {
                // We can divide this term by the pivot term. So no need to weaken on it.
                return None;
            }

            let bound = term.lower_bound_at_trail_position(&state.assignments, trail_position);
            Some(BoundPredicate::new(predicate![term >= bound]).expect("only doing bounds"))
        })
        .collect();

    let mut tightly_propagating_reason = original_reason.clone();
    for bound in bounds_to_weaken {
        tightly_propagating_reason.hypercube =
            std::mem::take(&mut tightly_propagating_reason.hypercube)
                .with_predicate(bound.into())
                .expect("bound is true and original is true so hypercube is not inconsistent");

        tightly_propagating_reason.linear = tightly_propagating_reason
            .linear
            .weaken_to_zero(bound)
            .expect("never becomes trivially satisfiable");
    }

    tightly_propagating_reason.linear.divide(divisor);

    tightly_propagating_reason
}

fn compute_linear_slack_at_trail_position(
    state: &State,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| {
            i64::from(term.lower_bound_at_trail_position(&state.assignments, trail_position))
        })
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}

#[derive(Clone)]
struct HypercubeLinear {
    hypercube: Hypercube,
    linear: LinearInequality,
}

impl Display for HypercubeLinear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.hypercube, self.linear,)
    }
}

#[derive(Clone)]
enum HypercubeLinearExplanation {
    Proper(HypercubeLinear),
    Conjunction(Vec<Predicate>),
}

impl Default for HypercubeLinearExplanation {
    fn default() -> Self {
        HypercubeLinearExplanation::Conjunction(vec![])
    }
}

impl HypercubeLinearExplanation {
    fn into_clause(self, state: &State, trail_position: usize) -> Vec<Predicate> {
        let mut hypercube_linear = match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => hypercube_linear,
            HypercubeLinearExplanation::Conjunction(predicates) => return predicates,
        };

        let mut clause = hypercube_linear
            .hypercube
            .iter_predicates()
            .collect::<Vec<_>>();

        while !hypercube_linear.linear.is_trivially_false() {
            let next_term = hypercube_linear
                .linear
                .terms()
                .next()
                .expect("conversion to clause only possible when linear is conflicting");

            let term_bound =
                next_term.lower_bound_at_trail_position(&state.assignments, trail_position);

            let predicate_to_weaken_on = BoundPredicate::new(predicate![next_term >= term_bound])
                .expect("the predicate is a bound predicate");

            hypercube_linear.linear = std::mem::take(&mut hypercube_linear.linear)
                .weaken_to_zero(predicate_to_weaken_on)
                .expect("conversion to clause only possible when linear is conflicting");

            clause.push(predicate_to_weaken_on.into());
        }

        clause
    }

    fn weaken_to_zero(self, bound: BoundPredicate) -> Option<Self> {
        match self {
            HypercubeLinearExplanation::Proper(mut hypercube_linear) => {
                hypercube_linear.linear =
                    std::mem::take(&mut hypercube_linear.linear).weaken_to_zero(bound)?;

                hypercube_linear.hypercube = std::mem::take(&mut hypercube_linear.hypercube)
                    .with_predicate(bound.into())
                    .expect("should never construct inconsistent hypercube");

                Some(HypercubeLinearExplanation::Proper(hypercube_linear))
            }

            HypercubeLinearExplanation::Conjunction(predicates) => {
                Some(HypercubeLinearExplanation::Conjunction(predicates))
            }
        }
    }
}

impl Display for HypercubeLinearExplanation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HypercubeLinearExplanation::Proper(hypercube_linear) => {
                write!(f, "{hypercube_linear}")
            }
            HypercubeLinearExplanation::Conjunction(predicates) => {
                write!(f, "{} ->  <= -1", predicates.iter().format(" & "))
            }
        }
    }
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
