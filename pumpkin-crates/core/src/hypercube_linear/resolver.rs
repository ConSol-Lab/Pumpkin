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
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
#[cfg(feature = "hl-checks")]
use crate::proof::ConstraintTag;
use crate::propagation::ExplanationContext;
use crate::state::Conflict;
use crate::state::CurrentNogood;
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

    /// A temporary buffer for storing reasons.
    reason_buffer: Vec<Predicate>,

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
            reason_buffer: Default::default(),
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
struct LearnedHypercubeLinear {
    hypercube: Hypercube,
    linear: LinearInequality,
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

        context.restore_to(learned_constraint.propagates_at);

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
                    .map(|p| format!("{p} @ {}", state.trail_position(p).unwrap()))
                    .format(", "),
            );

            if cfg!(feature = "hl-checks") {
                self.assert_invariants(state, trail_position);
            }

            if let Some(dl) = self.will_propagate_on_previous_dl(state) {
                return self.extract_learned_hypercube_linear(state, dl);
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

                    return HypercubeLinearExplanation::Proper(HypercubeLinear {
                        hypercube,
                        linear,
                    });
                }
            }
        }

        trace!("explaining with clause");
        let _ =
            state.get_propagation_reason(pivot, &mut self.reason_buffer, CurrentNogood::empty());

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
                trace!("constructing conflict from HL");
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
    fn extract_learned_hypercube_linear(
        &mut self,
        state: &State,
        propagates_at: usize,
    ) -> LearnedHypercubeLinear {
        let hypercube = std::mem::take(&mut self.working_hypercube)
            .with_predicates(self.hypercube_predicates_on_conflict_dl.drain())
            .expect("can never encounter inconsistent hypercube");

        let _ = self.predicates_to_explain.drain();

        // Determine whether the linear can propagate something at some point.
        // If not, replace it with a trivially false linear to save memory and
        // registrations for bound events.
        let root_trail_position = state.assignments.get_trail_position_at_checkpoint(0);
        let hl_slack_at_root = compute_hl_slack_at_trail_position(
            state,
            &hypercube,
            &self.conflicting_linear,
            root_trail_position,
        );

        let linear = if hl_slack_at_root < 0 {
            self.statistics.num_learned_clauses += 1;
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

        trace!("applying fourier elimination on {pivot}");
        trace!(
            "  - slack conflict: {}",
            compute_linear_slack_at_trail_position(state, &self.conflicting_linear, trail_position),
        );
        trace!("  - slack b: {reason_slack}");

        let tightly_propagating_reason = compute_tightly_propagating_reason(
            state,
            trail_position,
            explanation,
            reason_slack,
            pivot.get_domain().scaled(weight_in_reason),
        );

        trace!("  - tightly propagating: {tightly_propagating_reason}");

        let tp_slack = compute_linear_slack_at_trail_position(
            state,
            &tightly_propagating_reason.linear,
            trail_position,
        );
        trace!("     - slack: {tp_slack}");

        // Extend the hypercube of the conflict with the predicates from the hypercube of
        // the explanation.
        for predicate in tightly_propagating_reason.hypercube.iter_predicates() {
            self.add_hypercube_predicate(state, predicate);
        }

        self.explain_linear(state, &tightly_propagating_reason.linear, trail_position);

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
            if self
                .conflicting_linear
                .terms()
                .any(|t| predicate_applies_to_term(pivot, t))
            {
                self.hypercube_predicates_on_conflict_dl
                    .push(bound_predicate.into(), state);
                self.conflicting_linear = std::mem::take(&mut self.conflicting_linear)
                .weaken_to_zero(bound_predicate)
                .expect(
                    "weakening the conflict will never result in a trivially satisfiable linear",
                );
            }

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

        // No propositional resolution happens when the following are both true:
        // - The pivot does not imply a predicate in the current hypercube,
        // - and its negation is not implied by a predicate in the explanation.
        let pivot_implies_predicate_in_conflict = self
            .hypercube_predicates_on_conflict_dl
            .iter()
            .any(|p| pivot.implies(p));

        let not_pivot_is_implied_by_hypercube_of_explanation =
            explanation.iter_predicates().any(|p| p.implies(!pivot));

        if !pivot_implies_predicate_in_conflict && !not_pivot_is_implied_by_hypercube_of_explanation
        {
            trace!("  => skipping propositional resolution");
            return;
        }

        self.statistics.num_propositional_resolutions += 1;

        // Remove the predicates from the hypercube that are resolved on.
        self.hypercube_predicates_on_conflict_dl
            .retain(|p| !pivot.implies(p));

        let can_substitute_with_explanation_linear =
            explanation.iter_predicates().any(|p| p == !pivot);

        if self.conflicting_linear.is_trivially_false() && can_substitute_with_explanation_linear {
            // If the conflicting linear is a clause, then we do not need to clausify
            // the explanation. Instead, the linear of the conflicting constraint
            // becomes the linear of the explanation and the hypercube of the conflict
            // is extended with the hypercube of the conflict.

            trace!(
                "since the linear in the conflict is trivially false, use linear from explanation"
            );

            for predicate in explanation.iter_predicates() {
                let truth_value = state
                    .assignments
                    .evaluate_predicate_at_trail_position(predicate, trail_position)
                    .expect("all predicates in explanation hypercube are assigned");

                // If the predicate is false, then it is propagated. In that case, it does
                // not feature in the resolvent so we can continue to the next predicate.
                if !truth_value {
                    continue;
                }

                self.add_hypercube_predicate(state, predicate);
            }

            // Set the linear from the explanation as the conflict linear.
            //
            // This requires explaining why the linear is conflicting.
            let linear = explanation.take_linear();
            self.explain_linear(state, &linear, trail_position);
            self.conflicting_linear = linear;

            self.statistics
                .num_propositional_resolutions_use_explanation_linear += 1;
        } else {
            let clausal_explanation = explanation.into_clause(state, pivot, trail_position);

            trace!(
                "clausal explanation: {}",
                clausal_explanation.iter().format(" & ")
            );
            for predicate in clausal_explanation {
                let truth_value = state
                    .assignments
                    .evaluate_predicate_at_trail_position(predicate, trail_position)
                    .expect("all predicates in explanation hypercube are assigned");

                // If the predicate is false, then it is propagated. In that case, it does
                // not feature in the resolvent so we can continue to the next predicate.
                if !truth_value {
                    continue;
                }

                self.add_hypercube_predicate(state, predicate);
            }
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

            // TODO: Optimize this
            let final_hypercube = self
                .working_hypercube
                .clone()
                .with_predicates(self.hypercube_predicates_on_conflict_dl.iter())
                .expect("no inconsistent hypercube");
            if !propagates_at(
                state,
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

    /// Assert the invariants of the resolver are kept.
    fn assert_invariants(&self, state: &State, trail_position: usize) {
        let last_trail_position_at_previous_checkpoint = state
            .assignments
            .get_trail_position_at_checkpoint(state.get_checkpoint() - 1);

        assert!(trail_position > last_trail_position_at_previous_checkpoint);

        let linear_slack =
            compute_linear_slack_at_trail_position(state, &self.conflicting_linear, trail_position);

        if !linear_slack.is_negative() {
            eprintln!("Bounds in conflicting linear:");

            for term in self.conflicting_linear.terms() {
                let lb = term.lower_bound_at_trail_position(&state.assignments, trail_position);
                eprintln!(
                    "  - {} {} >= {} @ {}",
                    term.scale,
                    term.inner,
                    lb,
                    state
                        .trail_position(predicate![term >= lb])
                        .expect("predicate is true")
                );
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
            .is_some_and(|term| predicate_applies_to_term(pivot, term));

        is_in_hypercube || contributes_to_linear_conflict
    }

    /// Enqueue the contributions of the linear terms to be explained.
    fn explain_linear(&mut self, state: &State, linear: &LinearInequality, trail_position: usize) {
        for term in linear.terms() {
            let term_bound = term.lower_bound_at_trail_position(&state.assignments, trail_position);
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
    state: &State,
    trail_position: usize,
    hypercube: &Hypercube,
    linear: &LinearInequality,
) -> bool {
    // Get the predicates that are not assigned to true.
    let unsatisfied_predicates_in_hypercube = hypercube
        .iter_predicates()
        .filter(|&predicate| {
            state
                .assignments
                .evaluate_predicate_at_trail_position(predicate, trail_position)
                != Some(true)
        })
        .collect::<Vec<_>>();

    if unsatisfied_predicates_in_hypercube.len() > 1 {
        // If more than one predicate remains unassigned, we cannot do anything.
        return false;
    }

    let slack = compute_hl_slack_at_trail_position(state, hypercube, linear, trail_position);

    if unsatisfied_predicates_in_hypercube.len() == 1 {
        let unassigned_predicate = unsatisfied_predicates_in_hypercube[0];
        let domain_of_predicate = unassigned_predicate.get_domain();

        if slack < 0 {
            return true;
        } else if let Some(term) = linear.term_for_domain(domain_of_predicate) {
            let bound_in_state =
                term.lower_bound_at_trail_position(&state.assignments, trail_position);
            let bound_in_hypercube = hypercube.lower_bound(&term);
            let bound_i64 = i64::from(i32::max(bound_in_state, bound_in_hypercube));

            let new_upper_bound = match i32::try_from(slack + bound_i64) {
                Ok(bound) => bound,
                Err(_) => return false,
            };

            let predicate_to_propagate = predicate![term <= new_upper_bound];

            let predicate_truth_value = state
                .assignments
                .evaluate_predicate_at_trail_position(predicate_to_propagate, trail_position);

            return (!unassigned_predicate).implies(predicate_to_propagate)
                && predicate_truth_value != Some(true);
        }
    } else {
        assert!(unsatisfied_predicates_in_hypercube.is_empty());
        return linear_propagates_at(state, trail_position, linear);
    }

    false
}

/// Returns true if the given linear propagates at the given trail position.
fn linear_propagates_at(
    state: &State,
    trail_position: usize,
    conflicting_linear: &LinearInequality,
) -> bool {
    if conflicting_linear.is_trivially_false() {
        return true;
    }

    let slack = compute_linear_slack_at_trail_position(state, conflicting_linear, trail_position);

    for term in conflicting_linear.terms() {
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

/// Use weakening to obtain a hypercube linear that propagates the given term without any rounding.
fn compute_tightly_propagating_reason<'expl>(
    state: &State,
    trail_position: usize,
    original_reason: &'expl HypercubeLinear,
    reason_slack: i64,
    pivot_term: AffineView<DomainId>,
) -> Cow<'expl, HypercubeLinear> {
    let pivot_term_upper_bound = reason_slack
        + i64::from(pivot_term.lower_bound_at_trail_position(&state.assignments, trail_position));

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
    state: &State,
    hypercube: &Hypercube,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    // trace!("Computing HL slack:");
    let lower_bound_terms = linear
        .terms()
        .map(|term| {
            let state_lb = term.lower_bound_at_trail_position(&state.assignments, trail_position);
            let hypercube_lb = hypercube.lower_bound(&term);

            // trace!("  - {term:?} >= state {state_lb}, hl {hypercube_lb}");

            i64::from(i32::max(state_lb, hypercube_lb))
        })
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}

fn compute_linear_slack_at_trail_position(
    state: &State,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| {
            let lb = term.lower_bound_at_trail_position(&state.assignments, trail_position);

            i64::from(lb)
        })
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
    use crate::predicate;
    use crate::state::State;

    // === HypercubeLinearResolver::resolve ===

    #[test]
    fn resolve_propositional_with_conjunction_adds_reason_to_hypercube() {
        // Tests that propositional resolution with a clausal (Conjunction) explanation removes the
        // pivot from the conflict hypercube and adds the reason predicates to the working
        // hypercube.
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));

        // DL 1: post y >= 3 (reason predicate, at a previous checkpoint)
        state.new_checkpoint();
        let _ = state.post(predicate![y >= 3]).expect("domain not empty");

        // DL 2 (conflict DL): post x >= 5 (the pivot)
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 5]).expect("domain not empty");
        let pivot = predicate![x >= 5];
        let trail_position = state.trail_position(pivot).expect("pivot is on trail");

        let mut resolver = HypercubeLinearResolver::new(Trace::discard());
        resolver.conflicting_linear = LinearInequality::trivially_false();
        resolver
            .hypercube_predicates_on_conflict_dl
            .push(pivot, &state);

        // Explanation: why x >= 5 became true (Conjunction includes !pivot = x <= 4)
        let explanation =
            HypercubeLinearExplanation::Conjunction(vec![predicate![y >= 3], predicate![x <= 4]]);

        resolver.resolve(&mut state, trail_position, pivot, explanation);

        // The pivot should be removed from the conflict DL heap.
        assert!(!resolver.hypercube_predicates_on_conflict_dl.contains(pivot));

        // y >= 3 was posted at checkpoint 1 (< current 2), so it should be in working_hypercube.
        assert!(
            resolver
                .working_hypercube
                .iter_predicates()
                .any(|p| p == predicate![y >= 3])
        );
    }

    #[test]
    fn resolve_fourier_eliminates_pivot_variable_from_conflict_linear() {
        // Tests that Fourier resolution eliminates the pivot variable from the conflict linear.
        //
        // Conflict:  {} -> x + y <= 5  (violated: x=5, y=1, slack=-1)
        // Reason:    {} -> -x + z <= -2  (propagated x >= 5 given z >= 3:
        //                                 slack = -2 - (-10 + 3) = 5,
        //                                 -x <= -5, i.e. x >= 5)
        // Fourier combination: x eliminated, result is y + z <= 3.
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z = state.new_interval_variable(0, 10, Some("z".into()));

        // DL 1: post z >= 3
        state.new_checkpoint();
        let _ = state.post(predicate![z >= 3]).expect("domain not empty");

        // DL 2 (conflict DL): post x >= 5, y >= 1
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 5]).expect("domain not empty");
        let _ = state.post(predicate![y >= 1]).expect("domain not empty");
        let pivot = predicate![x >= 5];
        let trail_position = state.trail_position(pivot).expect("pivot is on trail");

        // conflicting_linear: x + y <= 5, slack = 5 - 5 - 1 = -1 at conflict
        let conflicting_linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            5,
        )
        .expect("not trivially satisfiable");

        // Explanation: {} -> -x + z <= -2
        // (reason hypercube is empty, so it is always satisfied — Fourier can apply)
        let explanation_linear = LinearInequality::new(
            [
                (NonZero::new(-1).unwrap(), x),
                (NonZero::new(1).unwrap(), z),
            ],
            -2,
        )
        .expect("not trivially satisfiable");

        let explanation = HypercubeLinearExplanation::Proper(HypercubeLinear {
            hypercube: Hypercube::new([]).expect("not inconsistent"),
            linear: explanation_linear,
        });

        let mut resolver = HypercubeLinearResolver::new(Trace::discard());
        resolver.conflicting_linear = conflicting_linear;
        resolver
            .hypercube_predicates_on_conflict_dl
            .push(pivot, &state);

        resolver.resolve(&mut state, trail_position, pivot, explanation);

        // After Fourier elimination of x: conflict becomes y + z <= 3.
        let expected_linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
            3,
        )
        .expect("not trivially satisfiable");
        assert_eq!(resolver.conflicting_linear, expected_linear);

        // z >= 3 (at checkpoint 1 < conflict DL 2) should be added to the working hypercube.
        assert!(
            resolver
                .working_hypercube
                .iter_predicates()
                .any(|p| p == predicate![z >= 3])
        );

        // x >= 5 should be removed from the conflict DL heap.
        assert!(!resolver.hypercube_predicates_on_conflict_dl.contains(pivot));
    }

    #[test]
    fn resolve_propositional_with_hl_propagation_case() {
        // Tests propositional resolution where the explanation comes from a HL constraint
        // in the "weaker propagation" case: H /\ l -> R propagated k (weaker than !l).
        //
        // Constraint: [a >= 1, x >= 2] -> x + y <= 5
        //   With a=1 satisfied and x >= 2 unassigned (lb(x)=0), y=2:
        //   slack = 5 - 0 - 2 = 3, propagates x <= 3 (weaker than !(x>=2) = x <= 1).
        //
        // The pivot is x <= 3 (the propagated upper bound). Because x >= 2 is NOT satisfied
        // at the pivot's trail position (lb(x) = 0), Fourier resolution fails and propositional
        // resolution handles it instead.
        //
        // Conflict linear: -x + w <= -4 (violated because ub(x)=3, so lb(-x)=-3, lb(w)=0,
        //   slack = -4 - (-3 + 0) = -1).
        let mut state = State::default();
        let a = state.new_interval_variable(0, 10, Some("a".into()));
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into())); // initial lb = 2
        let w = state.new_interval_variable(0, 10, Some("w".into()));

        // DL 1: post a >= 1
        state.new_checkpoint();
        let _ = state.post(predicate![a >= 1]).expect("domain not empty");

        // DL 2 (conflict DL): post x <= 3 (the propagated upper bound / pivot)
        state.new_checkpoint();
        let _ = state.post(predicate![x <= 3]).expect("domain not empty");
        let pivot = predicate![x <= 3];
        let trail_position = state.trail_position(pivot).expect("pivot is on trail");

        // conflicting_linear: -x + w <= -4 (violated with ub(x)=3)
        let conflicting_linear = LinearInequality::new(
            [
                (NonZero::new(-1).unwrap(), x),
                (NonZero::new(1).unwrap(), w),
            ],
            -4,
        )
        .expect("not trivially satisfiable");

        // Explanation: [a >= 1, x >= 2] -> x + y <= 5
        // Note: x >= 2 is in the hypercube but is NOT satisfied at the pivot's trail position
        // (lb(x) = 0 at that point), so Fourier resolution cannot apply.
        let explanation_hypercube =
            Hypercube::new([predicate![a >= 1], predicate![x >= 2]]).expect("not inconsistent");
        let explanation_linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            5,
        )
        .expect("not trivially satisfiable");

        let explanation = HypercubeLinearExplanation::Proper(HypercubeLinear {
            hypercube: explanation_hypercube,
            linear: explanation_linear,
        });

        let mut resolver = HypercubeLinearResolver::new(Trace::discard());
        resolver.conflicting_linear = conflicting_linear;
        resolver
            .hypercube_predicates_on_conflict_dl
            .push(pivot, &state);

        resolver.resolve(&mut state, trail_position, pivot, explanation);

        // The conflict linear should be weakened on x <= 3 (UpperBound):
        // -x term (scale=-1) is removed; bound = -4 + 1*3 = -1. Result: w <= -1.
        let expected_linear =
            LinearInequality::new([(NonZero::new(1).unwrap(), w)], -1).expect("not trivially sat");
        assert_eq!(resolver.conflicting_linear, expected_linear);

        // x <= 3 should be removed from the conflict DL heap.
        assert!(!resolver.hypercube_predicates_on_conflict_dl.contains(pivot));

        // a >= 1 (at checkpoint 1 < conflict DL 2) should be added to working_hypercube.
        // (x >= 4 from the clause is false in state, y >= 2 is at checkpoint 0 and is ignored.)
        assert!(
            resolver
                .working_hypercube
                .iter_predicates()
                .any(|p| p == predicate![a >= 1])
        );
    }

    #[test]
    fn propagates_at_takes_hypercube_into_account() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);
        let z = state.new_interval_variable(-6, 6, None);

        let hypercube = Hypercube::from_single_predicate(predicate![z <= -2]);
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(-1).unwrap(), z),
            ],
            0,
        )
        .expect("not trivially satisfiable");

        assert!(propagates_at(
            &state,
            state.trail_len(),
            &hypercube,
            &linear,
        ));
    }

    #[test]
    fn tightly_propagating_reason_weakens_appropriately() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(1, 10, None);

        let original_reason = HypercubeLinear {
            hypercube: Hypercube::default(),
            linear: LinearInequality::new(
                [(NonZero::new(2).unwrap(), x), (NonZero::new(5).unwrap(), y)],
                10,
            )
            .expect("not trivially satisfiable"),
        };

        let slack = compute_linear_slack_at_trail_position(
            &state,
            &original_reason.linear,
            state.trail_len(),
        );

        let expected = HypercubeLinear {
            hypercube: Hypercube::from_single_predicate(predicate![y >= 1]),
            linear: LinearInequality::new(
                [(NonZero::new(1).unwrap(), x), (NonZero::new(2).unwrap(), y)],
                4,
            )
            .expect("not trivially satisfiable"),
        };

        let actual = compute_tightly_propagating_reason(
            &state,
            state.trail_len(),
            &original_reason,
            slack,
            x.scaled(2),
        );

        assert_eq!(actual.into_owned(), expected);
    }

    #[test]
    fn tightly_propagating_reason_considers_negative_weights() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(1, 10, None);

        let original_reason = HypercubeLinear {
            hypercube: Hypercube::default(),
            linear: LinearInequality::new(
                [
                    (NonZero::new(2).unwrap(), x),
                    (NonZero::new(-5).unwrap(), y),
                ],
                9,
            )
            .expect("not trivially satisfiable"),
        };

        let slack = compute_linear_slack_at_trail_position(
            &state,
            &original_reason.linear,
            state.trail_len(),
        );

        let expected = HypercubeLinear {
            hypercube: Hypercube::from_single_predicate(predicate![y <= 10]),
            linear: LinearInequality::new(
                [
                    (NonZero::new(1).unwrap(), x),
                    (NonZero::new(-2).unwrap(), y),
                ],
                9,
            )
            .expect("not trivially satisfiable"),
        };

        let actual = compute_tightly_propagating_reason(
            &state,
            state.trail_len(),
            &original_reason,
            slack,
            x.scaled(2),
        );

        assert_eq!(actual.into_owned(), expected);
    }
}
