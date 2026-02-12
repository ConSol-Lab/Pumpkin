use std::borrow::Cow;
use std::num::NonZero;

use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
use crate::containers::HashSet;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::reason::ReasonRef;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearConstructor;
use crate::hypercube_linear::HypercubeLinearExplanation;
use crate::hypercube_linear::LinearInequality;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::ExplanationContext;
use crate::state::CurrentNogood;
use crate::state::EmptyDomainConflict;
use crate::state::State;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::statistics::moving_averages::CumulativeMovingAverage;
use crate::statistics::moving_averages::MovingAverage;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

create_statistics_struct!(HypercubeLinearResolutionStatistics {
    average_num_linear_terms_in_learned_hypercube_linear: CumulativeMovingAverage<usize>,
    num_attempted_fourier_resolves: usize,
    num_successful_fourier_resolves: usize,
    num_learned_hypercube_linear: usize,
    num_learned_clauses: usize,
    num_overflow_errors: usize,
    num_backjumps: usize,
    average_backtrack_amount: CumulativeMovingAverage<usize>,
});

#[derive(Clone, Debug, Default)]
pub struct HypercubeLinearResolver {
    statistics: HypercubeLinearResolutionStatistics,
    /// Used to check whether the same constraint is re-learned multiple times.
    ///
    /// This should never happen in a correct implementation, so this is used only for debugging
    /// purposes.
    learned_constraints: HashSet<HypercubeLinearExplanation<'static>>,
}

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext<'_>) {
        let conflict_hypercube_linear =
            self.compute_conflicting_hypercube_linear(context.reborrow());

        debug!(
            "Resolving conflict (dl = {}) {}",
            context.state.get_checkpoint(),
            conflict_hypercube_linear.display(&context.state.variable_names),
        );

        let conflicting_hypercube_linear = conflict_hypercube_linear.clone();
        let (learned_hypercube_linear, backump_level) =
            self.learn_hypercube_linear(context.state, conflicting_hypercube_linear.clone());

        self.restore_solver(context.reborrow(), backump_level, learned_hypercube_linear);
    }
}

impl HypercubeLinearResolver {
    /// Computes the conflicting hypercube linear constraint.
    ///
    /// If the conflict is explainable as a hypercube linear, then that hypercube linear
    /// becomes the conflict. Otherwise, the conflict nogood is converted into a hypercube
    /// linear.
    fn compute_conflicting_hypercube_linear(
        &mut self,
        context: ConflictAnalysisContext<'_>,
    ) -> HypercubeLinearExplanation<'static> {
        let StoredConflictInfo::EmptyDomain(EmptyDomainConflict {
            trigger_reason,
            trigger_predicate,
            ..
        }) = context.solver_state.get_conflict_info()
        else {
            panic!(
                "Cannot start hypercube analysis from {:?}",
                context.solver_state.get_conflict_info()
            );
        };

        let propagator_id = context.state.reason_store.get_propagator(trigger_reason);

        trace!("conflicting predicate = {}", trigger_predicate);

        if let Some(reason_code) = context.state.reason_store.get_lazy_code(trigger_reason) {
            let explanation_context = ExplanationContext::new(
                &context.state.assignments,
                CurrentNogood::empty(),
                context.state.trail_len() - 1,
                &mut context.state.notification_engine,
            );

            if let Some(hypercube_linear) = context.state.propagators[propagator_id]
                .explain_as_hypercube_linear(*reason_code, explanation_context)
            {
                hypercube_linear.into_owned()
            } else {
                clausal_reason_as_hypercube(context.state, trigger_reason)
            }
        } else {
            clausal_reason_as_hypercube(context.state, trigger_reason)
        }
    }

    fn restore_solver(
        &mut self,
        context: ConflictAnalysisContext<'_>,
        decision_level: usize,
        learned_constraint: HypercubeLinearExplanation<'static>,
    ) {
        debug!(
            "Learned {}",
            learned_constraint.display(&context.state.variable_names)
        );
        debug!(
            "Jumping to decision level {decision_level} from decision level {}",
            context.state.get_checkpoint()
        );

        assert!(
            self.learned_constraints.insert(learned_constraint.clone()),
            "learning the same constraint twice"
        );

        self.statistics
            .average_num_linear_terms_in_learned_hypercube_linear
            .add_term(learned_constraint.linear.terms().len());
        self.statistics.num_learned_hypercube_linear += 1;

        if learned_constraint.linear.terms().count() == 0 {
            self.statistics.num_learned_clauses += 1;
        }

        let backjump_amount = context.state.get_checkpoint() - decision_level;

        if backjump_amount > 1 {
            self.statistics.num_backjumps += 1;
        }

        self.statistics
            .average_backtrack_amount
            .add_term(backjump_amount);

        let _ = context.state.restore_to(decision_level);

        let constraint_tag = context.state.new_constraint_tag();
        let handle = context.state.add_propagator(HypercubeLinearConstructor {
            hypercube: learned_constraint.hypercube.into_owned(),
            linear: learned_constraint.linear.into_owned(),
            constraint_tag,
        });

        trace!(
            "New constraint tag = {constraint_tag:?} (pid = {})",
            handle.propagator_id()
        );
    }

    fn resolve(
        &mut self,
        mut conflicting_hypercube_linear: HypercubeLinearExplanation<'static>,
        reason: HypercubeLinearReason,
        state: &State,
        trail_index: usize,
        pivot_predicate: Predicate,
    ) -> HypercubeLinearExplanation<'static> {
        let mut elimination_happened = false;

        match fourier_eliminate(
            state,
            trail_index,
            conflicting_hypercube_linear.clone(),
            reason.original,
            pivot_predicate,
        ) {
            Ok(hypercube_linear) => {
                self.statistics.num_attempted_fourier_resolves += 1;

                let new_slack = compute_slack_at_trail_position(
                    &hypercube_linear.linear,
                    &state.assignments,
                    trail_index,
                );

                trace!("new slack = {new_slack}");
                assert!(new_slack < 0);
                conflicting_hypercube_linear = hypercube_linear;

                elimination_happened = true;
            }
            Err(
                e @ (FourierError::ResultOfEliminationTriviallySatisfiable
                | FourierError::IntegerOverflow),
            ) => {
                self.statistics.num_attempted_fourier_resolves += 1;
                if matches!(e, FourierError::IntegerOverflow) {
                    self.statistics.num_overflow_errors += 1;
                }

                let weakened_conflicting = conflicting_hypercube_linear.weaken(pivot_predicate);
                let weakened_conflicting_slack = compute_slack_at_trail_position(
                    &weakened_conflicting.linear,
                    &state.assignments,
                    trail_index,
                );
                assert!(
                    weakened_conflicting_slack < 0,
                    "weakening like this does not affect slack"
                );

                conflicting_hypercube_linear = propositional_resolution(
                    state,
                    weakened_conflicting,
                    reason.as_clause.clone(),
                    pivot_predicate,
                );

                elimination_happened = true;
            }
            Err(FourierError::NoVariableElimination) => {
                trace!("no fourier resolution");
            }
        }

        if !elimination_happened {
            // If the pivot contributes to the conflict via the linear part of the conflicting
            // hypercube linear, and is propagated by the hypercube of the reason, we need to
            // weaken the conflict here to ensure propositional resolution can eliminate the
            // pivot.
            conflicting_hypercube_linear = conflicting_hypercube_linear.weaken(pivot_predicate);
        }

        if hypercube_models_bound(&conflicting_hypercube_linear, pivot_predicate) {
            // Here we need to perform propositional resolution to eliminate the
            // variable from the hypercube.

            let result = propositional_resolution(
                state,
                conflicting_hypercube_linear,
                reason.as_clause,
                pivot_predicate,
            );
            conflicting_hypercube_linear = result;
            elimination_happened = true;
            let new_slack = compute_slack_at_trail_position(
                &conflicting_hypercube_linear.linear,
                &state.assignments,
                trail_index,
            );
            trace!("slack after resh = {new_slack}");
        } else {
            trace!("no propositional resolution");
        }

        if !elimination_happened {
            trace!(
                "current conflict: {}",
                conflicting_hypercube_linear.display(&state.variable_names)
            );
            panic!(
                "if the predicate contributes to the conflict, an elimination should be possible"
            );
        }

        conflicting_hypercube_linear
    }

    fn learn_hypercube_linear(
        &mut self,
        state: &mut State,
        mut conflicting_hypercube_linear: HypercubeLinearExplanation<'static>,
    ) -> (HypercubeLinearExplanation<'static>, usize) {
        let mut trail_index = state.trail_len();

        // Iterate the trail backwards until a constraint is obtained that propagates at a
        // previous decision level.
        loop {
            assert_eq!(
                is_conflicting(
                    &conflicting_hypercube_linear,
                    trail_index,
                    &state.assignments,
                ),
                ConflictingStatus::Conflicting
            );

            trail_index -= 1;
            let top_of_trail = state.trail_entry(trail_index);

            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            let pivot_predicate = top_of_trail.predicate;
            trace!("processing {}", pivot_predicate);

            // Remove the top of trail from the reason set if it is in the reason set.
            if !conflicting_hypercube_linear
                .reason_set(&state.assignments)
                .any(|p| pivot_predicate.implies(p))
            {
                trace!("  => skipping, does not contribute to conflict");
                // The current top of trail is not in the reason set and therefore does not
                // contribute to the conflict.
                continue;
            }

            let Some((reason_ref, _)) = top_of_trail.reason else {
                panic!("resolution should never reach the last decision");
            };

            let reason = hypercube_from_reason(state, reason_ref);

            conflicting_hypercube_linear = self.resolve(
                conflicting_hypercube_linear,
                reason,
                state,
                trail_index,
                top_of_trail.predicate,
            );

            if cfg!(feature = "hypercube-linear-assertions") {
                assert!(
                    !conflicting_hypercube_linear
                        .reason_set(&state.assignments)
                        .any(|p| p == pivot_predicate),
                    "the pivot predicate is removed from the reason of the conflict"
                );
            }

            if let Some(backjump_dl) =
                will_propagate_on_previous_dl(&conflicting_hypercube_linear, &state.assignments)
            {
                return (conflicting_hypercube_linear, backjump_dl);
            }
        }
    }
}

fn will_propagate_on_previous_dl(
    hypercube_linear: &HypercubeLinearExplanation<'_>,
    assignments: &Assignments,
) -> Option<usize> {
    trace!("Testing propagation at previous dl");
    let current_dl = assignments.get_checkpoint();
    let decision_levels = (0..current_dl).rev();

    for decision_level in decision_levels {
        trace!("  => testing dl = {decision_level}");
        let trail_position = assignments.get_trail_position_at_decision_level(decision_level);

        if propagates_at(hypercube_linear, assignments, trail_position) {
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
fn propagates_at(
    hypercube_linear: &HypercubeLinearExplanation<'_>,
    assignments: &Assignments,
    trail_position: usize,
) -> bool {
    if hypercube_linear
        .hypercube
        .iter_predicates()
        .any(|predicate| assignments.evaluate_predicate(predicate) == Some(false))
    {
        // If the hypercube contains at least one false predicate, the propagator will not do
        // anything.
        return false;
    }

    // Get the predicates that are not assigned to true.
    let unsatisfied_predicates_in_hypercubes = hypercube_linear
        .hypercube
        .iter_predicates()
        .filter(|&predicate| {
            assignments.evaluate_predicate_at_trail_position(predicate, trail_position)
                != Some(true)
        })
        .collect::<Vec<_>>();

    if unsatisfied_predicates_in_hypercubes.len() > 1 {
        // If more than one predicate remains unassigned, we cannot do anything.
        return false;
    }

    let slack =
        compute_slack_at_trail_position(&hypercube_linear.linear, assignments, trail_position);

    if unsatisfied_predicates_in_hypercubes.len() == 1 {
        let unassigned_predicate = unsatisfied_predicates_in_hypercubes[0];

        if slack < 0 {
            return true;
        } else if let Some(term) = hypercube_linear
            .linear
            .term_for_domain(unassigned_predicate.get_domain())
        {
            let term_lower_bound = term.lower_bound_at_trail_position(assignments, trail_position);
            let new_upper_bound = match i32::try_from(slack + i64::from(term_lower_bound)) {
                Ok(bound) => bound,
                Err(_) => return false,
            };

            return new_upper_bound
                < term.upper_bound_at_trail_position(assignments, trail_position);
        }
    } else {
        assert!(unsatisfied_predicates_in_hypercubes.is_empty());
        return linear_propagates_at(&hypercube_linear.linear, assignments, trail_position);
    }

    false
}

/// Returns true if the given linear propagates at the given trail position.
fn linear_propagates_at(
    linear: &LinearInequality,
    assignments: &Assignments,
    trail_position: usize,
) -> bool {
    if linear.is_trivially_false() {
        return true;
    }

    let slack = compute_slack_at_trail_position(linear, assignments, trail_position);

    for term in linear.terms() {
        let term_lower_bound =
            i64::from(term.lower_bound_at_trail_position(assignments, trail_position));
        let new_term_upper_bound_i64 = slack + term_lower_bound;
        let new_term_upper_bound = match i32::try_from(new_term_upper_bound_i64) {
            Ok(bound) => bound,
            // The upper bound is smaller than i32::MIN, which means this would propagate (even if
            // wee cannot perform the propagation due to our domains being 32-bit).
            Err(_) if new_term_upper_bound_i64.is_negative() => return true,
            // If we want to set the upper bound to a value larger than i32::MAX,
            // it can never tighten the existing bound of `term_to_propagate`.
            Err(_) => continue,
        };

        if new_term_upper_bound < term.upper_bound_at_trail_position(assignments, trail_position) {
            return true;
        }
    }

    false
}

fn compute_slack_at_trail_position(
    linear: &LinearInequality,
    assignments: &Assignments,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| i64::from(term.lower_bound_at_trail_position(assignments, trail_position)))
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}

fn propositional_resolution(
    state: &State,
    conflicting_hypercube_linear: HypercubeLinearExplanation<'static>,
    reason: HypercubeLinearExplanation<'static>,
    pivot_predicate: Predicate,
) -> HypercubeLinearExplanation<'static> {
    trace!("applying propositional resolution on {pivot_predicate}",);

    // Make sure that the pivot is not also contributing to the conflict in the linear part.
    trace!(
        "  - {}",
        conflicting_hypercube_linear.display(&state.variable_names)
    );
    let mut weakened_conflict = conflicting_hypercube_linear.weaken(pivot_predicate);
    trace!(
        "    - weakened: {}",
        weakened_conflict.display(&state.variable_names)
    );

    trace!("  - {}", reason.display(&state.variable_names));

    assert!(
        reason.linear.is_trivially_false(),
        "the reason should be a clause"
    );

    let hypercube = std::mem::take(weakened_conflict.hypercube.to_mut());
    let new_hypercube = hypercube
        .with_predicates(reason.hypercube.iter_predicates())
        .expect("propositional resolution causes hypercube to become inconsistent");

    let new_constraint = HypercubeLinearExplanation {
        hypercube: Cow::Owned(new_hypercube),
        linear: weakened_conflict.linear,
    };

    trace!("result = {}", new_constraint.display(&state.variable_names));

    new_constraint
}

fn hypercube_models_bound(
    hypercube_linear: &HypercubeLinearExplanation<'_>,
    predicate: Predicate,
) -> bool {
    hypercube_linear
        .hypercube
        .iter_predicates()
        .any(|hypercube_predicate| predicate.implies(hypercube_predicate))
}

enum FourierError {
    NoVariableElimination,
    ResultOfEliminationTriviallySatisfiable,
    IntegerOverflow,
}

/// Apply fourier elimination between two hypercube linear constraints.
///
/// If there is no possible combination, then None is returned.
fn fourier_eliminate(
    state: &State,
    trail_position: usize,
    conflicting_hypercube_linear: HypercubeLinearExplanation<'static>,
    reason: HypercubeLinearExplanation<'_>,
    pivot_predicate: Predicate,
) -> Result<HypercubeLinearExplanation<'static>, FourierError> {
    let maybe_term_in_conflicting = conflicting_hypercube_linear
        .linear
        .term_for_domain(pivot_predicate.get_domain());
    let maybe_term_in_reason = conflicting_hypercube_linear
        .linear
        .term_for_domain(pivot_predicate.get_domain());

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

    let reason_hypercube_satisfied = reason.hypercube.iter_predicates().all(|p| {
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
        && pivot_predicate.is_lower_bound_predicate())
        || (weight_in_conflicting.is_negative() && pivot_predicate.is_upper_bound_predicate());

    // We should only do fourier elimination if the pivot predicate actually contributes to the
    // conflict. Otherwise performing the combination will not remove any contribution to the
    // conflict.
    if !contributes_to_conflict_in_linear {
        return Err(FourierError::NoVariableElimination);
    }

    let reason_slack =
        compute_slack_at_trail_position(&reason.linear, &state.assignments, trail_position);

    trace!("applying fourier elimination on {pivot_predicate}",);
    trace!(
        "  - {}",
        conflicting_hypercube_linear.display(&state.variable_names)
    );
    trace!("  - {}", reason.display(&state.variable_names));
    trace!(
        "  - slack a: {}",
        compute_slack_at_trail_position(
            &conflicting_hypercube_linear.linear,
            &state.assignments,
            trail_position
        )
    );
    trace!("  - slack b: {reason_slack}");

    if reason_slack < 0 {
        trace!("  => no fourier possible as reason propagated through hypercube");
        return Err(FourierError::ResultOfEliminationTriviallySatisfiable);
    }

    let tightly_propagating_reason = compute_tightly_propagating_reason(
        &state.assignments,
        trail_position,
        reason,
        pivot_predicate.get_domain().scaled(weight_in_reason),
    );

    trace!(
        "  - tightly propagating: {}",
        tightly_propagating_reason.display(&state.variable_names)
    );
    let tp_slack = compute_slack_at_trail_position(
        &tightly_propagating_reason.linear,
        &state.assignments,
        trail_position,
    );
    trace!("     - slack: {tp_slack}");

    let term_in_reformulated_reason = tightly_propagating_reason
        .linear
        .term_for_domain(pivot_predicate.get_domain())
        .unwrap();
    assert_eq!(term_in_reformulated_reason.scale.abs(), 1);

    let scale_reason = weight_in_conflicting.abs();
    let mut linear_terms = conflicting_hypercube_linear
        .linear
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

    let hypercube = conflicting_hypercube_linear
        .hypercube
        .iter_predicates()
        .chain(tightly_propagating_reason.hypercube.iter_predicates())
        .collect::<Result<Hypercube, _>>()
        .expect("hypercube after fourier is not inconsistent");

    let mut linear_rhs = conflicting_hypercube_linear
        .linear
        .bound()
        .checked_add(
            tightly_propagating_reason
                .linear
                .bound()
                .checked_mul(scale_reason)
                .ok_or(FourierError::IntegerOverflow)?,
        )
        .ok_or(FourierError::IntegerOverflow)?;

    // Normalize the linear component of the hypercube linear to hopefully avoid overflows in the
    // future.
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

    let linear = LinearInequality::new(
        linear_terms
            .into_iter()
            .map(|(weight, domain)| (NonZero::new(weight).unwrap(), domain)),
        linear_rhs,
    )
    .ok_or(FourierError::ResultOfEliminationTriviallySatisfiable)?;

    let new_constraint = HypercubeLinearExplanation {
        hypercube: Cow::Owned(hypercube),
        linear: Cow::Owned(linear),
    };

    trace!("result = {}", new_constraint.display(&state.variable_names));
    Ok(new_constraint)
}

/// Use weakening to obtain a hypercube linear that has the term for `pivot_domain` be unit in the
/// linear part.
fn compute_tightly_propagating_reason<'a>(
    assignment: &Assignments,
    trail_position: usize,
    orignal_reason: HypercubeLinearExplanation<'a>,
    pivot_term: AffineView<DomainId>,
) -> HypercubeLinearExplanation<'a> {
    let divisor = pivot_term.scale.abs();

    let bounds_to_weaken: Vec<_> = orignal_reason
        .linear
        .terms()
        .filter_map(|term| {
            // If the weight of the term is divisible by the weight of the pivot term, then we
            // keep it in the linear part. Otherwise, it is weakened on.

            if term.scale % divisor == 0 {
                // We can divide this term by the pivot term. So no need to weaken on it.
                return None;
            }

            let bound = term.lower_bound_at_trail_position(assignment, trail_position);
            Some(predicate![term >= bound])
        })
        .collect();

    let mut tightly_propagating_reason = orignal_reason.clone();

    for bound in bounds_to_weaken {
        tightly_propagating_reason = tightly_propagating_reason.weaken(bound);
    }

    for term in tightly_propagating_reason.linear.to_mut().terms_mut() {
        term.scale /= divisor;
    }

    tightly_propagating_reason
}

struct HypercubeLinearReason<'state> {
    original: HypercubeLinearExplanation<'state>,
    as_clause: HypercubeLinearExplanation<'static>,
}

fn clausal_reason_as_hypercube(
    state: &mut State,
    reason: ReasonRef,
) -> HypercubeLinearExplanation<'static> {
    let mut clausal_reason = Vec::new();
    assert!(state.reason_store.get_or_compute(
        reason,
        ExplanationContext::new(
            &state.assignments,
            CurrentNogood::empty(),
            0,
            &mut state.notification_engine,
        ),
        &mut state.propagators,
        &mut clausal_reason,
    ));

    HypercubeLinearExplanation {
        hypercube: Cow::Owned(
            Hypercube::new(clausal_reason)
                .expect("reason does not contain inconsistent predicates"),
        ),
        linear: Cow::Owned(LinearInequality::trivially_false()),
    }
}

/// Get the hypercube linear reason for the propagation.
fn hypercube_from_reason(state: &mut State, reason: ReasonRef) -> HypercubeLinearReason<'static> {
    let clausal_reason = clausal_reason_as_hypercube(state, reason);
    let propagator_id = state.reason_store.get_propagator(reason);

    let hypercube_linear_reason =
        if let Some(reason_code) = state.reason_store.get_lazy_code(reason) {
            let explanation_context = ExplanationContext::new(
                &state.assignments,
                CurrentNogood::empty(),
                state.trail_len() - 1,
                &mut state.notification_engine,
            );

            if let Some(original) = state.propagators[propagator_id]
                .explain_as_hypercube_linear(*reason_code, explanation_context)
            {
                original.into_owned()
            } else {
                clausal_reason.clone()
            }
        } else {
            clausal_reason.clone()
        };

    trace!(
        "hypercube linear reason = {}",
        hypercube_linear_reason.display(&state.variable_names)
    );
    trace!(
        "clausal reason = {}",
        clausal_reason.display(&state.variable_names)
    );

    HypercubeLinearReason {
        original: hypercube_linear_reason,
        as_clause: clausal_reason,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConflictingStatus {
    PremisesNotTrue,
    NonNegativeSlack,
    Conflicting,
}

/// Returns true if the given hypercube linear is conflicting at the given trail position.
fn is_conflicting(
    hypercube_linear: &HypercubeLinearExplanation<'_>,
    trail_position: usize,
    assignments: &Assignments,
) -> ConflictingStatus {
    // For the hypercube linear to be conflicting the hypercube must be satisfied.
    if hypercube_linear
        .hypercube
        .iter_predicates()
        .any(|predicate| {
            let predicate_value =
                assignments.evaluate_predicate_at_trail_position(predicate, trail_position);
            dbg!(predicate);
            dbg!(predicate_value);

            predicate_value != Some(true)
        })
    {
        return ConflictingStatus::PremisesNotTrue;
    }

    if compute_slack_at_trail_position(&hypercube_linear.linear, assignments, trail_position) < 0 {
        ConflictingStatus::Conflicting
    } else {
        ConflictingStatus::NonNegativeSlack
    }
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
