#![allow(unused, reason = "under heavy development")]
use std::num::NonZero;

use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
use crate::containers::HashSet;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::VariableNames;
use crate::engine::notifications::NotificationEngine;
use crate::engine::reason::ReasonStore;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearConstructor;
use crate::hypercube_linear::HypercubeLinearExplanation;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::Term;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagation::ExplanationContext;
use crate::propagation::store::PropagatorStore;
use crate::state::CurrentNogood;
use crate::state::EmptyDomainConflict;
use crate::state::State;
use crate::statistics::Statistic;
use crate::statistics::moving_averages::CumulativeMovingAverage;
use crate::statistics::moving_averages::MovingAverage;
use crate::variables::IntegerVariable;

create_statistics_struct!(HypercubeLinearResolutionStatistics {
    average_num_linear_terms_in_learned_hypercube_linear: CumulativeMovingAverage<usize>,
    num_attempted_fourier_resolves: usize,
    num_successful_fourier_resolves: usize,
    num_learned_hypercube_linear: usize,
    num_learned_clauses: usize,
    num_overflow_errors: usize,
    num_propositional_resolutions: usize,
    num_equal_linear_in_resh: usize,
});

#[derive(Clone, Debug, Default)]
pub struct HypercubeLinearResolver {
    statistics: HypercubeLinearResolutionStatistics,
    learned_constraints: HashSet<HypercubeLinearExplanation>,
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
    ) -> HypercubeLinearExplanation {
        match context.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator(propagator_conflict) => {
                trace!("Converting propagator conflict to hypercube");

                HypercubeLinearExplanation::nogood(propagator_conflict.conjunction)
                    .expect("propagator conflict contains mutually exclusive predicates")
            }
            StoredConflictInfo::EmptyDomain(empty_domain_conflict) => self
                .compute_conflicting_hypercube_linear_from_empty_domain(
                    context,
                    empty_domain_conflict,
                ),

            StoredConflictInfo::InconsistentAssumptions(_)
            | StoredConflictInfo::RootLevelConflict(_) => {
                unreachable!("will never resolve these conflicts")
            }
        }
    }

    fn compute_conflicting_hypercube_linear_from_empty_domain(
        &mut self,
        mut context: ConflictAnalysisContext<'_>,
        empty_domain_conflict: EmptyDomainConflict,
    ) -> HypercubeLinearExplanation {
        let EmptyDomainConflict {
            trigger_reason,
            trigger_predicate,
            ..
        } = empty_domain_conflict;

        let trigger_reason =
            trigger_reason.expect("cannot resolve conflict that was triggered by an assumption");

        trace!(
            "{} caused an empty domain, computing conflict constraint",
            trigger_predicate.display(&context.state.variable_names)
        );

        let conflict_nogood = context.get_conflict_nogood();
        let clausal_conflict = HypercubeLinearExplanation::nogood(conflict_nogood)
            .expect("conflict contains mutually exclusive predicates");

        let propagator_id = context.state.reason_store.get_propagator(trigger_reason);

        trace!(
            "conflicting predicate = {}",
            trigger_predicate.display(&context.state.variable_names)
        );

        if let Some(reason_code) = context.state.reason_store.get_lazy_code(trigger_reason) {
            let explanation_context = ExplanationContext::without_working_nogood(
                &context.state.assignments,
                context.state.trail_len() - 1,
                &mut context.state.notification_engine,
            );

            if let Some(hypercube_linear) = context.state.propagators[propagator_id]
                .explain_as_hypercube_linear(*reason_code, explanation_context)
            {
                hypercube_linear
            } else {
                clausal_conflict
            }
        } else {
            clausal_conflict
        }
    }

    fn restore_solver(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        decision_level: usize,
        learned_constraint: HypercubeLinearExplanation,
    ) {
        debug!(
            "Learned {}",
            learned_constraint.display(&context.state.variable_names)
        );
        debug!(
            "Jumping to decision level {decision_level} from decision level {}",
            context.state.get_checkpoint(),
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

        let _ = context.state.restore_to(decision_level);

        let constraint_tag = context.state.new_constraint_tag();

        // dbg!(learned_constraint.compute_slack(context.assignments));
        // for p in learned_constraint.hypercube.iter_predicates() {
        //     let domain = p.get_domain();
        //     println!(
        //         "{} in [{}, {}]",
        //         domain,
        //         context.assignments.get_lower_bound(domain),
        //         context.assignments.get_upper_bound(domain)
        //     );
        // }
        // for (weight, domain) in learned_constraint.linear.terms() {
        //     let term = domain.scaled(weight.get());

        //     use crate::engine::variables::IntegerVariable;

        //     println!(
        //         "{:?} in [{}, {}]",
        //         term,
        //         term.lower_bound(context.assignments),
        //         term.upper_bound(context.assignments)
        //     );
        // }

        let new_propagator_id = context.state.add_propagator(HypercubeLinearConstructor {
            hypercube: learned_constraint.hypercube,
            linear: learned_constraint.linear,
            constraint_tag,
            is_learned: true,
        });

        trace!(
            "New constraint tag = {constraint_tag:?} (pid = {})",
            new_propagator_id.propagator_id()
        );
    }

    fn resolve(
        &mut self,
        mut conflicting_hypercube_linear: HypercubeLinearExplanation,
        reason: HypercubeLinearReason,
        state: &mut State,
        trail_index: usize,
        pivot_predicate: Predicate,
    ) -> HypercubeLinearExplanation {
        let mut elimination_happened = false;

        match fourier_eliminate(
            state,
            trail_index,
            &conflicting_hypercube_linear,
            &reason.original,
            pivot_predicate,
        ) {
            Ok(hypercube_linear) => {
                self.statistics.num_attempted_fourier_resolves += 1;

                let new_slack = compute_hl_slack_at_trail_position(
                    &hypercube_linear,
                    &state.assignments,
                    trail_index,
                );

                trace!("new slack = {new_slack}");
                assert!(new_slack < 0);
                conflicting_hypercube_linear = hypercube_linear;

                // let new_conflicting = if new_slack >= 0 {
                //     let weakened_conflicting =
                //             conflicting_hypercube_linear.weaken(pivot_predicate).expect("if we
                // could do fourier elimination then the top of trail contributes to the conflict
                // and can therefore be weakened on");
                //     let weakened_conflicting_slack = weakened_conflicting
                //         .compute_slack_at_trail_position(context.assignments, trail_index);
                //     assert!(
                //         weakened_conflicting_slack < 0,
                //         "weakening like this does not affect slack"
                //     );

                //     propositional_resolution(
                //         context.assignments,
                //         trail_index,
                //         weakened_conflicting,
                //         &reason,
                //         pivot_predicate,
                //     )
                // } else {
                //     self.statistics.num_successful_fourier_resolves += 1;
                //     hypercube_linear
                // };

                // conflicting_hypercube_linear = new_conflicting;
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
                let weakened_conflicting_slack = compute_hl_slack_at_trail_position(
                    &weakened_conflicting,
                    &state.assignments,
                    trail_index,
                );
                assert!(
                    weakened_conflicting_slack < 0,
                    "weakening like this does not affect slack"
                );

                conflicting_hypercube_linear = self.propositional_resolution(
                    state,
                    weakened_conflicting,
                    &reason.as_clause,
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

            conflicting_hypercube_linear = self.propositional_resolution(
                state,
                conflicting_hypercube_linear,
                &reason.as_clause,
                pivot_predicate,
            );
            elimination_happened = true;
            let new_slack = compute_hl_slack_at_trail_position(
                &conflicting_hypercube_linear,
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
        mut conflicting_hypercube_linear: HypercubeLinearExplanation,
    ) -> (HypercubeLinearExplanation, usize) {
        let conflict_dl = state.get_checkpoint();
        let mut trail_index = state.trail_len();

        let mut conflicting_reason_set = explain_conflict(
            &conflicting_hypercube_linear,
            trail_index,
            &state.assignments,
        );

        // Iterate the trail backwards until a constraint is obtained that propagates at a
        // previous decision level.
        loop {
            // println!("{conflicting_hypercube_linear}");
            // dbg!(&conflicting_reason_set);

            // for p in conflicting_reason_set.hypercube.iter_predicates() {
            //     let domain = p.get_domain();
            //     println!(
            //         "{} in [{}, {}]",
            //         domain,
            //         domain.lower_bound_at_trail_position(context.assignments, trail_index),
            //         domain.upper_bound_at_trail_position(context.assignments, trail_index)
            //     );
            // }

            dbg!(trail_index);
            let unsatisfied_reason_set_predicates = conflicting_reason_set
                .hypercube
                .iter_predicates()
                .filter(|&predicate| {
                    let truth_value = state
                        .assignments
                        .evaluate_predicate_at_trail_position(predicate, trail_index);

                    let domain = predicate.get_domain();
                    println!(
                        "{} -> {} in [{}, {}]",
                        predicate.display(&state.variable_names),
                        domain.display(&state.variable_names),
                        domain.lower_bound_at_trail_position(&state.assignments, trail_index),
                        domain.upper_bound_at_trail_position(&state.assignments, trail_index)
                    );

                    truth_value != Some(true)
                })
                .collect::<PropositionalConjunction>();

            assert!(
                unsatisfied_reason_set_predicates.is_empty(),
                "not all predicates in conflict RS are true: {}",
                unsatisfied_reason_set_predicates.display(&state.variable_names),
            );

            // println!("Reasonset:");
            let reasons_on_current_dl = conflicting_reason_set
                .hypercube
                .iter_predicates()
                .filter(|predicate| {
                    let predicate_dl = state
                        .assignments
                        .get_checkpoint_for_predicate(predicate)
                        .unwrap();

                    // println!("{predicate}: {predicate_dl}");

                    predicate_dl == conflict_dl
                })
                .collect::<Vec<_>>();

            trace!(
                "reasons on conflict dl = {} (out of {})",
                PropositionalConjunction::from(reasons_on_current_dl)
                    .display(&state.variable_names),
                conflicting_reason_set.hypercube.iter_predicates().count()
            );

            // if reasons_on_current_dl.len() <= 1 {
            //     return (
            //         conflicting_hypercube_linear,
            //         conflicting_reason_set.hypercube.iter_predicates().collect(),
            //     );
            // }

            assert_eq!(
                is_conflicting(
                    &conflicting_hypercube_linear,
                    trail_index,
                    &state.assignments,
                ),
                ConflictingStatus::Conflicting
            );

            trail_index -= 1;
            let top_of_trail = state.assignments.get_trail_entry(trail_index);

            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            let pivot_predicate = top_of_trail.predicate;
            trace!(
                "processing {}",
                pivot_predicate.display(&state.variable_names)
            );

            // Remove the top of trail from the reason set if it is in the reason set.
            if !conflicting_reason_set
                .hypercube
                .iter_predicates()
                .any(|p| pivot_predicate.implies(p))
            {
                trace!("  => skipping, does not contribute to conflict");
                // The current top of trail is not in the reason set and therefore does not
                // contribute to the conflict.
                continue;
            }

            let Some(reason) = hypercube_from_reason(state, top_of_trail.predicate) else {
                continue;
            };

            conflicting_hypercube_linear = self.resolve(
                conflicting_hypercube_linear,
                reason,
                state,
                trail_index,
                top_of_trail.predicate,
            );

            // In this case the fourier elimination may have removed multiple variables, which we
            // also need to remove from the new reason set.
            conflicting_reason_set = explain_conflict(
                &conflicting_hypercube_linear,
                trail_index,
                &state.assignments,
            );

            assert!(
                !conflicting_reason_set
                    .hypercube
                    .iter_predicates()
                    .any(|p| p == pivot_predicate),
                "the pivot predicate is removed from the reason of the conflict"
            );

            if let Some(backjump_dl) =
                will_propagate_on_previous_dl(&conflicting_hypercube_linear, &state.assignments)
            {
                return (conflicting_hypercube_linear, backjump_dl);
            }
        }
    }

    fn propositional_resolution(
        &mut self,
        state: &State,
        conflicting_hypercube_linear: HypercubeLinearExplanation,
        reason: &HypercubeLinearExplanation,
        pivot_predicate: Predicate,
    ) -> HypercubeLinearExplanation {
        self.statistics.num_propositional_resolutions += 1;

        if conflicting_hypercube_linear.linear == reason.linear {
            self.statistics.num_equal_linear_in_resh += 1;
        }

        trace!("applying propositional resolution on {pivot_predicate}",);

        // Make sure that the pivot is not also contributing to the conflict in the linear part.
        trace!(
            "  - {}",
            conflicting_hypercube_linear.display(&state.variable_names)
        );
        let weakened_conflict = conflicting_hypercube_linear.weaken(pivot_predicate);
        trace!(
            "    - weakened: {}",
            weakened_conflict.display(&state.variable_names)
        );

        trace!("  - {}", reason.display(&state.variable_names));

        assert!(
            reason.linear.terms().next().is_none(),
            "the reason should be a clause"
        );

        let hypercube = weakened_conflict
            .hypercube
            .iter_predicates()
            .chain(reason.hypercube.iter_predicates())
            .filter(|&predicate| {
                // Only keep predicates that are not the propagated predicate or its
                // opposite.
                !pivot_predicate.implies(predicate) && !pivot_predicate.implies(!predicate)
            })
            .filter(|&predicate| {
                // Ignore predicates that are true at the root.
                state.get_checkpoint_for_predicate(predicate).unwrap() > 0
            });

        let new_constraint = HypercubeLinearExplanation {
            hypercube: Hypercube::new(hypercube).expect("inconsistent hypercube"),
            linear: weakened_conflict.linear.clone(),
        };

        trace!("result = {}", new_constraint.display(&state.variable_names));

        new_constraint
    }
}

fn will_propagate_on_previous_dl(
    hypercube_linear: &HypercubeLinearExplanation,
    assignments: &Assignments,
) -> Option<usize> {
    trace!("Testing propagation at previous dl");
    let current_dl = assignments.get_checkpoint();
    let decision_levels = (0..current_dl).rev();

    for decision_level in decision_levels {
        trace!("  => testing dl = {decision_level}");
        let trail_position = assignments.get_trail_position_at_checkpoint(decision_level);

        if !propagates_at(hypercube_linear, assignments, trail_position) {
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
    hypercube_linear: &HypercubeLinearExplanation,
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

    let slack = compute_linear_slack_at_trail_position(
        &hypercube_linear.linear,
        assignments,
        trail_position,
    );

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

    let slack = compute_linear_slack_at_trail_position(linear, assignments, trail_position);

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

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: crate::statistics::StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        let original_conflicting_hypercube_linear =
            self.compute_conflicting_hypercube_linear(context.reborrow());

        debug!(
            "Resolving conflict (dl = {}) {}",
            context.state.get_checkpoint(),
            original_conflicting_hypercube_linear.display(&context.state.variable_names),
        );

        let conflicting_hypercube_linear = original_conflicting_hypercube_linear.clone();
        let (learned_hypercube_linear, backump_level) =
            self.learn_hypercube_linear(context.state, conflicting_hypercube_linear.clone());

        self.restore_solver(context, backump_level, learned_hypercube_linear);

        // #[allow(unused, reason = "may be used in assertion in future")]
        // let learned_hypercube_linear = loop {
        //     let (learned_hypercube_linear, learned_reason_set) = self.learn_hypercube_linear(
        //         HlResolverContext {
        //             assignments: context.assignments,
        //             reason_store: context.reason_store,
        //             propagators: context.propagators,
        //             notification_engine: context.notification_engine,
        //         },
        //         conflicting_hypercube_linear.clone(),
        //         reason_set.clone(),
        //     );

        //     let backjump = self.compute_backjump_level(
        //         context.assignments,
        //         &learned_hypercube_linear,
        //         &learned_reason_set,
        //     );

        //     if backjump.num_reasons_on_current_dl == 1 {
        //         self.restore_solver(
        //             &mut context,
        //             backjump.backjump_level,
        //             learned_hypercube_linear.clone(),
        //         );
        //         break learned_hypercube_linear;
        //     }

        //     trace!(
        //         "Learning resulted in zero reasons on the current DL. We backtrack and continue
        // learning."     );

        //     // There are 0 reasons on the current decision level that contribute to the conflict.
        //     // Therefore, the conflict exists at a previous decision level. We backtrack there
        // and     // restart the analysis.
        //     context.backtrack(backjump.backjump_level);

        //     conflicting_hypercube_linear = learned_hypercube_linear;
        //     reason_set = learned_reason_set;
        // };

        // assert_ne!(
        //     learned_hypercube_linear, original_conflicting_hypercube_linear,
        //     "learning should produce a new constraint"
        // );
    }
}

fn explain_conflict(
    hypercube_linear: &HypercubeLinearExplanation,
    trail_position: usize,
    assignments: &Assignments,
) -> HypercubeLinearExplanation {
    let hypercube =
        hypercube_linear
            .hypercube
            .iter_predicates()
            .chain(hypercube_linear.linear.terms().map(|term| {
                let term_lower_bound =
                    term.lower_bound_at_trail_position(assignments, trail_position);
                predicate![term >= term_lower_bound]
            }));

    HypercubeLinearExplanation::nogood(hypercube).expect("inconsistent nogood")
}

fn hypercube_models_bound(
    hypercube_linear: &HypercubeLinearExplanation,
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
    conflicting_hypercube_linear: &HypercubeLinearExplanation,
    reason: &HypercubeLinearExplanation,
    pivot_predicate: Predicate,
) -> Result<HypercubeLinearExplanation, FourierError> {
    let maybe_term_in_conflict = conflicting_hypercube_linear
        .linear
        .term_for_domain(pivot_predicate.get_domain());
    let maybe_term_in_reason = conflicting_hypercube_linear
        .linear
        .term_for_domain(pivot_predicate.get_domain());

    let (term_in_conflict, term_in_reason) = match (maybe_term_in_conflict, maybe_term_in_reason) {
        (Some(a_term), Some(b_term))
            if a_term.weight.is_positive() != b_term.weight.is_positive() =>
        {
            (a_term, b_term)
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

    let contributes_to_conflict_in_linear = (term_in_conflict.weight.is_positive()
        && pivot_predicate.is_lower_bound_predicate())
        || (term_in_conflict.weight.is_negative() && pivot_predicate.is_upper_bound_predicate());

    // We should only do fourier elimination if the pivot predicate actually contributes to the
    // conflict. Otherwise performing the combination will not remove any contribution to the
    // conflict.
    if !contributes_to_conflict_in_linear {
        return Err(FourierError::NoVariableElimination);
    }

    let reason_slack =
        compute_hl_slack_at_trail_position(reason, &state.assignments, trail_position);

    trace!(
        "applying fourier elimination on {}",
        pivot_predicate.display(&state.variable_names)
    );
    trace!(
        "  - {}",
        conflicting_hypercube_linear.display(&state.variable_names)
    );
    trace!("  - {}", reason.display(&state.variable_names));
    trace!(
        "  - slack a: {}",
        compute_hl_slack_at_trail_position(
            conflicting_hypercube_linear,
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
        term_in_reason,
    );

    trace!(
        "  - tightly propagating: {}",
        tightly_propagating_reason.display(&state.variable_names)
    );
    let tp_slack = compute_hl_slack_at_trail_position(
        &tightly_propagating_reason,
        &state.assignments,
        trail_position,
    );
    trace!("     - slack: {tp_slack}");

    let weight_in_reformulated_reason = tightly_propagating_reason
        .linear
        .term_for_domain(pivot_predicate.get_domain())
        .unwrap()
        .weight;
    assert_eq!(weight_in_reformulated_reason.get().abs(), 1);

    let scale_reason = weight_in_reformulated_reason.abs();
    let mut linear_terms = conflicting_hypercube_linear
        .linear
        .terms()
        .map(|term| Ok((term.weight, term.domain)))
        .chain(
            tightly_propagating_reason
                .linear
                .terms()
                .map(|reason_term| {
                    reason_term
                        .weight
                        .checked_mul(scale_reason)
                        .ok_or(FourierError::IntegerOverflow)
                        .map(|scaled_weight| (scaled_weight, reason_term.domain))
                }),
        )
        .collect::<Result<Vec<(_, _)>, _>>()?;

    let hypercube = conflicting_hypercube_linear
        .hypercube
        .iter_predicates()
        .chain(tightly_propagating_reason.hypercube.iter_predicates());

    let mut linear_rhs = conflicting_hypercube_linear
        .linear
        .bound()
        .checked_add(
            tightly_propagating_reason
                .linear
                .bound()
                .checked_mul(scale_reason.get())
                .ok_or(FourierError::IntegerOverflow)?,
        )
        .ok_or(FourierError::IntegerOverflow)?;

    // // Determine by how much to scale both linear terms.
    // let g = gcd(
    //     weight_in_conflicting.get().abs(),
    //     weight_in_reason.get().abs(),
    // );
    // // We have to remake the non-zero as there is no API for division on NonZero yet.
    // let scale_conflicting = NonZero::new(weight_in_reason.abs().get() / g).unwrap();
    // let scale_reason = NonZero::new(weight_in_conflicting.abs().get() / g).unwrap();

    // // The linear terms of the new hypercube linear is the addition of the scaled terms of both
    // // input constraints.
    // let mut linear_terms = conflicting_hypercube_linear
    //     .linear.terms()
    //     .map(|term| {
    //         term.weight
    //             .checked_mul(scale_conflicting)
    //             .ok_or(FourierError::IntegerOverflow)
    //             .map(|scaled_weight| (scaled_weight, term.domain))
    //     })
    //     .chain(reason.linear.terms().map(|term| {
    //         term.weight
    //             .checked_mul(scale_reason)
    //             .ok_or(FourierError::IntegerOverflow)
    //             .map(|scaled_weight| (scaled_weight, term.domain))
    //     }))
    //     .collect::<Result<Vec<_>, _>>()?;

    // let hypercube = conflicting_hypercube_linear
    //     .hypercube.iter_predicates()
    //     .chain(reason.hypercube.iter_predicates())
    //     .collect();

    // let scaled_conflict_linear_rhs = conflicting_hypercube_linear
    //     .linear_rhs()
    //     .checked_mul(scale_conflicting.get())
    //     .ok_or(FourierError::IntegerOverflow)?;
    // let scaled_reason_linear_rhs = reason
    //     .linear_rhs()
    //     .checked_mul(scale_reason.get())
    //     .ok_or(FourierError::IntegerOverflow)?;
    // let mut linear_rhs = scaled_conflict_linear_rhs
    //     .checked_add(scaled_reason_linear_rhs)
    //     .ok_or(FourierError::IntegerOverflow)?;

    // Normalize the linear component of the hypercube linear to hopefully avoid overflows in the
    // future.
    let normalize_by = linear_terms
        .iter()
        .map(|(weight, _)| weight.get())
        .chain(std::iter::once(linear_rhs))
        .reduce(gcd)
        .unwrap_or(linear_rhs);

    linear_terms.iter_mut().for_each(|(weight, _)| {
        *weight = NonZero::new(<i32 as NumExt>::div_ceil(weight.get(), normalize_by)).unwrap();
    });
    linear_rhs = <i32 as NumExt>::div_ceil(linear_rhs, normalize_by);

    let new_constraint = HypercubeLinearExplanation {
        hypercube: Hypercube::new(hypercube).expect("inconsistent hypercube"),
        linear: LinearInequality::new(linear_terms, linear_rhs)
            .ok_or(FourierError::ResultOfEliminationTriviallySatisfiable)?,
    };

    trace!("result = {}", new_constraint.display(&state.variable_names));
    Ok(new_constraint)
}

/// Use weakening to obtain a hypercube linear that has the term for `pivot_domain` be unit in the
/// linear part.
fn compute_tightly_propagating_reason(
    assignment: &Assignments,
    trail_position: usize,
    orignal_reason: &HypercubeLinearExplanation,
    pivot_term: Term,
) -> HypercubeLinearExplanation {
    let divisor = pivot_term.weight.abs();

    let bounds_to_weaken: Vec<_> = orignal_reason
        .linear
        .terms()
        .filter_map(|term| {
            // If the weight of the term is divisible by the weight of the pivot term, then we
            // keep it in the linear part. Otherwise, it is weakened on.

            if term.weight.get() % divisor.get() == 0 {
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

    // Divide everything in the linear.
    for term in tightly_propagating_reason.linear.terms_mut() {
        term.weight = NonZero::new(term.weight.get() / divisor.get()).unwrap();
    }
    tightly_propagating_reason.linear.bound =
        <i32 as NumExt>::div_floor(tightly_propagating_reason.linear.bound, divisor.get());

    tightly_propagating_reason
}

struct HypercubeLinearReason {
    original: HypercubeLinearExplanation,
    as_clause: HypercubeLinearExplanation,
}

fn hypercube_from_reason(
    state: &mut State,
    propagated_predicate: Predicate,
) -> Option<HypercubeLinearReason> {
    let mut clausal_reason = vec![];

    let _ = state.get_propagation_reason(
        propagated_predicate,
        &mut clausal_reason,
        CurrentNogood::empty(),
    );

    let mut clausal_reason = PropositionalConjunction::from(clausal_reason);

    trace!(
        "{} -> {}",
        clausal_reason.display(&state.variable_names),
        propagated_predicate.display(&state.variable_names),
    );

    let Some(trail_entry) = state.trail_entry_for(propagated_predicate) else {
        // The propagated predicate is implied. We return nothing, as we cannot create a
        // hypercube (the clausal reason for an implied predicate naturally contains
        // inconsistent predicates).

        // self.should_process_predicates(context.state, context.brancher, clausal_reason);

        trace!("predicate is implied");

        return None;
    };

    // self.should_process_predicates(
    //     context.state,
    //     context.brancher,
    //     clausal_reason.iter().copied(),
    // );

    clausal_reason.push(!propagated_predicate);
    let clausal_reason_as_hl = HypercubeLinearExplanation::nogood(clausal_reason)
        .expect("reason for a propagated predicate does not contain inconsistent premises");

    // If the propatated predicate is on the trail, we want to try to explain the propagation
    // with a hypercube linear constraint.
    let Some((reason_ref, _)) = trail_entry.reason else {
        panic!("resolution should never reach the last decision");
    };

    let propagator_id = state.reason_store.get_propagator(reason_ref);

    let hypercube_linear_reason =
        if let Some(reason_code) = state.reason_store.get_lazy_code(reason_ref) {
            let explanation_context = ExplanationContext::without_working_nogood(
                &state.assignments,
                state.trail_len() - 1,
                &mut state.notification_engine,
            );

            if let Some(original) = state.propagators[propagator_id]
                .explain_as_hypercube_linear(*reason_code, explanation_context)
            {
                original
            } else {
                clausal_reason_as_hl.clone()
            }
        } else {
            clausal_reason_as_hl.clone()
        };

    trace!(
        "hypercube linear reason = {}",
        hypercube_linear_reason.display(&state.variable_names)
    );
    trace!(
        "clausal reason = {}",
        clausal_reason_as_hl.display(&state.variable_names)
    );

    Some(HypercubeLinearReason {
        original: hypercube_linear_reason,
        as_clause: clausal_reason_as_hl,
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConflictingStatus {
    PremisesNotTrue,
    NonNegativeSlack,
    Conflicting,
}

/// Returns true if the given hypercube linear is conflicting at the given trail position.
fn is_conflicting(
    hypercube_linear: &HypercubeLinearExplanation,
    trail_position: usize,
    assignments: &Assignments,
) -> ConflictingStatus {
    // For the hypercube linear to be conflicting the hypercube must be satisfied.
    if hypercube_linear
        .hypercube
        .iter_predicates()
        .any(|predicate| {
            assignments.evaluate_predicate_at_trail_position(predicate, trail_position)
                != Some(true)
        })
    {
        return ConflictingStatus::PremisesNotTrue;
    }

    if compute_hl_slack_at_trail_position(hypercube_linear, assignments, trail_position) < 0 {
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

fn compute_linear_slack_at_trail_position(
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

fn compute_hl_slack_at_trail_position(
    hypercube_linear: &HypercubeLinearExplanation,
    assignments: &Assignments,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = hypercube_linear
        .linear
        .terms()
        .map(|term| {
            let assignment_lb =
                i64::from(term.lower_bound_at_trail_position(assignments, trail_position));
            let hypercube_lb = hypercube_linear
                .hypercube
                .lower_bound(term)
                .unwrap_or(i64::MIN);

            assignment_lb.max(hypercube_lb)
        })
        .sum::<i64>();

    i64::from(hypercube_linear.linear.bound()) - lower_bound_terms
}
