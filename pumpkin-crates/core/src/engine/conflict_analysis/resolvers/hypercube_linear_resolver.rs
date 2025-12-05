use std::num::NonZero;

use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::containers::HashSet;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::ConflictResolver;
use crate::engine::notifications::NotificationEngine;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagators::HypercubeLinear;
use crate::propagators::HypercubeLinearPropagator;
use crate::propagators::HypercubeLinearPropagatorArgs;
use crate::propagators::Term;
use crate::statistics::Statistic;

create_statistics_struct!(HypercubeLinearResolutionStatistics {
    average_num_linear_terms_in_learned_hypercube_linear: CumulativeMovingAverage<usize>,
    num_attempted_fourier_resolves: usize,
    num_successful_fourier_resolves: usize,
    num_learned_hypercube_linear: usize,
    num_learned_clauses: usize,
    num_overflow_errors: usize,
});

#[derive(Debug, Default)]
pub(crate) struct HypercubeLinearResolver {
    statistics: HypercubeLinearResolutionStatistics,
    learned_constraints: HashSet<HypercubeLinear>,
}

impl HypercubeLinearResolver {
    fn compute_conflicting_hypercube_linear(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
    ) -> (HypercubeLinear, Vec<Predicate>) {
        let StoredConflictInfo::EmptyDomainTwo {
            conflict_trigger_reason,
            conflict_trigger,
            ..
        } = context.solver_state.get_conflict_info()
        else {
            panic!(
                "Cannot start hypercube analysis from {:?}",
                context.solver_state.get_conflict_info()
            );
        };

        trace!("conflicting predicate = {}", conflict_trigger);

        let propagator_id = context.reason_store.get_propagator(conflict_trigger_reason);

        let handle = context
            .propagators
            .as_propagator_handle::<HypercubeLinearPropagator>(propagator_id)
            .expect("all propagations by hypercube propagator");

        let propagator = context
            .propagators
            .get_propagator(handle)
            .expect("we already tested that it is a hypercube linear");

        let mut reason_set = explain_initial_conflict(
            &propagator.hypercube_linear,
            context.assignments.num_trail_entries() - 1,
            context.assignments,
            conflict_trigger,
        );

        reason_set.push(!conflict_trigger);

        trace!(
            "constraint ID = {:?}, pid = {}",
            propagator.constraint_tag, propagator_id
        );
        trace!("learned = {:?}", propagator.is_learned);

        (propagator.hypercube_linear.clone(), reason_set)
    }

    fn restore_solver(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
        decision_level: usize,
        learned_constraint: HypercubeLinear,
    ) {
        debug!("Learned {learned_constraint}");
        debug!(
            "Jumping to decision level {decision_level} from decision level {}",
            context.assignments.get_decision_level()
        );

        assert!(
            self.learned_constraints.insert(learned_constraint.clone()),
            "learning the same constraint twice"
        );

        self.statistics
            .average_num_linear_terms_in_learned_hypercube_linear
            .add_term(learned_constraint.iter_linear_terms().len());
        self.statistics.num_learned_hypercube_linear += 1;

        if learned_constraint.iter_linear_terms().count() == 0 {
            self.statistics.num_learned_clauses += 1;
        }

        let backjump_amount = context.assignments.get_decision_level() - decision_level;

        if backjump_amount > 1 {
            context.counters.engine_statistics.num_backjumps += 1;
        }

        context
            .counters
            .learned_clause_statistics
            .average_backtrack_amount
            .add_term(backjump_amount as u64);

        context.backtrack(decision_level);

        let constraint_tag = context.proof_log.new_constraint_tag();

        let propagator_slot = context.propagators.new_propagator();

        // dbg!(learned_constraint.compute_slack(context.assignments));
        // for p in learned_constraint.iter_hypercube() {
        //     let domain = p.get_domain();
        //     println!(
        //         "{} in [{}, {}]",
        //         domain,
        //         context.assignments.get_lower_bound(domain),
        //         context.assignments.get_upper_bound(domain)
        //     );
        // }
        // for (weight, domain) in learned_constraint.iter_linear_terms() {
        //     let term = domain.scaled(weight.get());

        //     use crate::engine::variables::IntegerVariable;

        //     println!(
        //         "{:?} in [{}, {}]",
        //         term,
        //         term.lower_bound(context.assignments),
        //         term.upper_bound(context.assignments)
        //     );
        // }

        let constructor_context = PropagatorConstructorContext::new(
            context.notification_engine,
            context.trailed_values,
            context.proof_log,
            propagator_slot.key().propagator_id(),
            context.assignments,
        );

        let propagator = HypercubeLinearPropagatorArgs {
            hypercube_linear: learned_constraint,
            constraint_tag,
            is_learned: true,
        }
        .create(constructor_context);

        let new_propagator_id = propagator_slot.populate(propagator);
        trace!(
            "New constraint tag = {constraint_tag:?} (pid = {})",
            new_propagator_id.propagator_id()
        );

        context
            .propagator_queue
            .enqueue_propagator(new_propagator_id.propagator_id(), 0);
    }

    fn resolve(
        &mut self,
        mut conflicting_hypercube_linear: HypercubeLinear,
        reason: HypercubeLinearReason,
        context: &HlResolverContext<'_>,
        trail_index: usize,
        pivot_predicate: Predicate,
    ) -> HypercubeLinear {
        let mut elimination_happened = false;

        match fourier_eliminate(
            context.assignments,
            trail_index,
            &conflicting_hypercube_linear,
            &reason.original,
            pivot_predicate,
        ) {
            Ok(hypercube_linear) => {
                self.statistics.num_attempted_fourier_resolves += 1;

                let new_slack = hypercube_linear
                    .compute_slack_at_trail_position(context.assignments, trail_index);

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

                let weakened_conflicting =
                            conflicting_hypercube_linear.weaken(pivot_predicate).expect("if we could do fourier elimination then the top of trail contributes to the conflict and can therefore be weakened on");
                let weakened_conflicting_slack = weakened_conflicting
                    .compute_slack_at_trail_position(context.assignments, trail_index);
                assert!(
                    weakened_conflicting_slack < 0,
                    "weakening like this does not affect slack"
                );

                conflicting_hypercube_linear = propositional_resolution(
                    context.assignments,
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
            if let Some(hypercube_linear) = conflicting_hypercube_linear.weaken(pivot_predicate) {
                trace!(
                    "Pivot is propagated by the hypercube of the reason, but is in the linear of the conflict."
                );
                trace!("weakening conflict to {hypercube_linear}");
                conflicting_hypercube_linear = hypercube_linear;
            }
        }

        if hypercube_models_bound(&conflicting_hypercube_linear, pivot_predicate) {
            // Here we need to perform propositional resolution to eliminate the
            // variable from the hypercube.

            conflicting_hypercube_linear = propositional_resolution(
                context.assignments,
                conflicting_hypercube_linear,
                &reason.as_clause,
                pivot_predicate,
            );
            elimination_happened = true;
            let new_slack = conflicting_hypercube_linear
                .compute_slack_at_trail_position(context.assignments, trail_index);
            trace!("slack after resh = {new_slack}");
        } else {
            trace!("no propositional resolution");
        }

        if !elimination_happened {
            trace!("current conflict: {conflicting_hypercube_linear}");
            panic!(
                "if the predicate contributes to the conflict, an elimination should be possible"
            );
        }

        conflicting_hypercube_linear
    }

    fn learn_hypercube_linear(
        &mut self,
        context: HlResolverContext,
        mut conflicting_hypercube_linear: HypercubeLinear,
        conflicting_reason_set: Vec<Predicate>,
        // ) -> (HypercubeLinear, Vec<Predicate>) {
    ) -> (HypercubeLinear, usize) {
        let mut conflicting_reason_set = HypercubeLinear::clause(conflicting_reason_set);

        let conflict_dl = context.assignments.get_decision_level();
        let mut trail_index = context.assignments.num_trail_entries();

        // Iterate the trail backwards until a constraint is obtained that propagates at a
        // previous decision level.
        loop {
            // println!("{conflicting_hypercube_linear}");
            // dbg!(&conflicting_reason_set);

            // for p in conflicting_reason_set.iter_hypercube() {
            //     let domain = p.get_domain();
            //     println!(
            //         "{} in [{}, {}]",
            //         domain,
            //         domain.lower_bound_at_trail_position(context.assignments, trail_index),
            //         domain.upper_bound_at_trail_position(context.assignments, trail_index)
            //     );
            // }

            assert!(conflicting_reason_set.iter_hypercube().all(|predicate| {
                let truth_value = context
                    .assignments
                    .evaluate_predicate_at_trail_position(predicate, trail_index);

                // let domain = predicate.get_domain();
                // println!(
                //     "{predicate} -> {} in [{}, {}]",
                //     domain,
                //     domain.lower_bound_at_trail_position(context.assignments, trail_index),
                //     domain.upper_bound_at_trail_position(context.assignments, trail_index)
                // );

                truth_value == Some(true)
            }));

            // println!("Reasonset:");
            let reasons_on_current_dl = conflicting_reason_set
                .iter_hypercube()
                .filter(|predicate| {
                    let predicate_dl = context
                        .assignments
                        .get_decision_level_for_predicate(predicate)
                        .unwrap();

                    // println!("{predicate}: {predicate_dl}");

                    predicate_dl == conflict_dl
                })
                .collect::<Vec<_>>();

            trace!(
                "reasons on conflict dl = {reasons_on_current_dl:?} (out of {})",
                conflicting_reason_set.iter_hypercube().count()
            );

            // if reasons_on_current_dl.len() <= 1 {
            //     return (
            //         conflicting_hypercube_linear,
            //         conflicting_reason_set.iter_hypercube().collect(),
            //     );
            // }

            assert_eq!(
                is_conflicting(
                    &conflicting_hypercube_linear,
                    trail_index,
                    context.assignments,
                ),
                ConflictingStatus::Conflicting
            );

            trail_index -= 1;
            let top_of_trail = context.assignments.get_trail_entry(trail_index);

            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            let pivot_predicate = top_of_trail.predicate;
            trace!("processing {}", pivot_predicate);

            // Remove the top of trail from the reason set if it is in the reason set.
            if !conflicting_reason_set
                .iter_hypercube()
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

            let reason = hypercube_from_reason(
                &mut HlResolverContext {
                    assignments: context.assignments,
                    reason_store: context.reason_store,
                    propagators: context.propagators,
                    notification_engine: context.notification_engine,
                },
                top_of_trail.predicate,
                reason_ref,
            );

            conflicting_hypercube_linear = self.resolve(
                conflicting_hypercube_linear,
                reason,
                &context,
                trail_index,
                top_of_trail.predicate,
            );

            // In this case the fourier elimination may have removed multiple variables, which we
            // also need to remove from the new reason set.
            conflicting_reason_set = explain_conflict(
                &conflicting_hypercube_linear,
                trail_index,
                context.assignments,
            );

            assert!(
                !conflicting_reason_set
                    .iter_hypercube()
                    .any(|p| p == pivot_predicate),
                "the pivot predicate is removed from the reason of the conflict"
            );

            if let Some(backjump_dl) =
                will_propagate_on_previous_dl(&conflicting_hypercube_linear, context.assignments)
            {
                return (conflicting_hypercube_linear, backjump_dl);
            }
        }
    }
}

fn will_propagate_on_previous_dl(
    hypercube_linear: &HypercubeLinear,
    assignments: &Assignments,
) -> Option<usize> {
    trace!("Testing propagation at previous dl");
    let current_dl = assignments.get_decision_level();
    let decision_levels = (0..current_dl).rev();

    for decision_level in decision_levels {
        trace!("  => testing dl = {decision_level}");
        let trail_position = assignments.get_trail_position_at_decision_level(decision_level);
        let mut propagations = hypercube_linear.propagate_at(assignments, trail_position, None);
        propagations.retain(|(predicate, _)| {
            assignments.evaluate_predicate_at_trail_position(*predicate, trail_position)
                != Some(true)
        });

        if propagations.is_empty() {
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

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: crate::statistics::StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, mut context: ConflictAnalysisContext) -> bool {
        let (original_conflicting_hypercube_linear, reason_set) =
            self.compute_conflicting_hypercube_linear(&mut context);

        debug!(
            "Resolving conflict (dl = {}) {original_conflicting_hypercube_linear}",
            context.assignments.get_decision_level()
        );

        let conflicting_hypercube_linear = original_conflicting_hypercube_linear.clone();
        let (learned_hypercube_linear, backump_level) = self.learn_hypercube_linear(
            HlResolverContext {
                assignments: context.assignments,
                reason_store: context.reason_store,
                propagators: context.propagators,
                notification_engine: context.notification_engine,
            },
            conflicting_hypercube_linear.clone(),
            reason_set.clone(),
        );

        self.restore_solver(&mut context, backump_level, learned_hypercube_linear);

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

        true
    }
}

fn explain_initial_conflict(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
    conflict_trigger: Predicate,
) -> Vec<Predicate> {
    hypercube_linear
        .iter_hypercube()
        .chain(hypercube_linear.iter_linear_terms().filter_map(|term| {
            let term_lower_bound = term.lower_bound_at_trail_position(assignments, trail_position);
            let predicate = predicate![term >= term_lower_bound];

            if conflict_trigger.implies(predicate) {
                None
            } else {
                Some(predicate)
            }
        }))
        .collect()
}

fn explain_conflict(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> HypercubeLinear {
    let hypercube = hypercube_linear
        .iter_hypercube()
        .chain(hypercube_linear.iter_linear_terms().map(|term| {
            let term_lower_bound = term.lower_bound_at_trail_position(assignments, trail_position);
            predicate![term >= term_lower_bound]
        }))
        .collect();

    HypercubeLinear::clause(hypercube)
}

fn propositional_resolution(
    assignments: &Assignments,
    conflicting_hypercube_linear: HypercubeLinear,
    reason: &HypercubeLinear,
    pivot_predicate: Predicate,
) -> HypercubeLinear {
    trace!("applying propositional resolution on {pivot_predicate}",);

    // Make sure that the pivot is not also contributing to the conflict in the linear part.
    trace!("  - {conflicting_hypercube_linear}");
    let weakened_conflict = conflicting_hypercube_linear.weaken(pivot_predicate);
    let weakened_conflict = weakened_conflict.unwrap_or(conflicting_hypercube_linear);
    trace!("    - weakened: {weakened_conflict}");

    trace!("  - {reason}");

    assert!(
        reason.iter_linear_terms().next().is_none(),
        "the reason should be a clause"
    );

    let hypercube = weakened_conflict
        .iter_hypercube()
        .chain(reason.iter_hypercube())
        .filter(|&predicate| {
            // Only keep predicates that are not the propagated predicate or its
            // opposite.
            !pivot_predicate.implies(predicate) && !pivot_predicate.implies(!predicate)
        })
        .filter(|predicate| {
            // Ignore predicates that are true at the root.
            assignments
                .get_decision_level_for_predicate(predicate)
                .unwrap()
                > 0
        })
        .collect();

    let new_constraint = HypercubeLinear::new(
        hypercube,
        weakened_conflict
            .iter_linear_terms()
            .map(|term| (term.weight, term.domain))
            .collect(),
        weakened_conflict.linear_rhs(),
    )
    .expect("not trivially satisfiable");

    trace!("result = {new_constraint}");

    new_constraint
}

fn hypercube_models_bound(hypercube_linear: &HypercubeLinear, predicate: Predicate) -> bool {
    hypercube_linear
        .iter_hypercube()
        .any(|hypercube_predicate| predicate.implies(hypercube_predicate))
}

struct HlResolverContext<'a> {
    assignments: &'a Assignments,
    reason_store: &'a mut ReasonStore,
    propagators: &'a mut PropagatorStore,
    notification_engine: &'a mut NotificationEngine,
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
    assignments: &Assignments,
    trail_position: usize,
    conflicting_hypercube_linear: &HypercubeLinear,
    reason: &HypercubeLinear,
    pivot_predicate: Predicate,
) -> Result<HypercubeLinear, FourierError> {
    let maybe_weight_in_conflicting =
        conflicting_hypercube_linear.variable_weight(pivot_predicate.get_domain());
    let maybe_weight_in_reason = reason.variable_weight(pivot_predicate.get_domain());

    let (weight_in_conflicting, weight_in_reason) =
        match (maybe_weight_in_conflicting, maybe_weight_in_reason) {
            (Some(a_weight), Some(b_weight))
                if a_weight.is_positive() != b_weight.is_positive() =>
            {
                (a_weight, b_weight)
            }

            // Either the domain is not in one of the two constraints, or they don't have
            // opposing signs. In both cases, we cannot perform fourier
            // elimination on the specified domain.
            _ => return Err(FourierError::NoVariableElimination),
        };

    let reason_hypercube_satisfied = reason
        .iter_hypercube()
        .all(|p| assignments.evaluate_predicate_at_trail_position(p, trail_position) == Some(true));

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

    let reason_slack = reason.compute_slack_at_trail_position(assignments, trail_position);

    trace!("applying fourier elimination on {pivot_predicate}",);
    trace!("  - {conflicting_hypercube_linear}");
    trace!("  - {reason}");
    trace!(
        "  - slack a: {}",
        conflicting_hypercube_linear.compute_slack_at_trail_position(assignments, trail_position)
    );
    trace!("  - slack b: {reason_slack}");

    if reason_slack < 0 {
        trace!("  => no fourier possible as reason propagated through hypercube");
        return Err(FourierError::ResultOfEliminationTriviallySatisfiable);
    }

    let tightly_propagating_reason = compute_tightly_propagating_reason(
        assignments,
        trail_position,
        reason,
        Term {
            weight: weight_in_reason,
            domain: pivot_predicate.get_domain(),
        },
    );

    trace!("  - tightly propagating: {tightly_propagating_reason}");
    let tp_slack =
        tightly_propagating_reason.compute_slack_at_trail_position(assignments, trail_position);
    trace!("     - slack: {tp_slack}");

    let weight_in_reformulated_reason = tightly_propagating_reason
        .variable_weight(pivot_predicate.get_domain())
        .unwrap();
    assert_eq!(weight_in_reformulated_reason.get().abs(), 1);

    let scale_reason = weight_in_conflicting.abs();
    let mut linear_terms = conflicting_hypercube_linear
        .iter_linear_terms()
        .map(|term| Ok((term.weight, term.domain)))
        .chain(
            tightly_propagating_reason
                .iter_linear_terms()
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
        .iter_hypercube()
        .chain(tightly_propagating_reason.iter_hypercube())
        .collect();

    let mut linear_rhs = conflicting_hypercube_linear
        .linear_rhs()
        .checked_add(
            tightly_propagating_reason
                .linear_rhs()
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
    //     .iter_linear_terms()
    //     .map(|term| {
    //         term.weight
    //             .checked_mul(scale_conflicting)
    //             .ok_or(FourierError::IntegerOverflow)
    //             .map(|scaled_weight| (scaled_weight, term.domain))
    //     })
    //     .chain(reason.iter_linear_terms().map(|term| {
    //         term.weight
    //             .checked_mul(scale_reason)
    //             .ok_or(FourierError::IntegerOverflow)
    //             .map(|scaled_weight| (scaled_weight, term.domain))
    //     }))
    //     .collect::<Result<Vec<_>, _>>()?;

    // let hypercube = conflicting_hypercube_linear
    //     .iter_hypercube()
    //     .chain(reason.iter_hypercube())
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

    let new_constraint = HypercubeLinear::new(hypercube, linear_terms, linear_rhs)
        .ok_or(FourierError::ResultOfEliminationTriviallySatisfiable)?;

    trace!("result = {new_constraint}");
    Ok(new_constraint)
}

/// Use weakening to obtain a hypercube linear that has the term for `pivot_domain` be unit in the
/// linear part.
fn compute_tightly_propagating_reason(
    assignment: &Assignments,
    trail_position: usize,
    orignal_reason: &HypercubeLinear,
    pivot_term: Term,
) -> HypercubeLinear {
    let divisor = pivot_term.weight.abs();

    let bounds_to_weaken: Vec<_> = orignal_reason
        .iter_linear_terms()
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
        tightly_propagating_reason = tightly_propagating_reason
            .weaken(bound)
            .expect("by construction this bound can be weakened on");
    }

    tightly_propagating_reason.divide_linear(divisor);

    tightly_propagating_reason
}

struct HypercubeLinearReason {
    original: HypercubeLinear,
    as_clause: HypercubeLinear,
}

/// Get the hypercube linear reason for the propagation.
fn hypercube_from_reason(
    context: &mut HlResolverContext<'_>,
    propagated_predicate: Predicate,
    reason: ReasonRef,
) -> HypercubeLinearReason {
    let mut clausal_reason = Vec::new();
    assert!(context.reason_store.get_or_compute(
        reason,
        ExplanationContext::new(
            context.assignments,
            CurrentNogood::empty(),
            0,
            context.notification_engine,
        ),
        context.propagators,
        &mut clausal_reason,
    ));

    let propagator_id = context.reason_store.get_propagator(reason);
    let handle = context
        .propagators
        .as_propagator_handle::<HypercubeLinearPropagator>(propagator_id)
        .expect("all propagations by hypercube propagator");

    let propagator = context
        .propagators
        .get_propagator(handle)
        .expect("we already tested that it is an affine view");

    trace!(
        "combining with constraint ID {:?} (is_learned = {})",
        propagator.constraint_tag, propagator.is_learned
    );
    trace!("{}", propagator.hypercube_linear);
    trace!("clausal reason = {clausal_reason:?}",);

    HypercubeLinearReason {
        original: propagator.hypercube_linear.clone(),
        as_clause: HypercubeLinear::clause(
            clausal_reason
                .iter()
                .copied()
                .chain(std::iter::once(!propagated_predicate))
                .collect(),
        ),
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
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> ConflictingStatus {
    // For the hypercube linear to be conflicting the hypercube must be satisfied.
    if hypercube_linear.iter_hypercube().any(|predicate| {
        assignments.evaluate_predicate_at_trail_position(predicate, trail_position) != Some(true)
    }) {
        return ConflictingStatus::PremisesNotTrue;
    }

    if hypercube_linear.compute_slack_at_trail_position(assignments, trail_position) < 0 {
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
