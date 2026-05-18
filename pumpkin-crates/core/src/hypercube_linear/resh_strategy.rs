use std::num::NonZero;

use dyn_clone::DynClone;
use itertools::Itertools;
use log::trace;

use crate::create_statistics_struct;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::conflict_state::ConflictState;
use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::hypercube_linear::trail_view::TrailView;
use crate::hypercube_linear::trail_view::affine_lower_bound_at;
use crate::predicate;
use crate::predicates::Predicate;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::AffineView;
use crate::variables::DomainId;

pub(crate) trait ResHStrategy: DynClone + std::fmt::Debug {
    fn apply(
        &mut self,
        state: &mut ConflictState,
        trail: &mut dyn TrailView,
        trail_position: usize,
        pivot: Predicate,
        explanation: HypercubeLinearExplanation,
    );

    fn log_statistics(&self, _logger: StatisticLogger) {}
}

dyn_clone::clone_trait_object!(ResHStrategy);

// ======== StandardResH ========

create_statistics_struct!(StandardResHStatistics {
    num_propositional_resolutions_use_explanation_linear: usize,
});

#[derive(Clone, Debug, Default)]
pub(crate) struct StandardResH {
    statistics: StandardResHStatistics,
}

impl ResHStrategy for StandardResH {
    fn apply(
        &mut self,
        state: &mut ConflictState,
        trail: &mut dyn TrailView,
        trail_position: usize,
        pivot: Predicate,
        mut explanation: HypercubeLinearExplanation,
    ) {
        let linear_propagated_pivot_to_false = explanation.iter_predicates().any(|p| p == !pivot);

        let linear_slack_is_negative = if let Some(linear) = explanation.linear() {
            compute_linear_slack_at_trail_position(trail, linear, trail_position - 1) < 0
        } else {
            true
        };

        let can_substitute_with_explanation_linear =
            linear_propagated_pivot_to_false && linear_slack_is_negative;

        if state.conflicting_linear.is_trivially_false() && can_substitute_with_explanation_linear {
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

                if !truth_value {
                    continue;
                }

                state.add_hypercube_predicate(trail, predicate);
            }

            let linear = explanation.take_linear();
            state.explain_linear(trail, &linear, trail_position - 1);
            state.conflicting_linear = linear;

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

                if !truth_value {
                    continue;
                }

                state.add_hypercube_predicate(trail, predicate);
            }
        }
    }

    fn log_statistics(&self, logger: StatisticLogger) {
        self.statistics.log(logger);
    }
}

/// A [`ResHStrategy`] that weakens the conflict and explanation linear until both are the same.
#[derive(Clone, Debug, Default)]
pub(super) struct MiddlingResH {
    backup: StandardResH,
    additional_predicates_buffer: Vec<Predicate>,
}

impl ResHStrategy for MiddlingResH {
    fn apply(
        &mut self,
        state: &mut ConflictState,
        trail: &mut dyn TrailView,
        trail_position: usize,
        pivot: Predicate,
        explanation: HypercubeLinearExplanation,
    ) {
        if explanation.is_clause() || state.conflicting_linear.is_trivially_false() {
            // In the case that either the conflict or the explanation is a clause, we
            // use the backup propositional resolution operation.
            self.backup
                .apply(state, trail, trail_position, pivot, explanation);
            return;
        }

        let explanation_linear = explanation.linear().expect("explanation was not a clause");

        let mut weakened_conflict_bound = state.conflicting_linear.bound();
        let mut weakened_explanation_bound = explanation_linear.bound();

        let num_conflict_terms = state.conflicting_linear.terms().len();
        let num_explanation_terms = explanation_linear.terms().len();
        let mut conflict_index = 0;
        let mut explanation_index = 0;
        let mut linear_terms = vec![];

        let mut weaken = |count: u32, predicate: Predicate, bound: &mut i32| {
            self.additional_predicates_buffer.push(predicate);

            if predicate.is_lower_bound_predicate() {
                *bound += -(count as i32) * predicate.get_right_hand_side();
            } else {
                *bound += (count as i32) * predicate.get_right_hand_side();
            }
        };

        while conflict_index < num_conflict_terms && explanation_index < num_explanation_terms {
            let conflict_term = state.conflicting_linear.term_by_index(conflict_index);
            let explanation_term = explanation_linear.term_by_index(explanation_index);

            let predicate_to_weaken_conflict_on = if conflict_term.scale.is_positive() {
                let bound =
                    trail.lower_bound_at_trail_position(conflict_term.inner, trail_position);
                predicate![conflict_term.inner >= bound]
            } else {
                let bound =
                    trail.upper_bound_at_trail_position(conflict_term.inner, trail_position);
                predicate![conflict_term.inner <= bound]
            };

            let predicate_to_weaken_explanation_on = if explanation_term.scale.is_positive() {
                let bound =
                    trail.lower_bound_at_trail_position(explanation_term.inner, trail_position);
                predicate![explanation_term.inner >= bound]
            } else {
                let bound =
                    trail.upper_bound_at_trail_position(explanation_term.inner, trail_position);
                predicate![explanation_term.inner <= bound]
            };

            match conflict_term.inner.cmp(&explanation_term.inner) {
                std::cmp::Ordering::Equal => {
                    conflict_index += 1;
                    explanation_index += 1;

                    if conflict_term.scale.is_positive() != explanation_term.scale.is_positive() {
                        // If the weights do not have the same sign, both must be weakened to zero.
                        weaken(
                            explanation_term.scale.unsigned_abs(),
                            predicate_to_weaken_explanation_on,
                            &mut weakened_explanation_bound,
                        );
                        weaken(
                            conflict_term.scale.unsigned_abs(),
                            predicate_to_weaken_conflict_on,
                            &mut weakened_conflict_bound,
                        );
                        continue;
                    }
                }
                std::cmp::Ordering::Less => {
                    weaken(
                        conflict_term.scale.unsigned_abs(),
                        predicate_to_weaken_conflict_on,
                        &mut weakened_conflict_bound,
                    );
                    conflict_index += 1;
                    continue;
                }
                std::cmp::Ordering::Greater => {
                    weaken(
                        explanation_term.scale.unsigned_abs(),
                        predicate_to_weaken_explanation_on,
                        &mut weakened_explanation_bound,
                    );
                    explanation_index += 1;
                    continue;
                }
            }

            assert_eq!(
                conflict_term.scale.is_positive(),
                explanation_term.scale.is_positive()
            );

            let target_weight = conflict_term.scale.abs().min(explanation_term.scale.abs());

            if conflict_term.scale != target_weight {
                assert!(conflict_term.scale > target_weight);

                let weaken_count = conflict_term.scale.abs_diff(target_weight);
                weaken(
                    weaken_count,
                    predicate_to_weaken_conflict_on,
                    &mut weakened_conflict_bound,
                );
            }

            if explanation_term.scale != target_weight {
                assert!(explanation_term.scale > target_weight);

                let weaken_count = explanation_term.scale.abs_diff(target_weight);
                weaken(
                    weaken_count,
                    predicate_to_weaken_explanation_on,
                    &mut weakened_explanation_bound,
                );
            }

            linear_terms.push((NonZero::new(target_weight).unwrap(), conflict_term.inner));
        }

        while conflict_index < num_conflict_terms {
            let conflict_term = state.conflicting_linear.term_by_index(conflict_index);
            let explanation_term = explanation_linear.term_by_index(explanation_index - 1);

            let predicate_to_weaken_conflict_on = if conflict_term.scale.is_positive() {
                let bound =
                    trail.lower_bound_at_trail_position(conflict_term.inner, trail_position);
                predicate![conflict_term.inner >= bound]
            } else {
                let bound =
                    trail.upper_bound_at_trail_position(conflict_term.inner, trail_position);
                predicate![conflict_term.inner <= bound]
            };

            let predicate_to_weaken_explanation_on = if explanation_term.scale.is_positive() {
                let bound =
                    trail.lower_bound_at_trail_position(explanation_term.inner, trail_position);
                predicate![explanation_term.inner >= bound]
            } else {
                let bound =
                    trail.upper_bound_at_trail_position(explanation_term.inner, trail_position);
                predicate![explanation_term.inner <= bound]
            };

            match conflict_term.inner.cmp(&explanation_term.inner) {
                std::cmp::Ordering::Equal => {
                    conflict_index += 1;

                    if conflict_term.scale.is_positive() != explanation_term.scale.is_positive() {
                        // If the weights do not have the same sign, both must be weakened to zero.
                        weaken(
                            explanation_term.scale.unsigned_abs(),
                            predicate_to_weaken_explanation_on,
                            &mut weakened_explanation_bound,
                        );
                        weaken(
                            conflict_term.scale.unsigned_abs(),
                            predicate_to_weaken_conflict_on,
                            &mut weakened_conflict_bound,
                        );
                        continue;
                    }
                }
                std::cmp::Ordering::Less => {
                    weaken(
                        conflict_term.scale.unsigned_abs(),
                        predicate_to_weaken_conflict_on,
                        &mut weakened_conflict_bound,
                    );
                    conflict_index += 1;
                    continue;
                }
                std::cmp::Ordering::Greater => {
                    todo!("unclear");
                    // weaken(
                    //     explanation_term.scale.unsigned_abs(),
                    //     predicate_to_weaken_explanation_on,
                    //     &mut weakened_explanation_bound,
                    // );
                    // explanation_index += 1;
                    // continue;
                }
            }

            assert_eq!(
                conflict_term.scale.is_positive(),
                explanation_term.scale.is_positive()
            );

            let target_weight = conflict_term.scale.abs().min(explanation_term.scale.abs());

            if conflict_term.scale != target_weight {
                assert!(conflict_term.scale > target_weight);

                let weaken_count = conflict_term.scale.abs_diff(target_weight);
                weaken(
                    weaken_count,
                    predicate_to_weaken_conflict_on,
                    &mut weakened_conflict_bound,
                );
            }

            if explanation_term.scale != target_weight {
                assert!(explanation_term.scale > target_weight);

                let weaken_count = explanation_term.scale.abs_diff(target_weight);
                weaken(
                    weaken_count,
                    predicate_to_weaken_explanation_on,
                    &mut weakened_explanation_bound,
                );
            }

            linear_terms.push((NonZero::new(target_weight).unwrap(), conflict_term.inner));
        }

        while explanation_index < num_explanation_terms {
            let conflict_term = state.conflicting_linear.term_by_index(conflict_index - 1);
            let explanation_term = explanation_linear.term_by_index(explanation_index);

            let predicate_to_weaken_conflict_on = if conflict_term.scale.is_positive() {
                let bound =
                    trail.lower_bound_at_trail_position(conflict_term.inner, trail_position);
                predicate![conflict_term.inner >= bound]
            } else {
                let bound =
                    trail.upper_bound_at_trail_position(conflict_term.inner, trail_position);
                predicate![conflict_term.inner <= bound]
            };

            let predicate_to_weaken_explanation_on = if explanation_term.scale.is_positive() {
                let bound =
                    trail.lower_bound_at_trail_position(explanation_term.inner, trail_position);
                predicate![explanation_term.inner >= bound]
            } else {
                let bound =
                    trail.upper_bound_at_trail_position(explanation_term.inner, trail_position);
                predicate![explanation_term.inner <= bound]
            };

            match conflict_term.inner.cmp(&explanation_term.inner) {
                std::cmp::Ordering::Equal => {
                    explanation_index += 1;

                    if conflict_term.scale.is_positive() != explanation_term.scale.is_positive() {
                        // If the weights do not have the same sign, both must be weakened to zero.
                        weaken(
                            explanation_term.scale.unsigned_abs(),
                            predicate_to_weaken_explanation_on,
                            &mut weakened_explanation_bound,
                        );
                        weaken(
                            conflict_term.scale.unsigned_abs(),
                            predicate_to_weaken_conflict_on,
                            &mut weakened_conflict_bound,
                        );
                        continue;
                    }
                }
                std::cmp::Ordering::Less => {
                    todo!("unclear")
                    // weaken(
                    //     conflict_term.scale.unsigned_abs(),
                    //     predicate_to_weaken_conflict_on,
                    //     &mut weakened_conflict_bound,
                    // );
                    // conflict_index += 1;
                    // continue;
                }
                std::cmp::Ordering::Greater => {
                    weaken(
                        explanation_term.scale.unsigned_abs(),
                        predicate_to_weaken_explanation_on,
                        &mut weakened_explanation_bound,
                    );
                    explanation_index += 1;
                    continue;
                }
            }

            assert_eq!(
                conflict_term.scale.is_positive(),
                explanation_term.scale.is_positive()
            );

            let target_weight = conflict_term.scale.abs().min(explanation_term.scale.abs());

            if conflict_term.scale != target_weight {
                assert!(conflict_term.scale > target_weight);

                let weaken_count = conflict_term.scale.abs_diff(target_weight);
                weaken(
                    weaken_count,
                    predicate_to_weaken_conflict_on,
                    &mut weakened_conflict_bound,
                );
            }

            if explanation_term.scale != target_weight {
                assert!(explanation_term.scale > target_weight);

                let weaken_count = explanation_term.scale.abs_diff(target_weight);
                weaken(
                    weaken_count,
                    predicate_to_weaken_explanation_on,
                    &mut weakened_explanation_bound,
                );
            }

            linear_terms.push((NonZero::new(target_weight).unwrap(), conflict_term.inner));
        }

        let new_conflicting_linear = LinearInequality::new(
            linear_terms,
            weakened_explanation_bound.max(weakened_conflict_bound),
        )
        .expect("propositional resolution keeps linear conflicting");

        state.explain_linear(trail, &new_conflicting_linear, trail_position);
        state.conflicting_linear = new_conflicting_linear;

        // Add the explanation hypercube and the predicates introduced through weakening to
        // the conflicting hypercube
        for predicate in explanation
            .iter_predicates()
            .filter(|&p| p != !pivot) // Do not add the negation of propagated predicate
            .chain(self.additional_predicates_buffer.drain(..))
        {
            state.add_hypercube_predicate(trail, predicate);
        }
    }

    fn log_statistics(&self, logger: StatisticLogger) {
        self.backup.log_statistics(logger.clone());
    }
}

fn compute_linear_slack_at_trail_position(
    trail: &dyn TrailView,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| i64::from(affine_lower_bound_at(trail, term, trail_position)))
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}
