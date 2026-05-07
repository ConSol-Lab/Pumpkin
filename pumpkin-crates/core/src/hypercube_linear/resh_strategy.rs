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

        let mut linear_terms: Vec<_> = state
            .conflicting_linear
            .terms()
            .map(|term| {
                (
                    NonZero::new(term.scale).expect("scale is non zero"),
                    term.inner,
                    TermSource::Conflict,
                )
            })
            .merge_by(
                explanation_linear.terms().map(|term| {
                    (
                        NonZero::new(term.scale).expect("scale is non zero"),
                        term.inner,
                        TermSource::Explanation,
                    )
                }),
                |(_, x1, _), (_, x2, _)| x1 <= x2,
            )
            .collect();

        let mut weakened_conflict_bound = state.conflicting_linear.bound();
        let mut weakened_explanation_bound = explanation_linear.bound();

        for i in 0..linear_terms.len() - 1 {
            let (w1, x1, s1) = linear_terms[i];
            let (w2, x2, s2) = linear_terms[i + 1];

            if x1 != x2 {
                continue;
            }

            assert_ne!(
                s1, s2,
                "a linear inequality never yields multiple terms for the same variable"
            );

            if w1 == w2 {
                // If both weights are equal, then we do not need to weaken.
                continue;
            }

            let (
                target_weight,
                variable,
                actual_weight,
                source_to_weaken,
                index_to_weaken_in_terms,
            ) = if w1.abs() > w2.abs() {
                (w2.get(), x1, w1.get(), s1, i)
            } else {
                (w1.get(), x1, w2.get(), s2, i + 1)
            };

            assert!(target_weight.abs() < actual_weight.abs());
            assert_eq!(target_weight.is_positive(), actual_weight.is_positive());

            // Determine what predicate will be weakened on.
            let predicate_to_weaken_on = if target_weight.is_positive() {
                let bound = trail.lower_bound_at_trail_position(variable, trail_position);
                predicate![variable >= bound]
            } else {
                let bound = trail.upper_bound_at_trail_position(variable, trail_position);
                predicate![variable <= bound]
            };

            // Make sure the predicate is added to the final hypercube.
            self.additional_predicates_buffer
                .push(predicate_to_weaken_on);

            let weaken_bound_by = if predicate_to_weaken_on.is_lower_bound_predicate() {
                -predicate_to_weaken_on.get_right_hand_side()
                    * (actual_weight.abs() - target_weight.abs())
            } else {
                predicate_to_weaken_on.get_right_hand_side()
                    * (actual_weight.abs() - target_weight.abs())
            };

            linear_terms[index_to_weaken_in_terms].0 =
                NonZero::new(target_weight).expect("all weights are non-zero");

            match source_to_weaken {
                TermSource::Conflict => {
                    weakened_conflict_bound += weaken_bound_by;
                }
                TermSource::Explanation => {
                    weakened_explanation_bound += weaken_bound_by;
                }
            }
        }

        let new_conflicting_linear = LinearInequality::new(
            linear_terms
                .into_iter()
                .map(|(weight, domain, _)| (weight, domain))
                .dedup(),
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

/// From which HL does a term come from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TermSource {
    Conflict,
    Explanation,
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
