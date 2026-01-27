mod checker;
mod hypercube;
mod linear;

pub use checker::*;
pub use hypercube::*;
pub use linear::*;

use crate::basic_types::PredicateId;
use crate::declare_inference_label;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::EnqueueDecision;
use crate::propagation::InferenceCheckers;
use crate::propagation::NotificationContext;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_eq_advanced;
use crate::results::PropagationStatusCP;
use crate::state::Conflict;
use crate::state::PropagatorConflict;
use crate::variables::AffineView;
use crate::variables::DomainId;

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub struct HypercubeLinearConstructor {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
    pub constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for HypercubeLinearConstructor {
    type PropagatorImpl = HypercubeLinearPropagator;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, HypercubeLinear),
            Box::new(HypercubeLinearChecker {
                hypercube: self.hypercube.iter_predicates().collect(),
                terms: self.linear.terms().collect(),
                bound: self.linear.bound(),
            }),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        } = self;

        let hypercube_predicates = hypercube.iter_predicates().collect::<Box<[_]>>();

        let watched_predicates = if hypercube_predicates.is_empty() {
            let true_predicate = Predicate::trivially_true();
            let true_predicate_id = context.register_predicate(true_predicate);
            [true_predicate_id; 3]
        } else {
            let last_idx = hypercube_predicates.len() - 1;
            [
                context.register_predicate(hypercube_predicates[0.min(last_idx)]),
                context.register_predicate(hypercube_predicates[1.min(last_idx)]),
                context.register_predicate(hypercube_predicates[2.min(last_idx)]),
            ]
        };

        HypercubeLinearPropagator {
            hypercube,
            linear,

            hypercube_predicates,
            watched_predicates,
            inference_code: InferenceCode::new(constraint_tag, HypercubeLinear),
        }
    }
}

declare_inference_label!(HypercubeLinear);

const NUM_WATCHED_PREDICATES: usize = 3;

/// A [`Propagator`] for the hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct HypercubeLinearPropagator {
    hypercube: Hypercube,
    linear: LinearInequality,

    hypercube_predicates: Box<[Predicate]>,
    /// The predicate ID at index i corresponds to the predicate at index i in
    /// `hypercube_predicates`.
    watched_predicates: [PredicateId; NUM_WATCHED_PREDICATES],

    inference_code: InferenceCode,
}

impl HypercubeLinearPropagator {
    /// Get the index from [`Self::watched_predicates`] that corresponds to `predicate_id`.
    ///
    /// If no such index exists, a panic occurs.
    fn watcher_index_for(&self, predicate_id: PredicateId) -> usize {
        self.watched_predicates
            .iter()
            .position(|&pid| pid == predicate_id)
            .expect("can only get the watcher index for watched predicates")
    }

    /// Get the watcher index for the predicate that is unassigned.
    ///
    /// Assumes 0 or 1 watched predicates are/is unassigned.
    fn unassigned_watcher_index(&self, mut context: PropagationContext<'_>) -> Option<usize> {
        self.watched_predicates
            .iter()
            .position(|&pid| !context.is_predicate_id_satisfied(pid))
    }
}

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn notify_predicate_id_satisfied(
        &mut self,
        mut context: NotificationContext,
        predicate_id: PredicateId,
    ) -> EnqueueDecision {
        let watcher_index = self.watcher_index_for(predicate_id);

        pumpkin_assert_advanced!(watcher_index < NUM_WATCHED_PREDICATES);

        pumpkin_assert_eq_advanced!(
            Some(true),
            context.evaluate_predicate(self.hypercube_predicates[watcher_index])
        );

        let next_predicate_to_watch = self
            .hypercube_predicates
            .iter()
            .skip(NUM_WATCHED_PREDICATES)
            .position(|&predicate| context.evaluate_predicate(predicate) != Some(true))
            .map(|index| index + NUM_WATCHED_PREDICATES);

        if let Some(predicate_index) = next_predicate_to_watch {
            // To update the watcher we find a new predicate that is not assigned to true and put
            // it in the spot of the predicate that became true.

            let next_predicate_to_watch = self.hypercube_predicates[predicate_index];

            context.unwatch_predicate(predicate_id);
            let new_predicate_id = context.watch_predicate(next_predicate_to_watch);

            self.hypercube_predicates
                .swap(watcher_index, predicate_index);
            self.watched_predicates[watcher_index] = new_predicate_id;
        }

        let satisfied_watchers = self
            .watched_predicates
            .iter()
            .filter(|&&predicate_id| context.is_predicate_id_satisfied(predicate_id))
            .count();

        if satisfied_watchers >= NUM_WATCHED_PREDICATES - 1 {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        let satisfied_watchers = self
            .watched_predicates
            .iter()
            .filter(|&&predicate_id| context.is_predicate_id_satisfied(predicate_id))
            .count();

        if satisfied_watchers < 2 {
            return Ok(());
        }

        let unassigned_watcher_index = self.unassigned_watcher_index(context.reborrow());

        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| i64::from(context.lower_bound(&term)))
            .sum::<i64>();

        let slack = i64::from(self.linear.bound()) - lower_bound_terms;

        match unassigned_watcher_index {
            Some(index) => {
                let predicate_in_hypercube = self.hypercube_predicates[index];

                let maybe_term = self
                    .linear
                    .term_for_domain(self.hypercube_predicates[index].get_domain());

                if slack < 0 {
                    // We have one unassigned predicate in the hypercube over a variable that
                    // does not appear in the linear inequality. Since the slack is negative, we
                    // can propagate that predicate to false.

                    let conjunction: PropositionalConjunction = self
                        .linear
                        .terms()
                        .map(|term| predicate![term >= context.lower_bound(&term)])
                        .chain(
                            self.hypercube
                                .iter_predicates()
                                .filter(|&predicate| predicate != predicate_in_hypercube),
                        )
                        .collect();

                    context.post(!predicate_in_hypercube, conjunction, &self.inference_code)?;
                } else if let Some(term_to_propagate) = maybe_term {
                    // The slack is at least 0, but it may be that the linear could propagate
                    // something weaker than `!predicate_in_hypercube`.

                    if !could_propagate_weaker_predicate(predicate_in_hypercube, term_to_propagate)
                    {
                        return Ok(());
                    }

                    let bound_i64 = slack + i64::from(context.lower_bound(&term_to_propagate));
                    let bound = match i32::try_from(bound_i64) {
                        Ok(bound) => bound,
                        Err(_) if bound_i64.is_negative() => todo!(
                            "wanting to tighten the upper bound to a value smaller than i32::MIN"
                        ),
                        // If we want to set the upper bound to a value larger than i32::MAX,
                        // it can never tighten the existing bound of `term_to_propagate`.
                        Err(_) => return Ok(()),
                    };

                    let conjunction: PropositionalConjunction = self
                        .linear
                        .terms()
                        .map(|term| predicate![term >= context.lower_bound(&term)])
                        .chain(
                            self.hypercube
                                .iter_predicates()
                                .filter(|&predicate| predicate != predicate_in_hypercube),
                        )
                        .collect();

                    context.post(
                        predicate![term_to_propagate <= bound],
                        conjunction,
                        &self.inference_code,
                    )?;
                }
            }

            None => {
                // All watchers are true. Propagate the linear inequality.

                let lower_bound_terms = self
                    .linear
                    .terms()
                    .map(|term| i64::from(context.lower_bound(&term)))
                    .sum::<i64>();

                if lower_bound_terms > i64::from(self.linear.bound()) {
                    let conjunction = self
                        .linear
                        .terms()
                        .map(|term| predicate![term >= context.lower_bound(&term)])
                        .chain(self.hypercube.iter_predicates())
                        .collect();

                    return Err(Conflict::Propagator(PropagatorConflict {
                        conjunction,
                        inference_code: self.inference_code.clone(),
                    }));
                }
            }
        }

        Ok(())
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if self
            .hypercube_predicates
            .iter()
            .any(|&predicate| context.evaluate_predicate(predicate) == Some(false))
        {
            // If the hypercube contains at least one false predicate, the propagator will not do
            // anything.
            return Ok(());
        }

        // Get the predicates that are not assigned to true.
        let unsatisfied_predicates_in_hypercubes = self
            .hypercube_predicates
            .iter()
            .filter(|&&predicate| context.evaluate_predicate(predicate) != Some(true))
            .copied()
            .collect::<Vec<_>>();

        if unsatisfied_predicates_in_hypercubes.len() > 1 {
            // If more than one predicate remains unassigned, we cannot do anything.
            return Ok(());
        }

        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| i64::from(context.lower_bound(&term)))
            .sum::<i64>();

        let slack = i64::from(self.linear.bound()) - lower_bound_terms;

        if unsatisfied_predicates_in_hypercubes.len() == 1 {
            let unassigned_predicate = unsatisfied_predicates_in_hypercubes[0];

            if slack < 0 {
                let reason = self
                    .linear
                    .terms()
                    .map(|term| predicate![term >= context.lower_bound(&term)])
                    .chain(
                        self.hypercube
                            .iter_predicates()
                            .filter(|&p| p != unassigned_predicate),
                    )
                    .collect::<PropositionalConjunction>();

                context.post(!unassigned_predicate, reason, &self.inference_code)?;
            } else if let Some(term) = self
                .linear
                .term_for_domain(unassigned_predicate.get_domain())
            {
                let term_lower_bound = context.lower_bound(&term);
                let new_upper_bound = match i32::try_from(slack + i64::from(term_lower_bound)) {
                    Ok(bound) => bound,
                    Err(_) => return Ok(()),
                };

                let reason = self
                    .linear
                    .terms()
                    .filter(|&t| t != term)
                    .map(|term| predicate![term >= context.lower_bound(&term)])
                    .chain(self.hypercube.iter_predicates())
                    .collect::<PropositionalConjunction>();

                context.post(
                    predicate![term <= new_upper_bound],
                    reason,
                    &self.inference_code,
                )?;
            }
        } else if slack < 0 {
            let conjunction = self
                .linear
                .terms()
                .map(|term| predicate![term >= context.lower_bound(&term)])
                .chain(self.hypercube.iter_predicates())
                .collect();

            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code: self.inference_code.clone(),
            }));
        }

        Ok(())
    }
}

/// Returns true if the given term could propagate a weaker predicate than the given one.
fn could_propagate_weaker_predicate(predicate: Predicate, term: AffineView<DomainId>) -> bool {
    (term.scale.is_positive() && predicate.is_lower_bound_predicate())
        || (term.scale.is_negative() && predicate.is_upper_bound_predicate())
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;
    use crate::predicate;
    use crate::state::State;

    #[test]
    fn conflict_detected() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into()));
        let z = state.new_interval_variable(2, 5, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");
        // x + y + z <= 5.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            5,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn incremental_hypercube_evaluation() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z = state.new_interval_variable(0, 5, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2], predicate![z <= 3]])
                .expect("not inconsistent");

        let linear = LinearInequality::trivially_false();

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        let _ = state.post(predicate![x >= 2]).expect("domain not empty");
        assert!(state.propagate_to_fixed_point().is_ok());
        let _ = state.post(predicate![y >= 2]).expect("domain not empty");
        let _ = state.post(predicate![z <= 3]).expect("domain not empty");
        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn empty_hypercube_simplifies_to_linear_conflict() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into()));
        let z = state.new_interval_variable(2, 5, Some("z".into()));

        let hypercube = Hypercube::new([]).expect("not inconsistent");
        // x + y + z <= 5.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            5,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn conflicting_linear_propagates_last_unassigned_hypercube_bound_to_false() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 5, Some("y".into()));
        let z = state.new_interval_variable(2, 10, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

        // y + z <= 3.
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
            3,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        assert_eq!(1, state.upper_bound(x));
    }

    #[test]
    fn propagate_weaker_than_unassigned_predicate_in_hypercube() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 5, Some("y".into()));
        let z = state.new_interval_variable(0, 10, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

        // x + y + z <= 5.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            5,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        assert_eq!(3, state.upper_bound(x));
    }
}
