use crate::basic_types::PredicateId;
use crate::declare_inference_label;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::InferenceCheckers;
use crate::propagation::LocalId;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::propagation::checkers::ConsistencyChecker;
#[allow(deprecated, reason = "TODO to implement for reified")]
use crate::propagation::checkers::DefaultChecker;
use crate::propagators::hypercube_linear::Hypercube;
use crate::propagators::hypercube_linear::HypercubeLinearChecker;
use crate::propagators::hypercube_linear::LinearInequality;
use crate::pumpkin_assert_simple;
use crate::results::PropagationStatusCP;
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

    fn add_inference_checkers(
        &self,
        mut checkers: InferenceCheckers<'_>,
    ) -> impl ConsistencyChecker + 'static {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, HypercubeLinear),
            Box::new(HypercubeLinearChecker {
                hypercube: self.hypercube.iter_predicates().collect(),
                terms: self.linear.terms().collect(),
                bound: self.linear.bound(),
            }),
        );

        #[allow(deprecated, reason = "TODO to implement for reified")]
        DefaultChecker
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
            [true_predicate_id; NUM_WATCHED_PREDICATES]
        } else {
            let last_idx = hypercube_predicates.len() - 1;
            [
                context.register_predicate(hypercube_predicates[0]),
                context.register_predicate(hypercube_predicates[1.min(last_idx)]),
            ]
        };

        HypercubeLinearPropagator {
            linear,

            hypercube_predicates,
            watched_predicates,
            inference_code: InferenceCode::new(constraint_tag, HypercubeLinear),
        }
    }
}

declare_inference_label!(HypercubeLinear);

const NUM_WATCHED_PREDICATES: usize = 2;

/// A [`Propagator`] for the hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct HypercubeLinearPropagator {
    linear: LinearInequality,

    hypercube_predicates: Box<[Predicate]>,
    /// The predicate ID at index i corresponds to the predicate at index i in
    /// `hypercube_predicates`.
    watched_predicates: [PredicateId; NUM_WATCHED_PREDICATES],

    inference_code: InferenceCode,
}

impl HypercubeLinearPropagator {
    /// Get the watcher index for the predicate that is unassigned.
    ///
    /// Assumes 0 or 1 watched predicates are/is unassigned.
    fn unassigned_watcher_index(&self, mut context: PropagationContext<'_>) -> Option<usize> {
        self.watched_predicates
            .iter()
            .position(|&pid| !context.is_predicate_id_satisfied(pid))
    }

    /// Propagates the linear inequality of the hypercube linear.
    ///
    /// Does _not_ check that the hypercube is satisfied.
    fn propagate_linear_inequality(
        &self,
        mut context: PropagationContext<'_>,
        slack: i64,
    ) -> PropagationStatusCP {
        if self.linear.is_trivially_false() {
            // In this case the terms iterator is empty so the loop-body below is never executed.
            // Therefore we explicitly check for this case, and trigger a conflict. If the linear
            // is not trivially false, the conflict check is unnecessary as the propagation will
            // also trigger a conflict.

            let conjunction = self
                .linear
                .terms()
                .map(|term| predicate![term >= context.lower_bound(&term)])
                .chain(self.hypercube_predicates.iter().copied())
                .collect::<PropositionalConjunction>();

            return Err(crate::state::Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code: self.inference_code.clone(),
            }));
        }

        for term in self.linear.terms() {
            let term_lower_bound = i64::from(context.lower_bound(&term));
            let term_upper_bound_i64 = slack + term_lower_bound;
            let term_upper_bound = match i32::try_from(term_upper_bound_i64) {
                Ok(bound) => bound,
                Err(_) if term_upper_bound_i64.is_negative() => {
                    todo!("wanting to tighten the upper bound to a value smaller than i32::MIN")
                }
                // If we want to set the upper bound to a value larger than i32::MAX,
                // it can never tighten the existing bound of `term_to_propagate`.
                Err(_) => return Ok(()),
            };

            let reason = self
                .linear
                .terms()
                .filter(|&t| t != term)
                .map(|term| predicate![term >= context.lower_bound(&term)])
                .chain(self.hypercube_predicates.iter().copied())
                .collect::<PropositionalConjunction>();

            context.post(
                predicate![term <= term_upper_bound],
                reason,
                &self.inference_code,
            )?;
        }

        Ok(())
    }

    /// Register the bound events on the integer variables in the linear inequality.
    fn register_bound_events_on_linear(&self, mut context: PropagationContext<'_>) {
        for (idx, term) in self.linear.terms().enumerate() {
            // The implementation of register_domain_event already handles duplicate registration,
            // so we do not need to check whether we are already registered.
            context.register_domain_event(
                term,
                DomainEvents::LOWER_BOUND,
                LocalId::from(idx as u32),
            );
        }
    }

    /// Stop being enqueued for the bound events on the terms in the linear inequality.
    fn unregister_bound_events_on_linear(&self, mut context: PropagationContext<'_>) {
        for (idx, term) in self.linear.terms().enumerate() {
            // The implementation of register_domain_event already handles duplicate registration,
            // so we do not need to check whether we are already registered.
            context.unregister_domain_event(term, LocalId::from(idx as u32));
        }
    }

    /// Updates the watched predicate at `watcher_index`.
    ///
    /// Returns true if a new unassigned predicate is now watched, or false if all other predicates
    /// are already true. If false is returned, the state of `self` is unaltered.
    fn find_new_watcher(
        &mut self,
        mut context: PropagationContext<'_>,
        watcher_index: usize,
    ) -> bool {
        let old_watcher = self.watched_predicates[watcher_index];

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

            context.unregister_predicate(old_watcher);
            let new_predicate_id = context.register_predicate(next_predicate_to_watch);

            self.hypercube_predicates
                .swap(watcher_index, predicate_index);
            self.watched_predicates[watcher_index] = new_predicate_id;

            true
        } else {
            false
        }
    }

    /// Update the watched predicates of the hypercube.
    ///
    /// Returns the number of satisfied watchers, having tried replacing them with unassigned
    /// predicates.
    fn update_watched_predicates(&mut self, mut context: PropagationContext<'_>) -> usize {
        let mut satisfied_watchers = 0;

        for watcher_index in 0..self.watched_predicates.len() {
            let watched_predicate = self.watched_predicates[watcher_index];

            if context.is_predicate_id_satisfied(watched_predicate) {
                satisfied_watchers +=
                    usize::from(!self.find_new_watcher(context.reborrow(), watcher_index));
            }
        }

        satisfied_watchers
    }
}

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        let satisfied_watchers = self.update_watched_predicates(context.reborrow());

        if satisfied_watchers < NUM_WATCHED_PREDICATES - 1 {
            self.unregister_bound_events_on_linear(context.reborrow());
            // More than one watcher is unassigned, so we do not need to propagate anything.
            return Ok(());
        } else if satisfied_watchers == NUM_WATCHED_PREDICATES {
            // The hypercube is satisfied, so we should be registered to bound events on the terms
            // of the linear inequality.
            self.register_bound_events_on_linear(context.reborrow());
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
                            self.hypercube_predicates
                                .iter()
                                .copied()
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
                            self.hypercube_predicates
                                .iter()
                                .copied()
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

                self.propagate_linear_inequality(context, slack)?;
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
                        self.hypercube_predicates
                            .iter()
                            .copied()
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
                    .chain(self.hypercube_predicates.iter().copied())
                    .collect::<PropositionalConjunction>();

                context.post(
                    predicate![term <= new_upper_bound],
                    reason,
                    &self.inference_code,
                )?;
            }
        } else {
            pumpkin_assert_simple!(unsatisfied_predicates_in_hypercubes.is_empty());
            self.propagate_linear_inequality(context, slack)?;
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

    #[test]
    fn linear_component_propagates_if_hypercube_is_satisfied() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z1 = state.new_interval_variable(0, 10, Some("z1".into()));
        let z2 = state.new_interval_variable(0, 10, Some("z2".into()));
        let z3 = state.new_interval_variable(0, 10, Some("z3".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

        // z1 + z2 + z3 <= 10.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), z1),
                (NonZero::new(1).unwrap(), z2),
                (NonZero::new(1).unwrap(), z3),
            ],
            10,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());

        assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());

        assert!(state.post(predicate![z1 >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(z2), 8);
        assert_eq!(state.upper_bound(z3), 8);
    }

    #[test]
    fn backtracking_does_not_break_the_propagator() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z1 = state.new_interval_variable(0, 10, Some("z1".into()));
        let z2 = state.new_interval_variable(0, 10, Some("z2".into()));
        let z3 = state.new_interval_variable(0, 10, Some("z3".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

        // z1 + z2 + z3 <= 10.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), z1),
                (NonZero::new(1).unwrap(), z2),
                (NonZero::new(1).unwrap(), z3),
            ],
            10,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        state.new_checkpoint();

        assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        assert!(state.post(predicate![z1 >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());

        let _ = state.restore_to(0);

        assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
        assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
        assert!(state.post(predicate![z1 >= 4]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());

        assert_eq!(state.upper_bound(z2), 6);
        assert_eq!(state.upper_bound(z3), 6);
    }
}
