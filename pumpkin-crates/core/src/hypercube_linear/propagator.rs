use std::cmp::Reverse;

use crate::basic_types::PredicateId;
use crate::declare_inference_label;
use crate::engine::PropagationStatusCP;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearChecker;
use crate::hypercube_linear::HypercubeLinearPropagation;
#[cfg(doc)]
use crate::hypercube_linear::HypercubeLinearStore;
use crate::hypercube_linear::LinearInequality;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::EventsToRegister;
use crate::propagation::LocalId;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorSpec;
use crate::propagation::ReadDomains;
use crate::propagation::RuntimeCheckers;
use crate::pumpkin_assert_simple;
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

    fn create(
        self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        } = self;

        let mut hypercube_predicates = hypercube.iter_predicates().collect::<Box<[_]>>();

        // Make sure the predicates with highest decision level are at the start. If
        // predicates are not assigned, we consider them at the highest decision level.
        hypercube_predicates.sort_unstable_by_key(|&p| {
            Reverse(
                context
                    .domains()
                    .get_checkpoint_for_predicate(p)
                    .unwrap_or(usize::MAX),
            )
        });

        #[allow(clippy::get_first, reason = "is more consistent")]
        let watched_predicates = [
            context.register_predicate(
                hypercube_predicates
                    .get(0)
                    .copied()
                    .unwrap_or_else(Predicate::trivially_true),
            ),
            context.register_predicate(
                hypercube_predicates
                    .get(1)
                    .copied()
                    .unwrap_or_else(Predicate::trivially_true),
            ),
        ];

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            constraint_tag,
            HypercubeLinear,
            HypercubeLinearChecker {
                hypercube: hypercube.iter_predicates().collect(),
                terms: linear.terms().collect(),
                bound: linear.bound(),
            },
        );

        let propagator = HypercubeLinearPropagator {
            hypercube,
            linear,

            hypercube_predicates,
            watched_predicates,
            is_watching_linear: false,
            propagation: context.hypercube_linear_propagation(),
            member_index: 0,

            inference_code,
        };

        // TODO: This will be expanded with registration of predicates.
        let registration = EventsToRegister::empty();

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator,
        }
    }
}

declare_inference_label!(HypercubeLinear);

pub(crate) const NUM_WATCHED_PREDICATES: usize = 2;

/// The extended propagation removes at most this many values from the interior of a domain at
/// once. Larger intervals are not removed, which only weakens the propagation.
const MAX_INTERIOR_REMOVALS: i64 = 1000;

/// A [`Propagator`] for the hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct HypercubeLinearPropagator {
    hypercube: Hypercube,
    linear: LinearInequality,

    hypercube_predicates: Box<[Predicate]>,
    /// The predicate ID at index i corresponds to the predicate at index i in
    /// `hypercube_predicates`.
    watched_predicates: [PredicateId; NUM_WATCHED_PREDICATES],

    /// True when we are watching the linear inequality.
    is_watching_linear: bool,

    /// How the hypercube is propagated.
    propagation: HypercubeLinearPropagation,

    /// The index of this constraint in the [`HypercubeLinearStore`] that holds it, or 0 if it is
    /// a propagator on its own. It is the code of the lazy explanations and the local id of the
    /// domain events, so that the store can tell its constraints apart.
    member_index: u32,

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

    /// The code with which propagations are explained lazily.
    fn lazy_code(&self) -> u64 {
        u64::from(self.member_index)
    }

    /// Sets the index of this constraint in the [`HypercubeLinearStore`] that holds it.
    pub(crate) fn set_member_index(&mut self, member_index: u32) {
        self.member_index = member_index;
    }

    /// The predicates that are watched in the hypercube.
    pub(crate) fn watched_predicate_ids(&self) -> [PredicateId; NUM_WATCHED_PREDICATES] {
        self.watched_predicates
    }

    /// The hypercube linear slack: the bound minus, for every term, the larger of its lower bound
    /// in the state and in the hypercube.
    fn hypercube_linear_slack(&self, context: &PropagationContext<'_>) -> i64 {
        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| {
                let bound_in_state = context.lower_bound(&term);
                let bound_in_hypercube = self.hypercube.lower_bound(&term);

                i64::from(i32::max(bound_in_state, bound_in_hypercube))
            })
            .sum::<i64>();

        i64::from(self.linear.bound()) - lower_bound_terms
    }

    /// Propagates when all predicates of the hypercube except `predicate_in_hypercube` are true.
    fn propagate_single_unsatisfied_predicate(
        &self,
        mut context: PropagationContext<'_>,
        predicate_in_hypercube: Predicate,
        slack: i64,
    ) -> PropagationStatusCP {
        let maybe_term = self
            .linear
            .term_for_domain(predicate_in_hypercube.get_domain());

        if slack < 0 {
            // Since the hypercube linear slack is negative, the constraint is violated if the
            // predicate becomes true, so it is propagated to false.
            context.post(!predicate_in_hypercube, self.lazy_code())?;
        } else if let Some(term_to_propagate) = maybe_term {
            // The slack is at least 0, but it may be that the linear could propagate
            // something weaker than `!predicate_in_hypercube`.

            if !could_propagate_weaker_predicate(predicate_in_hypercube, term_to_propagate) {
                return Ok(());
            }

            let bound_in_state = context.lower_bound(&term_to_propagate);
            let bound_in_hypercube = self.hypercube.lower_bound(&term_to_propagate);
            let bound_i64 = slack + i64::from(i32::max(bound_in_state, bound_in_hypercube));

            // The slack is at least 0 and both bounds are at least i32::MIN, so the bound
            // can only be out of range by exceeding i32::MAX. Then it can never tighten
            // the existing bound of `term_to_propagate`.
            let Ok(bound) = i32::try_from(bound_i64) else {
                pumpkin_assert_simple!(bound_i64 > i64::from(i32::MAX));
                return Ok(());
            };

            context.post(predicate![term_to_propagate <= bound], self.lazy_code())?;
        }

        Ok(())
    }

    /// The propagation for [`HypercubeLinearPropagation::Extended`].
    ///
    /// The watchers are kept on predicates that are not true and, where possible, concern
    /// different domains. Propagation happens when the predicates that are not true all concern
    /// one domain.
    fn propagate_extended(&mut self, mut context: PropagationContext<'_>) -> PropagationStatusCP {
        let _ = self.update_watched_predicates(context.reborrow());

        let mut unsatisfied = vec![];
        for (index, &predicate) in self.hypercube_predicates.iter().enumerate() {
            match context.evaluate_predicate(predicate) {
                // A false predicate satisfies the constraint.
                Some(false) => return Ok(()),
                Some(true) => {}
                None => unsatisfied.push(index),
            }
        }

        if let Some(&first) = unsatisfied.first() {
            let domain = self.hypercube_predicates[first].get_domain();
            let other_domain = unsatisfied
                .iter()
                .copied()
                .find(|&index| self.hypercube_predicates[index].get_domain() != domain);

            if let Some(second) = other_domain {
                // Two domains are unassigned, so nothing can be propagated. The watchers are set
                // to predicates over these two domains.
                self.watch(context.reborrow(), first, second);

                if self.is_watching_linear {
                    self.unregister_bound_events_on_linear(context.reborrow());
                    self.is_watching_linear = false;
                }

                return Ok(());
            }
        }

        if !self.is_watching_linear {
            self.register_bound_events_on_linear(context.reborrow());
            self.is_watching_linear = true;
        }

        let slack = self.hypercube_linear_slack(&context);

        match unsatisfied.as_slice() {
            [] => self.propagate_linear_inequality(context, slack),
            &[index] => {
                // The standard propagation explains its propagations lazily with the hypercube
                // linear itself, which conflict analysis can use. It is followed by the extended
                // propagation, which can additionally remove values from the interior of the
                // domain.
                let predicate = self.hypercube_predicates[index];
                self.propagate_single_unsatisfied_predicate(context.reborrow(), predicate, slack)?;

                if context.evaluate_predicate(predicate).is_none() {
                    self.propagate_single_unsatisfied_domain(context, &[predicate])?;
                }

                Ok(())
            }
            indices => {
                let predicates = indices
                    .iter()
                    .map(|&index| self.hypercube_predicates[index])
                    .collect::<Vec<_>>();
                self.propagate_single_unsatisfied_domain(context, &predicates)
            }
        }
    }

    /// Makes the predicates at `first` and `second` in the hypercube the watched predicates.
    fn watch(&mut self, mut context: PropagationContext<'_>, first: usize, second: usize) {
        pumpkin_assert_simple!(first != second);

        // Move the predicates to the watched positions, keeping track of where the second one ends
        // up if the first swap moves it.
        self.hypercube_predicates.swap(0, first);
        let second = if second == 0 { first } else { second };
        self.hypercube_predicates.swap(1, second);

        for watcher_index in 0..NUM_WATCHED_PREDICATES {
            let predicate = self.hypercube_predicates[watcher_index];
            let old_watcher = self.watched_predicates[watcher_index];

            if context.get_predicate(old_watcher) != predicate {
                context.unregister_predicate(old_watcher);
                self.watched_predicates[watcher_index] = context.register_predicate(predicate);
            }
        }
    }

    /// Propagates when all predicates of the hypercube except `unsatisfied` are true, and the
    /// predicates in `unsatisfied` all concern one domain `x`.
    ///
    /// Let `R` be the set of values of `x` for which all of `unsatisfied` hold, and let `S` be the
    /// bound of the linear minus the lower bounds of the terms other than the one of `x`. The
    /// constraint forbids exactly the values `v` in `R` with `w * v > S`, where `w` is the weight
    /// of `x` in the linear (0 if `x` does not appear in it). These values form an interval of `R`,
    /// which is removed from the domain of `x`.
    fn propagate_single_unsatisfied_domain(
        &self,
        mut context: PropagationContext<'_>,
        unsatisfied: &[Predicate],
    ) -> PropagationStatusCP {
        let domain = unsatisfied[0].get_domain();
        pumpkin_assert_simple!(unsatisfied.iter().all(|p| p.get_domain() == domain));

        let domain_lower_bound = context.lower_bound(&domain);
        let domain_upper_bound = context.upper_bound(&domain);

        // The interval [region_lower, region_upper] and the exceptions describe `R`.
        let mut region_lower = domain_lower_bound;
        let mut region_upper = domain_upper_bound;
        let mut exceptions = vec![];
        for predicate in unsatisfied {
            let value = predicate.get_right_hand_side();
            match predicate.get_predicate_type() {
                PredicateType::LowerBound => region_lower = region_lower.max(value),
                PredicateType::UpperBound => region_upper = region_upper.min(value),
                PredicateType::Equal => {
                    region_lower = region_lower.max(value);
                    region_upper = region_upper.min(value);
                }
                PredicateType::NotEqual => exceptions.push(value),
            }
        }

        let term = self.linear.term_for_domain(domain);
        pumpkin_assert_simple!(term.is_none_or(|term| term.offset == 0));

        let other_terms_lower_bound = self
            .linear
            .terms()
            .filter(|t| t.inner != domain)
            .map(|t| i64::from(context.lower_bound(&t)))
            .sum::<i64>();
        let rest = i64::from(self.linear.bound()) - other_terms_lower_bound;

        // The forbidden values of `R` are those `v` with `w * v > rest`.
        let (forbidden_lower, forbidden_upper) = match term.map(|term| term.scale) {
            None if rest < 0 => (i64::from(region_lower), i64::from(region_upper)),
            None => return Ok(()),
            Some(weight) if weight > 0 => {
                let weight = i64::from(weight);
                (
                    i64::from(region_lower).max(rest.div_euclid(weight) + 1),
                    i64::from(region_upper),
                )
            }
            Some(weight) => {
                // `w * v > rest` with `w < 0` is `v < rest / w`, i.e. `v <= ceil(rest / w) - 1`.
                let weight = i64::from(weight);
                let ceil = -(rest.div_euclid(-weight));
                (
                    i64::from(region_lower),
                    i64::from(region_upper).min(ceil - 1),
                )
            }
        };

        if forbidden_lower > forbidden_upper {
            return Ok(());
        }

        // Both are within the bounds of the domain, so they fit in an i32.
        let forbidden_lower = forbidden_lower as i32;
        let forbidden_upper = forbidden_upper as i32;

        // The reason consists of the true predicates of the hypercube and the lower bounds of the
        // other terms. A bound propagation additionally uses the bound of `x` that it moves.
        let reason = || {
            self.hypercube_predicates
                .iter()
                .copied()
                .filter(|p| !unsatisfied.contains(p))
                .chain(
                    self.linear
                        .terms()
                        .filter(|t| t.inner != domain)
                        .map(|t| predicate![t >= context.lower_bound(&t)]),
                )
                .collect::<Vec<_>>()
        };
        let base_reason = reason();

        let mut new_lower_bound = domain_lower_bound;
        let mut new_upper_bound = domain_upper_bound;

        if forbidden_lower <= domain_lower_bound {
            // The lower part of the domain is forbidden, up to the first exception.
            new_lower_bound = exceptions
                .iter()
                .copied()
                .filter(|&e| e >= domain_lower_bound && e <= forbidden_upper)
                .min()
                .unwrap_or(forbidden_upper + 1);

            let mut reason = base_reason.clone();
            reason.push(predicate![domain >= domain_lower_bound]);
            context.post(
                predicate![domain >= new_lower_bound],
                (PropositionalConjunction::from(reason), &self.inference_code),
            )?;
        }

        if forbidden_upper >= domain_upper_bound {
            // The upper part of the domain is forbidden, down to the last exception.
            new_upper_bound = exceptions
                .iter()
                .copied()
                .filter(|&e| e <= domain_upper_bound && e >= forbidden_lower)
                .max()
                .unwrap_or(forbidden_lower - 1);

            let mut reason = base_reason.clone();
            reason.push(predicate![domain <= domain_upper_bound]);
            context.post(
                predicate![domain <= new_upper_bound],
                (PropositionalConjunction::from(reason), &self.inference_code),
            )?;
        }

        // The remaining forbidden values lie strictly inside the domain and are removed one by
        // one. Removing a large interval value by value is expensive, so it is skipped then.
        let interior_lower = forbidden_lower.max(new_lower_bound);
        let interior_upper = forbidden_upper.min(new_upper_bound);
        if i64::from(interior_upper) - i64::from(interior_lower) < MAX_INTERIOR_REMOVALS {
            for value in interior_lower..=interior_upper {
                if !exceptions.contains(&value) && context.contains(&domain, value) {
                    context.post(
                        predicate![domain != value],
                        (
                            PropositionalConjunction::from(base_reason.clone()),
                            &self.inference_code,
                        ),
                    )?;
                }
            }
        }

        Ok(())
    }

    /// The conflict when the hypercube is satisfied and the lower bounds of the terms violate the
    /// linear inequality.
    fn linear_conflict(&self, context: &PropagationContext<'_>) -> Conflict {
        let conjunction = self
            .linear
            .terms()
            .map(|term| predicate![term >= context.lower_bound(&term)])
            .chain(self.hypercube_predicates.iter().copied())
            .collect::<PropositionalConjunction>();

        Conflict::Propagator(PropagatorConflict {
            conjunction,
            inference_code: self.inference_code.clone(),
        })
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
            return Err(self.linear_conflict(&context));
        }

        for term in self.linear.terms() {
            let term_lower_bound = i64::from(context.lower_bound(&term));
            let term_upper_bound_i64 = slack + term_lower_bound;
            let term_upper_bound = match i32::try_from(term_upper_bound_i64) {
                Ok(bound) => bound,
                // The upper bound is smaller than i32::MIN, and therefore smaller than the lower
                // bound of the term. So the lower bounds of the terms violate the linear
                // inequality.
                Err(_) if term_upper_bound_i64.is_negative() => {
                    return Err(self.linear_conflict(&context));
                }
                // If we want to set the upper bound to a value larger than i32::MAX, it can never
                // tighten the existing bound of this term. The other terms may still propagate.
                Err(_) => continue,
            };

            context.post(predicate![term <= term_upper_bound], self.lazy_code())?;
        }

        Ok(())
    }

    /// Register the bound events on the integer variables in the linear inequality.
    fn register_bound_events_on_linear(&self, mut context: PropagationContext<'_>) {
        for term in self.linear.terms() {
            // The implementation of register_domain_event already handles duplicate registration,
            // so we do not need to check whether we are already registered.
            context.register_domain_event(
                term,
                DomainEvents::LOWER_BOUND,
                LocalId::from(self.member_index),
            );
        }
    }

    /// Stop being enqueued for the bound events on the terms in the linear inequality.
    fn unregister_bound_events_on_linear(&self, mut context: PropagationContext<'_>) {
        for term in self.linear.terms() {
            // The implementation of register_domain_event already handles duplicate registration,
            // so we do not need to check whether we are already registered.
            context.unregister_domain_event(term, LocalId::from(self.member_index));
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

    fn explain_as_hypercube_linear(
        &mut self,
        _code: u64,
        _context: crate::propagation::ExplanationContext,
    ) -> Option<(Hypercube, LinearInequality, InferenceCode)> {
        Some((
            self.hypercube.clone(),
            self.linear.clone(),
            self.inference_code.clone(),
        ))
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if self.propagation == HypercubeLinearPropagation::Extended {
            return self.propagate_extended(context);
        }

        let satisfied_watchers = self.update_watched_predicates(context.reborrow());

        if satisfied_watchers < NUM_WATCHED_PREDICATES - 1 {
            if self.is_watching_linear {
                self.unregister_bound_events_on_linear(context.reborrow());
                self.is_watching_linear = false;
            }

            // More than one watcher is unassigned, so we do not need to propagate anything.
            return Ok(());
        } else {
            // The hypercube is satisfied, so we should be registered to bound events on the terms
            // of the linear inequality.
            if !self.is_watching_linear {
                self.register_bound_events_on_linear(context.reborrow());
                self.is_watching_linear = true;
            }
        }

        let unassigned_watcher_index = self.unassigned_watcher_index(context.reborrow());
        let slack = self.hypercube_linear_slack(&context);

        match unassigned_watcher_index {
            Some(index) => {
                let predicate_in_hypercube = self.hypercube_predicates[index];
                self.propagate_single_unsatisfied_predicate(context, predicate_in_hypercube, slack)
            }

            // All watchers are true. Propagate the linear inequality.
            None => self.propagate_linear_inequality(context, slack),
        }
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
            .map(|term| {
                let bound_in_state = context.lower_bound(&term);
                let bound_in_hypercube = self.hypercube.lower_bound(&term);

                i64::from(i32::max(bound_in_state, bound_in_hypercube))
            })
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

                context.post(!unassigned_predicate, (reason, &self.inference_code))?;
            } else if let Some(term) = self
                .linear
                .term_for_domain(unassigned_predicate.get_domain())
            {
                // As in the incremental propagation, the weaker bound only holds if the
                // unassigned predicate bounds the term from below; otherwise the negation of the
                // predicate does not bound the term from above.
                if !could_propagate_weaker_predicate(unassigned_predicate, term) {
                    return Ok(());
                }

                let bound_in_state = context.lower_bound(&term);
                let bound_in_hypercube = self.hypercube.lower_bound(&term);
                let bound_i64 = slack + i64::from(i32::max(bound_in_state, bound_in_hypercube));
                let Ok(new_upper_bound) = i32::try_from(bound_i64) else {
                    pumpkin_assert_simple!(bound_i64 > i64::from(i32::MAX));
                    return Ok(());
                };

                // The bound holds whether or not the unassigned predicate becomes true, so the
                // reason consists of the other predicates of the hypercube.
                let reason = self
                    .linear
                    .terms()
                    .filter(|&t| t != term)
                    .map(|term| predicate![term >= context.lower_bound(&term)])
                    .chain(
                        self.hypercube_predicates
                            .iter()
                            .copied()
                            .filter(|&p| p != unassigned_predicate),
                    )
                    .collect::<PropositionalConjunction>();

                context.post(
                    predicate![term <= new_upper_bound],
                    (reason, &self.inference_code),
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

    #[test]
    fn single_predicate_in_hypercube_with_trivially_false_linear_triggers() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));

        let hypercube = Hypercube::new([predicate![x >= 2]]).expect("not inconsistent");

        let linear = LinearInequality::trivially_false();

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(x), 1);
    }

    #[test]
    fn slack_should_be_hypercube_linear_slack() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));

        let hypercube = Hypercube::new([predicate![x >= 4]]).expect("not inconsistent");

        let linear =
            LinearInequality::new([(NonZero::new(1).unwrap(), x)], 3).expect("not trivially false");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(x), 3);
    }

    #[test]
    fn hypercube_is_taken_into_slack_calculation() {
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
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.lower_bound(z), -1);
    }

    #[test]
    fn upper_bound_below_i32_min_is_a_conflict() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(5, 10, None);

        // x + y <= i32::MIN, so the upper bound for x would be i32::MIN - 5.
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            i32::MIN,
        )
        .expect("not trivially satisfiable");
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube: Hypercube::default(),
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn upper_bound_above_i32_max_does_not_stop_propagation_of_other_terms() {
        let mut state = State::default();

        let x = state.new_interval_variable(2_147_483_000, 2_147_483_600, None);
        let y = state.new_interval_variable(0, 2000, None);
        let z = state.new_interval_variable(-2_147_483_000, 0, None);

        // x + y + z <= 1000 has slack 1000. The upper bound for x would be larger than
        // i32::MAX, while y can be tightened to at most 1000.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            1000,
        )
        .expect("not trivially satisfiable");
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube: Hypercube::default(),
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(y), 1000);
    }

    #[test]
    fn no_weaker_propagation_for_an_unassigned_upper_bound_on_a_positive_term() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(2, 10, None);

        // [x <= 5] /\ [y >= 2] -> x + y <= 4 has slack 2. If [x <= 5] becomes false, x can be
        // up to 10, so no upper bound on x follows.
        let hypercube =
            Hypercube::new([predicate![x <= 5], predicate![y >= 2]]).expect("not inconsistent");
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            4,
        )
        .expect("not trivially satisfiable");
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(x), 10);
    }

    #[test]
    fn explanation_of_falsified_hypercube_predicate_includes_bound_of_its_variable() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(2, 10, None);

        // [x <= 5] /\ [y >= 2] -> x + y <= 4. Once x >= 3, the slack is 4 - 3 - 2 = -1, so
        // [x <= 5] is propagated to false. The lower bound of x contributes to that slack.
        let hypercube =
            Hypercube::new([predicate![x <= 5], predicate![y >= 2]]).expect("not inconsistent");
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            4,
        )
        .expect("not trivially satisfiable");
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });
        assert!(state.propagate_to_fixed_point().is_ok());

        state.new_checkpoint();
        let _ = state.post(predicate![x >= 3]).expect("domain not empty");
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.lower_bound(x), 6);

        let mut reason = vec![];
        let _ = state.get_propagation_reason(
            predicate![x >= 6],
            &mut reason,
            crate::state::CurrentNogood::empty(),
        );
        reason.sort();
        // A bound can appear both in the hypercube and as the bound of its term.
        reason.dedup();

        let mut expected = vec![predicate![x >= 3], predicate![y >= 2]];
        expected.sort();
        assert_eq!(reason, expected);
    }

    fn extended_state() -> State {
        let mut state = State::default();
        state.hypercube_linear_propagation = HypercubeLinearPropagation::Extended;
        state
    }

    fn add_hypercube_linear(state: &mut State, hypercube: Hypercube, linear: LinearInequality) {
        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });
    }

    #[test]
    fn extended_propagation_removes_region_of_single_unassigned_domain() {
        let mut state = extended_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(2, 10, None);

        // [x >= 3] /\ [x <= 7] /\ [y >= 2] -> false, where [y >= 2] is true.
        let hypercube =
            Hypercube::new([predicate![x >= 3], predicate![x <= 7], predicate![y >= 2]])
                .expect("not inconsistent");
        add_hypercube_linear(&mut state, hypercube, LinearInequality::trivially_false());

        assert!(state.propagate_to_fixed_point().is_ok());
        assert!((3..=7).all(|value| !state.contains(x, value)));
        assert!(state.contains(x, 2));
        assert!(state.contains(x, 8));
    }

    #[test]
    fn extended_propagation_removes_values_violating_the_linear() {
        let mut state = extended_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(2, 10, None);

        // [x <= 5] /\ [y >= 2] -> x + y <= 4 forbids x in {3, 4, 5}: those values satisfy the
        // hypercube, but then x + y >= x + 2 > 4.
        let hypercube =
            Hypercube::new([predicate![x <= 5], predicate![y >= 2]]).expect("not inconsistent");
        let linear = LinearInequality::new(
            [(NonZero::new(1).unwrap(), x), (NonZero::new(1).unwrap(), y)],
            4,
        )
        .expect("not trivially satisfiable");
        add_hypercube_linear(&mut state, hypercube, linear);

        assert!(state.propagate_to_fixed_point().is_ok());
        assert!((3..=5).all(|value| !state.contains(x, value)));
        assert!((0..=2).all(|value| state.contains(x, value)));
        assert!((6..=10).all(|value| state.contains(x, value)));
    }

    #[test]
    fn extended_propagation_with_a_negative_weight() {
        let mut state = extended_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(2, 10, None);

        // [x >= 3] /\ [y >= 2] -> y - x <= -3 forbids x in {3, 4}: then y - x >= 2 - 4 > -3.
        let hypercube =
            Hypercube::new([predicate![x >= 3], predicate![y >= 2]]).expect("not inconsistent");
        let linear = LinearInequality::new(
            [
                (NonZero::new(-1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
            ],
            -3,
        )
        .expect("not trivially satisfiable");
        add_hypercube_linear(&mut state, hypercube, linear);

        assert!(state.propagate_to_fixed_point().is_ok());
        assert!(!state.contains(x, 3));
        assert!(!state.contains(x, 4));
        assert!(state.contains(x, 2));
        assert!(state.contains(x, 5));
    }

    #[test]
    fn extended_propagation_raises_the_lower_bound() {
        let mut state = extended_state();

        let x = state.new_interval_variable(3, 10, None);

        // [x >= 3] /\ [x <= 7] -> false with x >= 3 forbids the lower part of the domain.
        let hypercube =
            Hypercube::new([predicate![x >= 3], predicate![x <= 7]]).expect("not inconsistent");
        add_hypercube_linear(&mut state, hypercube, LinearInequality::trivially_false());

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.lower_bound(x), 8);
    }

    #[test]
    fn standard_propagation_does_not_propagate_two_unassigned_predicates() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, None);

        let hypercube =
            Hypercube::new([predicate![x >= 3], predicate![x <= 7]]).expect("not inconsistent");
        add_hypercube_linear(&mut state, hypercube, LinearInequality::trivially_false());

        assert!(state.propagate_to_fixed_point().is_ok());
        assert!((0..=10).all(|value| state.contains(x, value)));
    }
}
