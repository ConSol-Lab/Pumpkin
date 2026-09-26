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
use crate::hypercube_linear::linear::TermUpperBound;
use crate::hypercube_linear::linear::term_upper_bound;
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

    /// The larger of the lower bounds of `term` in the domains and in the hypercube.
    fn hypercube_linear_term_lower_bound(
        &self,
        domains: &impl ReadDomains,
        term: AffineView<DomainId>,
    ) -> i64 {
        let bound_in_state = term_lower_bound(domains, term);
        self.hypercube
            .term_lower_bound(term)
            .map_or(bound_in_state, |bound_in_hypercube| {
                bound_in_hypercube.max(bound_in_state)
            })
    }

    /// The hypercube linear slack: the bound minus, for every term, the larger of its lower bound
    /// in the state and in the hypercube.
    fn hypercube_linear_slack(&self, context: &PropagationContext<'_>) -> i64 {
        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| self.hypercube_linear_term_lower_bound(context, term))
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

            let bound = slack + self.hypercube_linear_term_lower_bound(&context, term_to_propagate);

            // The slack is at least 0, so the bound is at least the lower bound of the term and
            // the propagation is never infeasible.
            if let TermUpperBound::Predicate(predicate) = term_upper_bound(term_to_propagate, bound)
            {
                context.post(predicate, self.lazy_code())?;
            }
        }

        Ok(())
    }

    /// The propagation for [`HypercubeLinearPropagation::Extended`].
    ///
    /// The watchers are kept on predicates that are not true and, where possible, concern
    /// different domains. Propagation happens when the predicates that are not true all concern
    /// one domain.
    fn propagate_extended(&mut self, mut context: PropagationContext<'_>) -> PropagationStatusCP {
        let mut unsatisfied = vec![];
        for (index, &predicate) in self.hypercube_predicates.iter().enumerate() {
            match context.evaluate_predicate(predicate) {
                // A false predicate satisfies the constraint.
                Some(false) => return Ok(()),
                Some(true) => {}
                None => unsatisfied.push(index),
            }
        }

        // Updating the watchers reorders the predicates of the hypercube.
        let unsatisfied_predicates = unsatisfied
            .iter()
            .map(|&index| self.hypercube_predicates[index])
            .collect::<Vec<_>>();
        let spans_two_domains = self.update_extended_watchers(context.reborrow(), &unsatisfied);
        if spans_two_domains {
            // Two domains are unassigned, so nothing can be propagated.
            if self.is_watching_linear {
                self.unregister_bound_events_on_linear(context.reborrow());
                self.is_watching_linear = false;
            }

            return Ok(());
        }

        if !self.is_watching_linear {
            self.register_bound_events_on_linear(context.reborrow());
            self.is_watching_linear = true;
        }

        let slack = self.hypercube_linear_slack(&context);

        match unsatisfied_predicates.as_slice() {
            [] => self.propagate_linear_inequality(context, slack),
            &[predicate] => {
                // The standard propagation explains its propagations lazily with the hypercube
                // linear itself, which conflict analysis can use. It is followed by the extended
                // propagation, which can additionally remove values from the interior of the
                // domain.
                self.propagate_single_unsatisfied_predicate(context.reborrow(), predicate, slack)?;

                if context.evaluate_predicate(predicate).is_none() {
                    self.propagate_single_unsatisfied_domain(context, &[predicate])?;
                }

                Ok(())
            }
            predicates => self.propagate_single_unsatisfied_domain(context, predicates),
        }
    }

    /// Updates the watched predicates for the extended propagation, where `unsatisfied` are the
    /// indices of the predicates of the hypercube that are not true. Returns true if these concern
    /// at least two domains.
    ///
    /// If the predicates that are not true concern two domains, two of them over different
    /// domains are watched, so that the constraint is notified before they concern one domain.
    /// Otherwise, one watcher is on a predicate that is not true, if any, and the other on the
    /// predicate over another domain that became true last. Backtracking unassigns that predicate
    /// before the other predicates over other domains, so the watchers again concern two domains
    /// as soon as the predicates that are not true do.
    fn update_extended_watchers(
        &mut self,
        context: PropagationContext<'_>,
        unsatisfied: &[usize],
    ) -> bool {
        let domain_of = |index: usize| self.hypercube_predicates[index].get_domain();

        let first = unsatisfied.first().copied();
        let other_domain = first.and_then(|first| {
            unsatisfied
                .iter()
                .copied()
                .find(|&index| domain_of(index) != domain_of(first))
        });

        if self.hypercube_predicates.len() < NUM_WATCHED_PREDICATES {
            return other_domain.is_some();
        }

        if let (Some(first), Some(second)) = (first, other_domain) {
            self.watch(context, first, second);
            return true;
        }

        // The predicate that became true last among those that satisfy `condition`.
        let last_true = |condition: &dyn Fn(usize) -> bool| {
            (0..self.hypercube_predicates.len())
                .filter(|&index| !unsatisfied.contains(&index) && condition(index))
                .max_by_key(|&index| {
                    context
                        .assignments
                        .get_trail_position(&self.hypercube_predicates[index])
                })
        };

        let first = first
            .or_else(|| last_true(&|_| true))
            .expect("the hypercube has at least two predicates");
        let second = last_true(&|index| domain_of(index) != domain_of(first))
            .or_else(|| (0..self.hypercube_predicates.len()).find(|&index| index != first))
            .expect("the hypercube has at least two predicates");

        self.watch(context, first, second);
        false
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

        let term = self.linear.term_for_domain(domain);
        pumpkin_assert_simple!(term.is_none_or(|term| term.offset == 0));

        let other_terms_lower_bound = self
            .linear
            .terms()
            .filter(|t| t.inner != domain)
            .map(|t| term_lower_bound(&context, t))
            .sum::<i64>();
        let rest = i64::from(self.linear.bound()) - other_terms_lower_bound;

        let Some(inferences) = extended_inferences(
            unsatisfied,
            domain_lower_bound,
            domain_upper_bound,
            |value| context.contains(&domain, value),
            term.map(|term| term.scale),
            rest,
        ) else {
            return Ok(());
        };

        // The reason consists of the true predicates of the hypercube and the lower bounds of the
        // other terms. A bound propagation additionally uses the bound of `x` that it moves.
        let base_reason = self
            .hypercube_predicates
            .iter()
            .copied()
            .filter(|p| !unsatisfied.contains(p))
            .chain(
                self.linear
                    .terms()
                    .filter(|t| t.inner != domain)
                    .map(|t| term_lower_bound_predicate(&context, t)),
            )
            .collect::<Vec<_>>();

        if inferences.lower_bound > domain_lower_bound {
            let mut reason = base_reason.clone();
            reason.push(predicate![domain >= domain_lower_bound]);
            context.post(
                predicate![domain >= inferences.lower_bound],
                (PropositionalConjunction::from(reason), &self.inference_code),
            )?;
        }

        if inferences.upper_bound < domain_upper_bound {
            let mut reason = base_reason.clone();
            reason.push(predicate![domain <= domain_upper_bound]);
            context.post(
                predicate![domain <= inferences.upper_bound],
                (PropositionalConjunction::from(reason), &self.inference_code),
            )?;
        }

        for value in inferences.removed_values {
            context.post(
                predicate![domain != value],
                (
                    PropositionalConjunction::from(base_reason.clone()),
                    &self.inference_code,
                ),
            )?;
        }

        Ok(())
    }

    /// The conflict when the hypercube is satisfied and the lower bounds of the terms violate the
    /// linear inequality.
    fn linear_conflict(&self, context: &PropagationContext<'_>) -> Conflict {
        let conjunction = self
            .linear
            .terms()
            .map(|term| term_lower_bound_predicate(context, term))
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
            let upper_bound = slack + term_lower_bound(&context, term);

            match term_upper_bound(term, upper_bound) {
                TermUpperBound::Predicate(predicate) => {
                    context.post(predicate, self.lazy_code())?;
                }
                // The bound does not restrict the domain; the other terms may still propagate.
                TermUpperBound::AlwaysTrue => continue,
                // No value of the domain satisfies the bound, so the lower bounds of the terms
                // violate the linear inequality.
                TermUpperBound::Infeasible => return Err(self.linear_conflict(&context)),
            }
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

    /// Propagation from scratch for [`HypercubeLinearPropagation::Standard`].
    fn propagate_from_scratch_standard(
        &self,
        mut context: PropagationContext<'_>,
    ) -> PropagationStatusCP {
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
            .map(|term| self.hypercube_linear_term_lower_bound(&context, term))
            .sum::<i64>();

        let slack = i64::from(self.linear.bound()) - lower_bound_terms;

        if unsatisfied_predicates_in_hypercubes.len() == 1 {
            let unassigned_predicate = unsatisfied_predicates_in_hypercubes[0];

            if slack < 0 {
                let reason = self
                    .linear
                    .terms()
                    .map(|term| term_lower_bound_predicate(&context, term))
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

                let bound = slack + self.hypercube_linear_term_lower_bound(&context, term);
                let TermUpperBound::Predicate(new_upper_bound) = term_upper_bound(term, bound)
                else {
                    return Ok(());
                };

                // The bound holds whether or not the unassigned predicate becomes true, so the
                // reason consists of the other predicates of the hypercube.
                let reason = self
                    .linear
                    .terms()
                    .filter(|&t| t != term)
                    .map(|term| term_lower_bound_predicate(&context, term))
                    .chain(
                        self.hypercube_predicates
                            .iter()
                            .copied()
                            .filter(|&p| p != unassigned_predicate),
                    )
                    .collect::<PropositionalConjunction>();

                context.post(new_upper_bound, (reason, &self.inference_code))?;
            }
        } else {
            pumpkin_assert_simple!(unsatisfied_predicates_in_hypercubes.is_empty());
            self.propagate_linear_inequality(context, slack)?;
        }

        Ok(())
    }

    /// Propagation from scratch for [`HypercubeLinearPropagation::Extended`]: the standard
    /// propagation, followed by the extended propagation when the predicates of the hypercube that
    /// are not true all concern one domain.
    fn propagate_from_scratch_extended(
        &self,
        mut context: PropagationContext<'_>,
    ) -> PropagationStatusCP {
        let mut unsatisfied = vec![];
        for &predicate in self.hypercube_predicates.iter() {
            match context.evaluate_predicate(predicate) {
                // A false predicate satisfies the constraint.
                Some(false) => return Ok(()),
                Some(true) => {}
                None => unsatisfied.push(predicate),
            }
        }

        let Some(domain) = unsatisfied.first().map(|p| p.get_domain()) else {
            return self.propagate_from_scratch_standard(context);
        };
        if unsatisfied.iter().any(|p| p.get_domain() != domain) {
            return Ok(());
        }

        if let &[predicate] = unsatisfied.as_slice() {
            self.propagate_from_scratch_standard(context.reborrow())?;

            if context.evaluate_predicate(predicate).is_some() {
                return Ok(());
            }
        }

        self.propagate_single_unsatisfied_domain(context, &unsatisfied)
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

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        if self.propagation == HypercubeLinearPropagation::Extended {
            self.propagate_from_scratch_extended(context)
        } else {
            self.propagate_from_scratch_standard(context)
        }
    }
}

/// What the extended propagation infers for a domain `x`, see
/// [`HypercubeLinearPropagator::propagate_single_unsatisfied_domain`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ExtendedInferences {
    /// The lower bound of `x` after the propagation.
    pub(crate) lower_bound: i32,
    /// The upper bound of `x` after the propagation.
    pub(crate) upper_bound: i32,
    /// The values strictly between the new bounds that are removed from the domain of `x`.
    pub(crate) removed_values: Vec<i32>,
}

/// Computes what the extended propagation infers for the domain `x` of the predicates in
/// `unsatisfied`, which are all the predicates of the hypercube that are not true.
///
/// The domain of `x` has the given bounds and contains the values for which `contains` holds;
/// `weight` is the weight of `x` in the linear, and `rest` is the bound of the linear minus the
/// lower bounds of the other terms. Returns `None` if no value is forbidden. The new bounds may
/// cross, in which case the propagation is a conflict.
///
/// This is shared by the propagator and by conflict analysis, which tests at which decision level
/// a learned constraint propagates.
pub(crate) fn extended_inferences(
    unsatisfied: &[Predicate],
    domain_lower_bound: i32,
    domain_upper_bound: i32,
    contains: impl Fn(i32) -> bool,
    weight: Option<i32>,
    rest: i64,
) -> Option<ExtendedInferences> {
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

    // The forbidden values of `R` are those `v` with `w * v > rest`.
    let (forbidden_lower, forbidden_upper) = match weight {
        None if rest < 0 => (i64::from(region_lower), i64::from(region_upper)),
        None => return None,
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
        return None;
    }

    // Both are within the bounds of the domain, so they fit in an i32.
    let forbidden_lower = forbidden_lower as i32;
    let forbidden_upper = forbidden_upper as i32;

    let mut lower_bound = domain_lower_bound;
    let mut upper_bound = domain_upper_bound;

    if forbidden_lower <= domain_lower_bound {
        // The lower part of the domain is forbidden, up to the first exception.
        lower_bound = exceptions
            .iter()
            .copied()
            .filter(|&e| e >= domain_lower_bound && e <= forbidden_upper)
            .min()
            .unwrap_or(forbidden_upper + 1);
    }

    if forbidden_upper >= domain_upper_bound {
        // The upper part of the domain is forbidden, down to the last exception.
        upper_bound = exceptions
            .iter()
            .copied()
            .filter(|&e| e <= domain_upper_bound && e >= forbidden_lower)
            .max()
            .unwrap_or(forbidden_lower - 1);
    }

    // The remaining forbidden values lie strictly inside the domain and are removed one by one.
    // Removing a large interval value by value is expensive, so it is skipped then.
    let interior_lower = forbidden_lower.max(lower_bound);
    let interior_upper = forbidden_upper.min(upper_bound);
    let removed_values =
        if i64::from(interior_upper) - i64::from(interior_lower) < MAX_INTERIOR_REMOVALS {
            (interior_lower..=interior_upper)
                .filter(|value| !exceptions.contains(value) && contains(*value))
                .collect()
        } else {
            vec![]
        };

    Some(ExtendedInferences {
        lower_bound,
        upper_bound,
        removed_values,
    })
}

/// The lower bound of `term` in the domains, computed in i64, since the scaled bound of a domain
/// need not fit in an i32.
fn term_lower_bound(domains: &impl ReadDomains, term: AffineView<DomainId>) -> i64 {
    let bound = if term.scale < 0 {
        domains.upper_bound(&term.inner)
    } else {
        domains.lower_bound(&term.inner)
    };
    i64::from(term.scale) * i64::from(bound) + i64::from(term.offset)
}

/// The predicate `[term >= lb(term)]` expressed over the domain of the term, so that it can be
/// represented even if the scaled bound does not fit in an i32.
fn term_lower_bound_predicate(domains: &impl ReadDomains, term: AffineView<DomainId>) -> Predicate {
    let domain = term.inner;
    if term.scale < 0 {
        let bound = domains.upper_bound(&domain);
        predicate![domain <= bound]
    } else {
        let bound = domains.lower_bound(&domain);
        predicate![domain >= bound]
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

    #[test]
    fn scaled_bounds_beyond_i32_do_not_overflow() {
        let mut state = State::default();

        let x = state.new_interval_variable(50_000, 100_000, None);

        // The lower bound of 50000 * x is 2.5e9, which does not fit in an i32, and exceeds the
        // bound 2e9, so the constraint is violated.
        let linear = LinearInequality::new([(NonZero::new(50_000).unwrap(), x)], 2_000_000_000)
            .expect("not trivially satisfiable");
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube: Hypercube::default(),
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
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

    /// When `[y >= 2]` becomes true, the watcher on it cannot move to a predicate over another
    /// domain. It stays on `[y >= 2]`, so the constraint is notified again when `[y >= 2]` becomes
    /// true after backtracking.
    #[test]
    fn extended_propagation_propagates_again_after_backtracking() {
        let mut state = extended_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);

        // [x >= 3] /\ [x <= 7] /\ [y >= 2] -> false.
        let hypercube =
            Hypercube::new([predicate![x >= 3], predicate![x <= 7], predicate![y >= 2]])
                .expect("not inconsistent");
        add_hypercube_linear(&mut state, hypercube, LinearInequality::trivially_false());
        assert!(state.propagate_to_fixed_point().is_ok());

        for _ in 0..2 {
            state.new_checkpoint();
            assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
            assert!(state.propagate_to_fixed_point().is_ok());
            assert!((3..=7).all(|value| !state.contains(x, value)));

            let _ = state.restore_to(0);
            assert!((3..=7).all(|value| state.contains(x, value)));
        }
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
