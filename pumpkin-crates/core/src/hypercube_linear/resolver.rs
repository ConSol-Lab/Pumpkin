use std::num::NonZero;

use itertools::Itertools;
use log::debug;
use log::trace;
use log::warn;

use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
#[cfg(feature = "hl-checks")]
use crate::containers::HashSet;
use crate::create_statistics_struct;
use crate::hypercube_linear::BoundPredicate;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearConstructor;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::predicate_heap::PredicateHeap;
use crate::predicates::Predicate;
use crate::propagation::ExplanationContext;
use crate::state::Conflict;
use crate::state::CurrentNogood;
use crate::state::EmptyDomainConflict;
use crate::state::State;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;

create_statistics_struct!(ResolverStatistics {
    num_propositional_resolutions: usize,
    num_fourier_resolutions: usize,
    num_conflicts: usize,
});

#[derive(Clone, Debug, Default)]
pub struct HypercubeLinearResolver {
    /// The hypercube that is being discovered during conflict resolution.
    working_hypercube: Hypercube,
    /// Predicates in the conflict hypercube that are set at the decision level of the conflict.
    predicates_to_explain: PredicateHeap,
    /// The linear inequality of the conflict constraint.
    conflicting_linear: LinearInequality,

    /// The statistics gathered by the resolver.
    statistics: ResolverStatistics,

    /// A temporary buffer for storing reasons.
    reason_buffer: Vec<Predicate>,

    /// Set of all learned constraints.
    ///
    /// Used to detect when re-learning the same constraint again.
    #[cfg(feature = "hl-checks")]
    learned_constraints: HashSet<(Hypercube, LinearInequality)>,
}

impl ConflictResolver for HypercubeLinearResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        debug!("Resolving conflict with hypercube linear resolution");

        self.statistics.num_conflicts += 1;

        let conflict = match context.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator(conflict) => conflict.into(),
            StoredConflictInfo::EmptyDomain(conflict) => conflict.into(),
            _ => unreachable!("can only resolve empty domain or propagator conflicts"),
        };

        self.resolve_conflict_impl(context.state, conflict);
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
    fn resolve_conflict_impl(&mut self, state: &mut State, conflict: Conflict) {
        let learned_constraint = self.learn_hypercube_linear(state, conflict);

        #[cfg(feature = "hl-checks")]
        self.assert_new_constraint(learned_constraint.clone());

        let predicates_str = learned_constraint.hypercube.iter_predicates().join("& ");

        let _ = state.restore_to(learned_constraint.propagates_at);

        let constraint_tag = state.new_constraint_tag();
        let handle = state.add_propagator(HypercubeLinearConstructor {
            hypercube: learned_constraint.hypercube,
            linear: learned_constraint.linear,
            constraint_tag,
        });

        debug!(
            "Learned {} -> {} with ID {}",
            predicates_str,
            "false", // TODO: Print linear
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
        self.compute_conflicting_hypercube_linear(state, conflict);

        let mut last_tp = usize::MAX;

        loop {
            trace!(
                "conflict constraint: {} -> {}",
                self.working_hypercube
                    .iter_predicates()
                    .chain(self.predicates_to_explain.iter())
                    .join(" & "),
                "false"
            );

            if let Some(dl) = self.will_propagate_on_previous_dl(state) {
                return self.extract_learned_hypercube_linear(dl);
            }

            let pivot = self
                .predicates_to_explain
                .pop()
                .expect("there are at least two predicates to explain");

            let tp = state
                .trail_position(pivot)
                .expect("all predicates are true");

            trace!("applying HL resolution on {pivot} @ {tp}");
            assert!(last_tp >= tp, "last_tp = {last_tp}, tp = {tp}");
            last_tp = tp;

            let Some(explanation) = self.explain(state, pivot) else {
                trace!("  implied predicate, skipping");
                continue;
            };

            trace!(
                "explanation = {} -> false",
                explanation.hypercube.iter_predicates().join(" & ")
            );

            self.resolve(state, pivot, explanation);

            trace!(
                "resolvent: {} -> {}",
                self.working_hypercube
                    .iter_predicates()
                    .chain(self.predicates_to_explain.iter())
                    .join(" & "),
                "false"
            );
        }
    }

    fn resolve(&mut self, state: &mut State, pivot: Predicate, explanation: HypercubeLinear) {
        self.fourier_resolve(pivot, &explanation);
        self.propositional_resolve(state, pivot, explanation);
    }

    fn explain(&mut self, state: &mut State, pivot: Predicate) -> Option<HypercubeLinear> {
        assert!(self.reason_buffer.is_empty());

        let maybe_inference_code =
            state.get_propagation_reason(pivot, &mut self.reason_buffer, CurrentNogood::empty());

        // Enqueue predicates to be explained.
        //
        // For this we do some borrow-checker evasion.
        let reason_buffer = std::mem::take(&mut self.reason_buffer);
        for predicate in reason_buffer.iter().copied() {
            self.add_hypercube_predicate(state, predicate);

            // DEBUGGING
            let tp = state
                .trail_position(predicate)
                .expect("all predicates are true");

            let pivot_tp = state
                .trail_position(pivot)
                .expect("all predicates are true");

            assert!(pivot_tp >= tp, "pivot_tp = {pivot_tp}, tp = {tp}");
            // END DEBUGGING
        }
        self.reason_buffer = reason_buffer;

        if maybe_inference_code.is_some() {
            Some(HypercubeLinear {
                hypercube: Hypercube::new(self.reason_buffer.drain(..).chain([!pivot]))
                    .expect("reason contains inconsistent predicates"),
                linear: LinearInequality::trivially_false(),
            })
        } else {
            // In this case the explanation explains the semantics of the atomic
            // constraint. That results in an inconsistent hypercube, and we can just
            // skip any resolution.
            //
            // We do need to clear the reason buffer for later use though.
            self.reason_buffer.clear();

            None
        }
    }

    /// Computes the conflicting hypercube linear constraint.
    ///
    /// The hypercube is added to the hypercube on `self`, and the linear inequality is
    /// returned here.
    fn compute_conflicting_hypercube_linear(&mut self, state: &mut State, conflict: Conflict) {
        match conflict {
            Conflict::Propagator(propagator_conflict) => {
                trace!("Converting propagator conflict to hypercube");

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

        let mut clausal_conflict = vec![!trigger_predicate];
        let _ = state.reason_store.get_or_compute(
            trigger_reason,
            ExplanationContext::without_working_nogood(
                &state.assignments,
                state.trail_len() - 1,
                &mut state.notification_engine,
            ),
            &mut state.propagators,
            &mut clausal_conflict,
        );

        // let pid = state.reason_store.get_propagator(trigger_reason);
        // dbg!(state.propagators[pid].name());

        trace!("conflicting predicate = {trigger_predicate:?}");
        // dbg!(&clausal_conflict);

        assert!(
            clausal_conflict
                .iter()
                .all(|&predicate| state.truth_value(predicate) == Some(true))
        );

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
            .unwrap_or_else(|| panic!("adding untrue predicate {predicate} to hypercube"));

        if checkpoint == state.get_checkpoint() {
            self.predicates_to_explain.push(predicate, state);
        } else {
            self.working_hypercube = std::mem::take(&mut self.working_hypercube)
                .with_predicate(predicate)
                .expect("cannot create trivially false hypercube");
        }
    }

    /// Build the learned hypercube linear from the current state of the resolver.
    fn extract_learned_hypercube_linear(&mut self, propagates_at: usize) -> LearnedHypercubeLinear {
        let hypercube = std::mem::take(&mut self.working_hypercube)
            .with_predicates(self.predicates_to_explain.drain())
            .expect("can never encounter inconsistent hypercube");

        LearnedHypercubeLinear {
            hypercube,
            linear: std::mem::take(&mut self.conflicting_linear),
            propagates_at,
        }
    }

    /// Ensures the learned constraint is a new constraint, rather than a previously learned
    /// one.
    #[cfg(feature = "hl-checks")]
    fn assert_new_constraint(&mut self, constraint: LearnedHypercubeLinear) {
        assert!(
            self.learned_constraints
                .insert((constraint.hypercube, constraint.linear)),
            "relearned the same constraint"
        );
    }

    fn fourier_resolve(&self, _pivot: Predicate, explanation: &HypercubeLinear) {
        // We leave it for now.
        assert!(self.conflicting_linear.is_trivially_false());
        assert!(explanation.linear.is_trivially_false());
    }

    fn propositional_resolve(
        &mut self,
        state: &State,
        pivot: Predicate,
        mut explanation: HypercubeLinear,
    ) {
        trace!("applying propositional resolution on {pivot}");

        let pivot_as_bound = BoundPredicate::new(pivot);

        // Both the conflict and the explanation are weakened on the pivot. This ensures
        // the propositional resolution removes all contribution of the pivot to the
        // conflict in the linear inequalities.
        if let Some(bound_predicate) = pivot_as_bound {
            self.conflicting_linear.weaken_to_zero(bound_predicate);

            // Then, we have to do the same on the explanation.
            explanation.linear.weaken_to_zero(bound_predicate);
        }

        for predicate in explanation.hypercube.iter_predicates() {
            let truth_value = state
                .truth_value(predicate)
                .expect("all predicates in explanation hypercube are assigned");

            // If the predicate is false, then it is propagated. In that case, it does
            // not feature in the resolvent so we can continue to the next predicate.
            if !truth_value {
                continue;
            }

            let checkpoint = state
                .get_checkpoint_for_predicate(predicate)
                .expect("everything is assigned in the hypercube");

            if checkpoint < state.get_checkpoint() {
                // If the predicate is assigned at a previous decision level, it will
                // be part of the hypercube of the final learned constraint.
                self.working_hypercube = std::mem::take(&mut self.working_hypercube)
                    .with_predicate(predicate)
                    .expect("predicate is true so is not inconsistent with working hypercube");
            } else {
                // If the predicate is assiged at the current decision level, it may
                // need to be explained in the future.
                assert_eq!(checkpoint, state.get_checkpoint());
                self.predicates_to_explain.push(predicate, state);
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

            if !self.propagates_at(state, trail_position) {
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
    fn propagates_at(&self, state: &State, trail_position: usize) -> bool {
        // TODO: Optimize this
        let final_hypercube = self
            .working_hypercube
            .clone()
            .with_predicates(self.predicates_to_explain.iter())
            .expect("no inconsistent hypercube");

        // Get the predicates that are not assigned to true.
        let unsatisfied_predicates_in_hypercubes = final_hypercube
            .iter_predicates()
            .filter(|&predicate| {
                state
                    .assignments
                    .evaluate_predicate_at_trail_position(predicate, trail_position)
                    != Some(true)
            })
            .collect::<Vec<_>>();

        if unsatisfied_predicates_in_hypercubes.len() > 1 {
            // If more than one predicate remains unassigned, we cannot do anything.
            return false;
        }

        let slack = self.compute_linear_slack_at_trail_position(state, trail_position);

        if unsatisfied_predicates_in_hypercubes.len() == 1 {
            let unassigned_predicate = unsatisfied_predicates_in_hypercubes[0];

            if slack < 0 {
                return true;
            } else if let Some(term) = self
                .conflicting_linear
                .term_for_domain(unassigned_predicate.get_domain())
            {
                let term_lower_bound =
                    term.lower_bound_at_trail_position(&state.assignments, trail_position);
                let new_upper_bound = match i32::try_from(slack + i64::from(term_lower_bound)) {
                    Ok(bound) => bound,
                    Err(_) => return false,
                };

                return new_upper_bound
                    < term.upper_bound_at_trail_position(&state.assignments, trail_position);
            }
        } else {
            assert!(unsatisfied_predicates_in_hypercubes.is_empty());
            return self.linear_propagates_at(state, trail_position);
        }

        false
    }

    /// Returns true if the given linear propagates at the given trail position.
    fn linear_propagates_at(&self, state: &State, trail_position: usize) -> bool {
        if self.conflicting_linear.is_trivially_false() {
            return true;
        }

        let slack = self.compute_linear_slack_at_trail_position(state, trail_position);

        for term in self.conflicting_linear.terms() {
            let term_lower_bound =
                i64::from(term.lower_bound_at_trail_position(&state.assignments, trail_position));
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

            if new_term_upper_bound
                < term.upper_bound_at_trail_position(&state.assignments, trail_position)
            {
                return true;
            }
        }

        false
    }

    fn compute_linear_slack_at_trail_position(&self, state: &State, trail_position: usize) -> i64 {
        let lower_bound_terms = self
            .conflicting_linear
            .terms()
            .map(|term| {
                i64::from(term.lower_bound_at_trail_position(&state.assignments, trail_position))
            })
            .sum::<i64>();

        i64::from(self.conflicting_linear.bound()) - lower_bound_terms
    }
}

struct HypercubeLinear {
    hypercube: Hypercube,
    linear: LinearInequality,
}
