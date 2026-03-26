use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
#[cfg(feature = "hl-checks")]
use crate::containers::HashSet;
use crate::create_statistics_struct;
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

create_statistics_struct!(ResolverStatistics {
    num_propositional_resolutions: usize,
    num_fourier_resolutions: usize,
});

#[derive(Clone, Debug)]
pub struct HypercubeLinearResolver {
    /// The hypercube that is being discovered during conflict resolution.
    working_hypercube: Hypercube,
    /// Predicates in the conflict hypercube that are set at the decision level of the conflict.
    predicates_to_explain: PredicateHeap,
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
        let conflict = match context.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator(conflict) => conflict.into(),
            StoredConflictInfo::EmptyDomain(conflict) => conflict.into(),
            _ => unreachable!("can only resolve empty domain or propagator conflicts"),
        };

        self.resolve_conflict_impl(context.state, conflict);
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

        let _ = state.restore_to(learned_constraint.propagates_at);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube: learned_constraint.hypercube,
            linear: learned_constraint.linear,
            constraint_tag,
        });
    }

    /// Learns a conflicting hypercube linear that would have propagated at an earlier
    /// checkpoint.
    fn learn_hypercube_linear(
        &mut self,
        state: &mut State,
        conflict: Conflict,
    ) -> LearnedHypercubeLinear {
        let mut conflicting_linear = self.compute_conflicting_hypercube_linear(state, conflict);

        while self.predicates_to_explain.len() > 1 {
            let pivot = self
                .predicates_to_explain
                .pop()
                .expect("there are at least two predicates to explain");

            self.explain(state, pivot);
            conflicting_linear = self.resolve(state, pivot, conflicting_linear);
        }

        self.extract_learned_hypercube_linear(conflicting_linear, state.get_checkpoint() - 1)
    }

    fn resolve(
        &mut self,
        state: &mut State,
        pivot: Predicate,
        conflicting_linear: LinearInequality,
    ) -> LinearInequality {
        let conflicting_linear = self.fourier_resolve(pivot, conflicting_linear);
        let conflicting_linear = self.propositional_resolve(pivot, conflicting_linear);

        conflicting_linear
    }

    fn explain(&mut self, state: &mut State, pivot: Predicate) {
        let _ =
            state.get_propagation_reason(pivot, &mut self.reason_buffer, CurrentNogood::empty());
    }

    /// Computes the conflicting hypercube linear constraint.
    ///
    /// The hypercube is added to the hypercube on `self`, and the linear inequality is
    /// returned here.
    fn compute_conflicting_hypercube_linear(
        &mut self,
        state: &mut State,
        conflict: Conflict,
    ) -> LinearInequality {
        match conflict {
            Conflict::Propagator(propagator_conflict) => {
                trace!("Converting propagator conflict to hypercube");

                for predicate in propagator_conflict.conjunction {
                    self.add_hypercube_predicate(state, predicate);
                }

                LinearInequality::trivially_false()
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
    ) -> LinearInequality {
        let EmptyDomainConflict {
            trigger_reason,
            trigger_predicate,
            ..
        } = empty_domain_conflict;

        let trigger_reason =
            trigger_reason.expect("cannot resolve conflict that was triggered by an assumption");

        trace!("{trigger_predicate:?} caused an empty domain, computing conflict constraint");

        let mut clausal_conflict = vec![];
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

        trace!("conflicting predicate = {trigger_predicate:?}");

        for predicate in clausal_conflict {
            self.add_hypercube_predicate(state, predicate);
        }

        LinearInequality::trivially_false()
    }

    /// Adds a predicate to the conflicting hypercube.
    ///
    /// Depending on the checkpoint that the predicate is assigned, the predicate is either
    /// explained further or stored as part of the final learned constraint.
    fn add_hypercube_predicate(&mut self, state: &State, predicate: Predicate) {
        let checkpoint = state
            .get_checkpoint_for_predicate(predicate)
            .expect("can only add hypercube predicates that are true");

        if checkpoint == state.get_checkpoint() {
            self.predicates_to_explain.push(predicate, state);
        } else {
            self.working_hypercube = std::mem::take(&mut self.working_hypercube)
                .with_predicate(predicate)
                .expect("cannot create trivially false hypercube");
        }
    }

    /// Build the learned hypercube linear from the current state of the resolver.
    fn extract_learned_hypercube_linear(
        &mut self,
        conflicting_linear: LinearInequality,
        propagates_at: usize,
    ) -> LearnedHypercubeLinear {
        let hypercube = std::mem::take(&mut self.working_hypercube)
            .with_predicates(self.predicates_to_explain.drain())
            .expect("can never encounter inconsistent hypercube");

        LearnedHypercubeLinear {
            hypercube,
            linear: conflicting_linear,
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
}
