use crate::basic_types::StoredConflictInfo;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::ConflictResolver;
#[cfg(feature = "hl-checks")]
use crate::containers::HashSet;
use crate::create_statistics_struct;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::predicate_heap::PredicateHeap;
use crate::state::Conflict;
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
}

impl HypercubeLinearResolver {
    fn resolve_conflict_impl(&mut self, state: &mut State, conflict: Conflict) {
        let learned_constraint = self.learn_hypercube_linear(state, conflict);

        #[cfg(feature = "hl-checks")]
        self.assert_new_constraint(learned_constraint.clone());

        todo!()
    }

    fn learn_hypercube_linear(
        &mut self,
        state: &mut State,
        conflict: Conflict,
    ) -> LearnedHypercubeLinear {
        todo!()
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
