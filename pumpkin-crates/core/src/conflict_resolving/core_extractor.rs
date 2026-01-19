use std::fmt::Debug;

use dyn_clone::DynClone;
use dyn_clone::clone_trait_object;

use crate::conflict_resolving::ConflictAnalysisContext;
use crate::conflict_resolving::LearnedNogood;
#[cfg(doc)]
use crate::engine::reason::ReasonStore;

clone_trait_object!(CoreExtractor);

/// A [`CoreExtractor`] is responsible for extracting a core from an unsatisfiable state under
/// assumptions.
///
/// See [`CoreExtractor::extract_core`] for more information.
pub trait CoreExtractor: Debug + DynClone {
    /// Returns an unsatisfiable core.
    ///
    /// We define an unsatisfiable core as a clause containing only negated assumption literals,
    /// which is implied by the formula. Alternatively, it is the negation of a conjunction of
    /// assumptions which cannot be satisfied together with the rest of the formula. The clause is
    /// not necessarily unique or minimal.
    ///
    /// The unsatisfiable core can be verified with reverse unit propagation (RUP).
    ///
    /// *Notes:*
    ///   - If the solver is not in an unsatisfied state, this method will panic.
    ///   - If the solver is in an unsatisfied state, but solving was done without assumptions, this
    ///     will return an empty vector.
    ///   - If the assumptions are inconsistent, i.e. both literal x and !x are assumed, an error is
    ///     returned, with the literal being one of the inconsistent assumptions.
    ///
    /// # Example usage
    /// ```rust
    /// // We construct the following SAT instance:
    /// //   (x0 \/ x1 \/ x2) /\ (x0 \/ !x1 \/ x2)
    /// // And solve under the assumptions:
    /// //   !x0 /\ x1 /\ !x2
    /// # use pumpkin_core::Solver;
    /// # use pumpkin_core::termination::Indefinite;
    /// # use pumpkin_core::results::SatisfactionResultUnderAssumptions;
    /// # use pumpkin_solver::default_core_extractor;
    /// # use pumpkin_solver::default_conflict_resolver;
    /// let mut solver = Solver::default();
    ///
    /// // We use a dummy constraint tag for this example.
    /// let constraint_tag = solver.new_constraint_tag();
    ///
    /// let x = vec![
    ///     solver.new_literal().get_true_predicate(),
    ///     solver.new_literal().get_true_predicate(),
    ///     solver.new_literal().get_true_predicate(),
    /// ];
    ///
    /// solver.add_clause([x[0], x[1], x[2]], constraint_tag);
    /// solver.add_clause([x[0], !x[1], x[2]], constraint_tag);
    ///
    /// let assumptions = [!x[0], x[1], !x[2]];
    /// let mut termination = Indefinite;
    /// let mut brancher = solver.default_brancher();
    /// let mut resolver = default_conflict_resolver();
    /// let result = solver.satisfy_under_assumptions(
    ///     &mut brancher,
    ///     &mut termination,
    ///     &mut resolver,
    ///     &assumptions,
    /// );
    ///
    /// if let SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(mut unsatisfiable) =
    ///     result
    /// {
    ///     {
    ///         let mut core_extractor = default_core_extractor();
    ///         let core = unsatisfiable.extract_core(&mut core_extractor);
    ///
    ///         // The order of the literals in the core is undefined, so we check for unordered
    ///         // equality.
    ///         assert_eq!(
    ///             core.len(),
    ///             assumptions.len(),
    ///             "The core has the length of the number of assumptions"
    ///         );
    ///         assert!(
    ///             core.iter().all(|&lit| assumptions.contains(&lit)),
    ///             "All literals in the core are assumptions"
    ///         );
    ///     }
    /// }
    /// ```
    fn extract_core(&mut self, context: &mut ConflictAnalysisContext) -> LearnedNogood;
}
