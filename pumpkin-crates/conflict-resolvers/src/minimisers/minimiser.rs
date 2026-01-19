use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::predicates::Predicate;

/// A trait for the behaviour of nogood minimisation approaches.
///
/// See [`NogoodMinimiser::minimise`] for more information.
pub trait NogoodMinimiser: Default {
    /// Takes as input a nogood represented by a [`Vec`] of [`Predicate`]s and minimises the
    /// nogood by removing redundant [`Predicate`]s.
    fn minimise(&mut self, context: &mut ConflictAnalysisContext, nogood: &mut Vec<Predicate>);
}
