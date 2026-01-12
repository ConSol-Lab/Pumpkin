use crate::engine::conflict_analysis::MinimisationContext;
use crate::predicates::Predicate;

/// A trait for the behaviour of nogood minimisation approaches.
pub trait NogoodMinimiser: Default {
    /// Takes as input a nogood represented by a [`Vec`] of [`Predicate`]s and minimises the
    /// nogood.
    fn minimise(&mut self, context: MinimisationContext, nogood: &mut Vec<Predicate>);
}
