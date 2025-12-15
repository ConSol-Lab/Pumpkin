use crate::predicates::Predicate;
use crate::propagation::MinimisationContext;

/// A trait for the behaviour of nogood minimisation approaches.
pub(crate) trait NogoodMinimiser: Default {
    /// Takes as input a nogood represented by a [`Vec`] of [`Predicate`]s and minimises the
    /// nogood.
    fn minimise(&mut self, context: MinimisationContext, nogood: &mut Vec<Predicate>);
}
