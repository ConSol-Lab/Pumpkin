use crate::engine::conflict_analysis::MinimisationContext;
use crate::predicates::Predicate;

/// A trait for the behaviour of nogood minimisation approaches.
pub(crate) trait NogoodMinimiser: Default {
    /// Takes as input a [`LearnedNogood`] and minimises the nogood based on some strategy.
    fn minimise(&mut self, context: MinimisationContext, nogood: &mut Vec<Predicate>);
}
