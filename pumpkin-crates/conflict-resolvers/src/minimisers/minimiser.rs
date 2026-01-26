use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
#[cfg(doc)]
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::statistics::StatisticLogger;

/// A trait for the behaviour of nogood minimisation approaches.
///
/// See [`NogoodMinimiser::minimise`] for more information.
pub trait NogoodMinimiser: Default {
    /// Takes as input a nogood represented by a [`Vec`] of [`Predicate`]s and minimises the
    /// nogood by removing redundant [`Predicate`]s.
    fn minimise(&mut self, context: &mut ConflictAnalysisContext, nogood: &mut Vec<Predicate>);

    /// Logs statistics of the nogood minimiser using the provided [`StatisticLogger`].
    ///
    /// It is recommended to create a struct through the [`create_statistics_struct!`] macro!
    fn log_statistics(&self, _statistic_logger: StatisticLogger) {}
}
