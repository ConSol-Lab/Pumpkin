use std::fmt::Debug;

use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::statistics::StatisticLogger;

pub(crate) trait ConflictResolver: Debug {
    /// Restore the solver such that search can continue. If the solver cannot be repaired, then
    /// false is returned. Otherwise, true is returned.
    fn resolve_conflict(&mut self, context: ConflictAnalysisContext) -> bool;

    /// Logs statistics of the propagator using the provided [`StatisticLogger`].
    fn log_statistics(&self, _statistic_logger: StatisticLogger) {}
}
