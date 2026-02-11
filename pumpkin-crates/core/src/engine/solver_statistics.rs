use crate::basic_types::time::Duration;
use crate::statistics::StatisticLogger;
use crate::statistics::log_statistic;

/// Structure responsible for storing several statistics of the solving process of the solver.
#[derive(Debug, Default)]
pub struct SolverStatistics {
    /// Core statistics of the solver engine (e.g. the number of decisions)
    pub(crate) engine_statistics: EngineStatistics,
}

impl SolverStatistics {
    pub(crate) fn log(&self, _statistic_logger: StatisticLogger, _verbose: bool) {
        log_statistic("nodes", self.engine_statistics.num_decisions);
        log_statistic("restarts", self.engine_statistics.num_restarts);
        log_statistic("peakDepth", self.engine_statistics.peak_depth);
        log_statistic(
            "solveTime",
            self.engine_statistics.time_spent_in_solver.as_secs_f64(),
        );
    }
}

/// Core statistics of the solver engine (e.g. the number of decisions)
#[derive(Debug, Default)]
pub(crate) struct EngineStatistics {
    /// The number of decisions taken by the solver
    pub(crate) num_decisions: u64,
    /// The number of times the solver has restarted
    pub(crate) num_restarts: u64,
    /// The amount of time which is spent in the solver.
    pub(crate) time_spent_in_solver: Duration,
    /// The peak depth of the seach tree
    pub(crate) peak_depth: u64,
}
