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
    pub(crate) fn log(&self, _statistic_logger: StatisticLogger, verbose: bool) {
        log_statistic("nodes", self.engine_statistics.num_decisions);
        log_statistic("restarts", self.engine_statistics.num_restarts);
        log_statistic("peakDepth", self.engine_statistics.peak_depth);
        log_statistic("backjumps", self.engine_statistics.sum_of_backjumps);
        log_statistic(
            "solveTime",
            self.engine_statistics.time_spent_in_solver.as_secs_f64(),
        );
        if verbose {
            log_statistic("numberOfBackjumps", self.engine_statistics.num_backjumps);
        }
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
    /// The number of levels which were backjumped.
    ///
    /// For an individual backtrack due to a learned nogood, this is calculated according to the
    /// formula `CurrentDecisionLevel - 1 - BacktrackLevel` (i.e. how many levels (in total) has
    /// the solver backtracked and not backjumped)
    pub(crate) sum_of_backjumps: u64,
    /// The number of times a backjump (i.e. backtracking more than a single decision level due to
    /// a learned nogood) occurs.
    pub(crate) num_backjumps: u64,
}
