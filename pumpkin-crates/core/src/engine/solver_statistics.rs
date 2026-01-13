use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::time::Duration;
use crate::create_statistics_struct;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::statistics::log_statistic;

/// Structure responsible for storing several statistics of the solving process of the solver.
#[derive(Debug, Default)]
pub struct SolverStatistics {
    /// Core statistics of the solver engine (e.g. the number of decisions)
    pub(crate) engine_statistics: EngineStatistics,
    /// The statistics related to clause learning
    pub(crate) learned_clause_statistics: LearnedClauseStatistics,
}

impl SolverStatistics {
    pub(crate) fn log(&self, statistic_logger: StatisticLogger, verbose: bool) {
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
            self.learned_clause_statistics.log(statistic_logger)
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

create_statistics_struct!(
    /// The statistics related to clause learning
    LearnedClauseStatistics {
        /// The average number of elements in the conflict explanation
        average_conflict_size: CumulativeMovingAverage<u64>,
        /// The average number of atomic constraints removed by recursive minimisation during conflict analysis
        average_number_of_removed_atomic_constraints_recursive: CumulativeMovingAverage<u64>,
        /// The average number of atomic constraints removed by semantic minimisation during conflict analysis
        average_number_of_removed_atomic_constraints_semantic: CumulativeMovingAverage<u64>,
        /// The number of learned clauses which have a size of 1
        num_unit_nogoods_learned: u64,
        /// The average length of the learned nogood
        average_learned_nogood_length: CumulativeMovingAverage<u64>,
        /// The average number of levels which have been backtracked by the solver (e.g. when a learned clause is created)
        average_backtrack_amount: CumulativeMovingAverage<u64>,
        /// The average literal-block distance (LBD) metric for newly added learned nogoods
        average_lbd: CumulativeMovingAverage<u64>,
});
