use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::create_statistics_struct;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::Assignments;
use crate::statistics::log_statistic;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;

/// Structure responsible for storing several statistics of the solving process of the
/// [`ConstraintSatisfactionSolver`].
#[derive(Debug, Default)]
pub(crate) struct SolverStatistics {
    /// Core statistics of the solver engine (e.g. the number of decisions)
    pub(crate) engine_statistics: EngineStatistics,
    /// The statistics related to clause learning
    pub(crate) learned_clause_statistics: LearnedClauseStatistics,
}

impl SolverStatistics {
    pub(crate) fn log(
        &self,
        assignments: &Assignments,
        propagators: &PropagatorStore,
        statistic_logger: StatisticLogger,
        verbose: bool,
    ) {
        log_statistic("nodes", self.engine_statistics.num_decisions);
        log_statistic("failures", self.engine_statistics.num_conflicts);
        log_statistic("restarts", self.engine_statistics.num_restarts);
        log_statistic("variables", assignments.num_domains());
        log_statistic("propagators", propagators.num_propagators());
        log_statistic("propagations", self.engine_statistics.num_propagations);
        log_statistic("peakDepth", self.engine_statistics.peak_depth);
        log_statistic("nogoods", self.engine_statistics.num_conflicts);
        log_statistic("backjumps", self.engine_statistics.num_backjumps);
        log_statistic(
            "solveTime",
            self.engine_statistics.time_spent_in_solver as f64 / 1000_f64,
        );
        if verbose {
            self.learned_clause_statistics.log(statistic_logger)
        }
    }
}

/// Core statistics of the solver engine (e.g. the number of decisions)
#[derive(Debug, Default)]
pub(crate) struct EngineStatistics {
    /// The number of decisions taken by the solver
    pub(crate) num_decisions: u64,
    /// The number of conflicts encountered by the solver
    pub(crate) num_conflicts: u64,
    /// The number of times the solver has restarted
    pub(crate) num_restarts: u64,
    /// The average number of (integer) propagations made by the solver
    pub(crate) num_propagations: u64,
    /// The amount of time which is spent in the solver
    pub(crate) time_spent_in_solver: u64,
    /// The peak depth of the seach tree
    pub(crate) peak_depth: u64,
    /// The number of backjumps (i.e. when a learned nogood resulted in backtracking more than a
    /// single level)
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
