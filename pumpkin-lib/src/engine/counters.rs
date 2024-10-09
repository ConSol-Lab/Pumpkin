use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::statistics;

statistics!(
/// Structure responsible for storing several statistics of the solving process of the
/// [`ConstraintSatisfactionSolver`].
    Counters {
        /// The number of decisions taken by the solver
         num_decisions: u64,
        /// The number of conflicts encountered by the solver
         num_conflicts: u64,
        /// The number of times the solver has restarted
         num_restarts: u64,
        /// The average number of elements in the conflict explanation
         average_conflict_size: CumulativeMovingAverage,
        /// The average number of (integer) propagations made by the solver
         num_propagations: u64,
        /// The number of learned clauses which have a size of 1
         num_unit_clauses_learned: u64,
        /// The average length of the learned clauses
         average_learned_clause_length: CumulativeMovingAverage,
        /// The amount of time which is spent in the solver
         time_spent_in_solver: u64,
        /// The average number of levels which have been backtracked by the solver (e.g. when a learned clause is created)
         average_backtrack_amount: CumulativeMovingAverage,
        /// The average number of literals removed by recursive minimisation during conflict analysis
         average_number_of_removed_literals_recursive: CumulativeMovingAverage,
        /// The average number of literals removed by semantic minimisation during conflict analysis
         average_number_of_removed_literals_semantic: CumulativeMovingAverage,
    }
);
