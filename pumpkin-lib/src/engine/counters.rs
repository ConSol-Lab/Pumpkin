use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::statistics::statistic_logging::log_statistic;

macro_rules! statistics {
    ($name:ident { $($field:ident : $type:ty),+ $(,)? }) => {
        #[derive(Default, Debug, Copy, Clone)]
        pub(crate) struct $name {
            $(pub(crate) $field: $type),+
        }

        impl $name {
            pub(crate) fn log_statistics(
                &self,
            ) {
                $(log_statistic(self.$field, self.$field.to_string()));+
            }
        }
    };
}

statistics!(Counters {
    num_decisions: u64,
    num_conflicts: u64,
    num_restarts: u64,
    average_conflict_size: CumulativeMovingAverage,
    num_propagations: u64,
    num_unit_clauses_learned: u64,
    average_learned_clause_length: CumulativeMovingAverage,
    time_spent_in_solver: u64,
    average_backtrack_amount: CumulativeMovingAverage,
    average_number_of_removed_literals_recursive: CumulativeMovingAverage,
    average_number_of_removed_literals_semantic: CumulativeMovingAverage,
});

///// Structure responsible for storing several statistics of the solving process of the
///// [`ConstraintSatisfactionSolver`].
//#[derive(Default, Debug, Copy, Clone)]
// pub(crate) struct Counters {
//    /// The number of decisions taken by the solver
//    pub(crate) num_decisions: u64,
//    /// The number of conflicts encountered by the solver
//    pub(crate) num_conflicts: u64,
//    /// The number of times the solver has restarted
//    pub(crate) num_restarts: u64,
//    /// The average number of elements in the conflict explanation
//    pub(crate) average_conflict_size: CumulativeMovingAverage,
//    /// The average number of (integer) propagations made by the solver
//    pub(crate) num_propagations: u64,
//    /// The number of learned clauses which have a size of 1
//    pub(crate) num_unit_clauses_learned: u64,
//    /// The average length of the learned clauses
//    pub(crate) average_learned_clause_length: CumulativeMovingAverage,
//    /// The amount of time which is spent in the solver
//    pub(crate) time_spent_in_solver: u64,
//    /// The average number of levels which have been backtracked by the solver (e.g. when a
// learned    /// clause is created)
//    pub(crate) average_backtrack_amount: CumulativeMovingAverage,
//    /// The average number of literals removed by recursive minimisation during conflict analysis
//    pub(crate) average_number_of_removed_literals_recursive: CumulativeMovingAverage,
//    /// The average number of literals removed by semantic minimisation during conflict analysis
//    pub(crate) average_number_of_removed_literals_semantic: CumulativeMovingAverage,
//}
// impl Counters {
//    pub(crate) fn log_statistics(&self) {
//        log_statistic("numberOfDecisions", self.num_decisions);
//        log_statistic("numberOfConflicts", self.num_conflicts);
//        log_statistic("numberOfRestarts", self.num_restarts);
//        log_statistic(
//            "averageSizeOfConflictExplanation",
//            self.average_conflict_size.value(),
//        );
//        log_statistic("numberOfPropagations", self.num_propagations);
//        log_statistic("numberOfLearnedUnitClauses", self.num_unit_clauses_learned);
//        log_statistic(
//            "averageLearnedClauseLength",
//            self.average_learned_clause_length.value(),
//        );
//        log_statistic("timeSpentInSolverInMilliseconds", self.time_spent_in_solver);
//        log_statistic(
//            "averageBacktrackAmount",
//            self.average_backtrack_amount.value(),
//        );
//        log_statistic(
//            "averageNumberOfRemovedLiteralsRecursive",
//            self.average_number_of_removed_literals_recursive.value(),
//        );
//        log_statistic(
//            "averageNumberOfRemovedLiteralsSemantic",
//            self.average_number_of_removed_literals_semantic.value(),
//        );
//    }
//}
