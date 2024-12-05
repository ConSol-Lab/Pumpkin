use std::fmt::Display;

use crate::basic_types::moving_averages::MovingAverage;
use crate::propagators::CumulativeStatistics;
use crate::propagators::OverIntervalTimeTableType;
use crate::variables::IntegerVariable;

#[derive(Default, Debug, Clone, Copy)]
pub enum CumulativeMergeStrategy {
    #[default]
    Never,
    Constant(usize),
    AverageTimeTableSize,
}

impl Display for CumulativeMergeStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CumulativeMergeStrategy::Never => write!(f, "never"),
            CumulativeMergeStrategy::Constant(_) => write!(f, "constant"),
            CumulativeMergeStrategy::AverageTimeTableSize => write!(f, "average-time-table"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct MergeChecker {
    strategy: CumulativeMergeStrategy,
    number_of_iterations_since_last_merge: usize,
}

const NUMBER_OF_ITERATIONS_BEFORE_MERGE_IS_CONSIDERED: usize = 1000;

impl MergeChecker {
    pub(super) fn new(strategy: CumulativeMergeStrategy) -> Self {
        Self {
            strategy,
            number_of_iterations_since_last_merge: 0,
        }
    }

    pub(super) fn should_merge<Var: IntegerVariable + 'static>(
        &mut self,
        time_table: &OverIntervalTimeTableType<Var>,
        statistics: &CumulativeStatistics,
    ) -> bool {
        if time_table.len() <= 1 {
            return false;
        }

        match self.strategy {
            CumulativeMergeStrategy::Never => false,
            CumulativeMergeStrategy::Constant(number_of_iterations_without_merge) => {
                if self.number_of_iterations_since_last_merge >= number_of_iterations_without_merge
                {
                    self.number_of_iterations_since_last_merge = 0;
                    true
                } else {
                    self.number_of_iterations_since_last_merge += 1;
                    false
                }
            }
            CumulativeMergeStrategy::AverageTimeTableSize => {
                if self.number_of_iterations_since_last_merge
                    > NUMBER_OF_ITERATIONS_BEFORE_MERGE_IS_CONSIDERED
                {
                    time_table.len() > statistics.average_size_of_time_table.value() as usize
                } else {
                    self.number_of_iterations_since_last_merge += 1;
                    false
                }
            }
        }
    }
}
