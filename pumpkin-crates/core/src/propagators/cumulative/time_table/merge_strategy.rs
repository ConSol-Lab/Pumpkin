use std::fmt::Display;

use clap::ValueEnum;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::propagators::OverIntervalTimeTableType;
use crate::variables::IntegerVariable;

#[derive(Default, Debug, Clone, Copy, ValueEnum)]
pub enum CumulativeMergeStrategy {
    #[default]
    Never,
    Constant,
    AverageSize,
}

impl Display for CumulativeMergeStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CumulativeMergeStrategy::Never => write!(f, "never"),
            CumulativeMergeStrategy::Constant => write!(f, "constant"),
            CumulativeMergeStrategy::AverageSize => write!(f, "average-size"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct MergeChecker {
    strategy: CumulativeMergeStrategy,
    constant: u32,
    number_of_iterations_since_last_merge: usize,
    average_size_of_time_table: CumulativeMovingAverage<usize>,
}

impl MergeChecker {
    pub(super) fn new(strategy: CumulativeMergeStrategy, constant: u32) -> Self {
        let mut average_size_of_time_table = CumulativeMovingAverage::default();
        average_size_of_time_table.add_term(20);

        Self {
            strategy,
            constant,
            number_of_iterations_since_last_merge: 0,
            average_size_of_time_table,
        }
    }

    pub(super) fn should_merge<Var: IntegerVariable + 'static>(
        &mut self,
        time_table: &OverIntervalTimeTableType<Var>,
    ) -> bool {
        if time_table.len() <= 1 {
            return false;
        }

        match self.strategy {
            CumulativeMergeStrategy::Never => false,
            CumulativeMergeStrategy::Constant => {
                if self.number_of_iterations_since_last_merge >= self.constant as usize {
                    self.number_of_iterations_since_last_merge = 0;
                    true
                } else {
                    self.number_of_iterations_since_last_merge += 1;
                    false
                }
            }
            CumulativeMergeStrategy::AverageSize => {
                let should_recalculate =
                    time_table.len() > self.average_size_of_time_table.value() as usize;
                if should_recalculate {
                    self.number_of_iterations_since_last_merge = 0;
                    true
                } else {
                    self.number_of_iterations_since_last_merge += 1;
                    false
                }
            }
        }
    }

    pub(super) fn has_recalculated(&mut self) {
        self.number_of_iterations_since_last_merge = 0;
    }
}
