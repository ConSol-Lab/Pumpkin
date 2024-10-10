//! Contains structures related to the statistic logging of the [`Solver`]
pub(crate) mod statistic_logger;
pub(crate) mod statistic_logging;

use std::fmt::Display;

pub use statistic_logger::StatisticLogger;
pub use statistic_logging::configure_statistic_logging;
pub use statistic_logging::log_statistic;
pub use statistic_logging::log_statistic_postfix;
pub use statistic_logging::StatisticOptions;

#[cfg(doc)]
use crate::Solver;

pub(crate) trait LogStatistics {
    fn log_statistics(&self, statistic_logger: &StatisticLogger);
}

impl<Name: Display + Clone, Value: Display + Clone> LogStatistics for (Name, Value) {
    fn log_statistics(&self, statistic_logger: &StatisticLogger) {
        statistic_logger.log_statistic(self.0.clone(), self.1.clone())
    }
}

/// A macro for generating a struct for storing statistics.
///
/// # Example
/// ```rust
/// # use pumpkin_lib::create_statistics_struct;
/// create_statistics_struct!(Statistics {
///     number_of_calls: usize
/// });
///
/// let statistics = Statistics::default();
///
/// assert_eq!(statistics.number_of_calls, 0);
/// ```
#[macro_export]
macro_rules! create_statistics_struct {
    ($(#[$struct_documentation:meta])* $name:ident { $($(#[$variable_documentation:meta])* $field:ident : $type:ident),+ $(,)? }) => {
        $(#[$struct_documentation])*
        #[derive(Default, Debug, Copy, Clone)]
        pub(crate) struct $name {
            $($(#[$variable_documentation])* pub(crate) $field: $type),+
        }

        impl<Name: std::fmt::Display> $crate::statistics::LogStatistics for (Name, $name) {
            fn log_statistics(&self, statistic_logger: &$crate::statistics::StatisticLogger) {
                self.1.log_statistics(statistic_logger)
            }
        }

        impl $crate::statistics::LogStatistics for $name {
            fn log_statistics(
                &self,
                statistic_logger: &$crate::statistics::StatisticLogger
            ) {
                $((stringify!($field), self.$field).log_statistics(&statistic_logger));+
            }
        }
    };
}
