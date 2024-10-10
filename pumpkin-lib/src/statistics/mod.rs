//! Contains structures related to the statistic logging of the [`Solver`]
pub(crate) mod statistic_logger;
pub(crate) mod statistic_logging;

use std::fmt::Display;
use std::fmt::Write;

pub use statistic_logger::StatisticLogger;
pub use statistic_logging::configure_statistic_logging;
pub use statistic_logging::log_statistic;
pub use statistic_logging::log_statistic_postfix;
pub use statistic_logging::StatisticOptions;

#[cfg(doc)]
use crate::Solver;

/// A simple trait for defining a loggable statistic.
///
/// See [`create_statistics_struct!`] for creating a statistic struct automatically!
pub(crate) trait Statistic {
    /// Logs the [`Statistic`] using the provided [`StatisticLogger`].
    fn log(&self, statistic_logger: StatisticLogger);
}

impl<Value: Display> Statistic for Value {
    fn log(&self, mut statistic_logger: StatisticLogger) {
        write!(statistic_logger, "{self}").expect("Expected statistic to be logged");
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

        impl $crate::statistics::Statistic for $name {
            fn log(&self, statistic_logger: $crate::statistics::StatisticLogger) {
                $(self.$field.log(statistic_logger.attach_to_prefix(stringify!($field),)));+
            }
        }
    };
}
