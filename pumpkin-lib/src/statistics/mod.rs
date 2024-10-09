//! Contains structures related to the statistic logging of the [`Solver`]
pub(crate) mod statistic_logger;
pub mod statistic_logging;
pub use statistic_logger::StatisticLogger;

#[cfg(doc)]
use crate::Solver;

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
    ($(#[$struct_documentation:meta])* $name:ident { $($(#[$variable_documentation:meta])* $field:ident : $type:ty),+ $(,)? }) => {
        use $crate::statistics::StatisticLogger;

        $(#[$struct_documentation])*
        #[derive(Default, Debug, Copy, Clone)]
        pub(crate) struct $name {
            $($(#[$variable_documentation])* pub(crate) $field: $type),+
        }

        impl $name {
            pub(crate) fn log_statistics(
                &self,
                statistic_logger: StatisticLogger
            ) {
                $(statistic_logger.log_statistic(stringify!($field), self.$field.to_string()));+
            }
        }
    };
}
