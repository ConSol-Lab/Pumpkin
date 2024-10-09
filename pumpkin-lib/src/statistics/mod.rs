//! Contains structures related to the statistic logging of the [`Solver`]
pub(crate) mod statistic_logger;
pub mod statistic_logging;
pub use statistic_logger::StatisticLogger;

#[cfg(doc)]
use crate::Solver;

#[macro_export]
macro_rules! statistics {
    ($(#[$struct_documentation:meta])* $name:ident { $($(#[$variable_documentation:meta])* $field:ident : $type:ty),+ $(,)? }) => {
        use $crate::statistics::statistic_logging::log_statistic;

        $(#[$struct_documentation])*
        #[derive(Default, Debug, Copy, Clone)]
        pub(crate) struct $name {
            $($(#[$variable_documentation])* pub(crate) $field: $type),+
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
