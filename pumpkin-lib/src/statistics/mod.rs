//! Contains structures related to the statistic logging of the [`Solver`]
pub(crate) mod statistic_logger;
pub mod statistic_logging;

pub use statistic_logger::StatisticLogger;

#[cfg(doc)]
use crate::Solver;
