use std::fmt::Display;

use super::statistic_logging::log_statistic;
#[cfg(doc)]
use crate::engine::propagation::Propagator;

/// Responsible for logging the statistics with the provided prefix; currently used when logging
/// the statistics of propagators.
#[derive(Debug, Default)]
pub struct StatisticLogger {
    /// The prefix which will be attached to the statistic name
    name_prefix: String,
}

impl StatisticLogger {
    pub fn new(name_prefix: impl Display) -> Self {
        Self {
            name_prefix: name_prefix.to_string(),
        }
    }

    /// Logs the statistic with the provided `name` and `value`.
    #[allow(unused)]
    pub fn log_statistic(&self, name: impl Display, value: impl Display) {
        log_statistic(format!("{}{name}", self.name_prefix), value);
    }
}
