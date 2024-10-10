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

    pub fn attach_to_prefix(&self, addition_to_prefix: impl Display) -> Self {
        Self {
            name_prefix: format!("{}_{}", self.name_prefix, addition_to_prefix),
        }
    }
}

impl std::fmt::Write for StatisticLogger {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        log_statistic(self.name_prefix.clone(), s);
        Ok(())
    }
}
