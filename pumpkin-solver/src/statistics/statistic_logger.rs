use std::fmt::Display;

use itertools::Itertools;

use super::statistic_logging::log_statistic;
#[cfg(doc)]
use crate::engine::propagation::Propagator;

/// Responsible for logging the statistics with the provided prefix; currently used when logging
/// the statistics of propagators.
#[derive(Debug, Default, Clone)]
pub struct StatisticLogger {
    /// The prefix which will be attached to the statistic name
    name_prefix: String,
}

impl StatisticLogger {
    pub fn new<Input: IntoIterator<Item = impl Display>>(name_prefix: Input) -> Self {
        Self {
            name_prefix: name_prefix.into_iter().join("_"),
        }
    }

    /// Attaches the provided `addition_to_prefix` to the stored internal prefix and returns a new
    /// [`StatisticLogger`] with these two prefixes.
    pub fn attach_to_prefix(&self, addition_to_prefix: impl Display) -> Self {
        Self {
            name_prefix: format!("{}_{}", self.name_prefix, addition_to_prefix),
        }
    }

    pub fn log_statistic(&self, value: impl Display) {
        log_statistic(&self.name_prefix, value);
    }
}
